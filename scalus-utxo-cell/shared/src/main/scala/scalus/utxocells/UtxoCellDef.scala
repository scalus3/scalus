package scalus.utxocells

import scalus.uplc.CompiledPlutus
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{AssetName, PolicyId, ScriptHash, Utxo, Utxos, Value}
import scalus.cardano.node.{UtxoQuery, UtxoSource}
import scalus.cardano.txbuilder.{TxBuilder, TransactionBuilderStep}
import scalus.cardano.onchain.plutus.prelude.{Option as POption}

/** A stateless descriptor for a UtxoCell — a UTxO-based state machine on Cardano.
  *
  * Combines the compiled script, beacon token, and transition function into a single object. All
  * operations return `TxBuilder => TxBuilder` for composition via `pipe`.
  *
  * Usage:
  * {{{
  * val counter = UtxoCellDef(CounterCellCompilation.compiled, utf8"counter", CounterCellV2.transition)
  *
  * // Init: mint beacon + send initial state
  * TxBuilder(env)
  *     .pipe(counter.init(CounterState(0), Value.ada(10)))
  *     .complete(emulator.utxos, Alice.address)
  *     .sign(Alice.signer).transaction
  *
  * // Transition: deferred — resolves cell UTxO at complete time
  * TxBuilder(env)
  *     .pipe(counter.apply(CounterAction.Increment))
  *     .complete(reader, Alice.address)
  *
  * // Transition: eager — when UTxOs are already available
  * TxBuilder(env)
  *     .pipe(counter.apply(CounterAction.Increment, emulator.utxos))
  *     .spend(fundingUtxo)
  *     .build(changeTo = Alice.address)
  * }}}
  */
class UtxoCellDef[S: ToData: FromData, A: ToData](
    val compiled: CompiledPlutus[?],
    val tokenName: ByteString,
    val transition: (S, A, CellContext) => POption[S]
) {

    val scriptHash: ScriptHash = compiled.script.scriptHash
    val policyId: PolicyId = scriptHash
    val assetName: AssetName = AssetName(tokenName)

    def scriptAddress(network: Network): Address = compiled.address(network)

    /** Find the cell UTxO by beacon token in the provided UTxO set. */
    def findUtxo(utxos: Utxos): Option[Utxo] = {
        utxos.collectFirst {
            case (input, output)
                if output.value.assets.assets.exists { case (pid, tokens) =>
                    pid == policyId && tokens.exists { case (name, qty) =>
                        name == assetName && qty > 0
                    }
                } =>
                Utxo(input, output)
        }
    }

    /** Decode the current state from a cell UTxO's inline datum. */
    def currentState(utxo: Utxo): S = {
        val data = utxo.output.inlineDatum.getOrElse(
          throw new IllegalStateException("Cell UTxO has no inline datum")
        )
        data.to[S]
    }

    /** Initialize the cell: mint beacon + send initial state to script address.
      *
      * The initial state is passed as both the mint redeemer and the output datum, so the mint
      * handler can verify the datum matches via `UtxoCellLib.verifyMintResult`.
      */
    def init(initialState: S, outputValue: Value): TxBuilder => TxBuilder = { builder =>
        val cellDef = toCellDef(builder.env.network)
        import UtxoCellBuilder.*
        builder.mintCell(cellDef, initialState.toData, initialState, outputValue)
    }

    /** Apply an action to the cell (deferred). Records a [[TransactionBuilderStep.Deferred]] step
      * that resolves at `complete` time: finds cell UTxO by beacon, decodes state, runs transition,
      * builds spend + continuing output (or burn) + context-accumulated steps.
      */
    def apply(action: A): TxBuilder => TxBuilder = { builder =>
        val env = builder.env
        val redeemer = action.toData

        builder.addSteps(TransactionBuilderStep.Deferred(
          query = UtxoQuery(UtxoSource.FromAsset(policyId, assetName)),
          resolve = { utxos =>
              resolveTransition(action, redeemer, utxos, env)
          }
        ))
    }

    /** Apply an action to the cell (eager). Finds cell UTxO in the provided set, runs transition,
      * and adds concrete steps immediately. Use with `build(changeTo)` when UTxOs are available.
      */
    def apply(action: A, utxos: Utxos): TxBuilder => TxBuilder = { builder =>
        val env = builder.env
        val redeemer = action.toData
        val concreteSteps = resolveTransition(action, redeemer, utxos, env)
        builder.addSteps(concreteSteps*)
    }

    private def resolveTransition(
        action: A,
        redeemer: Data,
        utxos: Utxos,
        env: scalus.cardano.ledger.CardanoInfo
    ): Seq[TransactionBuilderStep] = {
        val cellUtxo = findUtxo(utxos).getOrElse(
          throw new IllegalStateException(
            s"Cell UTxO not found for beacon ${policyId.toHex}/${tokenName.toHex}"
          )
        )
        val state = currentState(cellUtxo)
        val cellDef = toCellDef(env.network)
        val ctx = new OffChainCellContext(cellDef, cellUtxo, redeemer, env)
        val nextState = transition(state, action, ctx)

        val scalaNextState: Option[S] = nextState match
            case POption.Some(s) => Some(s)
            case POption.None    => None

        import UtxoCellBuilder.*
        val outputValue = ctx.continuingValue.getOrElse(Value.zero)
        val temp = TxBuilder(env).spendCellCtx(cellDef, cellUtxo, redeemer, scalaNextState, ctx, outputValue)
        temp.steps
    }

    private def toCellDef(network: Network): UtxoCellBuilder.CellDef =
        UtxoCellBuilder.CellDef(compiled, tokenName, network)
}
