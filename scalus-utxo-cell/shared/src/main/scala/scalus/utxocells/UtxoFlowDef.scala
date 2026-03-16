package scalus.utxocells

import scalus.uplc.CompiledPlutus
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{AssetName, PolicyId, ScriptHash, Utxo, Utxos, Value}
import scalus.cardano.node.{UtxoQuery, UtxoSource}
import scalus.cardano.txbuilder.{TransactionBuilderStep, TxBuilder}
import scalus.cardano.onchain.plutus.prelude.Option as POption

/** A stateless descriptor for a UtxoFlow — a multi-transaction UTxO flow on Cardano.
  *
  * Combines the compiled script, beacon token, and macro-generated dispatch into a single object.
  * All operations return `TxBuilder => TxBuilder` for composition via `pipe`.
  *
  * Usage:
  * {{{
  * val flowDispatch = UtxoFlow.define { ctx =>
  *     val bid = await(UtxoFlow.suspend[Bid])
  *     ctx.txInfo.requireSignedBy(bid.bidder)
  *     val confirm = await(UtxoFlow.suspend[Confirm])
  * }
  * val auctionFlow = UtxoFlowDef(compiled, utf8"auction", flowDispatch)
  *
  * // Init: mint beacon + send initial datum
  * TxBuilder(env)
  *     .pipe(auctionFlow.init(initialDatum, Value.ada(10)))
  *     .complete(emulator.utxos, Alice.address)
  *
  * // Advance: spend cell with redeemer
  * TxBuilder(env)
  *     .pipe(auctionFlow.advance(bid.toData))
  *     .complete(emulator, Alice.address)
  * }}}
  */
class UtxoFlowDef(
    val compiled: CompiledPlutus[?],
    val tokenName: ByteString,
    val flowDispatch: (Data, Data, CellContext) => POption[Data]
) {

    val scriptHash: ScriptHash = compiled.script.scriptHash
    val policyId: PolicyId = scriptHash
    val assetName: AssetName = AssetName(tokenName)

    def scriptAddress(network: Network): Address = compiled.address(network)

    /** Find the flow UTxO by beacon token in the provided UTxO set. */
    def findUtxo(utxos: Utxos): Option[Utxo] = {
        utxos.collectFirst {
            case (input, output) if output.value.assets.assets.exists { case (pid, tokens) =>
                    pid == policyId && tokens.exists { case (name, qty) =>
                        name == assetName && qty > 0
                    }
                } =>
                Utxo(input, output)
        }
    }

    /** Read the current datum from a flow UTxO's inline datum. */
    def currentDatum(utxo: Utxo): Data = {
        utxo.output.inlineDatum.getOrElse(
          throw new IllegalStateException("Flow UTxO has no inline datum")
        )
    }

    /** Initialize the flow: mint beacon + send initial datum to script address. */
    def init(initialDatum: Data, outputValue: Value): TxBuilder => TxBuilder = { builder =>
        val cellDef = toCellDef(builder.env.network)
        import UtxoCellBuilder.*
        builder.mintCell(cellDef, initialDatum, initialDatum, outputValue)
    }

    /** Advance the flow (deferred). Records a deferred step that resolves at `complete` time. */
    def advance(redeemer: Data): TxBuilder => TxBuilder = { builder =>
        val env = builder.env
        builder.addSteps(
          TransactionBuilderStep.Deferred(
            query = UtxoQuery(UtxoSource.FromAsset(policyId, assetName)),
            resolve = { utxos =>
                resolveStep(redeemer, utxos, env)
            }
          )
        )
    }

    /** Advance the flow (eager). Finds flow UTxO in the provided set and adds steps immediately. */
    def advance(redeemer: Data, utxos: Utxos): TxBuilder => TxBuilder = { builder =>
        val env = builder.env
        val concreteSteps = resolveStep(redeemer, utxos, env)
        builder.addSteps(concreteSteps*)
    }

    private def resolveStep(
        redeemer: Data,
        utxos: Utxos,
        env: scalus.cardano.ledger.CardanoInfo
    ): Seq[TransactionBuilderStep] = {
        val flowUtxo = findUtxo(utxos).getOrElse(
          throw new IllegalStateException(
            s"Flow UTxO not found for beacon ${policyId.toHex}/${tokenName.toHex}"
          )
        )
        val datum = currentDatum(flowUtxo)
        val cellDef = toCellDef(env.network)
        val ctx = new OffChainCellContext(cellDef, flowUtxo, redeemer, env)
        val nextDatum = flowDispatch(datum, redeemer, ctx).asScala

        import UtxoCellBuilder.*
        val outputValue = ctx.continuingValue match
            case Some(v) => v + Value.asset(policyId, assetName, 1)
            case None    => Value.zero
        val temp = TxBuilder(env).spendCellCtx(
          cellDef,
          flowUtxo,
          redeemer,
          nextDatum,
          ctx,
          outputValue
        )
        temp.steps
    }

    private def toCellDef(network: Network): UtxoCellBuilder.CellDef =
        UtxoCellBuilder.CellDef(compiled, tokenName, network)
}
