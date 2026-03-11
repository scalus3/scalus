package scalus.utxocells

import scalus.uplc.CompiledPlutus
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.{FromData, ToData}
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.address.Address as OffchainAddress
import scalus.cardano.ledger.{AddrKeyHash, AssetName, Hash, PolicyId, ScriptHash, Utxo, Utxos, Value}
import scalus.cardano.txbuilder.TxBuilder
import scalus.cardano.onchain.plutus.v1 as onchain
import scalus.cardano.onchain.plutus.v1.Value as OnchainValue

/** Off-chain builder for UtxoCell transactions.
  *
  * Provides `CellDef` to describe a compiled UtxoCell and extension methods on `TxBuilder` for
  * minting (init), spending (transition), and terminating (burn beacon) cells.
  */
object UtxoCellBuilder {

    /** Descriptor for a compiled UtxoCell. */
    case class CellDef(
        compiled: CompiledPlutus[?],
        tokenName: ByteString,
        network: Network
    ) {
        def scriptHash: ScriptHash = compiled.script.scriptHash
        def policyId: PolicyId = scriptHash
        def assetName: AssetName = AssetName(tokenName)
        lazy val scriptAddress: OffchainAddress = compiled.address(network)
    }

    /** Convert an on-chain Plutus Address to an off-chain Shelley Address. */
    def toOffchainAddress(addr: onchain.Address, network: Network): OffchainAddress = {
        val payment = addr.credential match
            case onchain.Credential.PubKeyCredential(pkh) =>
                ShelleyPaymentPart.Key(AddrKeyHash(pkh.hash))
            case onchain.Credential.ScriptCredential(hash) =>
                ShelleyPaymentPart.Script(Hash.scriptHash(hash))

        val delegation = addr.stakingCredential match
            case scalus.cardano.onchain.plutus.prelude.Option.Some(stakingCred) =>
                stakingCred match
                    case onchain.StakingCredential.StakingHash(cred) =>
                        cred match
                            case onchain.Credential.PubKeyCredential(pkh) =>
                                ShelleyDelegationPart.Key(Hash.stakeKeyHash(pkh.hash))
                            case onchain.Credential.ScriptCredential(hash) =>
                                ShelleyDelegationPart.Script(Hash.scriptHash(hash))
                    case _: onchain.StakingCredential.StakingPtr =>
                        ShelleyDelegationPart.Null
            case scalus.cardano.onchain.plutus.prelude.Option.None =>
                ShelleyDelegationPart.Null

        ShelleyAddress(network, payment, delegation)
    }

    /** Convert an off-chain ledger Value to an on-chain Plutus Value. */
    def fromLedgerValue(v: Value): OnchainValue = {
        var result = OnchainValue.lovelace(BigInt(v.coin.value))
        for (policyId, tokens) <- v.assets.assets do
            for (assetName, amount) <- tokens do
                result = OnchainValue.plus(
                  result,
                  OnchainValue(policyId, assetName.bytes, BigInt(amount))
                )
        result
    }

    private def addCellOutputs(
        txBuilder: TxBuilder,
        network: Network,
        outputs: Seq[UtxoCellOutput]
    ): TxBuilder = {
        outputs.foldLeft(txBuilder) { (b, out) =>
            val addr = toOffchainAddress(out.address, network)
            val value = out.value.toLedgerValue
            b.payTo(addr, value)
        }
    }

    extension (builder: TxBuilder) {

        /** Mint (initialize) a new UtxoCell: mints the beacon token and sends the initial state to
          * the script address.
          */
        def mintCell[S: ToData](
            cellDef: CellDef,
            redeemer: Data,
            initialState: S,
            outputValue: Value
        ): TxBuilder = {
            builder
                .mint(cellDef.compiled, Map(cellDef.assetName -> 1L), redeemer)
                .payTo(
                  cellDef.compiled,
                  outputValue + Value.asset(cellDef.policyId, cellDef.assetName, 1),
                  initialState
                )
        }

        /** Initialize a UtxoCell: mint beacon + send initial state to script address.
          *
          * Usage:
          * {{{
          * TxBuilder(env)
          *     .initUtxoCell(counterCell, CounterState(0), Value.ada(10))
          *     .complete(emulator.utxos, Alice.address)
          * }}}
          */
        def initUtxoCell[S: ToData: FromData, A: ToData](
            cell: UtxoCellDef[S, A],
            initialState: S,
            outputValue: Value
        ): TxBuilder = cell.init(initialState, outputValue)(builder)

        /** Apply an action to a UtxoCell (deferred).
          *
          * Records a deferred step that resolves at `complete` time: finds cell UTxO by beacon,
          * decodes state, runs transition, builds spend + continuing output (or burn) +
          * context-accumulated steps.
          *
          * Usage:
          * {{{
          * TxBuilder(env)
          *     .spendUtxoCell(counterCell, CounterAction.Increment)
          *     .complete(emulator, Alice.address)
          * }}}
          */
        def spendUtxoCell[S: ToData: FromData, A: ToData](
            cell: UtxoCellDef[S, A],
            action: A
        ): TxBuilder = cell.apply(action)(builder)

        /** Apply an action to a UtxoCell (eager).
          *
          * Finds cell UTxO in the provided set, runs transition, and adds concrete steps
          * immediately.
          *
          * Usage:
          * {{{
          * TxBuilder(env)
          *     .spendUtxoCell(counterCell, CounterAction.Increment, emulator.utxos)
          *     .complete(emulator.utxos, Alice.address)
          * }}}
          */
        def spendUtxoCell[S: ToData: FromData, A: ToData](
            cell: UtxoCellDef[S, A],
            action: A,
            utxos: Utxos
        ): TxBuilder = cell.apply(action, utxos)(builder)

        /** Low-level spend for Phase 1 cells (without CellContext). If nextState is Some, creates a
          * continuing output. If nextState is None, burns the beacon token (terminal transition).
          * Any additional outputs from the transition result are added as payTo calls.
          */
        def spendCellRaw[S: ToData](
            cellDef: CellDef,
            utxo: Utxo,
            redeemer: Data,
            nextState: Option[S],
            outputValue: Value = Value.zero,
            outputs: Seq[UtxoCellOutput] = Seq.empty
        ): TxBuilder = {
            val withSpend = builder.spend(utxo, redeemer, cellDef.compiled)
            val withState = nextState match
                case Some(state) =>
                    val value =
                        if outputValue.isZero then utxo.output.value
                        else outputValue
                    withSpend.payTo(cellDef.compiled, value, state)
                case None =>
                    withSpend.mint(cellDef.compiled, Map(cellDef.assetName -> -1L), redeemer)
            addCellOutputs(withState, cellDef.network, outputs)
        }

        /** Low-level spend using CellContext. Spends the cell, creates a continuing output or burns
          * the beacon, and applies any accumulated steps from the context (outputs, signers,
          * validity range).
          */
        def spendCellCtx[S: ToData](
            cellDef: CellDef,
            utxo: Utxo,
            redeemer: Data,
            nextState: Option[S],
            ctx: OffChainCellContext,
            outputValue: Value = Value.zero
        ): TxBuilder = {
            val withSpend = builder.spend(utxo, redeemer, cellDef.compiled)
            val withState = nextState match
                case Some(state) =>
                    val value =
                        if outputValue.isZero then utxo.output.value
                        else outputValue
                    withSpend.payTo(cellDef.compiled, value, state)
                case None =>
                    withSpend.mint(cellDef.compiled, Map(cellDef.assetName -> -1L), redeemer)
            withState.addSteps(ctx.steps*)
        }
    }
}
