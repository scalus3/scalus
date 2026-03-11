package scalus.utxocells

import scala.collection.mutable.ListBuffer
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.Network
import scalus.cardano.ledger.{AddrKeyHash, AssetName, CardanoInfo, TransactionOutput, Utxo, Value as LedgerValue}
import scalus.cardano.txbuilder.{TwoArgumentPlutusScriptWitness, ScriptSource, TransactionBuilderStep}
import scalus.cardano.onchain.plutus.v1 as onchain

import java.time.Instant

/** Off-chain implementation of [[CellContext]] for use in transaction building.
  *
  * Accumulates `TransactionBuilderStep` items as the transition function calls context methods.
  * After the transition, apply the accumulated steps to a `TxBuilder` via `builder.addSteps(ctx.steps*)`.
  *
  * Usage:
  * {{{
  * val ctx = OffChainCellContext(cellDef, cellUtxo, env)
  * val nextState = MyCell.transition(currentState, action, ctx)
  * val tx = TxBuilder(env)
  *     .spendCellRaw(cellDef, cellUtxo, redeemer, nextState)
  *     .addSteps(ctx.steps*)
  *     .spend(fundingUtxo)
  *     .build(changeTo = Alice.address)
  *     .sign(Alice.signer)
  *     .transaction
  * }}}
  */
class OffChainCellContext(
    cellDef: UtxoCellBuilder.CellDef,
    cellUtxo: Utxo,
    redeemer: Data,
    env: CardanoInfo
) extends CellContext {

    private val _steps = ListBuffer.empty[TransactionBuilderStep]

    private val _txInfo = new OffChainCellTxInfo(_steps, cellDef.network, env)

    override def txInfo: CellTxInfo = _txInfo

    override def ownPolicyId: onchain.PolicyId = cellDef.scriptHash

    override def ownInputValue: onchain.Value =
        UtxoCellBuilder.fromLedgerValue(cellUtxo.output.value)

    override def mint(tokenName: ByteString, amount: BigInt): Unit = {
        _steps += TransactionBuilderStep.Mint(
          scriptHash = cellDef.scriptHash,
          assetName = AssetName(tokenName),
          amount = amount.toLong,
          witness = TwoArgumentPlutusScriptWitness(
            scriptSource = ScriptSource.PlutusScriptAttached,
            redeemer = redeemer
          )
        )
    }

    private var _continuingValue: Option[LedgerValue] = None

    override def setContinuingValue(value: onchain.Value): Unit = {
        _continuingValue = Some(value.toLedgerValue)
    }

    /** The continuing output value set by the transition, if any. */
    def continuingValue: Option[LedgerValue] = _continuingValue

    /** Accumulated builder steps from the transition. Apply with `builder.addSteps(ctx.steps*)`. */
    def steps: Seq[TransactionBuilderStep] = _steps.toSeq
}

/** Off-chain implementation of [[CellTxInfo]]. */
class OffChainCellTxInfo(
    steps: ListBuffer[TransactionBuilderStep],
    network: Network,
    env: CardanoInfo
) extends CellTxInfo {

    private val _outputs = new OffChainCellOutputs(steps, network)

    override def outputs: CellOutputs = _outputs

    override def requireSignedBy(pkh: onchain.PubKeyHash): Unit = {
        steps += TransactionBuilderStep.RequireSignature(AddrKeyHash(pkh.hash))
    }

    override def requireValidAfter(time: onchain.PosixTime): Unit = {
        val slot = env.slotConfig.instantToSlot(Instant.ofEpochMilli(time.toLong)).toLong
        steps += TransactionBuilderStep.ValidityStartSlot(slot)
    }

    override def requireValidBefore(time: onchain.PosixTime): Unit = {
        val slot = env.slotConfig.instantToSlot(Instant.ofEpochMilli(time.toLong)).toLong
        steps += TransactionBuilderStep.ValidityEndSlot(slot)
    }
}

/** Off-chain implementation of [[CellOutputs]]. */
class OffChainCellOutputs(
    steps: ListBuffer[TransactionBuilderStep],
    network: Network
) extends CellOutputs {

    override def add(address: onchain.Address, value: onchain.Value): Unit = {
        val offchainAddr = UtxoCellBuilder.toOffchainAddress(address, network)
        val offchainValue = value.toLedgerValue
        steps += TransactionBuilderStep.Send(TransactionOutput(offchainAddr, offchainValue))
    }
}
