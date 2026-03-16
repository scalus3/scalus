package scalus.utxocells

import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.PolicyId
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*

/** Validator base trait for UtxoFlow multi-transaction state machines.
  *
  * Extends [[CellValidator]] with flow-specific spend and mint logic. The concrete `@Compile`
  * object provides `beaconName` and `flowDispatch`; the trait's `inline` methods handle dispatch
  * invocation, continuing output verification, and beacon mint/burn.
  *
  * Usage:
  * {{{
  * @Compile
  * object MyFlow extends FlowCellValidator {
  *     inline def beaconName: ByteString = utf8"my-flow"
  *     inline def flowDispatch = UtxoFlow.define { ctx =>
  *         val bid = await(UtxoFlow.suspend[Bid])
  *         ctx.txInfo.requireSignedBy(bid.bidder)
  *         val confirm = await(UtxoFlow.suspend[Confirm])
  *     }
  * }
  * }}}
  *
  * Both `beaconName` and `flowDispatch` must be `inline def` (not `val`) so they are fully inlined
  * before the Scalus plugin runs — same pattern as `spendCell`/`mintCell`.
  */
trait FlowCellValidator extends CellValidator {
    inline def beaconName: ByteString
    inline def flowDispatch: (Data, Data, CellContext) => Option[Data]

    inline override def spendCell(
        datum: Option[Data],
        redeemer: Data,
        sc: ScriptContext,
        ownRef: TxOutRef
    ): Unit = {
        val d = datum.getOrFail("UtxoFlow: missing datum")
        val ctx: CellContext = sc.toData.asInstanceOf[CellContext]
        val result = flowDispatch(d, redeemer, ctx)
        UtxoCellLib.verifyFlowSpend(result, beaconName, sc.txInfo, ownRef)
    }

    inline override def mintCell(
        redeemer: Data,
        policyId: PolicyId,
        sc: ScriptContext
    ): Unit = {
        val qty = sc.txInfo.mint.quantityOf(policyId, beaconName)
        if qty === BigInt(1) then
            UtxoCellLib.verifyMintResult[Data](redeemer, beaconName, policyId, sc.txInfo)
        else if qty === BigInt(-1) then
            UtxoCellLib.verifyBurnBeacon(beaconName, policyId, sc.txInfo)
        else fail("UtxoFlow: invalid beacon mint quantity")
    }
}
