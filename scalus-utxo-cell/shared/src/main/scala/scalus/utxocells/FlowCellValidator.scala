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

/** Validator base trait for parameterized UtxoFlow multi-transaction state machines.
  *
  * Like [[FlowCellValidator]] but the script takes a `Data` parameter applied at UPLC level.
  * Different parameter values produce different script hashes — true Cardano script
  * parameterization.
  *
  * Usage:
  * {{{
  * case class AuctionParams(seller: PubKeyHash, minBid: BigInt) derives FromData, ToData
  *
  * @Compile
  * object MyAuction extends DataParameterizedFlowCellValidator {
  *     inline def beaconName: ByteString = utf8"auction"
  *     inline def flowDispatch(param: Data) = {
  *         val p = param.to[AuctionParams]
  *         UtxoFlow.define { ctx =>
  *             val bid = await(UtxoFlow.suspend[Bid])
  *             require(bid.amount >= p.minBid)
  *             ctx.txInfo.requireSignedBy(p.seller)
  *         }
  *     }
  * }
  * }}}
  *
  * `flowDispatch` must be `inline def` taking `Data` and returning the dispatch function. The
  * parameter is decoded inside the method body before the `UtxoFlow.define` block, making its
  * fields available as captured variables in the flow.
  */
trait DataParameterizedFlowCellValidator extends DataParameterizedCellValidator {
    inline def beaconName: ByteString
    inline def flowDispatch(param: Data): (Data, Data, CellContext) => Option[Data]

    inline override def spendCell(
        param: Data,
        datum: Option[Data],
        redeemer: Data,
        sc: ScriptContext,
        ownRef: TxOutRef
    ): Unit = {
        val d = datum.getOrFail("UtxoFlow: missing datum")
        val ctx: CellContext = sc.toData.asInstanceOf[CellContext]
        val result = flowDispatch(param)(d, redeemer, ctx)
        UtxoCellLib.verifyFlowSpend(result, beaconName, sc.txInfo, ownRef)
    }

    inline override def mintCell(
        param: Data,
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
