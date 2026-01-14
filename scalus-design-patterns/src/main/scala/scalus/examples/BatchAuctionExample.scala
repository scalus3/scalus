package scalus.examples

import scalus.*
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.compiler.Options
import scalus.ledger.api.v1.{Credential, PubKeyHash}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.patterns.MerkelizedValidator
import scalus.prelude.*
import scalus.uplc.PlutusV3

/** Batch Auction Example - demonstrates when MerkelizedValidator is useful.
  *
  * Unlike MerkelizedPaymentSplitter (which uses StakeValidator.spendMinimal), this example shows a
  * case where the spending validator NEEDS to read verified data from the stake validator's
  * redeemer.
  *
  * '''Use Case:''' A batch auction where multiple bid UTxOs are settled at once.
  *   - Stake validator: Computes/verifies the clearing price ONCE
  *   - Spending validator: Reads the verified clearing price to determine if each bid is filled
  *
  * '''Why MerkelizedValidator is needed here:'''
  *   - Each bid UTxO has a different bid price in its datum
  *   - Each spending validator needs to compare its bid price against the clearing price
  *   - The clearing price is verified by the stake validator and stored in its redeemer
  *   - Using MerkelizedValidator.verifyAndGetRedeemer() allows reading this verified value
  *
  * @see
  *   [[MerkelizedPaymentSplitterExample]] for a case where MerkelizedValidator is NOT needed
  */

/** Datum for a bid UTxO */
case class BidDatum(
    bidder: PubKeyHash,
    bidPrice: BigInt, // Price per unit the bidder is willing to pay
    quantity: BigInt // Number of units the bidder wants
) derives ToData,
      FromData

/** Redeemer for the stake validator containing the verified clearing price */
case class AuctionSettlementRedeemer(
    clearingPrice: BigInt, // The computed clearing price
    totalUnitsAvailable: BigInt // Total units being sold
) derives ToData,
      FromData

/** Redeemer for spending a bid UTxO */
enum BidRedeemer derives ToData, FromData:
    case Fill // Bid is filled at clearing price
    case Refund // Bid is not filled (bid price < clearing price)

/** Batch Auction Validator
  *
  * Demonstrates the full MerkelizedValidator pattern where spending validators need to read
  * verified data from the stake validator.
  *
  * @param auctionId
  *   Unique identifier for this auction (as Data for parameterization)
  */
@Compile
object BatchAuctionValidator extends DataParameterizedValidator {

    /** Spending endpoint - reads verified clearing price from stake validator.
      *
      * This is why we need MerkelizedValidator instead of just StakeValidator.spendMinimal: we need
      * to READ the verified clearing price to compare against this bid's price.
      */
    inline override def spend(
        auctionIdData: Data,
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val bid = datum.getOrFail("Datum is required").to[BidDatum]
        val bidRedeemer = redeemer.to[BidRedeemer]

        // Get own script hash
        val ownInput = tx.findOwnInputOrFail(ownRef)
        val ownCredential = ownInput.resolved.address.credential
        val ownScriptHash = ownCredential.scriptOption.getOrFail("Must be script address")

        // KEY DIFFERENCE: We use MerkelizedValidator to get the verified redeemer
        // This allows us to read the clearing price that was verified by the stake validator
        val stakeRedeemer = MerkelizedValidator.verifyAndGetRedeemer(ownScriptHash, tx)
        val settlement = stakeRedeemer.to[AuctionSettlementRedeemer]

        bidRedeemer match
            case BidRedeemer.Fill =>
                // Bid is being filled - verify bid price >= clearing price
                require(
                  bid.bidPrice >= settlement.clearingPrice,
                  "Bid price must be >= clearing price to fill"
                )

                // Verify bidder receives the tokens (simplified check)
                val bidderCredential = Credential.PubKeyCredential(bid.bidder)
                val hasBidderOutput = tx.outputs.exists { output =>
                    output.address.credential === bidderCredential
                }
                require(hasBidderOutput, "Bidder must receive output")

            case BidRedeemer.Refund =>
                // Bid is being refunded - verify bid price < clearing price
                require(
                  bid.bidPrice < settlement.clearingPrice,
                  "Can only refund if bid price < clearing price"
                )

                // Verify bidder gets their funds back
                val bidderCredential = Credential.PubKeyCredential(bid.bidder)
                val refundValue = ownInput.resolved.value.getLovelace
                val hasRefund = tx.outputs.exists { output =>
                    output.address.credential === bidderCredential &&
                    output.value.getLovelace >= refundValue
                }
                require(hasRefund, "Bidder must receive refund")
    }

    /** Reward endpoint - verifies the clearing price calculation.
      *
      * This runs ONCE and verifies that the claimed clearing price is correct based on all bids and
      * available supply.
      */
    inline override def reward(
        auctionIdData: Data,
        redeemer: Data,
        stakingKey: Credential,
        tx: TxInfo
    ): Unit = {
        val settlement = redeemer.to[AuctionSettlementRedeemer]
        val ownScriptHash = stakingKey.scriptOption.getOrFail("Must be script credential")
        val ownScriptCredential = Credential.ScriptCredential(ownScriptHash)

        // Collect all bids from inputs
        val (totalDemandAtPrice, _) = tx.inputs.foldLeft((BigInt(0), BigInt(0))) {
            case ((demand, count), input) =>
                if input.resolved.address.credential === ownScriptCredential then
                    // This is a bid input - parse its datum
                    val bidDatum = input.resolved.datum match
                        case OutputDatum.OutputDatum(d) => d.to[BidDatum]
                        case _                          => fail("Bid must have inline datum")

                    // Count demand at or above clearing price
                    if bidDatum.bidPrice >= settlement.clearingPrice then
                        (demand + bidDatum.quantity, count + 1)
                    else (demand, count + 1)
                else (demand, count)
        }

        // Verify clearing price is valid:
        // Total demand at clearing price should be <= total units available
        // (In a real implementation, you'd verify this is the optimal clearing price)
        require(
          totalDemandAtPrice <= settlement.totalUnitsAvailable,
          "Demand at clearing price exceeds supply"
        )

        // Additional verification: ensure clearing price is not artificially low
        // (In production, you'd have more sophisticated price discovery logic)
        require(settlement.clearingPrice > 0, "Clearing price must be positive")
    }
}

private given batchAuctionOptions: Options = Options.release

lazy val BatchAuctionContract = PlutusV3.compile(BatchAuctionValidator.validate)
