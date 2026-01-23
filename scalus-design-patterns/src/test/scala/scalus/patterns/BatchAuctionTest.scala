package scalus.patterns

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.ByteString
import scalus.cardano.ledger.ExUnits
import scalus.examples.{AuctionSettlementRedeemer, BatchAuctionContract, BidDatum, BidRedeemer}
import scalus.cardano.onchain.plutus.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, TxOut}
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.v3.ScriptInfo.{RewardingScript, SpendingScript}
import scalus.cardano.onchain.plutus.prelude.{List, Option as POption, SortedMap}
import scalus.testing.kit.ScalusTest

/** Tests for the Batch Auction Example.
  *
  * Demonstrates that the MerkelizedValidator pattern is useful when spending validators need to
  * READ verified data from the stake validator's redeemer (unlike PaymentSplitter which only checks
  * that the stake validator ran).
  */
class BatchAuctionTest extends AnyFunSuite with ScalusTest {

    private val contract = BatchAuctionContract.withErrorTraces
    private val auctionTxId = random[TxId]
    private val bidderTxId = random[TxId]
    private val txId = random[TxId]
    private val scriptHash = contract.script.scriptHash
    private val auctionIdData = ByteString.fromHex("deadbeef").toData

    // Test bidders
    private val bidderA = PubKeyHash(genByteStringOfN(28).sample.get)
    private val bidderB = PubKeyHash(genByteStringOfN(28).sample.get)
    private val bidderC = PubKeyHash(genByteStringOfN(28).sample.get)

    test("success: single bid filled at clearing price") {
        TestCase(
          bids = scala.List(Bid(bidderA, bidPrice = 100, quantity = 10, lovelace = 1000)),
          clearingPrice = 100,
          totalUnitsAvailable = 10,
          actions = scala.List(BidAction(bidderA, BidRedeemer.Fill)),
          outputs = scala.List(BidderOutput(bidderA, 500)), // Bidder receives tokens
          expectedResult = success
        ).run()
    }

    test("success: bid filled when bid price > clearing price") {
        TestCase(
          bids = scala.List(Bid(bidderA, bidPrice = 150, quantity = 10, lovelace = 1500)),
          clearingPrice = 100, // Clearing price is lower than bid
          totalUnitsAvailable = 10,
          actions = scala.List(BidAction(bidderA, BidRedeemer.Fill)),
          outputs = scala.List(BidderOutput(bidderA, 500)),
          expectedResult = success
        ).run()
    }

    test("success: bid refunded when bid price < clearing price") {
        TestCase(
          bids = scala.List(Bid(bidderA, bidPrice = 50, quantity = 10, lovelace = 500)),
          clearingPrice = 100, // Clearing price higher than bid
          totalUnitsAvailable = 10,
          actions = scala.List(BidAction(bidderA, BidRedeemer.Refund)),
          outputs = scala.List(BidderOutput(bidderA, 500)), // Full refund
          expectedResult = success
        ).run()
    }

    test("success: multiple bids - some filled, some refunded") {
        // BidderA: 150 >= 100, should be filled
        // BidderB: 100 >= 100, should be filled
        // BidderC: 50 < 100, should be refunded
        TestCase(
          bids = scala.List(
            Bid(bidderA, bidPrice = 150, quantity = 5, lovelace = 750),
            Bid(bidderB, bidPrice = 100, quantity = 5, lovelace = 500),
            Bid(bidderC, bidPrice = 50, quantity = 10, lovelace = 500)
          ),
          clearingPrice = 100,
          totalUnitsAvailable = 10, // Only 10 units, filled by A and B
          actions = scala.List(
            BidAction(bidderA, BidRedeemer.Fill),
            BidAction(bidderB, BidRedeemer.Fill),
            BidAction(bidderC, BidRedeemer.Refund)
          ),
          outputs = scala.List(
            BidderOutput(bidderA, 300),
            BidderOutput(bidderB, 200),
            BidderOutput(bidderC, 500) // Refund
          ),
          expectedResult = success
        ).run()
    }

    test("failure: trying to fill bid below clearing price") {
        TestCase(
          bids = scala.List(Bid(bidderA, bidPrice = 50, quantity = 10, lovelace = 500)),
          clearingPrice = 100, // Bid price < clearing price
          totalUnitsAvailable = 10,
          actions = scala.List(BidAction(bidderA, BidRedeemer.Fill)), // Wrong action!
          outputs = scala.List(BidderOutput(bidderA, 500)),
          expectedResult = failure("Bid price must be >= clearing price to fill")
        ).run()
    }

    test("failure: trying to refund bid at or above clearing price") {
        TestCase(
          bids = scala.List(Bid(bidderA, bidPrice = 150, quantity = 10, lovelace = 1500)),
          clearingPrice = 100, // Bid price >= clearing price
          totalUnitsAvailable = 10,
          actions = scala.List(BidAction(bidderA, BidRedeemer.Refund)), // Wrong action!
          outputs = scala.List(BidderOutput(bidderA, 1500)),
          expectedResult = failure("Can only refund if bid price < clearing price")
        ).run()
    }

    test("failure: demand exceeds supply at clearing price") {
        // Both bids are at or above clearing price, but total demand (15) > supply (10)
        TestCase(
          bids = scala.List(
            Bid(bidderA, bidPrice = 100, quantity = 10, lovelace = 1000),
            Bid(bidderB, bidPrice = 100, quantity = 5, lovelace = 500)
          ),
          clearingPrice = 100,
          totalUnitsAvailable = 10, // Only 10 units, but demand is 15
          actions = scala.List(
            BidAction(bidderA, BidRedeemer.Fill),
            BidAction(bidderB, BidRedeemer.Fill)
          ),
          outputs = scala.List(
            BidderOutput(bidderA, 500),
            BidderOutput(bidderB, 250)
          ),
          expectedResult = failure("Demand at clearing price exceeds supply")
        ).run()
    }

    test("failure: clearing price is zero") {
        TestCase(
          bids = scala.List(Bid(bidderA, bidPrice = 100, quantity = 10, lovelace = 1000)),
          clearingPrice = 0, // Invalid!
          totalUnitsAvailable = 10,
          actions = scala.List(BidAction(bidderA, BidRedeemer.Fill)),
          outputs = scala.List(BidderOutput(bidderA, 500)),
          expectedResult = failure("Clearing price must be positive")
        ).run()
    }

    test("failure: bidder not receiving output on fill") {
        TestCase(
          bids = scala.List(Bid(bidderA, bidPrice = 100, quantity = 10, lovelace = 1000)),
          clearingPrice = 100,
          totalUnitsAvailable = 10,
          actions = scala.List(BidAction(bidderA, BidRedeemer.Fill)),
          outputs = scala.List(BidderOutput(bidderB, 500)), // Wrong recipient!
          expectedResult = failure("Bidder must receive output")
        ).run()
    }

    test("budget comparison: shows MerkelizedValidator reading verified data") {
        val testCase = TestCase(
          bids = scala.List(
            Bid(bidderA, bidPrice = 150, quantity = 5, lovelace = 750),
            Bid(bidderB, bidPrice = 100, quantity = 5, lovelace = 500),
            Bid(bidderC, bidPrice = 50, quantity = 10, lovelace = 500)
          ),
          clearingPrice = 100,
          totalUnitsAvailable = 10,
          actions = scala.List(
            BidAction(bidderA, BidRedeemer.Fill),
            BidAction(bidderB, BidRedeemer.Fill),
            BidAction(bidderC, BidRedeemer.Refund)
          ),
          outputs = scala.List(
            BidderOutput(bidderA, 300),
            BidderOutput(bidderB, 200),
            BidderOutput(bidderC, 500)
          ),
          expectedResult = success
        )

        val (rewardBudget, spendBudget) = testCase.runWithBudget()

        println(s"\n=== Batch Auction Budget (3 bids) ===")
        println(
          s"Reward endpoint (runs once):    mem=${rewardBudget.memory}, cpu=${rewardBudget.steps}"
        )
        println(
          s"Spend endpoint (per bid):       mem=${spendBudget.memory}, cpu=${spendBudget.steps}"
        )
        println(
          s"Total for 3 bids: mem=${rewardBudget.memory + 3 * spendBudget.memory}, cpu=${rewardBudget.steps + 3 * spendBudget.steps}"
        )
        println(
          s"Note: Each spend reads verified clearing price via MerkelizedValidator.verifyAndGetRedeemer()"
        )
        println()
    }

    // Test helpers

    case class Bid(bidder: PubKeyHash, bidPrice: Long, quantity: Long, lovelace: Long)
    case class BidAction(bidder: PubKeyHash, action: BidRedeemer)
    case class BidderOutput(bidder: PubKeyHash, lovelace: Long)

    case class TestCase(
        bids: scala.List[Bid],
        clearingPrice: Long,
        totalUnitsAvailable: Long,
        actions: scala.List[BidAction],
        outputs: scala.List[BidderOutput],
        expectedResult: (String | Unit, POption[ExUnits])
    ) {
        // Build bid datums - used by both run() and runWithBudget()
        private def buildBidDatums(): scala.List[BidDatum] = bids.map { bid =>
            BidDatum(
              bidder = bid.bidder,
              bidPrice = BigInt(bid.bidPrice),
              quantity = BigInt(bid.quantity)
            )
        }

        private def buildTxContext(
            bidDatums: scala.List[BidDatum],
            settlement: AuctionSettlementRedeemer
        ): (TxInfo, Credential) = {
            // Build bid inputs with inline datums
            val bidInputs = bids.zip(bidDatums).zipWithIndex.map { case ((bid, datum), idx) =>
                TxInInfo(
                  outRef = TxOutRef(auctionTxId, idx),
                  resolved = TxOut(
                    address = Address(ScriptCredential(scriptHash), POption.None),
                    value = Value.lovelace(BigInt(bid.lovelace)),
                    datum = OutputDatum.OutputDatum(datum.toData)
                  )
                )
            }

            val allInputs = List.from(bidInputs)

            // Build outputs
            val txOutputs = List.from(outputs.map { output =>
                TxOut(
                  address = Address(PubKeyCredential(output.bidder), POption.None),
                  value = Value.lovelace(BigInt(output.lovelace))
                )
            })

            // Build withdrawals (for withdraw zero trick)
            val withdrawals = SortedMap.fromList(
              List((Credential.ScriptCredential(scriptHash), BigInt(0)))
            )

            // Build redeemers map
            val rewardingRedeemer = settlement.toData
            val stakingCredential = Credential.ScriptCredential(scriptHash)

            // Create spending redeemers for each bid
            val spendingRedeemers = actions.zipWithIndex.map { case (action, idx) =>
                val outRef = TxOutRef(auctionTxId, idx)
                (ScriptPurpose.Spending(outRef), action.action.toData)
            }

            val redeemers = SortedMap.fromList(
              List.from(
                spendingRedeemers :+ (ScriptPurpose.Rewarding(stakingCredential), rewardingRedeemer)
              )
            )

            val txInfo = TxInfo(
              inputs = allInputs,
              outputs = txOutputs,
              fee = BigInt(200000),
              withdrawals = withdrawals,
              redeemers = redeemers,
              id = txId
            )

            (txInfo, stakingCredential)
        }

        def run(): Unit = {
            val settlement = AuctionSettlementRedeemer(
              clearingPrice = BigInt(clearingPrice),
              totalUnitsAvailable = BigInt(totalUnitsAvailable)
            )

            val bidDatums = buildBidDatums()
            val (txInfo, stakingCredential) = buildTxContext(bidDatums, settlement)
            val rewardingRedeemer = settlement.toData

            val applied = contract.program $ auctionIdData

            // Test reward endpoint first (verifies clearing price)
            val rewardContext = ScriptContext(
              txInfo = txInfo,
              redeemer = rewardingRedeemer,
              scriptInfo = RewardingScript(stakingCredential)
            )
            val rewardResult = (applied $ rewardContext.toData).evaluateDebug

            // For reward failures, check result immediately
            if !expectedResult._1.isInstanceOf[Unit] && rewardResult.isSuccess then {
                // Expected failure in reward but it succeeded - test spend endpoint
                val firstOutRef = TxOutRef(auctionTxId, 0)
                val firstDatum = bidDatums.head
                val spendContext = ScriptContext(
                  txInfo = txInfo,
                  redeemer = actions.head.action.toData,
                  scriptInfo = SpendingScript(
                    txOutRef = firstOutRef,
                    datum = POption.Some(firstDatum.toData)
                  )
                )
                val spendResult = (applied $ spendContext.toData).evaluateDebug
                checkResult(expectedResult, spendResult)
            } else {
                checkResult(expectedResult, rewardResult)
            }

            // Also test spending endpoint if reward succeeded
            if expectedResult._1.isInstanceOf[Unit] && rewardResult.isSuccess then {
                actions.zip(bidDatums).zipWithIndex.foreach { case ((action, datum), idx) =>
                    val outRef = TxOutRef(auctionTxId, idx)
                    val spendContext = ScriptContext(
                      txInfo = txInfo,
                      redeemer = action.action.toData,
                      scriptInfo =
                          SpendingScript(txOutRef = outRef, datum = POption.Some(datum.toData))
                    )
                    val spendResult = (applied $ spendContext.toData).evaluateDebug
                    checkResult(expectedResult, spendResult)
                }
            }
        }

        def runWithBudget(): (ExUnits, ExUnits) = {
            val settlement = AuctionSettlementRedeemer(
              clearingPrice = BigInt(clearingPrice),
              totalUnitsAvailable = BigInt(totalUnitsAvailable)
            )

            val bidDatums = buildBidDatums()
            val (txInfo, stakingCredential) = buildTxContext(bidDatums, settlement)
            val rewardingRedeemer = settlement.toData

            val applied = contract.program $ auctionIdData

            // Get reward endpoint budget
            val rewardContext = ScriptContext(
              txInfo = txInfo,
              redeemer = rewardingRedeemer,
              scriptInfo = RewardingScript(stakingCredential)
            )
            val rewardResult = (applied $ rewardContext.toData).evaluateDebug
            val rewardBudget = rewardResult.budget

            // Get spend endpoint budget (first bid)
            val firstOutRef = TxOutRef(auctionTxId, 0)
            val firstDatum = bidDatums.head
            val spendContext = ScriptContext(
              txInfo = txInfo,
              redeemer = actions.head.action.toData,
              scriptInfo =
                  SpendingScript(txOutRef = firstOutRef, datum = POption.Some(firstDatum.toData))
            )
            val spendResult = (applied $ spendContext.toData).evaluateDebug
            val spendBudget = spendResult.budget

            (rewardBudget, spendBudget)
        }
    }
}
