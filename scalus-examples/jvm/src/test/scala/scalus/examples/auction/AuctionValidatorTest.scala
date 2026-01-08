package scalus.examples.auction

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.builtin.ByteString.*
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.node.Emulator
import scalus.ledger.api.v1.PosixTime
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global

class AuctionValidatorTest extends AnyFunSuite, ScalusTest {
    import AuctionValidatorTest.*

    test(s"Auction validator size is ${AuctionContract.script.script.size} bytes") {
        println(s"Auction validator size: ${AuctionContract.script.script.size} bytes")
        assert(AuctionContract.script.script.size > 0)
    }

    test("seller can start auction") {
        TestCase(
          action = TestAction.Start,
          expected = Expected.Success
        ).run()
    }

    test("first bidder can place bid") {
        TestCase(
          action = TestAction.Bid(bidAmount = 3_000_000L),
          expected = Expected.Success
        ).run()
    }

    test("second bidder can outbid with refund to first bidder") {
        TestCase(
          action = TestAction.Outbid(newBidAmount = 5_000_000L),
          expected = Expected.Success
        ).run()
    }

    test("auction can end with winner") {
        TestCase(
          action = TestAction.EndWithWinner,
          expected = Expected.Success
        ).run()
    }

    test("seller can end auction without bids") {
        TestCase(
          action = TestAction.EndNoBids,
          expected = Expected.Success
        ).run()
    }
}

object AuctionValidatorTest extends ScalusTest {
    private given env: CardanoInfo = TestUtil.testEnvironment
    private val compiledContract = AuctionContract.withErrorTraces

    // Party to role mapping
    private val sellerParty = Alice
    private val bidder1Party = Bob
    private val bidder2Party = Charles

    private val sellerAddress: ShelleyAddress = sellerParty.address
    private val bidder1Address: ShelleyAddress = bidder1Party.address
    private val bidder2Address: ShelleyAddress = bidder2Party.address

    private val itemId = utf8"auction-item-001"
    private val startingBid = 2_000_000L
    private val initialAuctionValue = Coin(5_000_000L)

    private val slot: SlotNo = 100
    private val beforeSlot: SlotNo = slot - 10
    private val afterSlot: SlotNo = slot + 10
    private val auctionEndTime: PosixTime = BigInt(env.slotConfig.slotToTime(slot))

    enum TestAction:
        case Start
        case Bid(bidAmount: Long)
        case Outbid(newBidAmount: Long)
        case EndWithWinner
        case EndNoBids

    enum Expected:
        case Success
        case Failure(errorContains: String)

    case class TestCase(
        action: TestAction,
        expected: Expected
    ):
        def run(): Unit =
            val provider = createProvider()
            val endpoints = AuctionEndpoints(env, provider, compiledContract)

            action match
                case TestAction.Start =>
                    runStartTest(provider, endpoints)
                case TestAction.Bid(bidAmount) =>
                    runBidTest(provider, endpoints, bidAmount)
                case TestAction.Outbid(newBidAmount) =>
                    runOutbidTest(provider, endpoints, newBidAmount)
                case TestAction.EndWithWinner =>
                    runEndWithWinnerTest(provider, endpoints)
                case TestAction.EndNoBids =>
                    runEndNoBidsTest(provider, endpoints)

        private def runStartTest(provider: Emulator, endpoints: AuctionEndpoints): Unit =
            provider.setSlot(beforeSlot)

            val result = scala.util.Try {
                endpoints
                    .startAuction(
                      sellerAddress = sellerAddress,
                      itemId = itemId,
                      startingBid = startingBid,
                      auctionEndTime = auctionEndTime,
                      initialValue = initialAuctionValue,
                      signer = sellerParty.signer
                    )
                    .await()
            }

            verifyResult(result)

        private def runBidTest(
            provider: Emulator,
            endpoints: AuctionEndpoints,
            bidAmount: Long
        ): Unit =
            // First start the auction
            provider.setSlot(beforeSlot)
            endpoints
                .startAuction(
                  sellerAddress = sellerAddress,
                  itemId = itemId,
                  startingBid = startingBid,
                  auctionEndTime = auctionEndTime,
                  initialValue = initialAuctionValue,
                  signer = sellerParty.signer
                )
                .await()

            // Then place bid
            val result = scala.util.Try {
                endpoints
                    .bid(
                      itemId = itemId,
                      bidderAddress = bidder1Address,
                      bidAmount = bidAmount,
                      signer = bidder1Party.signer
                    )
                    .await()
            }

            verifyResult(result)

        private def runOutbidTest(
            provider: Emulator,
            endpoints: AuctionEndpoints,
            newBidAmount: Long
        ): Unit =
            // Start auction
            provider.setSlot(beforeSlot)
            endpoints
                .startAuction(
                  sellerAddress = sellerAddress,
                  itemId = itemId,
                  startingBid = startingBid,
                  auctionEndTime = auctionEndTime,
                  initialValue = initialAuctionValue,
                  signer = sellerParty.signer
                )
                .await()

            // First bid
            endpoints
                .bid(
                  itemId = itemId,
                  bidderAddress = bidder1Address,
                  bidAmount = 3_000_000L,
                  signer = bidder1Party.signer
                )
                .await()

            // Outbid
            val result = scala.util.Try {
                endpoints
                    .bid(
                      itemId = itemId,
                      bidderAddress = bidder2Address,
                      bidAmount = newBidAmount,
                      signer = bidder2Party.signer
                    )
                    .await()
            }

            verifyResult(result)

        private def runEndWithWinnerTest(
            provider: Emulator,
            endpoints: AuctionEndpoints
        ): Unit =
            // Start auction
            provider.setSlot(beforeSlot)
            endpoints
                .startAuction(
                  sellerAddress = sellerAddress,
                  itemId = itemId,
                  startingBid = startingBid,
                  auctionEndTime = auctionEndTime,
                  initialValue = initialAuctionValue,
                  signer = sellerParty.signer
                )
                .await()

            // Place bid
            endpoints
                .bid(
                  itemId = itemId,
                  bidderAddress = bidder1Address,
                  bidAmount = 3_000_000L,
                  signer = bidder1Party.signer
                )
                .await()

            // End auction after time
            provider.setSlot(afterSlot)
            val result = scala.util.Try {
                endpoints
                    .endAuction(
                      itemId = itemId,
                      sponsorAddress = sellerAddress,
                      signer = sellerParty.signer
                    )
                    .await()
            }

            verifyResult(result)

        private def runEndNoBidsTest(
            provider: Emulator,
            endpoints: AuctionEndpoints
        ): Unit =
            // Start auction
            provider.setSlot(beforeSlot)
            endpoints
                .startAuction(
                  sellerAddress = sellerAddress,
                  itemId = itemId,
                  startingBid = startingBid,
                  auctionEndTime = auctionEndTime,
                  initialValue = initialAuctionValue,
                  signer = sellerParty.signer
                )
                .await()

            // End auction after time (no bids)
            provider.setSlot(afterSlot)
            val result = scala.util.Try {
                endpoints
                    .endAuction(
                      itemId = itemId,
                      sponsorAddress = sellerAddress,
                      signer = sellerParty.signer
                    )
                    .await()
            }

            verifyResult(result)

        private def verifyResult(result: scala.util.Try[Transaction]): Unit =
            expected match
                case Expected.Success =>
                    assert(
                      result.isSuccess,
                      s"Should succeed but failed: ${result.failed.getOrElse("unknown")}"
                    )
                case Expected.Failure(errorContains) =>
                    assert(result.isFailure, "Should fail but succeeded")
                    val errorMsg = result.failed.get.getMessage
                    assert(
                      errorMsg.contains(errorContains),
                      s"Expected error '$errorContains' but got '$errorMsg'"
                    )

    private def createProvider(): Emulator =
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        Emulator(
          initialUtxos = Map(
            Input(genesisHash, 0) ->
                TransactionOutput.Babbage(
                  address = sellerAddress,
                  value = Value.lovelace(100_000_000L)
                ),
            Input(genesisHash, 1) ->
                TransactionOutput.Babbage(
                  address = bidder1Address,
                  value = Value.lovelace(100_000_000L)
                ),
            Input(genesisHash, 2) ->
                TransactionOutput.Babbage(
                  address = bidder2Address,
                  value = Value.lovelace(100_000_000L)
                )
          ),
          initialContext = Context.testMainnet(),
          mutators = Set(PlutusScriptsTransactionMutator)
        )
}
