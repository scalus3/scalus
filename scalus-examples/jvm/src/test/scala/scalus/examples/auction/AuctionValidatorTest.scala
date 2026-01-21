package scalus.examples.auction

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString.*
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.RedeemerPurpose
import scalus.ledger.api.v1.PosixTime
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.testing.kit.TestUtil.{genesisHash, getScriptContextV3}
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import scalus.uplc.eval.Result
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

    // Budget assertion tests - limits should decrease as the compiler improves
    test("budget: first bid") {
        val cpuLimit = 150_000_000L
        val memLimit = 450_000L
        val budget = TestCase(
          action = TestAction.Bid(bidAmount = 3_000_000L),
          expected = Expected.Success
        ).runWithBudget()
        assert(
          budget.steps <= cpuLimit,
          s"First bid CPU budget ${budget.steps} exceeded limit $cpuLimit"
        )
        assert(
          budget.memory <= memLimit,
          s"First bid memory budget ${budget.memory} exceeded limit $memLimit"
        )
    }

    test("budget: outbid with refund") {
        val cpuLimit = 200_000_000L
        val memLimit = 590_000L
        val budget = TestCase(
          action = TestAction.Outbid(newBidAmount = 5_000_000L),
          expected = Expected.Success
        ).runWithBudget()
        assert(
          budget.steps <= cpuLimit,
          s"Outbid CPU budget ${budget.steps} exceeded limit $cpuLimit"
        )
        assert(
          budget.memory <= memLimit,
          s"Outbid memory budget ${budget.memory} exceeded limit $memLimit"
        )
    }

    test("budget: end auction with winner") {
        val cpuLimit = 250_000_000L
        val memLimit = 650_000L // Increased due to double satisfaction prevention check
        val budget = TestCase(
          action = TestAction.EndWithWinner,
          expected = Expected.Success
        ).runWithBudget()
        assert(
          budget.steps <= cpuLimit,
          s"End with winner CPU budget ${budget.steps} exceeded limit $cpuLimit"
        )
        assert(
          budget.memory <= memLimit,
          s"End with winner memory budget ${budget.memory} exceeded limit $memLimit"
        )
    }

    test("budget: end auction without bids") {
        val cpuLimit = 200_000_000L
        val memLimit = 500_000L // Increased due to double satisfaction prevention check
        val budget = TestCase(
          action = TestAction.EndNoBids,
          expected = Expected.Success
        ).runWithBudget()
        assert(
          budget.steps <= cpuLimit,
          s"End without bids CPU budget ${budget.steps} exceeded limit $cpuLimit"
        )
        assert(
          budget.memory <= memLimit,
          s"End without bids memory budget ${budget.memory} exceeded limit $memLimit"
        )
    }
}

object AuctionValidatorTest extends ScalusTest {
    import scalus.ledger.api.v3.{TxId, TxOutRef}

    private given env: CardanoInfo = TestUtil.testEnvironment

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

    /** Create an AuctionInstance using the first seller UTxO as the oneShot parameter. Returns both
      * the instance and the UTxO that must be spent in startAuction.
      */
    private def createAuctionInstanceWithUtxo(provider: Emulator): (AuctionInstance, Utxo) = {
        val sellerUtxos = provider.findUtxos(sellerAddress).await().toOption.get
        val oneShotUtxo = Utxo(sellerUtxos.head)
        val oneShot = TxOutRef(
          TxId(oneShotUtxo.input.transactionId),
          BigInt(oneShotUtxo.input.index)
        )
        val factory = AuctionFactory(env, provider, withErrorTraces = true)
        (factory.createInstance(oneShot), oneShotUtxo)
    }

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
            val (auction, oneShotUtxo) = createAuctionInstanceWithUtxo(provider)

            action match
                case TestAction.Start =>
                    runStartTest(provider, auction, oneShotUtxo)
                case TestAction.Bid(bidAmount) =>
                    runBidTest(provider, auction, oneShotUtxo, bidAmount)
                case TestAction.Outbid(newBidAmount) =>
                    runOutbidTest(provider, auction, oneShotUtxo, newBidAmount)
                case TestAction.EndWithWinner =>
                    runEndWithWinnerTest(provider, auction, oneShotUtxo)
                case TestAction.EndNoBids =>
                    runEndNoBidsTest(provider, auction, oneShotUtxo)

        private def runStartTest(
            provider: Emulator,
            auction: AuctionInstance,
            oneShotUtxo: Utxo
        ): Unit =
            provider.setSlot(beforeSlot)

            val result = scala.util.Try {
                auction
                    .startAuction(
                      sellerAddress = sellerAddress,
                      oneShotUtxo = oneShotUtxo,
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
            auction: AuctionInstance,
            oneShotUtxo: Utxo,
            bidAmount: Long
        ): Unit =
            // First start the auction
            provider.setSlot(beforeSlot)
            auction
                .startAuction(
                  sellerAddress = sellerAddress,
                  oneShotUtxo = oneShotUtxo,
                  itemId = itemId,
                  startingBid = startingBid,
                  auctionEndTime = auctionEndTime,
                  initialValue = initialAuctionValue,
                  signer = sellerParty.signer
                )
                .await()

            // Then place bid
            val result = scala.util.Try {
                auction
                    .bid(
                      bidderAddress = bidder1Address,
                      bidAmount = bidAmount,
                      signer = bidder1Party.signer
                    )
                    .await()
            }

            verifyResult(result)

        private def runOutbidTest(
            provider: Emulator,
            auction: AuctionInstance,
            oneShotUtxo: Utxo,
            newBidAmount: Long
        ): Unit =
            // Start auction
            provider.setSlot(beforeSlot)
            auction
                .startAuction(
                  sellerAddress = sellerAddress,
                  oneShotUtxo = oneShotUtxo,
                  itemId = itemId,
                  startingBid = startingBid,
                  auctionEndTime = auctionEndTime,
                  initialValue = initialAuctionValue,
                  signer = sellerParty.signer
                )
                .await()

            // First bid
            auction
                .bid(
                  bidderAddress = bidder1Address,
                  bidAmount = 3_000_000L,
                  signer = bidder1Party.signer
                )
                .await()

            // Outbid
            val result = scala.util.Try {
                auction
                    .bid(
                      bidderAddress = bidder2Address,
                      bidAmount = newBidAmount,
                      signer = bidder2Party.signer
                    )
                    .await()
            }

            verifyResult(result)

        private def runEndWithWinnerTest(
            provider: Emulator,
            auction: AuctionInstance,
            oneShotUtxo: Utxo
        ): Unit =
            // Start auction
            provider.setSlot(beforeSlot)
            auction
                .startAuction(
                  sellerAddress = sellerAddress,
                  oneShotUtxo = oneShotUtxo,
                  itemId = itemId,
                  startingBid = startingBid,
                  auctionEndTime = auctionEndTime,
                  initialValue = initialAuctionValue,
                  signer = sellerParty.signer
                )
                .await()

            // Place bid
            auction
                .bid(
                  bidderAddress = bidder1Address,
                  bidAmount = 3_000_000L,
                  signer = bidder1Party.signer
                )
                .await()

            // End auction after time
            provider.setSlot(afterSlot)
            val result = scala.util.Try {
                auction
                    .endAuction(
                      sponsorAddress = sellerAddress,
                      signer = sellerParty.signer
                    )
                    .await()
            }

            verifyResult(result)

        private def runEndNoBidsTest(
            provider: Emulator,
            auction: AuctionInstance,
            oneShotUtxo: Utxo
        ): Unit =
            // Start auction
            provider.setSlot(beforeSlot)
            auction
                .startAuction(
                  sellerAddress = sellerAddress,
                  oneShotUtxo = oneShotUtxo,
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
                auction
                    .endAuction(
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

        /** Run the test and return the execution budget for spend validators */
        def runWithBudget(): ExUnits =
            val provider = createProvider()
            val (auction, oneShotUtxo) = createAuctionInstanceWithUtxo(provider)

            action match
                case TestAction.Start =>
                    throw RuntimeException("Start action uses mint, not spend - no budget test")
                case TestAction.Bid(bidAmount) =>
                    runBidWithBudget(provider, auction, oneShotUtxo, bidAmount)
                case TestAction.Outbid(newBidAmount) =>
                    runOutbidWithBudget(provider, auction, oneShotUtxo, newBidAmount)
                case TestAction.EndWithWinner =>
                    runEndWithWinnerWithBudget(provider, auction, oneShotUtxo)
                case TestAction.EndNoBids =>
                    runEndNoBidsWithBudget(provider, auction, oneShotUtxo)

        private def runBidWithBudget(
            provider: Emulator,
            auction: AuctionInstance,
            oneShotUtxo: Utxo,
            bidAmount: Long
        ): ExUnits =
            provider.setSlot(beforeSlot)
            auction
                .startAuction(
                  sellerAddress = sellerAddress,
                  oneShotUtxo = oneShotUtxo,
                  itemId = itemId,
                  startingBid = startingBid,
                  auctionEndTime = auctionEndTime,
                  initialValue = initialAuctionValue,
                  signer = sellerParty.signer
                )
                .await()

            // Capture the UTxO BEFORE the bid transaction consumes it
            val auctionUtxo = Utxo(
              provider.findUtxos(auction.scriptAddress).await().toOption.get.head
            )

            // Save the utxo map before submission
            val utxosBeforeBid = Map(auctionUtxo.toTuple)

            val tx = auction
                .bid(
                  bidderAddress = bidder1Address,
                  bidAmount = bidAmount,
                  signer = bidder1Party.signer
                )
                .await()

            runValidatorWithUtxos(provider, auction, tx, auctionUtxo.input, utxosBeforeBid).budget

        private def runOutbidWithBudget(
            provider: Emulator,
            auction: AuctionInstance,
            oneShotUtxo: Utxo,
            newBidAmount: Long
        ): ExUnits =
            provider.setSlot(beforeSlot)
            auction
                .startAuction(
                  sellerAddress = sellerAddress,
                  oneShotUtxo = oneShotUtxo,
                  itemId = itemId,
                  startingBid = startingBid,
                  auctionEndTime = auctionEndTime,
                  initialValue = initialAuctionValue,
                  signer = sellerParty.signer
                )
                .await()

            auction
                .bid(
                  bidderAddress = bidder1Address,
                  bidAmount = 3_000_000L,
                  signer = bidder1Party.signer
                )
                .await()

            // Capture ALL UTxOs BEFORE the outbid transaction consumes them
            val auctionUtxo = Utxo(
              provider.findUtxos(auction.scriptAddress).await().toOption.get.head
            )

            // Get all UTxOs from the provider before the transaction
            val allUtxosBeforeOutbid = provider.utxos

            val tx = auction
                .bid(
                  bidderAddress = bidder2Address,
                  bidAmount = newBidAmount,
                  signer = bidder2Party.signer
                )
                .await()

            runValidatorWithUtxos(
              provider,
              auction,
              tx,
              auctionUtxo.input,
              allUtxosBeforeOutbid
            ).budget

        private def runEndWithWinnerWithBudget(
            provider: Emulator,
            auction: AuctionInstance,
            oneShotUtxo: Utxo
        ): ExUnits =
            provider.setSlot(beforeSlot)
            auction
                .startAuction(
                  sellerAddress = sellerAddress,
                  oneShotUtxo = oneShotUtxo,
                  itemId = itemId,
                  startingBid = startingBid,
                  auctionEndTime = auctionEndTime,
                  initialValue = initialAuctionValue,
                  signer = sellerParty.signer
                )
                .await()

            auction
                .bid(
                  bidderAddress = bidder1Address,
                  bidAmount = 3_000_000L,
                  signer = bidder1Party.signer
                )
                .await()

            // Capture the UTxO BEFORE the end transaction consumes it
            val auctionUtxo = Utxo(
              provider.findUtxos(auction.scriptAddress).await().toOption.get.head
            )

            // Get all UTxOs from the provider before the transaction
            val allUtxosBeforeEnd = provider.utxos

            provider.setSlot(afterSlot)
            val tx = auction
                .endAuction(
                  sponsorAddress = sellerAddress,
                  signer = sellerParty.signer
                )
                .await()

            runValidatorWithUtxos(
              provider,
              auction,
              tx,
              auctionUtxo.input,
              allUtxosBeforeEnd
            ).budget

        private def runEndNoBidsWithBudget(
            provider: Emulator,
            auction: AuctionInstance,
            oneShotUtxo: Utxo
        ): ExUnits =
            provider.setSlot(beforeSlot)
            auction
                .startAuction(
                  sellerAddress = sellerAddress,
                  oneShotUtxo = oneShotUtxo,
                  itemId = itemId,
                  startingBid = startingBid,
                  auctionEndTime = auctionEndTime,
                  initialValue = initialAuctionValue,
                  signer = sellerParty.signer
                )
                .await()

            // Capture the UTxO BEFORE the end transaction consumes it
            val auctionUtxo = Utxo(
              provider.findUtxos(auction.scriptAddress).await().toOption.get.head
            )

            // Save the utxo map before submission
            val utxosBeforeEnd = Map(auctionUtxo.toTuple)

            provider.setSlot(afterSlot)
            val tx = auction
                .endAuction(
                  sponsorAddress = sellerAddress,
                  signer = sellerParty.signer
                )
                .await()

            runValidatorWithUtxos(provider, auction, tx, auctionUtxo.input, utxosBeforeEnd).budget

    /** Run validator with pre-captured UTxOs (for when the transaction has already been submitted)
      */
    private def runValidatorWithUtxos(
        provider: Emulator,
        auction: AuctionInstance,
        tx: Transaction,
        scriptInput: TransactionInput,
        knownUtxos: Map[TransactionInput, TransactionOutput]
    ): Result =
        // Merge known utxos with any remaining utxos from provider
        val body = tx.body.value
        val allInputs =
            (body.inputs.toSet.view ++ body.collateralInputs.toSet.view ++ body.referenceInputs.toSet.view).toSet
        val remainingInputs = allInputs -- knownUtxos.keySet
        val providerUtxos =
            if remainingInputs.nonEmpty then
                provider.findUtxos(remainingInputs).await().toOption.getOrElse(Map.empty)
            else Map.empty
        val utxos = knownUtxos ++ providerUtxos

        val scriptContext = tx.getScriptContextV3(utxos, RedeemerPurpose.ForSpend(scriptInput))

        val allResolvedPlutusScriptsMap =
            AllResolvedScripts.allResolvedPlutusScriptsMap(tx, utxos).toOption.get
        val plutusScript =
            auction.scriptAddress.scriptHashOption.flatMap(allResolvedPlutusScriptsMap.get).get
        val program = plutusScript.deBruijnedProgram.toProgram

        val result = program.runWithDebug(scriptContext)
        assert(result.isSuccess, s"Validator failed: $result, logs: ${result.logs.mkString(", ")}")
        result

    private def createProvider(): Emulator =
        Emulator(
          initialUtxos = Map(
            // Seller gets two UTxOs: one for oneShot, one for fees/collateral
            Input(genesisHash, 0) ->
                TransactionOutput.Babbage(
                  address = sellerAddress,
                  value = Value.lovelace(10_000_000L) // oneShot UTxO
                ),
            Input(genesisHash, 1) ->
                TransactionOutput.Babbage(
                  address = sellerAddress,
                  value = Value.lovelace(100_000_000L) // fees/collateral
                ),
            Input(genesisHash, 2) ->
                TransactionOutput.Babbage(
                  address = bidder1Address,
                  value = Value.lovelace(100_000_000L)
                ),
            Input(genesisHash, 3) ->
                TransactionOutput.Babbage(
                  address = bidder2Address,
                  value = Value.lovelace(100_000_000L)
                )
          ),
          initialContext = Context.testMainnet(),
          mutators = Set(PlutusScriptsTransactionMutator)
        )
}
