package scalus.examples.auction

import cps.*
import org.scalacheck.Prop
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address as CardanoAddress, Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.PlutusScriptsTransactionMutator
import scalus.cardano.node.{BlockchainReader, Emulator}
import scalus.cardano.onchain.plutus.v1.{PosixTime, PubKeyHash}
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.*
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import scalus.testing.kit.TestUtil.genesisHash
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data

import java.time.Instant
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class AuctionTestKitTest extends AnyFunSuite {

    private given ExecutionContext = ExecutionContext.global

    import AuctionTestKitTest.*

    // =========================================================================
    // ContractScalaCheckCommands Tests
    // =========================================================================

    test("ContractScalaCheckCommands: bid step") {
        resetCounters()
        val t0 = System.currentTimeMillis()
        val emulator = createEmulator()
        val sellerUtxos = Await.result(emulator.findUtxos(sellerAddress), Duration.Inf).toOption.get
        val oneShotUtxo = Utxo(sellerUtxos.head)

        val (_, script, scriptHash, scriptAddress) = startAuction(emulator, oneShotUtxo)

        val bidStep = AuctionBidStep(
          script,
          scriptHash,
          scriptAddress,
          bidder1Address,
          bidder1Party.signer,
          bidAmount = 3_000_000L
        )

        val commands = ContractScalaCheckCommands(emulator, bidStep) { (_, state) =>
            Future {
                val sellerPkh = extractPkh(sellerAddress)
                Prop(state.datum.seller == sellerPkh) &&
                Prop(state.datum.auctionEndTime == getAuctionEndTime(emulator.cardanoInfo))
            }
        }

        val result = org.scalacheck.Test.check(
          org.scalacheck.Test.Parameters.default
              .withMinSuccessfulTests(3)
              .withMaxDiscardRatio(20),
          commands.property()
        )
        System.err.println(
          s"  tests: ${result.succeeded}, variations: ${txVariationCount.get()}, time: ${System.currentTimeMillis() - t0}ms"
        )

        assert(result.passed, s"Bid step property test failed: $result")
    }

    test("ContractScalaCheckCommands: end step after bid") {
        resetCounters()
        val t0 = System.currentTimeMillis()
        val emulator = createEmulator()
        val sellerUtxos = Await.result(emulator.findUtxos(sellerAddress), Duration.Inf).toOption.get
        val oneShotUtxo = Utxo(sellerUtxos.head)

        val (instance, script, scriptHash, scriptAddress) = startAuction(emulator, oneShotUtxo)

        Await.result(
          instance.bid(
            bidderAddress = bidder1Address,
            bidAmount = 3_000_000L,
            itemId = itemId,
            signer = bidder1Party.signer
          ),
          Duration.Inf
        )

        emulator.setSlot(auctionEndSlot + 20)

        val endStep = AuctionEndStep(
          script,
          scriptHash,
          scriptAddress,
          sellerAddress,
          sellerParty.signer,
          Network.Mainnet
        )

        val commands = ContractScalaCheckCommands(emulator, endStep)()

        val result = org.scalacheck.Test.check(
          org.scalacheck.Test.Parameters.default
              .withMinSuccessfulTests(3)
              .withMaxDiscardRatio(20),
          commands.property()
        )
        System.err.println(
          s"  tests: ${result.succeeded}, variations: ${txVariationCount.get()}, time: ${System.currentTimeMillis() - t0}ms"
        )

        assert(result.passed, s"End step property test failed: $result")
    }

    // =========================================================================
    // Multiple Auctions Test
    // =========================================================================

    test("ContractScalaCheckCommands: bid step with two auctions in state") {
        resetCounters()
        val t0 = System.currentTimeMillis()
        val emulator = createEmulatorForTwoAuctions()
        val sellerUtxos = Await.result(emulator.findUtxos(sellerAddress), Duration.Inf).toOption.get
        val sortedUtxos = sellerUtxos.toSeq.sortBy(_._1.index)

        val oneShotUtxo1 = Utxo(sortedUtxos(0))
        val (_, script1, scriptHash1, scriptAddress1) =
            startAuction(emulator, oneShotUtxo1, utf8"auction-item-001")

        val oneShotUtxo2 = Utxo(sortedUtxos(1))
        val (_, _, _, scriptAddress2) =
            startAuction(emulator, oneShotUtxo2, utf8"auction-item-002")

        assert(
          scriptAddress1 != scriptAddress2,
          "Two auctions should have different script addresses"
        )

        val bidStep = AuctionBidStep(
          script1,
          scriptHash1,
          scriptAddress1,
          bidder1Address,
          bidder1Party.signer,
          bidAmount = 3_000_000L
        )

        val commands = ContractScalaCheckCommands(emulator, bidStep) { (reader, state) =>
            val sellerPkh = extractPkh(sellerAddress)
            val auction1Ok = Prop(state.datum.seller == sellerPkh) &&
                Prop(state.datum.itemId == utf8"auction-item-001")

            reader
                .queryUtxos { u => u.output.address == scriptAddress2 }
                .limit(1)
                .execute()
                .map {
                    case Right(utxos) if utxos.nonEmpty =>
                        val (_, output) = utxos.head
                        val datum2 = output.inlineDatum.get.to[Datum]
                        auction1Ok &&
                        Prop(datum2.itemId == utf8"auction-item-002") &&
                        Prop(datum2.seller == sellerPkh)
                    case _ =>
                        Prop.falsified
                }
        }

        val result = org.scalacheck.Test.check(
          org.scalacheck.Test.Parameters.default
              .withMinSuccessfulTests(3)
              .withMaxDiscardRatio(20),
          commands.property()
        )
        System.err.println(
          s"  tests: ${result.succeeded}, variations: ${txVariationCount.get()}, time: ${System.currentTimeMillis() - t0}ms"
        )

        assert(result.passed, s"Two-auction bid step property test failed: $result")
    }

    // =========================================================================
    // ScenarioExplorer Tests
    // =========================================================================

    test("ScenarioExplorer: multi-step auction lifecycle") {
        resetCounters()
        val t0 = System.currentTimeMillis()
        val emulator = createEmulator()
        val sellerUtxos = Await.result(emulator.findUtxos(sellerAddress), Duration.Inf).toOption.get
        val oneShotUtxo = Utxo(sellerUtxos.head)

        val (_, script, scriptHash, scriptAddress) = startAuction(emulator, oneShotUtxo)

        val auctionEnd = getAuctionEndTime(emulator.cardanoInfo)

        val scenario = ScenarioExplorer.explore(maxDepth = 3) { reader =>
            async[Scenario] {
                val currentSlot = reader.currentSlot.await
                val currentTime = reader.cardanoInfo.slotConfig.slotToTime(currentSlot)

                if currentTime < auctionEnd.toLong then
                    explorerBidStep(reader, script, scriptHash, scriptAddress, auctionEnd).await
                else explorerEndStep(reader, script, scriptHash, scriptAddress).await
            }
        }

        val results = Await.result(Scenario.runAll(emulator)(scenario), Duration.Inf)
        System.err.println(
          s"  paths: ${results.size}, txBuilds: ${txBuildCount.get()}, time: ${System.currentTimeMillis() - t0}ms"
        )
        val violations = results.flatMap(_._2)
        assert(violations.isEmpty, s"Found violations: ${violations.mkString("\n")}")
    }

    test("ScenarioExplorer: two auctions lifecycle") {
        resetCounters()
        val t0 = System.currentTimeMillis()
        val emulator = createEmulatorForTwoAuctions()
        val sellerUtxos = Await.result(emulator.findUtxos(sellerAddress), Duration.Inf).toOption.get
        val sortedUtxos = sellerUtxos.toSeq.sortBy(_._1.index)

        val oneShotUtxo1 = Utxo(sortedUtxos(0))
        val (_, script1, scriptHash1, scriptAddress1) =
            startAuction(emulator, oneShotUtxo1, utf8"item-1")

        val oneShotUtxo2 = Utxo(sortedUtxos(1))
        val (_, script2, scriptHash2, scriptAddress2) =
            startAuction(emulator, oneShotUtxo2, utf8"item-2")

        val auctionEnd = getAuctionEndTime(emulator.cardanoInfo)

        val scenario = ScenarioExplorer.explore(maxDepth = 2) { reader =>
            async[Scenario] {
                val currentSlot = reader.currentSlot.await
                val currentTime = reader.cardanoInfo.slotConfig.slotToTime(currentSlot)

                if currentTime < auctionEnd.toLong then
                    explorerTwoAuctionsBidStep(
                      reader,
                      script1,
                      scriptHash1,
                      scriptAddress1,
                      script2,
                      scriptHash2,
                      scriptAddress2,
                      auctionEnd
                    ).await
                else ()
            }
        }

        val results = Await.result(Scenario.runAll(emulator)(scenario), Duration.Inf)
        System.err.println(
          s"  paths: ${results.size}, txBuilds: ${txBuildCount.get()}, time: ${System.currentTimeMillis() - t0}ms"
        )
        val violations = results.flatMap(_._2)
        assert(
          violations.isEmpty,
          s"Found violations in two-auction test: ${violations.mkString("\n")}"
        )
        assert(results.size > 1, s"Should explore multiple paths, got ${results.size}")
    }

    // =========================================================================
    // Vulnerability Detection via Framework
    // =========================================================================

    test("ScalaCheck detects double satisfaction vulnerability in UnfixedAuction") {
        // This test uses ContractScalaCheckCommands with a TwoAuctionEndStep that
        // includes a double satisfaction attack variation.
        //
        // The vulnerability: When two auctions from the same seller end in one tx,
        // a single seller payment can satisfy validation for BOTH auctions.
        //
        // Expected behavior:
        // - UnfixedAuction: attack variation SUCCEEDS (vulnerability!)
        // - Fixed Auction: attack variation FAILS (correctly rejected)

        import scalus.compiler.Options
        import scalus.uplc.PlutusV3
        given Options = Options.release.copy(generateErrorTraces = true)

        resetCounters()
        val t0 = System.currentTimeMillis()

        // Compile the vulnerable contract
        val vulnerableContract = PlutusV3.compile(UnfixedAuctionValidator.validate)
        val script = vulnerableContract.script
        val scriptHash = script.scriptHash
        val scriptAddress = vulnerableContract.address(Network.Mainnet)

        val bidAmount = 5_000_000L
        val itemId1 = utf8"item-vuln-1"
        val itemId2 = utf8"item-vuln-2"

        // Create two won auctions (both from same seller, both won by same bidder)
        val wonDatum1 = Datum(
          seller = extractPkh(sellerAddress),
          highestBidder =
              scalus.cardano.onchain.plutus.prelude.Option.Some(extractPkh(bidder1Address)),
          highestBid = BigInt(bidAmount),
          auctionEndTime = getAuctionEndTime(CardanoInfo.mainnet),
          itemId = itemId1
        )
        val wonDatum2 = Datum(
          seller = extractPkh(sellerAddress),
          highestBidder =
              scalus.cardano.onchain.plutus.prelude.Option.Some(extractPkh(bidder1Address)),
          highestBid = BigInt(bidAmount),
          auctionEndTime = getAuctionEndTime(CardanoInfo.mainnet),
          itemId = itemId2
        )

        val auctionValue1 =
            Value.lovelace(bidAmount + 2_000_000L) + Value.asset(scriptHash, AssetName(itemId1), 1L)
        val auctionValue2 =
            Value.lovelace(bidAmount + 2_000_000L) + Value.asset(scriptHash, AssetName(itemId2), 1L)

        // Create emulator with the two auction UTxOs ready to end
        val emulator = Emulator(
          initialUtxos = Map(
            Input(genesisHash, 10) -> TransactionOutput.Babbage(
              scriptAddress,
              auctionValue1,
              datumOption = Some(DatumOption.Inline(Data.toData(wonDatum1)))
            ),
            Input(genesisHash, 11) -> TransactionOutput.Babbage(
              scriptAddress,
              auctionValue2,
              datumOption = Some(DatumOption.Inline(Data.toData(wonDatum2)))
            ),
            Input(genesisHash, 20) -> TransactionOutput.Babbage(
              bidder1Address,
              Value.lovelace(100_000_000L)
            )
          ),
          initialContext =
              scalus.cardano.ledger.rules.Context.testMainnet(slot = auctionEndSlot + 10),
          mutators = Set(PlutusScriptsTransactionMutator)
        )

        val twoAuctionStep = new TwoAuctionEndStep(
          script,
          scriptHash,
          scriptAddress,
          bidder1Address,
          bidder1Party.signer,
          Network.Mainnet
        )

        // Use ContractScalaCheckCommands to test variations
        // The invariant checks that seller receives correct total payment
        val commands = ContractScalaCheckCommands(emulator, twoAuctionStep) { (reader, state) =>
            // After ending both auctions, check seller balance
            reader.findUtxos(sellerAddress).map {
                case Right(utxos) =>
                    val sellerTotal: Long = utxos.values.map(_.value.coin.value).sum
                    val expectedTotal =
                        state.datum1.highestBid.toLong + state.datum2.highestBid.toLong
                    // This check will FAIL for the attack tx on vulnerable contract
                    // because seller only gets paid once
                    Prop(sellerTotal >= expectedTotal) :|
                        s"Seller should receive $expectedTotal lovelace but got $sellerTotal"
                case Left(_) => Prop.passed // no seller utxos yet is ok
            }
        }

        val result = org.scalacheck.Test.check(
          org.scalacheck.Test.Parameters.default
              .withMinSuccessfulTests(5)
              .withMaxDiscardRatio(20),
          commands.property()
        )

//        System.err.println(
//          s"  tests: ${result.succeeded}, variations: ${txVariationCount.get()}, time: ${System.currentTimeMillis() - t0}ms"
//        )

        // For the VULNERABLE contract, we expect the test to FAIL because
        // the attack variation succeeds but violates the invariant
        assert(
          !result.passed,
          "Checker should detect vulnerability: expected property failure but all passed"
        )
//        System.err.println(s"  VULNERABILITY DETECTED: ${result.status}")
    }

    test("ScenarioExplorer finds double satisfaction vulnerability in UnfixedAuction") {
        // This test explores different auction ending strategies.
        // The "end-batch" strategy ends all auctions in one tx with merged seller
        // payment (a natural optimization). For a vulnerable contract, this tx succeeds
        // even though the seller is underpaid — discovering the double satisfaction bug.

        import scalus.compiler.Options
        import scalus.uplc.PlutusV3
        given Options = Options.release.copy(generateErrorTraces = true)

        resetCounters()
        val t0 = System.currentTimeMillis()

        val vulnerableContract = PlutusV3.compile(UnfixedAuctionValidator.validate)
        val script = vulnerableContract.script
        val scriptHash = script.scriptHash
        val scriptAddress = vulnerableContract.address(Network.Mainnet)

        val bidAmount = 5_000_000L
        val iid1 = utf8"item-vuln-1"
        val iid2 = utf8"item-vuln-2"

        val wonDatum1 = Datum(
          seller = extractPkh(sellerAddress),
          highestBidder =
              scalus.cardano.onchain.plutus.prelude.Option.Some(extractPkh(bidder1Address)),
          highestBid = BigInt(bidAmount),
          auctionEndTime = getAuctionEndTime(CardanoInfo.mainnet),
          itemId = iid1
        )
        val wonDatum2 = Datum(
          seller = extractPkh(sellerAddress),
          highestBidder =
              scalus.cardano.onchain.plutus.prelude.Option.Some(extractPkh(bidder1Address)),
          highestBid = BigInt(bidAmount),
          auctionEndTime = getAuctionEndTime(CardanoInfo.mainnet),
          itemId = iid2
        )

        val auctionValue1 =
            Value.lovelace(bidAmount + 2_000_000L) + Value.asset(scriptHash, AssetName(iid1), 1L)
        val auctionValue2 =
            Value.lovelace(bidAmount + 2_000_000L) + Value.asset(scriptHash, AssetName(iid2), 1L)

        val emulator = Emulator(
          initialUtxos = Map(
            Input(genesisHash, 10) -> TransactionOutput.Babbage(
              scriptAddress,
              auctionValue1,
              datumOption = Some(DatumOption.Inline(Data.toData(wonDatum1)))
            ),
            Input(genesisHash, 11) -> TransactionOutput.Babbage(
              scriptAddress,
              auctionValue2,
              datumOption = Some(DatumOption.Inline(Data.toData(wonDatum2)))
            ),
            Input(genesisHash, 20) -> TransactionOutput.Babbage(
              bidder1Address,
              Value.lovelace(100_000_000L)
            ),
            Input(genesisHash, 21) -> TransactionOutput.Babbage(
              sellerAddress,
              Value.lovelace(100_000_000L)
            )
          ),
          initialContext =
              scalus.cardano.ledger.rules.Context.testMainnet(slot = auctionEndSlot + 10),
          mutators = Set(PlutusScriptsTransactionMutator)
        )

        // Quick pre-check: verify the batch tx works with the emulator
        val preCheckReader = ImmutableEmulator.fromEmulator(emulator).asReader
        val preCheckTx = buildBatchEndTx(
          preCheckReader,
          Await
              .result(
                preCheckReader
                    .queryUtxos(u => u.output.address == scriptAddress)
                    .limit(10)
                    .execute(),
                Duration.Inf
              )
              .toOption
              .get
              .toSeq
              .sortBy(_._1.index),
          script,
          scriptHash,
          bidder1Address,
          bidder1Party.signer
        )
        preCheckTx match
            case scala.Some(tx) =>
                val submitResult = ImmutableEmulator.fromEmulator(emulator).submit(tx)
                System.err.println(
                  s"    pre-check submit: ${submitResult.isRight}, ${submitResult.left.toOption}"
                )
            case _ =>
                System.err.println(s"    pre-check: batch tx failed to build")

        val scenario = ScenarioExplorer.explore(maxDepth = 2) { reader =>
            async[Scenario] {
                val utxosResult = Await.result(
                  reader.queryUtxos(u => u.output.address == scriptAddress).limit(10).execute(),
                  Duration.Inf
                )
                val auctionUtxos = utxosResult match
                    case Right(u) if u.nonEmpty => u.toSeq.sortBy(_._1.index)
                    case _                      => Seq.empty

                if auctionUtxos.isEmpty then ()
                else if auctionUtxos.size >= 2 then
                    // Multiple auctions available — explore ending strategies
                    val strategy = Scenario.choices("end-first", "end-batch").await
                    strategy match
                        case "end-first" =>
                            // End just the first auction
                            buildEndTx(reader, scriptAddress, script, scriptHash) match
                                case scala.Some(tx) =>
                                    val result = Scenario.submit(tx).await
                                    Scenario
                                        .check(
                                          result.isRight,
                                          s"Single end should succeed: $result"
                                        )
                                        .await
                                case _ => ()

                        case "end-batch" =>
                            // End all auctions in one tx with merged seller payment.
                            // This is a natural batching optimization: each validator
                            // independently checks >= its bid, so merged payment
                            // of max(bids) satisfies all validators individually.
                            // A secure contract should reject this.
                            val batchTxOpt = buildBatchEndTx(
                              reader,
                              auctionUtxos,
                              script,
                              scriptHash,
                              bidder1Address,
                              bidder1Party.signer
                            )
                            System.err.println(s"    buildBatchEndTx: ${batchTxOpt.isDefined}")
                            batchTxOpt match
                                case scala.Some(tx) =>
                                    val result = Scenario.submit(tx).await
                                    System.err.println(s"    submit result: ${result.isRight}")
                                    val datums = auctionUtxos.map(_._2.inlineDatum.get.to[Datum])
                                    val totalOwed = datums.map(_.highestBid.toLong).sum
                                    val actualPaid = datums.map(_.highestBid.toLong).max
                                    Scenario
                                        .check(
                                          result.isLeft,
                                          s"Batch end with merged seller payment accepted: " +
                                              s"pays $actualPaid but owes $totalOwed"
                                        )
                                        .await
                                case _ => ()

                        case _ => ()
                else
                    // Single remaining auction — just end it
                    buildEndTx(reader, scriptAddress, script, scriptHash) match
                        case scala.Some(tx) =>
                            val result = Scenario.submit(tx).await
                            Scenario
                                .check(
                                  result.isRight,
                                  s"Single end should succeed: $result"
                                )
                                .await
                        case _ => ()
            }
        }

        val results = Await.result(Scenario.runAll(emulator)(scenario), Duration.Inf)
        System.err.println(
          s"  paths: ${results.size}, time: ${System.currentTimeMillis() - t0}ms"
        )

        val violations = results.flatMap(_._2)
        assert(
          violations.nonEmpty,
          "Checker should detect double satisfaction vulnerability in UnfixedAuction"
        )
        violations.foreach { v =>
            System.err.println(s"  VULNERABILITY FOUND: ${v.message}")
        }
    }

    /** State for testing double satisfaction on two auctions with same script */
    case class TwoAuctionState(
        utxo1: Utxo,
        datum1: Datum,
        utxo2: Utxo,
        datum2: Datum,
        script: PlutusScript,
        scriptHash: ScriptHash,
        scriptAddress: CardanoAddress
    )

    /** Step that includes double satisfaction attack variation for vulnerability detection */
    class TwoAuctionEndStep(
        script: PlutusScript,
        scriptHash: ScriptHash,
        scriptAddress: CardanoAddress,
        sponsorAddress: ShelleyAddress,
        sponsorSigner: scalus.cardano.txbuilder.TransactionSigner,
        network: Network
    ) extends ContractStepVariations[TwoAuctionState] {

        override def extractState(
            reader: BlockchainReader
        )(using ExecutionContext): Future[TwoAuctionState] = {
            reader
                .queryUtxos { u => u.output.address == scriptAddress }
                .limit(10)
                .execute()
                .map {
                    case Right(utxos) if utxos.size >= 2 =>
                        val sorted = utxos.toSeq.sortBy(_._1.index)
                        val (in1, out1) = sorted(0)
                        val (in2, out2) = sorted(1)
                        TwoAuctionState(
                          Utxo(in1, out1),
                          out1.inlineDatum.get.to[Datum],
                          Utxo(in2, out2),
                          out2.inlineDatum.get.to[Datum],
                          script,
                          scriptHash,
                          scriptAddress
                        )
                    case _ =>
                        throw IllegalStateException(
                          s"Need at least 2 auction UTxOs at $scriptAddress"
                        )
                }
        }

        override def makeBaseTx(reader: BlockchainReader, state: TwoAuctionState)(using
            ExecutionContext
        ): Future[TxTemplate] = {
            // Base tx: end BOTH auctions correctly (pay seller twice)
            val sellerAddr1 = addressFromPkh(state.datum1.seller, network)
            val sellerAddr2 = addressFromPkh(state.datum2.seller, network)

            val winnerAddr1: scala.Option[ShelleyAddress] = state.datum1.highestBidder match
                case scalus.cardano.onchain.plutus.prelude.Option.Some(w) =>
                    scala.Some(addressFromPkh(w, network))
                case _ => scala.None

            val winnerAddr2: scala.Option[ShelleyAddress] = state.datum2.highestBidder match
                case scalus.cardano.onchain.plutus.prelude.Option.Some(w) =>
                    scala.Some(addressFromPkh(w, network))
                case _ => scala.None

            val nftValue1 = Value.asset(state.scriptHash, AssetName(state.datum1.itemId), 1L)
            val nftValue2 = Value.asset(state.scriptHash, AssetName(state.datum2.itemId), 1L)

            val redeemerBuilder1 = (tx: Transaction) => {
                val inputIdx = tx.body.value.inputs.toSeq.indexOf(state.utxo1.input)
                val sellerIdx = tx.body.value.outputs.indexWhere(_.value.address == sellerAddr1)
                // Find winner output that has NFT1
                val winnerIdx = winnerAddr1
                    .map { a =>
                        tx.body.value.outputs.indexWhere { out =>
                            out.value.address == a && out.value.value.hasAsset(
                              state.scriptHash,
                              AssetName(state.datum1.itemId)
                            )
                        }
                    }
                    .getOrElse(-1)
                Data.toData(Action.End(BigInt(inputIdx), BigInt(sellerIdx), BigInt(winnerIdx)))
            }

            val redeemerBuilder2 = (tx: Transaction) => {
                val inputIdx = tx.body.value.inputs.toSeq.indexOf(state.utxo2.input)
                // For correct tx, find the SECOND seller output (if same seller)
                val sellerOutputs =
                    tx.body.value.outputs.zipWithIndex.filter(_._1.value.address == sellerAddr2)
                val sellerIdx =
                    if sellerAddr1 == sellerAddr2 && sellerOutputs.size > 1 then sellerOutputs(1)._2
                    else sellerOutputs.headOption.map(_._2).getOrElse(-1)
                // Find winner output that has NFT2
                val winnerIdx = winnerAddr2
                    .map { a =>
                        tx.body.value.outputs.indexWhere { out =>
                            out.value.address == a && out.value.value.hasAsset(
                              state.scriptHash,
                              AssetName(state.datum2.itemId)
                            )
                        }
                    }
                    .getOrElse(-1)
                Data.toData(Action.End(BigInt(inputIdx), BigInt(sellerIdx), BigInt(winnerIdx)))
            }

            var builder = TxBuilder(reader.cardanoInfo)
                .spend(state.utxo1, redeemerBuilder1, state.script, Set.empty)
                .spend(state.utxo2, redeemerBuilder2, state.script, Set.empty)
                .validFrom(Instant.ofEpochMilli(state.datum1.auctionEndTime.toLong + 1000))

            // Pay sellers correctly (twice if same seller) and each winner gets their own NFT
            builder = (winnerAddr1, winnerAddr2) match {
                case (scala.Some(w1), scala.Some(w2)) if w1 == w2 =>
                    // Same winner - but each End expects to see the NFT in a separate output
                    // The validator checks: winnerOutput.value.quantityOf(scriptHash, itemId) === 1
                    // So we need TWO separate outputs to the winner, each with one NFT
                    builder
                        .payTo(
                          w1,
                          Value.lovelace(2_000_000L) + nftValue1
                        ) // winner output for auction1
                        .payTo(
                          w1,
                          Value.lovelace(2_000_000L) + nftValue2
                        ) // winner output for auction2
                        .payTo(sellerAddr1, Value.lovelace(state.datum1.highestBid.toLong))
                        .payTo(sellerAddr2, Value.lovelace(state.datum2.highestBid.toLong))
                case (scala.Some(w1), scala.Some(w2)) =>
                    builder
                        .payTo(w1, Value.lovelace(2_000_000L) + nftValue1)
                        .payTo(w2, Value.lovelace(2_000_000L) + nftValue2)
                        .payTo(sellerAddr1, Value.lovelace(state.datum1.highestBid.toLong))
                        .payTo(sellerAddr2, Value.lovelace(state.datum2.highestBid.toLong))
                case _ => builder
            }

            Future.successful(TxTemplate(builder, sponsorAddress, sponsorSigner))
        }

        override def variations: TxVariations[TwoAuctionState] = {
            new TxVariations[TwoAuctionState] {
                override def enumerate(
                    reader: BlockchainReader,
                    state: TwoAuctionState,
                    txTemplate: TxTemplate
                )(using ExecutionContext): Future[Seq[Transaction]] = {
                    val correctTx = txTemplate.complete(reader)

                    // ATTACK: Double satisfaction - end both auctions, pay seller only ONCE
                    val doubleSatAttackTx = {
                        val sellerAddr = addressFromPkh(state.datum1.seller, network)
                        val winnerAddr1: scala.Option[ShelleyAddress] =
                            state.datum1.highestBidder match
                                case scalus.cardano.onchain.plutus.prelude.Option.Some(w) =>
                                    scala.Some(addressFromPkh(w, network))
                                case _ => scala.None
                        val winnerAddr2: scala.Option[ShelleyAddress] =
                            state.datum2.highestBidder match
                                case scalus.cardano.onchain.plutus.prelude.Option.Some(w) =>
                                    scala.Some(addressFromPkh(w, network))
                                case _ => scala.None

                        val nftValue1 =
                            Value.asset(state.scriptHash, AssetName(state.datum1.itemId), 1L)
                        val nftValue2 =
                            Value.asset(state.scriptHash, AssetName(state.datum2.itemId), 1L)

                        // Both redeemers point to the SAME seller output index (the attack!)
                        val attackRedeemerBuilder1 = (tx: Transaction) => {
                            val inputIdx = tx.body.value.inputs.toSeq.indexOf(state.utxo1.input)
                            val sellerIdx =
                                tx.body.value.outputs.indexWhere(_.value.address == sellerAddr)
                            // Find winner output that has NFT1
                            val winnerIdx = winnerAddr1
                                .map { a =>
                                    tx.body.value.outputs.indexWhere { out =>
                                        out.value.address == a && out.value.value.hasAsset(
                                          state.scriptHash,
                                          AssetName(state.datum1.itemId)
                                        )
                                    }
                                }
                                .getOrElse(-1)
                            Data.toData(
                              Action.End(BigInt(inputIdx), BigInt(sellerIdx), BigInt(winnerIdx))
                            )
                        }

                        val attackRedeemerBuilder2 = (tx: Transaction) => {
                            val inputIdx = tx.body.value.inputs.toSeq.indexOf(state.utxo2.input)
                            // ATTACK: reuse SAME seller output index!
                            val sellerIdx =
                                tx.body.value.outputs.indexWhere(_.value.address == sellerAddr)
                            // Find winner output that has NFT2
                            val winnerIdx = winnerAddr2
                                .map { a =>
                                    tx.body.value.outputs.indexWhere { out =>
                                        out.value.address == a && out.value.value.hasAsset(
                                          state.scriptHash,
                                          AssetName(state.datum2.itemId)
                                        )
                                    }
                                }
                                .getOrElse(-1)
                            Data.toData(
                              Action.End(BigInt(inputIdx), BigInt(sellerIdx), BigInt(winnerIdx))
                            )
                        }

                        var attackBuilder = TxBuilder(reader.cardanoInfo)
                            .spend(state.utxo1, attackRedeemerBuilder1, state.script, Set.empty)
                            .spend(state.utxo2, attackRedeemerBuilder2, state.script, Set.empty)
                            .validFrom(
                              Instant.ofEpochMilli(state.datum1.auctionEndTime.toLong + 1000)
                            )

                        // Pay seller only ONCE (the bug!) but give winner their NFTs correctly
                        attackBuilder = (winnerAddr1, winnerAddr2) match {
                            case (scala.Some(w1), scala.Some(w2)) if w1 == w2 =>
                                // Same winner - needs separate outputs for each NFT (validator checks)
                                attackBuilder
                                    .payTo(
                                      w1,
                                      Value.lovelace(2_000_000L) + nftValue1
                                    ) // winner output for auction1
                                    .payTo(
                                      w1,
                                      Value.lovelace(2_000_000L) + nftValue2
                                    ) // winner output for auction2
                                    .payTo(
                                      sellerAddr,
                                      Value.lovelace(state.datum1.highestBid.toLong)
                                    ) // Only pay once! (attack)
                            case (scala.Some(w1), scala.Some(w2)) =>
                                attackBuilder
                                    .payTo(w1, Value.lovelace(2_000_000L) + nftValue1)
                                    .payTo(w2, Value.lovelace(2_000_000L) + nftValue2)
                                    .payTo(
                                      sellerAddr,
                                      Value.lovelace(state.datum1.highestBid.toLong)
                                    ) // Only pay once! (attack)
                            case _ => attackBuilder
                        }

                        TxTemplate(attackBuilder, txTemplate.sponsor, txTemplate.signer).complete(
                          reader
                        )
                    }

                    val namedTxs =
                        Seq("correct" -> correctTx, "doubleSatAttack" -> doubleSatAttackTx)
                    Future
                        .sequence(namedTxs.map { case (name, f) =>
                            f.map(scala.Some(_)).recover { case e =>
                                System.err.println(
                                  s"      TwoAuctionEndStep.$name failed: ${e.getMessage}"
                                )
                                scala.None
                            }
                        })
                        .map { results =>
                            val txs = results.flatten
                            txVariationCount.addAndGet(txs.size)
                            txs
                        }
                }
            }
        }
    }

    // =========================================================================
    // ScenarioExplorer step helpers (kept outside async[Scenario] to avoid
    // inline size issues)
    // =========================================================================

    private def explorerBidStep(
        reader: BlockchainReader,
        script: PlutusScript,
        scriptHash: ScriptHash,
        scriptAddress: CardanoAddress,
        auctionEnd: PosixTime
    ): Scenario[Unit] = async[Scenario] {
        val action = Scenario.choices("bid-bob", "bid-charles", "wait").await
        action match
            case "bid-bob" =>
                buildBidTx(
                  reader,
                  scriptAddress,
                  script,
                  scriptHash,
                  bidder1Address,
                  bidder1Party.signer
                ) match
                    case scala.Some(tx) =>
                        val result = Scenario.submit(tx).await
                        Scenario.check(result.isRight, s"Bob bid should succeed: $result").await
                    case _ => ()
            case "bid-charles" =>
                buildBidTx(
                  reader,
                  scriptAddress,
                  script,
                  scriptHash,
                  bidder2Address,
                  bidder2Party.signer
                ) match
                    case scala.Some(tx) =>
                        val result = Scenario.submit(tx).await
                        Scenario.check(result.isRight, s"Charles bid should succeed: $result").await
                    case _ => ()
            case "wait" =>
                Scenario.sleep(20).await
            case _ => ()
    }

    private def explorerEndStep(
        reader: BlockchainReader,
        script: PlutusScript,
        scriptHash: ScriptHash,
        scriptAddress: CardanoAddress
    ): Scenario[Unit] = async[Scenario] {
        val txOpt = buildEndTx(reader, scriptAddress, script, scriptHash)
        txOpt match
            case scala.Some(tx) =>
                val result = Scenario.submit(tx).await
                Scenario.check(result.isRight, s"End auction should succeed: $result").await
            case _ => ()
    }

    private def explorerTwoAuctionsBidStep(
        reader: BlockchainReader,
        script1: PlutusScript,
        scriptHash1: ScriptHash,
        scriptAddress1: CardanoAddress,
        script2: PlutusScript,
        scriptHash2: ScriptHash,
        scriptAddress2: CardanoAddress,
        auctionEnd: PosixTime
    ): Scenario[Unit] = async[Scenario] {
        val action = Scenario.choices("bid-auction1", "bid-auction2", "wait").await
        action match
            case "bid-auction1" =>
                buildBidTx(
                  reader,
                  scriptAddress1,
                  script1,
                  scriptHash1,
                  bidder1Address,
                  bidder1Party.signer
                ) match
                    case scala.Some(tx) =>
                        val result = Scenario.submit(tx).await
                        Scenario
                            .check(result.isRight, s"Bid on auction 1 should succeed: $result")
                            .await
                    case _ => ()
            case "bid-auction2" =>
                buildBidTx(
                  reader,
                  scriptAddress2,
                  script2,
                  scriptHash2,
                  bidder2Address,
                  bidder2Party.signer
                ) match
                    case scala.Some(tx) =>
                        val result = Scenario.submit(tx).await
                        Scenario
                            .check(result.isRight, s"Bid on auction 2 should succeed: $result")
                            .await
                    case _ => ()
            case "wait" =>
                Scenario.sleep(15).await
            case _ => ()
    }
}

object AuctionTestKitTest {

    private given ExecutionContext = ExecutionContext.global

    val txBuildCount = new java.util.concurrent.atomic.AtomicInteger(0)
    val txVariationCount = new java.util.concurrent.atomic.AtomicInteger(0)
    val txSubmitCount = new java.util.concurrent.atomic.AtomicInteger(0)

    def resetCounters(): Unit = {
        txBuildCount.set(0)
        txVariationCount.set(0)
        txSubmitCount.set(0)
    }

    val sellerParty = Alice
    val bidder1Party = Bob
    val bidder2Party = Charles

    val sellerAddress: ShelleyAddress = sellerParty.address(Network.Mainnet)
    val bidder1Address: ShelleyAddress = bidder1Party.address(Network.Mainnet)
    val bidder2Address: ShelleyAddress = bidder2Party.address(Network.Mainnet)

    val itemId = utf8"auction-item-001"
    val startingBid = 2_000_000L
    val initialAuctionValue = Coin(5_000_000L)

    val auctionStartSlot: SlotNo = 10
    val auctionEndSlot: SlotNo = 100

    /** State for auction contract testing */
    case class AuctionState(
        utxo: Utxo,
        datum: Datum,
        script: PlutusScript,
        scriptHash: ScriptHash,
        scriptAddress: CardanoAddress
    )

    def extractPkh(address: ShelleyAddress): PubKeyHash =
        address.payment match
            case ShelleyPaymentPart.Key(hash) => PubKeyHash(hash)
            case _ => throw IllegalArgumentException("Address must have key payment credential")

    def addressFromPkh(pkh: PubKeyHash, network: Network): ShelleyAddress =
        ShelleyAddress(
          network,
          ShelleyPaymentPart.Key(AddrKeyHash.fromByteString(pkh.hash)),
          ShelleyDelegationPart.Null
        )

    def getAuctionEndTime(info: CardanoInfo): PosixTime =
        BigInt(info.slotConfig.slotToTime(auctionEndSlot))

    def createEmulator(): Emulator = {
        Emulator(
          initialUtxos = Map(
            Input(genesisHash, 0) ->
                TransactionOutput
                    .Babbage(address = sellerAddress, value = Value.lovelace(10_000_000L)),
            Input(genesisHash, 1) ->
                TransactionOutput
                    .Babbage(address = sellerAddress, value = Value.lovelace(100_000_000L)),
            Input(genesisHash, 2) ->
                TransactionOutput
                    .Babbage(address = bidder1Address, value = Value.lovelace(100_000_000L)),
            Input(genesisHash, 3) ->
                TransactionOutput.Babbage(
                  address = bidder2Address,
                  value = Value.lovelace(100_000_000L)
                )
          ),
          initialContext = scalus.cardano.ledger.rules.Context.testMainnet(),
          mutators = Set(PlutusScriptsTransactionMutator)
        )
    }

    def createEmulatorForTwoAuctions(): Emulator = {
        Emulator(
          initialUtxos = Map(
            Input(genesisHash, 0) ->
                TransactionOutput
                    .Babbage(address = sellerAddress, value = Value.lovelace(10_000_000L)),
            Input(genesisHash, 1) ->
                TransactionOutput
                    .Babbage(address = sellerAddress, value = Value.lovelace(10_000_000L)),
            Input(genesisHash, 2) ->
                TransactionOutput
                    .Babbage(address = sellerAddress, value = Value.lovelace(200_000_000L)),
            Input(genesisHash, 3) ->
                TransactionOutput
                    .Babbage(address = bidder1Address, value = Value.lovelace(200_000_000L)),
            Input(genesisHash, 4) ->
                TransactionOutput.Babbage(
                  address = bidder2Address,
                  value = Value.lovelace(200_000_000L)
                )
          ),
          initialContext = scalus.cardano.ledger.rules.Context.testMainnet(),
          mutators = Set(PlutusScriptsTransactionMutator)
        )
    }

    def startAuction(
        emulator: Emulator,
        oneShotUtxo: Utxo,
        auctionItemId: scalus.uplc.builtin.ByteString = itemId
    ): (AuctionInstance, PlutusScript, ScriptHash, CardanoAddress) = {
        val oneShot = TxOutRef(
          TxId(oneShotUtxo.input.transactionId),
          BigInt(oneShotUtxo.input.index)
        )
        val factory = AuctionFactory(emulator, withErrorTraces = true)
        val instance = factory.createInstance(oneShot)

        val appliedContract = AuctionContract.withErrorTraces.apply(Data.toData(oneShot))
        val script = appliedContract.script
        val scriptHash = script.scriptHash
        val scriptAddress = appliedContract.address(emulator.cardanoInfo.network)

        emulator.setSlot(auctionStartSlot)
        Await.result(
          instance.startAuction(
            sellerAddress = sellerAddress,
            oneShotUtxo = oneShotUtxo,
            itemId = auctionItemId,
            startingBid = startingBid,
            auctionEndTime = getAuctionEndTime(emulator.cardanoInfo),
            initialValue = initialAuctionValue,
            signer = sellerParty.signer
          ),
          Duration.Inf
        )

        (instance, script, scriptHash, scriptAddress)
    }

    /** Build a bid transaction for the given auction */
    def buildBidTx(
        reader: BlockchainReader,
        scriptAddress: CardanoAddress,
        script: PlutusScript,
        scriptHash: ScriptHash,
        bidderAddr: ShelleyAddress,
        bidderSigner: scalus.cardano.txbuilder.TransactionSigner
    ): scala.Option[Transaction] = {
        txBuildCount.incrementAndGet()
        val utxosResult = Await.result(
          reader.queryUtxos { u => u.output.address == scriptAddress }.limit(1).execute(),
          Duration.Inf
        )
        utxosResult match
            case Right(utxos) if utxos.nonEmpty =>
                val (input, output) = utxos.head
                val auctionUtxo = Utxo(input, output)
                val datum = output.inlineDatum.get.to[Datum]
                val bidderPkh = extractPkh(bidderAddr)
                val newBidAmount = datum.highestBid.toLong + 1_000_000L
                val newDatum = datum.copy(
                  highestBidder = scalus.cardano.onchain.plutus.prelude.Option.Some(bidderPkh),
                  highestBid = BigInt(newBidAmount)
                )
                val nftAsset = AssetName(datum.itemId)
                val nftValue = Value.asset(scriptHash, nftAsset, 1L)

                val prevBidderAddr: scala.Option[ShelleyAddress] = datum.highestBidder match
                    case scalus.cardano.onchain.plutus.prelude.Option.Some(prev) =>
                        scala.Some(addressFromPkh(prev, reader.cardanoInfo.network))
                    case _ => scala.None

                val redeemerBuilder = buildBidRedeemer(
                  auctionUtxo,
                  scriptAddress,
                  prevBidderAddr,
                  newBidAmount,
                  bidderPkh
                )

                var builder = TxBuilder(reader.cardanoInfo)
                    .spend(
                      auctionUtxo,
                      redeemerBuilder,
                      script,
                      Set(AddrKeyHash.fromByteString(bidderPkh.hash))
                    )
                    .payTo(scriptAddress, Value.lovelace(newBidAmount) + nftValue, newDatum)
                    .validTo(Instant.ofEpochMilli(datum.auctionEndTime.toLong - 1000))

                builder = prevBidderAddr match
                    case scala.Some(addr) =>
                        builder.payTo(addr, Value.lovelace(datum.highestBid.toLong))
                    case scala.None => builder

                val tx = Await.result(
                  builder.complete(reader, bidderAddr).map(_.sign(bidderSigner).transaction),
                  Duration.Inf
                )
                scala.Some(tx)
            case _ => scala.None
    }

    /** Build an end auction transaction */
    def buildEndTx(
        reader: BlockchainReader,
        scriptAddress: CardanoAddress,
        script: PlutusScript,
        scriptHash: ScriptHash
    ): scala.Option[Transaction] = {
        txBuildCount.incrementAndGet()
        val utxosResult = Await.result(
          reader.queryUtxos { u => u.output.address == scriptAddress }.limit(1).execute(),
          Duration.Inf
        )
        utxosResult match
            case Right(utxos) if utxos.nonEmpty =>
                val (input, output) = utxos.head
                val auctionUtxo = Utxo(input, output)
                val datum = output.inlineDatum.get.to[Datum]
                val nftAsset = AssetName(datum.itemId)
                val nftValue = Value.asset(scriptHash, nftAsset, 1L)
                val sellerAddr = addressFromPkh(datum.seller, reader.cardanoInfo.network)
                val sellerAddrKeyHash = AddrKeyHash.fromByteString(datum.seller.hash)

                val spendSigners = datum.highestBidder match
                    case scalus.cardano.onchain.plutus.prelude.Option.Some(_) =>
                        Set.empty[AddrKeyHash]
                    case scalus.cardano.onchain.plutus.prelude.Option.None => Set(sellerAddrKeyHash)

                val winnerAddr: scala.Option[ShelleyAddress] = datum.highestBidder match
                    case scalus.cardano.onchain.plutus.prelude.Option.Some(winner) =>
                        scala.Some(addressFromPkh(winner, reader.cardanoInfo.network))
                    case _ => scala.None

                val redeemerBuilder = buildEndRedeemer(auctionUtxo, sellerAddr, winnerAddr)

                var builder = TxBuilder(reader.cardanoInfo)
                    .spend(auctionUtxo, redeemerBuilder, script, spendSigners)
                    .validFrom(Instant.ofEpochMilli(datum.auctionEndTime.toLong + 1000))

                builder = winnerAddr match
                    case scala.Some(addr) =>
                        builder
                            .payTo(addr, Value.lovelace(2_000_000L) + nftValue)
                            .payTo(sellerAddr, Value.lovelace(datum.highestBid.toLong))
                    case scala.None =>
                        builder.payTo(sellerAddr, Value.lovelace(2_000_000L) + nftValue)

                val tx = Await.result(
                  builder
                      .complete(reader, sellerAddress)
                      .map(_.sign(sellerParty.signer).transaction),
                  Duration.Inf
                )
                scala.Some(tx)
            case _ => scala.None
    }

    /** Build a batch-end transaction that ends all auctions in one tx.
      *
      * Uses a natural optimization: merges seller payments into a single output paying max(bids).
      * Each validator independently checks >= its own bid, so the merged output satisfies all
      * validators. A secure contract should reject this because the seller is underpaid (gets max
      * instead of sum).
      */
    def buildBatchEndTx(
        reader: BlockchainReader,
        auctionUtxos: Seq[(Input, TransactionOutput)],
        script: PlutusScript,
        scriptHash: ScriptHash,
        sponsorAddress: ShelleyAddress,
        sponsorSigner: scalus.cardano.txbuilder.TransactionSigner
    ): scala.Option[Transaction] = {
        if auctionUtxos.size < 2 then return scala.None

        val utxosWithDatum = auctionUtxos.map { case (input, output) =>
            (Utxo(input, output), output.inlineDatum.get.to[Datum])
        }
        val network = reader.cardanoInfo.network
        val sellerAddr = addressFromPkh(utxosWithDatum.head._2.seller, network)

        var builder = TxBuilder(reader.cardanoInfo)
            .validFrom(Instant.ofEpochMilli(utxosWithDatum.head._2.auctionEndTime.toLong + 1000))
            .minFee(Coin(600_000L)) // ensure fee buffer for multi-script batch tx

        // Spend each auction UTxO with delayed redeemer that computes indices
        for (utxo, datum) <- utxosWithDatum do
            val redeemerBuilder = (tx: Transaction) => {
                val inputIdx = tx.body.value.inputs.toSeq.indexOf(utxo.input)
                val sellerIdx = tx.body.value.outputs.indexWhere(_.value.address == sellerAddr)
                val winnerAddr = datum.highestBidder match
                    case scalus.cardano.onchain.plutus.prelude.Option.Some(w) =>
                        addressFromPkh(w, network)
                    case _ => throw IllegalStateException("Expected winner")
                val winnerIdx = tx.body.value.outputs.indexWhere { out =>
                    out.value.address == winnerAddr &&
                    out.value.value.hasAsset(scriptHash, AssetName(datum.itemId))
                }
                Data.toData(Action.End(BigInt(inputIdx), BigInt(sellerIdx), BigInt(winnerIdx)))
            }
            builder = builder.spend(utxo, redeemerBuilder, script, Set.empty)

        // Winner outputs: each auction's NFT goes to its winner separately
        for (_, datum) <- utxosWithDatum do
            val winnerAddr = datum.highestBidder match
                case scalus.cardano.onchain.plutus.prelude.Option.Some(w) =>
                    addressFromPkh(w, network)
                case _ => throw IllegalStateException("Expected winner")
            val nftValue = Value.asset(scriptHash, AssetName(datum.itemId), 1L)
            builder = builder.payTo(winnerAddr, Value.lovelace(2_000_000L) + nftValue)

        // Merged seller payment: pay max(bids) in one output.
        // Each validator checks >= its own bid, so max satisfies all individually.
        val mergedPayment = utxosWithDatum.map(_._2.highestBid.toLong).max
        builder = builder.payTo(sellerAddr, Value.lovelace(mergedPayment))

        scala.util.Try {
            Await.result(
              TxTemplate(builder, sponsorAddress, sponsorSigner).complete(reader)(using
                reader.executionContext
              ),
              Duration.Inf
            )
        } match
            case scala.util.Success(tx) => scala.Some(tx)
            case scala.util.Failure(e) =>
                System.err.println(s"    buildBatchEndTx failed: ${e.getMessage}")
                scala.None
    }

    private def buildBidRedeemer(
        auctionUtxo: Utxo,
        scriptAddress: CardanoAddress,
        prevBidderAddr: scala.Option[ShelleyAddress],
        newBidAmount: Long,
        bidderPkh: PubKeyHash
    ): Transaction => scalus.uplc.builtin.Data = { (tx: Transaction) =>
        val inputIdx = tx.body.value.inputs.toSeq.indexOf(auctionUtxo.input)
        val outputIdx = tx.body.value.outputs.indexWhere(_.value.address == scriptAddress)
        val refundIdx = prevBidderAddr
            .map(addr => tx.body.value.outputs.indexWhere(_.value.address == addr))
            .getOrElse(-1)
        Data.toData(
          Action.Bid(
            BigInt(newBidAmount),
            bidderPkh,
            BigInt(inputIdx),
            BigInt(outputIdx),
            BigInt(refundIdx)
          )
        )
    }

    private def buildEndRedeemer(
        auctionUtxo: Utxo,
        sellerAddr: ShelleyAddress,
        winnerAddr: scala.Option[ShelleyAddress]
    ): Transaction => scalus.uplc.builtin.Data = { (tx: Transaction) =>
        val inputIdx = tx.body.value.inputs.toSeq.indexOf(auctionUtxo.input)
        val sellerOutputIdx = tx.body.value.outputs.indexWhere(_.value.address == sellerAddr)
        val winnerOutputIdx = winnerAddr
            .map(addr => tx.body.value.outputs.indexWhere(_.value.address == addr))
            .getOrElse(-1)
        Data.toData(
          Action.End(BigInt(inputIdx), BigInt(sellerOutputIdx), BigInt(winnerOutputIdx))
        )
    }

    // =========================================================================
    // ContractStepVariations implementations
    // =========================================================================

    class AuctionBidStep(
        script: PlutusScript,
        scriptHash: ScriptHash,
        scriptAddress: CardanoAddress,
        bidderAddress: ShelleyAddress,
        bidderSigner: scalus.cardano.txbuilder.TransactionSigner,
        bidAmount: Long
    ) extends ContractStepVariations[AuctionState] {

        override def extractState(reader: BlockchainReader)(using
            ExecutionContext
        ): Future[AuctionState] = {
            reader
                .queryUtxos { u => u.output.address == scriptAddress }
                .limit(1)
                .execute()
                .map {
                    case Right(utxos) if utxos.nonEmpty =>
                        val (input, output) = utxos.head
                        val utxo = Utxo(input, output)
                        val datum = output.inlineDatum
                            .getOrElse(throw IllegalStateException("No inline datum"))
                            .to[Datum]
                        AuctionState(utxo, datum, script, scriptHash, scriptAddress)
                    case _ =>
                        throw IllegalStateException(s"No auction UTxO found at $scriptAddress")
                }
        }

        override def makeBaseTx(reader: BlockchainReader, state: AuctionState)(using
            ExecutionContext
        ): Future[TxTemplate] = {
            val bidderPkh = extractPkh(bidderAddress)
            val newDatum = state.datum.copy(
              highestBidder = scalus.cardano.onchain.plutus.prelude.Option.Some(bidderPkh),
              highestBid = BigInt(bidAmount)
            )

            val nftAsset = AssetName(state.datum.itemId)
            val nftValue = Value.asset(state.scriptHash, nftAsset, 1L)
            val newAuctionValue = Value.lovelace(bidAmount) + nftValue

            val prevBidderAddr: scala.Option[ShelleyAddress] = state.datum.highestBidder match
                case scalus.cardano.onchain.plutus.prelude.Option.Some(prevBidder) =>
                    scala.Some(addressFromPkh(prevBidder, reader.cardanoInfo.network))
                case scalus.cardano.onchain.plutus.prelude.Option.None =>
                    scala.None

            val redeemerBuilder = buildBidRedeemer(
              state.utxo,
              state.scriptAddress,
              prevBidderAddr,
              bidAmount,
              bidderPkh
            )

            var builder = TxBuilder(reader.cardanoInfo)
                .spend(
                  state.utxo,
                  redeemerBuilder,
                  state.script,
                  Set(AddrKeyHash.fromByteString(bidderPkh.hash))
                )
                .payTo(state.scriptAddress, newAuctionValue, newDatum)
                .validTo(Instant.ofEpochMilli(state.datum.auctionEndTime.toLong - 1000))

            builder = prevBidderAddr match
                case scala.Some(addr) =>
                    builder.payTo(addr, Value.lovelace(state.datum.highestBid.toLong))
                case scala.None => builder

            Future.successful(TxTemplate(builder, bidderAddress, bidderSigner))
        }

        override def variations: TxVariations[AuctionState] = {
            new TxVariations[AuctionState] {
                override def enumerate(
                    reader: BlockchainReader,
                    state: AuctionState,
                    txTemplate: TxTemplate
                )(using ExecutionContext): Future[Seq[Transaction]] = {
                    val correctTx = txTemplate.complete(reader)

                    val stealOutputTx = {
                        val bidderPkh = extractPkh(bidderAddress)
                        val redeemerBuilder = (tx: Transaction) => {
                            val inputIdx = tx.body.value.inputs.toSeq.indexOf(state.utxo.input)
                            Data.toData(
                              Action.Bid(
                                BigInt(bidAmount),
                                bidderPkh,
                                BigInt(inputIdx),
                                BigInt(0),
                                BigInt(-1)
                              )
                            )
                        }
                        val stealBuilder = TxBuilder(reader.cardanoInfo)
                            .spend(
                              state.utxo,
                              redeemerBuilder,
                              state.script,
                              Set(AddrKeyHash.fromByteString(bidderPkh.hash))
                            )
                            .payTo(bidderAddress, Value.lovelace(bidAmount))
                            .validTo(Instant.ofEpochMilli(state.datum.auctionEndTime.toLong - 1000))
                        TxTemplate(stealBuilder, bidderAddress, bidderSigner).complete(reader)
                    }

                    val bidTooLowTx = {
                        val lowBid = state.datum.highestBid.toLong - 1
                        val bidderPkh = extractPkh(bidderAddress)
                        val newDatum = state.datum.copy(
                          highestBidder =
                              scalus.cardano.onchain.plutus.prelude.Option.Some(bidderPkh),
                          highestBid = BigInt(lowBid)
                        )
                        val nftAsset = AssetName(state.datum.itemId)
                        val nftValue = Value.asset(state.scriptHash, nftAsset, 1L)
                        val redeemerBuilder = (tx: Transaction) => {
                            val inputIdx = tx.body.value.inputs.toSeq.indexOf(state.utxo.input)
                            val outputIdx = tx.body.value.outputs
                                .indexWhere(_.value.address == state.scriptAddress)
                            Data.toData(
                              Action.Bid(
                                BigInt(lowBid),
                                bidderPkh,
                                BigInt(inputIdx),
                                BigInt(outputIdx),
                                BigInt(-1)
                              )
                            )
                        }
                        val lowBuilder = TxBuilder(reader.cardanoInfo)
                            .spend(
                              state.utxo,
                              redeemerBuilder,
                              state.script,
                              Set(AddrKeyHash.fromByteString(bidderPkh.hash))
                            )
                            .payTo(state.scriptAddress, Value.lovelace(lowBid) + nftValue, newDatum)
                            .validTo(Instant.ofEpochMilli(state.datum.auctionEndTime.toLong - 1000))
                        TxTemplate(lowBuilder, bidderAddress, bidderSigner).complete(reader)
                    }

                    val corruptDatumTx = {
                        val bidderPkh = extractPkh(bidderAddress)
                        val corruptDatum = state.datum.copy(highestBid = BigInt(999_999_999L))
                        val nftAsset = AssetName(state.datum.itemId)
                        val nftValue = Value.asset(state.scriptHash, nftAsset, 1L)
                        val redeemerBuilder = (tx: Transaction) => {
                            val inputIdx = tx.body.value.inputs.toSeq.indexOf(state.utxo.input)
                            val outputIdx = tx.body.value.outputs
                                .indexWhere(_.value.address == state.scriptAddress)
                            Data.toData(
                              Action.Bid(
                                BigInt(bidAmount),
                                bidderPkh,
                                BigInt(inputIdx),
                                BigInt(outputIdx),
                                BigInt(-1)
                              )
                            )
                        }
                        val corruptBuilder = TxBuilder(reader.cardanoInfo)
                            .spend(
                              state.utxo,
                              redeemerBuilder,
                              state.script,
                              Set(AddrKeyHash.fromByteString(bidderPkh.hash))
                            )
                            .payTo(
                              state.scriptAddress,
                              Value.lovelace(bidAmount) + nftValue,
                              corruptDatum
                            )
                            .validTo(Instant.ofEpochMilli(state.datum.auctionEndTime.toLong - 1000))
                        TxTemplate(corruptBuilder, bidderAddress, bidderSigner).complete(reader)
                    }

                    val namedTxs = Seq(
                      "correct" -> correctTx,
                      "stealOutput" -> stealOutputTx,
                      "bidTooLow" -> bidTooLowTx,
                      "corruptDatum" -> corruptDatumTx
                    )
                    Future
                        .sequence(namedTxs.map { case (_, f) =>
                            f.map(scala.Some(_)).recover { case _ => scala.None }
                        })
                        .map { results =>
                            val txs = results.flatten
                            txVariationCount.addAndGet(txs.size)
                            txs
                        }
                }
            }
        }

        override def slotDelays(state: AuctionState): Seq[Long] = Seq(10L, 500L)
    }

    class AuctionEndStep(
        script: PlutusScript,
        scriptHash: ScriptHash,
        scriptAddress: CardanoAddress,
        sponsorAddress: ShelleyAddress,
        sponsorSigner: scalus.cardano.txbuilder.TransactionSigner,
        network: Network
    ) extends ContractStepVariations[AuctionState] {

        override def extractState(reader: BlockchainReader)(using
            ExecutionContext
        ): Future[AuctionState] = {
            reader
                .queryUtxos { u => u.output.address == scriptAddress }
                .limit(1)
                .execute()
                .map {
                    case Right(utxos) if utxos.nonEmpty =>
                        val (input, output) = utxos.head
                        val utxo = Utxo(input, output)
                        val datum = output.inlineDatum
                            .getOrElse(throw IllegalStateException("No inline datum"))
                            .to[Datum]
                        AuctionState(utxo, datum, script, scriptHash, scriptAddress)
                    case _ =>
                        throw IllegalStateException(s"No auction UTxO found at $scriptAddress")
                }
        }

        override def makeBaseTx(reader: BlockchainReader, state: AuctionState)(using
            ExecutionContext
        ): Future[TxTemplate] = {
            val nftAsset = AssetName(state.datum.itemId)
            val nftValue = Value.asset(state.scriptHash, nftAsset, 1L)
            val sellerAddr = addressFromPkh(state.datum.seller, network)
            val sellerAddrKeyHash = AddrKeyHash.fromByteString(state.datum.seller.hash)

            val spendRequiredSigners = state.datum.highestBidder match
                case scalus.cardano.onchain.plutus.prelude.Option.Some(_) => Set.empty[AddrKeyHash]
                case scalus.cardano.onchain.plutus.prelude.Option.None    => Set(sellerAddrKeyHash)

            val winnerAddr: scala.Option[ShelleyAddress] = state.datum.highestBidder match
                case scalus.cardano.onchain.plutus.prelude.Option.Some(winner) =>
                    scala.Some(addressFromPkh(winner, network))
                case scalus.cardano.onchain.plutus.prelude.Option.None =>
                    scala.None

            val redeemerBuilder = buildEndRedeemer(state.utxo, sellerAddr, winnerAddr)

            var builder = TxBuilder(reader.cardanoInfo)
                .spend(state.utxo, redeemerBuilder, state.script, spendRequiredSigners)
                .validFrom(Instant.ofEpochMilli(state.datum.auctionEndTime.toLong + 1000))

            builder = winnerAddr match
                case scala.Some(addr) =>
                    builder
                        .payTo(addr, Value.lovelace(2_000_000L) + nftValue)
                        .payTo(sellerAddr, Value.lovelace(state.datum.highestBid.toLong))
                case scala.None =>
                    builder.payTo(sellerAddr, Value.lovelace(2_000_000L) + nftValue)

            Future.successful(TxTemplate(builder, sponsorAddress, sponsorSigner))
        }

        override def variations: TxVariations[AuctionState] = {
            new TxVariations[AuctionState] {
                override def enumerate(
                    reader: BlockchainReader,
                    state: AuctionState,
                    txTemplate: TxTemplate
                )(using ExecutionContext): Future[Seq[Transaction]] = {
                    val correctTx = txTemplate.complete(reader)

                    val stealSellerTx = state.datum.highestBidder match
                        case scalus.cardano.onchain.plutus.prelude.Option.Some(winner) =>
                            val winnerAddr = addressFromPkh(winner, network)
                            val nftAsset = AssetName(state.datum.itemId)
                            val nftValue = Value.asset(state.scriptHash, nftAsset, 1L)
                            val redeemerBuilder = (tx: Transaction) => {
                                val inputIdx = tx.body.value.inputs.toSeq.indexOf(state.utxo.input)
                                val winnerOutputIdx =
                                    tx.body.value.outputs.indexWhere(_.value.address == winnerAddr)
                                Data.toData(
                                  Action.End(BigInt(inputIdx), BigInt(-1), BigInt(winnerOutputIdx))
                                )
                            }
                            val stealBuilder = TxBuilder(reader.cardanoInfo)
                                .spend(state.utxo, redeemerBuilder, state.script, Set.empty)
                                .payTo(winnerAddr, Value.lovelace(2_000_000L) + nftValue)
                                .validFrom(
                                  Instant.ofEpochMilli(state.datum.auctionEndTime.toLong + 1000)
                                )
                            scala.Some(
                              TxTemplate(stealBuilder, sponsorAddress, sponsorSigner).complete(
                                reader
                              )
                            )
                        case _ => scala.None

                    val nftWrongAddrTx = state.datum.highestBidder match
                        case scalus.cardano.onchain.plutus.prelude.Option.Some(_) =>
                            val sellerAddr = addressFromPkh(state.datum.seller, network)
                            val nftAsset = AssetName(state.datum.itemId)
                            val nftValue = Value.asset(state.scriptHash, nftAsset, 1L)
                            val redeemerBuilder = (tx: Transaction) => {
                                val inputIdx = tx.body.value.inputs.toSeq.indexOf(state.utxo.input)
                                val sellerOutputIdx =
                                    tx.body.value.outputs.indexWhere(_.value.address == sellerAddr)
                                val attackerOutputIdx = tx.body.value.outputs
                                    .indexWhere(_.value.address == bidder2Address)
                                Data.toData(
                                  Action.End(
                                    BigInt(inputIdx),
                                    BigInt(sellerOutputIdx),
                                    BigInt(attackerOutputIdx)
                                  )
                                )
                            }
                            val wrongAddrBuilder = TxBuilder(reader.cardanoInfo)
                                .spend(state.utxo, redeemerBuilder, state.script, Set.empty)
                                .payTo(bidder2Address, Value.lovelace(2_000_000L) + nftValue)
                                .payTo(sellerAddr, Value.lovelace(state.datum.highestBid.toLong))
                                .validFrom(
                                  Instant.ofEpochMilli(state.datum.auctionEndTime.toLong + 1000)
                                )
                            scala.Some(
                              TxTemplate(wrongAddrBuilder, sponsorAddress, sponsorSigner).complete(
                                reader
                              )
                            )
                        case _ => scala.None

                    val allTxs = Seq(scala.Some(correctTx)) ++ Seq(stealSellerTx, nftWrongAddrTx)
                    Future
                        .sequence(allTxs.flatten.map(_.recover { case _ => null }))
                        .map { results =>
                            val txs = results.filter(_ != null)
                            txVariationCount.addAndGet(txs.size)
                            txs
                        }
                }
            }
        }
    }
}
