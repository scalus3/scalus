package scalus.examples.crowdfunding

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TransactionSigner
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import scalus.testing.kit.TestUtil.genesisHash
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.utils.await

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global

/** Integration tests for crowdfunding contract using Emulator.
  *
  * Tests the full lifecycle: create campaign, donate, withdraw (success), reclaim (failure).
  */
class CrowdfundingEmulatorTest extends AnyFunSuite, ScalusTest {
    import CrowdfundingEmulatorTest.*

    test("recipient can create campaign") {
        TestCase(
          action = TestAction.Create,
          expected = Expected.Success
        ).run()
    }

    test("donor can donate to campaign before deadline") {
        TestCase(
          action = TestAction.Donate(amount = 5_000_000L),
          expected = Expected.Success
        ).run()
    }

    test("multiple donors can donate") {
        TestCase(
          action = TestAction.MultipleDonations,
          expected = Expected.Success
        ).run()
    }

    test("recipient can withdraw after deadline when goal reached") {
        TestCase(
          action = TestAction.WithdrawSuccess,
          expected = Expected.Success
        ).run()
    }

    test("donor can reclaim after deadline when goal not reached") {
        TestCase(
          action = TestAction.ReclaimSuccess,
          expected = Expected.Success
        ).run()
    }

    test("reclaim rejects duplicate donation indices (double-spend prevention)") {
        TestCase(
          action = TestAction.ReclaimDuplicateIndices,
          expected =
              Expected.Failure("script evaluation failed") // Validator rejects duplicate indices
        ).run()
    }
}

object CrowdfundingEmulatorTest extends ScalusTest {
    private given env: CardanoInfo = TestUtil.testEnvironment
    private val crowdfundingContract = CrowdfundingContract.withErrorTraces
    private val donationMintingContract = DonationMintingContract.withErrorTraces

    // Party to role mapping
    private val recipientParty = Alice
    private val donor1Party = Bob
    private val donor2Party = Charles

    private val recipientAddress: ShelleyAddress = recipientParty.address
    private val donor1Address: ShelleyAddress = donor1Party.address
    private val donor2Address: ShelleyAddress = donor2Party.address

    private val goal = 10_000_000L // 10 ADA goal
    private val initialCampaignValue = Coin(2_000_000L) // Min UTxO

    private val slot: SlotNo = 100
    private val beforeSlot: SlotNo = slot - 10
    private val afterSlot: SlotNo = slot + 10
    private val deadline: PosixTime = BigInt(env.slotConfig.slotToTime(slot))

    enum TestAction:
        case Create
        case Donate(amount: Long)
        case MultipleDonations
        case WithdrawSuccess
        case ReclaimSuccess
        case ReclaimDuplicateIndices

    enum Expected:
        case Success
        case Failure(errorContains: String)

    case class TestCase(
        action: TestAction,
        expected: Expected
    ):
        def run(): Unit =
            val provider = createProvider()
            val endpoints = CrowdfundingEndpoints(
              env,
              provider,
              crowdfundingContract,
              donationMintingContract
            )

            action match
                case TestAction.Create =>
                    runCreateTest(provider, endpoints)
                case TestAction.Donate(amount) =>
                    runDonateTest(provider, endpoints, amount)
                case TestAction.MultipleDonations =>
                    runMultipleDonationsTest(provider, endpoints)
                case TestAction.WithdrawSuccess =>
                    runWithdrawSuccessTest(provider, endpoints)
                case TestAction.ReclaimSuccess =>
                    runReclaimSuccessTest(provider, endpoints)
                case TestAction.ReclaimDuplicateIndices =>
                    runReclaimDuplicateIndicesTest(provider, endpoints)

        private def runCreateTest(provider: Emulator, endpoints: CrowdfundingEndpoints): Unit =
            provider.setSlot(beforeSlot)

            val result = scala.util.Try {
                endpoints
                    .createCampaign(
                      recipientAddress = recipientAddress,
                      goal = goal,
                      deadline = deadline.toLong,
                      initialValue = initialCampaignValue,
                      signer = recipientParty.signer
                    )
                    .await()
            }

            verifyResult(result.map(_._1))

        private def runDonateTest(
            provider: Emulator,
            endpoints: CrowdfundingEndpoints,
            amount: Long
        ): Unit =
            // First create the campaign
            provider.setSlot(beforeSlot)
            val (_, campaignId) = endpoints
                .createCampaign(
                  recipientAddress = recipientAddress,
                  goal = goal,
                  deadline = deadline.toLong,
                  initialValue = initialCampaignValue,
                  signer = recipientParty.signer
                )
                .await()

            // Then donate
            val result = scala.util.Try {
                endpoints
                    .donate(
                      campaignId = campaignId,
                      donorAddress = donor1Address,
                      amount = amount,
                      signer = donor1Party.signer
                    )
                    .await()
            }

            verifyResult(result)

        private def runMultipleDonationsTest(
            provider: Emulator,
            endpoints: CrowdfundingEndpoints
        ): Unit =
            // Create campaign
            provider.setSlot(beforeSlot)
            val (_, campaignId) = endpoints
                .createCampaign(
                  recipientAddress = recipientAddress,
                  goal = goal,
                  deadline = deadline.toLong,
                  initialValue = initialCampaignValue,
                  signer = recipientParty.signer
                )
                .await()

            // First donation
            endpoints
                .donate(
                  campaignId = campaignId,
                  donorAddress = donor1Address,
                  amount = 3_000_000L,
                  signer = donor1Party.signer
                )
                .await()

            // Second donation
            val result = scala.util.Try {
                endpoints
                    .donate(
                      campaignId = campaignId,
                      donorAddress = donor2Address,
                      amount = 4_000_000L,
                      signer = donor2Party.signer
                    )
                    .await()
            }

            verifyResult(result)

        private def runWithdrawSuccessTest(
            provider: Emulator,
            endpoints: CrowdfundingEndpoints
        ): Unit =
            // Create campaign
            provider.setSlot(beforeSlot)
            val (_, campaignId) = endpoints
                .createCampaign(
                  recipientAddress = recipientAddress,
                  goal = goal,
                  deadline = deadline.toLong,
                  initialValue = initialCampaignValue,
                  signer = recipientParty.signer
                )
                .await()

            // Donate enough to reach goal
            endpoints
                .donate(
                  campaignId = campaignId,
                  donorAddress = donor1Address,
                  amount = 6_000_000L,
                  signer = donor1Party.signer
                )
                .await()

            endpoints
                .donate(
                  campaignId = campaignId,
                  donorAddress = donor2Address,
                  amount = 5_000_000L,
                  signer = donor2Party.signer
                )
                .await()

            // Move past deadline
            provider.setSlot(afterSlot)

            // Find donation UTxOs at script address (unified design: tokens + ADA in same UTxO)
            val donationUtxos = endpoints.findDonationUtxos(campaignId).await()

            // Withdraw - only recipient needs to sign (tokens are at script address)
            val result = scala.util.Try {
                endpoints
                    .withdraw(
                      campaignId = campaignId,
                      recipientAddress = recipientAddress,
                      donationUtxos = donationUtxos,
                      signer = recipientParty.signer
                    )
                    .await()
            }

            verifyResult(result)

        private def runReclaimSuccessTest(
            provider: Emulator,
            endpoints: CrowdfundingEndpoints
        ): Unit =
            // Create campaign
            provider.setSlot(beforeSlot)
            val (_, campaignId) = endpoints
                .createCampaign(
                  recipientAddress = recipientAddress,
                  goal = goal,
                  deadline = deadline.toLong,
                  initialValue = initialCampaignValue,
                  signer = recipientParty.signer
                )
                .await()

            // Donate less than goal
            endpoints
                .donate(
                  campaignId = campaignId,
                  donorAddress = donor1Address,
                  amount = 3_000_000L,
                  signer = donor1Party.signer
                )
                .await()

            // Move past deadline
            provider.setSlot(afterSlot)

            // Find donation UTxOs at script address (unified design: tokens + ADA in same UTxO)
            val donationUtxos = endpoints.findDonationUtxos(campaignId).await()

            // Reclaim - donor identified from DonationDatum, funds go back to original donor
            val result = scala.util.Try {
                endpoints
                    .reclaim(
                      campaignId = campaignId,
                      donationUtxos = donationUtxos,
                      signer = donor1Party.signer
                    )
                    .await()
            }

            verifyResult(result)

        private def runReclaimDuplicateIndicesTest(
            provider: Emulator,
            endpoints: CrowdfundingEndpoints
        ): Unit =
            // Create campaign
            provider.setSlot(beforeSlot)
            val (_, campaignId) = endpoints
                .createCampaign(
                  recipientAddress = recipientAddress,
                  goal = goal,
                  deadline = deadline.toLong,
                  initialValue = initialCampaignValue,
                  signer = recipientParty.signer
                )
                .await()

            // Donate less than goal
            endpoints
                .donate(
                  campaignId = campaignId,
                  donorAddress = donor1Address,
                  amount = 3_000_000L,
                  signer = donor1Party.signer
                )
                .await()

            // Move past deadline
            provider.setSlot(afterSlot)

            // Find donation UTxOs
            val donationUtxos = endpoints.findDonationUtxos(campaignId).await()

            // Build malicious transaction with duplicate indices
            val result = scala.util.Try {
                buildMaliciousReclaimTx(
                  provider,
                  endpoints,
                  campaignId,
                  donationUtxos,
                  donor1Party.signer
                ).await()
            }

            verifyResult(result)

        /** Build a malicious reclaim transaction with duplicate donation indices.
          *
          * This simulates an attacker trying to double-claim a donation.
          */
        private def buildMaliciousReclaimTx(
            provider: Emulator,
            endpoints: CrowdfundingEndpoints,
            campaignId: ByteString,
            donationUtxos: Seq[Utxo],
            signer: TransactionSigner
        )(using ExecutionContext): Future[Transaction] =
            import scalus.cardano.txbuilder.TxBuilder
            import scalus.builtin.Data.toData
            import java.time.Instant

            for
                campaignUtxo <- endpoints.findCampaignUtxo(campaignId).map(_.get)
                currentDatum = campaignUtxo.output.requireInlineDatum.to[CampaignDatum]

                donationPolicyId = ScriptHash.fromByteString(currentDatum.donationPolicyId)
                donationScript = getDonationScript(endpoints, campaignId)

                // Build burn map
                burnMap = donationUtxos
                    .flatMap { utxo =>
                        utxo.output.value.assets.assets
                            .getOrElse(donationPolicyId, Map.empty)
                            .map { case (name, qty) => (name, -qty.toLong) }
                    }
                    .groupBy(_._1)
                    .map { case (name, pairs) => (name, pairs.map(_._2).sum) }

                nftAsset = AssetName(campaignId)
                crowdfundingPolicyId = crowdfundingContract.script.scriptHash

                // MALICIOUS: Use duplicate indices to try double-claiming
                maliciousRedeemer = (tx: Transaction) => {
                    val inputIdx = tx.body.value.inputs.toSeq.indexOf(campaignUtxo.input)
                    val donationIdx = donationUtxos.headOption
                        .map(u => tx.body.value.inputs.toSeq.indexOf(u.input))
                        .getOrElse(1)
                    // Attack: same index twice!
                    val duplicateIndices =
                        scalus.prelude.List(BigInt(donationIdx), BigInt(donationIdx))
                    val outputIndices = scalus.prelude.List(BigInt(0), BigInt(1))
                    Action
                        .Reclaim(
                          BigInt(inputIdx),
                          BigInt(-1),
                          duplicateIndices, // DUPLICATE!
                          outputIndices
                        )
                        .toData
                }

                donorPkh = extractDonorPkh(donor1Address)
                donorKeyHash = AddrKeyHash.fromByteString(donorPkh.hash)

                // Build the malicious transaction
                builderWithCampaign = TxBuilder(env).spend(
                  campaignUtxo,
                  redeemerBuilder = maliciousRedeemer,
                  crowdfundingContract.script,
                  Set(donorKeyHash)
                )

                builderWithDonations = donationUtxos.foldLeft(builderWithCampaign) {
                    (builder, utxo) =>
                        builder.spend(
                          utxo,
                          maliciousRedeemer,
                          crowdfundingContract.script,
                          Set.empty
                        )
                }

                builderWithBurn = builderWithDonations.mint(
                  donationScript,
                  burnMap,
                  maliciousRedeemer
                )

                // Two outputs to try double-claiming
                amount = 3_000_000L
                builderWithPayments = builderWithBurn
                    .payTo(donor1Address, Value.lovelace(amount))
                    .payTo(donor1Address, Value.lovelace(amount)) // Second output for double-claim

                tx <- builderWithPayments
                    .validFrom(Instant.ofEpochMilli(currentDatum.deadline.toLong + 1000))
                    .complete(provider, donor1Address)
                    .map(_.sign(signer).transaction)

                _ <- provider.submit(tx).map {
                    case Right(_)    => ()
                    case Left(error) => throw RuntimeException(s"Failed to submit: $error")
                }
            yield tx

        private def getDonationScript(
            endpoints: CrowdfundingEndpoints,
            campaignId: ByteString
        ): Script.PlutusV3 =
            val appliedProgram = donationMintingContract.program $ campaignId.toData
            Script.PlutusV3(appliedProgram.cborByteString)

        private def extractDonorPkh(address: ShelleyAddress): PubKeyHash =
            address.payment match
                case ShelleyPaymentPart.Key(hash) => PubKeyHash(hash)
                case _ => throw IllegalArgumentException("Expected key payment")

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
        // Each party gets multiple UTxOs - one for spending/fees, one for collateral
        Emulator(
          initialUtxos = Map(
            // Recipient UTxOs
            Input(genesisHash, 0) ->
                TransactionOutput.Babbage(
                  address = recipientAddress,
                  value = Value.lovelace(50_000_000L)
                ),
            Input(genesisHash, 1) ->
                TransactionOutput.Babbage(
                  address = recipientAddress,
                  value = Value.lovelace(50_000_000L)
                ),
            // Donor1 UTxOs
            Input(genesisHash, 2) ->
                TransactionOutput.Babbage(
                  address = donor1Address,
                  value = Value.lovelace(50_000_000L)
                ),
            Input(genesisHash, 3) ->
                TransactionOutput.Babbage(
                  address = donor1Address,
                  value = Value.lovelace(50_000_000L)
                ),
            // Donor2 UTxOs
            Input(genesisHash, 4) ->
                TransactionOutput.Babbage(
                  address = donor2Address,
                  value = Value.lovelace(50_000_000L)
                ),
            Input(genesisHash, 5) ->
                TransactionOutput.Babbage(
                  address = donor2Address,
                  value = Value.lovelace(50_000_000L)
                )
          ),
          initialContext = Context.testMainnet(),
          mutators = Set(PlutusScriptsTransactionMutator)
        )
}
