package scalus.examples.crowdfunding

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.node.Emulator
import scalus.ledger.api.v1.PosixTime
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import scalus.testing.kit.TestUtil.genesisHash
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.utils.await

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
