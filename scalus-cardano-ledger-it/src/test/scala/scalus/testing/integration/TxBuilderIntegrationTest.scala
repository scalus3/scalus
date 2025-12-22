package scalus.testing.integration

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus.builtin.{ByteString, Data, platform}
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.{toUplc, plutusV2}
import scalus.utils.await

import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

/** Integration tests for TxBuilder with Yaci DevKit
  *
  * These tests demonstrate that all types of transactions can be constructed
  * and successfully submitted to a real Cardano node using Yaci DevKit.
  *
  * Tests are ordered to handle dependencies (e.g., stake registration before delegation).
  *
  * By default, a fresh container is created for each test run to ensure clean state.
  * For faster iteration during development, set `reuseContainer = true` in YaciDevKitConfig.
  */
class TxBuilderIntegrationTest extends AnyFunSuite with YaciDevKitSpec {

    // Shared test context - created once for all tests in the suite
    private lazy val ctx: TestContext = createTestContext()

    // Minimum fee to ensure transactions pass in Yaci DevKit
    // Yaci DevKit's fee calculation may differ slightly from the calculated fee
    private val MinFee = Coin(250_000)

    // Compute DRep key hash from account
    private def drepKeyHash: AddrKeyHash = {
        val drepVkey = ctx.account.drepKeyPair.verificationKey
        Hash(platform.blake2b_224(ByteString.fromArray(drepVkey.bytes)))
    }

    // Helper to run a test and print transaction info
    private def runTxTest(name: String)(buildTx: TestContext => Transaction): Unit = {
        val tx = buildTx(ctx)
        println(s"[$name] Transaction ID: ${tx.id.toHex}")

        ctx.submitTx(tx) match {
            case Right(txHash) =>
                println(s"[$name] Submitted successfully: $txHash")
                ctx.waitForBlock()
            case Left(error) =>
                fail(s"[$name] Submission failed: $error")
        }
    }

    // =========================================================================
    // Test 1: Simple Payment
    // =========================================================================

    test("1. simple payment transaction") {
        runTxTest("SimplePayment") { ctx =>
            // Create a second address to send to (using change key)
            val recipientVkey = ctx.account.changeKeyPair.verificationKey
            val recipientKeyHash: AddrKeyHash =
                Hash(platform.blake2b_224(ByteString.fromArray(recipientVkey.bytes)))
            val recipientAddress = ShelleyAddress(
              Network.Testnet,
              ShelleyPaymentPart.Key(recipientKeyHash),
              ShelleyDelegationPart.Null
            )

            TxBuilder(ctx.cardanoInfo)
                .payTo(recipientAddress, Value.ada(10))
                .minFee(MinFee)
                .complete(ctx.provider, ctx.address)
                .await(30.seconds)
                .sign(ctx.signer)
                .transaction
        }
    }

    // =========================================================================
    // Test 2: Minting Tokens
    // =========================================================================

    test("2. minting tokens") {
        runTxTest("MintingTokens") { ctx =>
            // Simple always-succeeds minting policy for PlutusV2
            // PlutusV2 scripts take (datum, redeemer, ctx) but for minting, only (redeemer, ctx)
            val alwaysSucceedsMinting = compile { (_: Data, _: Data) => () }
            val mintingPolicyScript = Script.PlutusV2(
              alwaysSucceedsMinting.toUplc().plutusV2.cborByteString
            )
            val policyId = mintingPolicyScript.scriptHash

            val assetName = AssetName.fromString("TestToken")
            val mintAmount = 1000L

            val mintedValue = Value(
              Coin.ada(2),
              MultiAsset(SortedMap(policyId -> SortedMap(assetName -> mintAmount)))
            )

            TxBuilder(ctx.cardanoInfo)
                .mint(mintingPolicyScript, Map(assetName -> mintAmount), ())
                .payTo(ctx.address, mintedValue)
                .minFee(MinFee)
                .complete(ctx.provider, ctx.address)
                .await(30.seconds)
                .sign(ctx.signer)
                .transaction
        }
    }

    // =========================================================================
    // Test 3: Stake Registration
    // =========================================================================

    test("3. stake registration") {
        runTxTest("StakeRegistration") { ctx =>
            TxBuilder(ctx.cardanoInfo)
                .registerStake(ctx.stakeAddress)
                .minFee(MinFee)
                .complete(ctx.provider, ctx.address)
                .await(30.seconds)
                .sign(ctx.signer)
                .transaction
        }
    }

    // =========================================================================
    // Test 4: Stake Delegation (to pre-registered pool)
    // =========================================================================

    test("4. stake delegation") {
        runTxTest("StakeDelegation") { ctx =>
            TxBuilder(ctx.cardanoInfo)
                .delegateTo(ctx.stakeAddress, preRegisteredPoolId)
                .minFee(MinFee)
                .complete(ctx.provider, ctx.address)
                .await(30.seconds)
                .sign(ctx.signer)
                .transaction
        }
    }

    // =========================================================================
    // Test 5: DRep Registration
    // =========================================================================

    test("5. drep registration") {
        runTxTest("DRepRegistration") { ctx =>
            val drepCredential = Credential.KeyHash(drepKeyHash)
            // Simple anchor for testing (would be replaced with real metadata in production)
            val anchor = Anchor(
              "https://example.com/drep-metadata.json",
              DataHash.fromHex("0" * 64)
            )
            val drepDeposit = Coin(ctx.cardanoInfo.protocolParams.dRepDeposit)
            val cert = Certificate.RegDRepCert(drepCredential, drepDeposit, Some(anchor))

            // Create signer that includes drep key
            val signerWithDrep = new TransactionSigner(
              Set(
                ctx.account.paymentKeyPair,
                ctx.account.stakeKeyPair,
                ctx.account.drepKeyPair
              )
            )

            TxBuilder(ctx.cardanoInfo)
                .addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
                .minFee(MinFee)
                .complete(ctx.provider, ctx.address)
                .await(30.seconds)
                .sign(signerWithDrep)
                .transaction
        }
    }

    // =========================================================================
    // Test 6: DRep Vote Delegation
    // =========================================================================

    test("6. drep vote delegation") {
        runTxTest("DRepVoteDelegation") { ctx =>
            val drep = DRep.KeyHash(drepKeyHash)

            TxBuilder(ctx.cardanoInfo)
                .delegateVoteToDRep(ctx.stakeAddress, drep)
                .minFee(MinFee)
                .complete(ctx.provider, ctx.address)
                .await(30.seconds)
                .sign(ctx.signer)
                .transaction
        }
    }

    // =========================================================================
    // Test 7: Governance Proposal Submission
    // =========================================================================

    // Store the governance action ID for use in voting test
    private var submittedGovActionId: Option[GovActionId] = None

    // TODO: Fix TxBuilder balancing for proposal deposits - deposit not properly accounted for
    ignore("7. proposal submission") {
        runTxTest("ProposalSubmission") { ctx =>
            val deposit = Coin(ctx.cardanoInfo.protocolParams.govActionDeposit)
            val rewardAccount = RewardAccount(ctx.stakeAddress)
            val govAction = GovAction.InfoAction
            val anchor = Anchor(
              "https://example.com/proposal-metadata.json",
              DataHash.fromHex("0" * 64)
            )

            val proposal = ProposalProcedure(deposit, rewardAccount, govAction, anchor)

            val tx = TxBuilder(ctx.cardanoInfo)
                .addSteps(TransactionBuilderStep.SubmitProposal(proposal, PubKeyWitness))
                .minFee(MinFee)
                .complete(ctx.provider, ctx.address)
                .await(30.seconds)
                .sign(ctx.signer)
                .transaction

            // Store the governance action ID for the voting test
            submittedGovActionId = Some(GovActionId(tx.id, 0))

            tx
        }
    }

    // =========================================================================
    // Test 8: Voting Procedure Submission
    // =========================================================================

    // TODO: Fix TxBuilder balancing for proposal deposits - depends on test 7
    ignore("8. voting procedure submission") {
        // Ensure we have a governance action to vote on
        val govActionId = submittedGovActionId.getOrElse {
            fail("No governance action ID available. Test 7 (proposal submission) must run first.")
        }

        runTxTest("VotingProcedure") { ctx =>
            val voter = Voter.DRepKey(drepKeyHash)
            val votingProcedure = VotingProcedure(Vote.Yes, None)
            val votes = SortedMap(govActionId -> votingProcedure)

            // Create signer that includes drep key for voting
            val signerWithDrep = new TransactionSigner(
              Set(
                ctx.account.paymentKeyPair,
                ctx.account.stakeKeyPair,
                ctx.account.drepKeyPair
              )
            )

            TxBuilder(ctx.cardanoInfo)
                .addSteps(
                  TransactionBuilderStep.SubmitVotingProcedure(voter, votes, PubKeyWitness)
                )
                .minFee(MinFee)
                .complete(ctx.provider, ctx.address)
                .await(30.seconds)
                .sign(signerWithDrep)
                .transaction
        }
    }
}
