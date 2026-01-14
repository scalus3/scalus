package scalus.examples.crowdfunding

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.ledger.api.v1.{Address, Credential, PubKeyHash, Value}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.ScriptInfo.SpendingScript
import scalus.prelude.{List, Option}
import scalus.testing.kit.ScalusTest

class CrowdfundingValidatorTest extends AnyFunSuite, ScalusTest {

    private val crowdfundingContract = CrowdfundingContract.withErrorTraces

    test(s"Crowdfunding validator size is ${CrowdfundingContract.script.script.size} bytes") {
        println(s"Crowdfunding validator size: ${CrowdfundingContract.script.script.size} bytes")
        assert(CrowdfundingContract.script.script.size > 0)
    }

    test(s"Donation minting policy size is ${DonationMintingContract.script.script.size} bytes") {
        println(
          s"Donation minting policy size: ${DonationMintingContract.script.script.size} bytes"
        )
        assert(DonationMintingContract.script.script.size > 0)
    }

    test("donationTokenName is fixed empty ByteString") {
        assert(
          DonationMintingPolicy.donationTokenName == ByteString.empty,
          "Donation token name should be empty ByteString"
        )
    }

    test("Create campaign - validates recipient signature required") {
        val recipientPkh = random[PubKeyHash]
        val otherPkh = random[PubKeyHash]
        val deadline = BigInt(System.currentTimeMillis() + 86400000) // 1 day from now
        val goal = BigInt(10_000_000)

        val txOutRef = random[TxOutRef]
        val campaignId = scalus.builtin.Builtins.blake2b_256(
          scalus.builtin.Builtins.serialiseData(txOutRef.toData)
        )
        val policyId = crowdfundingContract.script.scriptHash

        // Donation policy ID would be computed from applied program
        val donationPolicyId = ByteString.fromHex("00" * 28)

        val datum = CampaignDatum(
          totalSum = BigInt(0),
          goal = goal,
          recipient = recipientPkh,
          deadline = deadline,
          withdrawn = BigInt(0),
          donationPolicyId = donationPolicyId
        )

        val redeemer = Action.Create(goal, recipientPkh, deadline)

        // Create a transaction context where recipient did NOT sign
        val context = ScriptContext(
          txInfo = TxInfo(
            inputs = List(
              TxInInfo(
                outRef = txOutRef,
                resolved = TxOut(
                  address = Address(Credential.PubKeyCredential(otherPkh), Option.None),
                  value = Value.lovelace(10_000_000)
                )
              )
            ),
            outputs = List(
              TxOut(
                address = Address(Credential.ScriptCredential(policyId), Option.None),
                value = Value.lovelace(5_000_000) + Value(policyId, campaignId, BigInt(1)),
                datum = OutputDatum.OutputDatum(datum.toData)
              )
            ),
            mint = Value(policyId, campaignId, BigInt(1)),
            signatories = List(otherPkh), // NOT the recipient!
            validRange = Interval.before(deadline),
            id = random[TxId]
          ),
          redeemer = redeemer.toData,
          scriptInfo = ScriptInfo.MintingScript(policyId)
        )

        val program = crowdfundingContract.program $ context.toData
        val result = program.evaluateDebug

        assert(result.isFailure, "Should fail when recipient doesn't sign")
        assert(
          result.logs.exists(_.contains("Recipient must sign")),
          s"Expected 'Recipient must sign' error, got: ${result.logs.mkString(", ")}"
        )
    }

    test("Create campaign - validates goal must be positive") {
        val recipientPkh = random[PubKeyHash]
        val deadline = BigInt(System.currentTimeMillis() + 86400000)
        val goal = BigInt(0) // Invalid: zero goal

        val txOutRef = random[TxOutRef]
        val policyId = crowdfundingContract.script.scriptHash

        val redeemer = Action.Create(goal, recipientPkh, deadline)

        val context = ScriptContext(
          txInfo = TxInfo(
            inputs = List(
              TxInInfo(
                outRef = txOutRef,
                resolved = TxOut(
                  address = Address(Credential.PubKeyCredential(recipientPkh), Option.None),
                  value = Value.lovelace(10_000_000)
                )
              )
            ),
            outputs = List.Nil,
            mint = Value.zero,
            signatories = List(recipientPkh),
            validRange = Interval.before(deadline),
            id = random[TxId]
          ),
          redeemer = redeemer.toData,
          scriptInfo = ScriptInfo.MintingScript(policyId)
        )

        val program = crowdfundingContract.program $ context.toData
        val result = program.evaluateDebug

        assert(result.isFailure, "Should fail when goal is zero")
        assert(
          result.logs.exists(_.contains("Goal must be positive")),
          s"Expected 'Goal must be positive' error, got: ${result.logs.mkString(", ")}"
        )
    }

    test("Donate - validates before deadline") {
        val recipientPkh = random[PubKeyHash]
        val deadline = BigInt(1000) // In the past
        val donationPolicyId = ByteString.fromHex("11" * 28)

        val currentDatum = CampaignDatum(
          totalSum = BigInt(0),
          goal = BigInt(10_000_000),
          recipient = recipientPkh,
          deadline = deadline,
          withdrawn = BigInt(0),
          donationPolicyId = donationPolicyId
        )

        val txOutRef = random[TxOutRef]
        val policyId = crowdfundingContract.script.scriptHash
        val amount = BigInt(5_000_000)

        val redeemer = Action.Donate(
          amount = amount,
          campaignInputIdx = BigInt(0),
          campaignOutputIdx = BigInt(0),
          donationOutputIdx = BigInt(1)
        )

        val context = ScriptContext(
          txInfo = TxInfo(
            inputs = List(
              TxInInfo(
                outRef = txOutRef,
                resolved = TxOut(
                  address = Address(Credential.ScriptCredential(policyId), Option.None),
                  value = Value.lovelace(5_000_000),
                  datum = OutputDatum.OutputDatum(currentDatum.toData)
                )
              )
            ),
            outputs = List.Nil,
            mint = Value.zero,
            signatories = List.Nil,
            validRange = Interval.after(deadline + 1000), // After deadline!
            id = random[TxId]
          ),
          redeemer = redeemer.toData,
          scriptInfo = SpendingScript(txOutRef, Option.None)
        )

        val program = crowdfundingContract.program $ context.toData
        val result = program.evaluateDebug

        assert(result.isFailure, "Should fail when donating after deadline")
        assert(
          result.logs.exists(_.contains("before deadline")),
          s"Expected deadline error, got: ${result.logs.mkString(", ")}"
        )
    }

    test("Withdraw - validates goal must be reached") {
        val recipientPkh = random[PubKeyHash]
        val deadline = BigInt(1000)
        val donationPolicyId = ByteString.fromHex("11" * 28)
        val campaignId = ByteString.fromHex("cc" * 32) // Mock campaign NFT token name

        val currentDatum = CampaignDatum(
          totalSum = BigInt(5_000_000), // Less than goal
          goal = BigInt(10_000_000),
          recipient = recipientPkh,
          deadline = deadline,
          withdrawn = BigInt(0),
          donationPolicyId = donationPolicyId
        )

        val txOutRef = random[TxOutRef]
        val policyId = crowdfundingContract.script.scriptHash

        val redeemer = Action.Withdraw(
          campaignInputIdx = BigInt(0),
          campaignOutputIdx = BigInt(-1),
          recipientOutputIdx = BigInt(0),
          donationInputIndices = List.Nil
        )

        val context = ScriptContext(
          txInfo = TxInfo(
            inputs = List(
              TxInInfo(
                outRef = txOutRef,
                resolved = TxOut(
                  address = Address(Credential.ScriptCredential(policyId), Option.None),
                  // Include campaign NFT in the value
                  value = Value.lovelace(5_000_000) + Value(policyId, campaignId, BigInt(1)),
                  datum = OutputDatum.OutputDatum(currentDatum.toData)
                )
              )
            ),
            outputs = List.Nil,
            mint = Value.zero,
            signatories = List(recipientPkh),
            validRange = Interval.after(deadline + 1), // Must start AFTER deadline
            id = random[TxId]
          ),
          redeemer = redeemer.toData,
          scriptInfo = SpendingScript(txOutRef, Option.None)
        )

        val program = crowdfundingContract.program $ context.toData
        val result = program.evaluateDebug

        assert(result.isFailure, "Should fail when goal not reached")
        assert(
          result.logs.exists(_.contains("Goal must be reached")),
          s"Expected goal error, got: ${result.logs.mkString(", ")}"
        )
    }

    test("Reclaim - validates goal must NOT be reached") {
        val recipientPkh = random[PubKeyHash]
        val deadline = BigInt(1000)
        val donationPolicyId = ByteString.fromHex("11" * 28)
        val campaignId = ByteString.fromHex("cc" * 32) // Mock campaign NFT token name

        val currentDatum = CampaignDatum(
          totalSum = BigInt(15_000_000), // More than goal - success!
          goal = BigInt(10_000_000),
          recipient = recipientPkh,
          deadline = deadline,
          withdrawn = BigInt(0),
          donationPolicyId = donationPolicyId
        )

        val txOutRef = random[TxOutRef]
        val policyId = crowdfundingContract.script.scriptHash

        val redeemer = Action.Reclaim(
          campaignInputIdx = BigInt(0),
          campaignOutputIdx = BigInt(-1),
          donationInputIndices = List.Nil,
          reclaimerOutputIndices = List.Nil
        )

        val context = ScriptContext(
          txInfo = TxInfo(
            inputs = List(
              TxInInfo(
                outRef = txOutRef,
                resolved = TxOut(
                  address = Address(Credential.ScriptCredential(policyId), Option.None),
                  // Include campaign NFT in the value
                  value = Value.lovelace(15_000_000) + Value(policyId, campaignId, BigInt(1)),
                  datum = OutputDatum.OutputDatum(currentDatum.toData)
                )
              )
            ),
            outputs = List.Nil,
            mint = Value.zero,
            signatories = List.Nil,
            validRange = Interval.after(deadline + 1), // Must start AFTER deadline
            id = random[TxId]
          ),
          redeemer = redeemer.toData,
          scriptInfo = SpendingScript(txOutRef, Option.None)
        )

        val program = crowdfundingContract.program $ context.toData
        val result = program.evaluateDebug

        assert(result.isFailure, "Should fail when goal was reached")
        assert(
          result.logs.exists(_.contains("Cannot reclaim if goal was reached")),
          s"Expected reclaim error, got: ${result.logs.mkString(", ")}"
        )
    }

    test("Reclaim - rejects duplicate donation input indices (double-spend prevention)") {
        val recipientPkh = random[PubKeyHash]
        val donorPkh = random[PubKeyHash]
        val deadline = BigInt(1000)
        val donationPolicyId = ByteString.fromHex("11" * 28)
        val campaignId = ByteString.fromHex("cc" * 32) // Mock campaign NFT token name
        val donationAmount = BigInt(5_000_000)
        val tokenName = DonationMintingPolicy.donationTokenName // Fixed token name

        val currentDatum = CampaignDatum(
          totalSum = donationAmount, // Less than goal - reclaim allowed
          goal = BigInt(10_000_000),
          recipient = recipientPkh,
          deadline = deadline,
          withdrawn = BigInt(0),
          donationPolicyId = donationPolicyId
        )

        val donationDatum = DonationDatum(donorPkh, donationAmount)

        val campaignTxOutRef = random[TxOutRef]
        val donationTxOutRef = random[TxOutRef]
        val policyId = crowdfundingContract.script.scriptHash

        // Attacker tries to use the same donation input index twice
        val redeemer = Action.Reclaim(
          campaignInputIdx = BigInt(0),
          campaignOutputIdx = BigInt(-1),
          donationInputIndices = List(BigInt(1), BigInt(1)), // DUPLICATE INDEX - attack attempt
          reclaimerOutputIndices = List(BigInt(0), BigInt(1)) // Two outputs to drain funds
        )

        val context = ScriptContext(
          txInfo = TxInfo(
            inputs = List(
              TxInInfo(
                outRef = campaignTxOutRef,
                resolved = TxOut(
                  address = Address(Credential.ScriptCredential(policyId), Option.None),
                  // Include campaign NFT in the value
                  value = Value.lovelace(2_000_000) + Value(policyId, campaignId, BigInt(1)),
                  datum = OutputDatum.OutputDatum(currentDatum.toData)
                )
              ),
              TxInInfo(
                outRef = donationTxOutRef,
                resolved = TxOut(
                  address = Address(Credential.ScriptCredential(policyId), Option.None),
                  value = Value
                      .lovelace(donationAmount) + Value(donationPolicyId, tokenName, BigInt(1)),
                  datum = OutputDatum.OutputDatum(donationDatum.toData)
                )
              )
            ),
            outputs = List(
              // Two outputs trying to claim the same donation
              TxOut(
                address = Address(Credential.PubKeyCredential(donorPkh), Option.None),
                value = Value.lovelace(donationAmount),
                datum = OutputDatum.NoOutputDatum
              ),
              TxOut(
                address = Address(Credential.PubKeyCredential(donorPkh), Option.None),
                value = Value.lovelace(donationAmount),
                datum = OutputDatum.NoOutputDatum
              )
            ),
            mint = Value(donationPolicyId, tokenName, BigInt(-1)),
            signatories = List(donorPkh),
            validRange = Interval.after(deadline + 1),
            id = random[TxId]
          ),
          redeemer = redeemer.toData,
          scriptInfo = SpendingScript(campaignTxOutRef, Option.None)
        )

        val program = crowdfundingContract.program $ context.toData
        val result = program.evaluateDebug

        assert(result.isFailure, "Should fail when duplicate donation indices are used")
        assert(
          result.logs.exists(_.contains("strictly ascending")),
          s"Expected duplicate index error, got: ${result.logs.mkString(", ")}"
        )
    }
}
