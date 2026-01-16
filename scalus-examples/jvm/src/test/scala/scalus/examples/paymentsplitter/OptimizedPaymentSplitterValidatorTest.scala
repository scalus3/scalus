package scalus.examples.paymentsplitter

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.Data.toData
import scalus.cardano.ledger.ExUnits
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v1.{Address, Credential, PubKeyHash, Value}
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.ScriptInfo.{RewardingScript, SpendingScript}
import scalus.prelude.{List as SList, Option as SOption, SortedMap}
import scalus.testing.kit.ScalusTest

/** Tests for OptimizedPaymentSplitterValidator using shared test cases. */
class OptimizedPaymentSplitterValidatorTest
    extends AnyFunSuite
    with ScalusTest
    with PaymentSplitterTestCases {

    private val contract = OptimizedPaymentSplitterContract.withErrorTraces
    private val lockTxId = random[TxId]
    private val payeesTxId = random[TxId]
    private val txId = random[TxId]
    private val scriptHash = contract.script.scriptHash

    // Run all shared test cases
    testCases.foreach { tc =>
        test(s"Optimized: ${tc.name}") {
            runTestCase(tc)
        }
    }

    test("Optimized: budget comparison with multiple UTxOs") {
        // Find the test case with multiple UTxOs
        val tc = testCases.find(_.name.contains("multiple contract UTxOs")).get

        val (rewardBudget, spendBudget) = runTestCaseWithBudget(tc)

        println(s"\n=== Optimized Payment Splitter Budget (${tc.contractInputs.size} UTxOs) ===")
        println(
          s"Reward endpoint (runs once):    mem=${rewardBudget.memory}, cpu=${rewardBudget.steps}"
        )
        println(
          s"Spend endpoint (per UTxO):      mem=${spendBudget.memory}, cpu=${spendBudget.steps}"
        )
        println(
          s"  (uses indexed access via SpendRedeemer.ownInputIndex - avoids TxOutRef comparison per input)"
        )
        val totalUtxos = tc.contractInputs.size
        println(
          s"Total for $totalUtxos UTxOs: mem=${rewardBudget.memory + totalUtxos * spendBudget.memory}, cpu=${rewardBudget.steps + totalUtxos * spendBudget.steps}"
        )
        println()
    }

    private def runTestCase(tc: PaymentSplitterTestCase): Unit = {
        val payeesList = tc.payees.map(_.pkh)
        val payeesData = payeesList.toData

        val applied = contract.program $ payeesData

        val sumContractInputs = tc.contractInputs.sum
        val nPayed = tc.outputs.asScala.map(_.payee).distinct.size

        val verification = SplitVerificationRedeemer(
          payeeWithChange = PubKeyHash(tc.feePayerInput._1.pkh),
          sumContractInputs = sumContractInputs,
          splitPerPayee = tc.splitPerPayee,
          nPayed = BigInt(nPayed)
        )

        // Build inputs
        val scriptInputs = tc.contractInputs.zipWithIndex.map { case (value, idx) =>
            TxInInfo(
              outRef = TxOutRef(lockTxId, idx),
              resolved = TxOut(
                address = Address(ScriptCredential(scriptHash), SOption.None),
                value = Value.lovelace(value)
              )
            )
        }

        val feePayerInputs =
            if tc.feePayerInput._2 > 0 then
                scala.List(
                  TxInInfo(
                    outRef = TxOutRef(payeesTxId, 0),
                    resolved = TxOut(
                      address = Address(
                        PubKeyCredential(PubKeyHash(tc.feePayerInput._1.pkh)),
                        SOption.None
                      ),
                      value = Value.lovelace(tc.feePayerInput._2)
                    )
                  )
                )
            else scala.List.empty

        val allInputs = SList.from(feePayerInputs ++ scriptInputs)

        // Build outputs
        val txOutputs = tc.outputs.map { case Output(payee, amount) =>
            TxOut(
              address = Address(PubKeyCredential(PubKeyHash(payee.pkh)), SOption.None),
              value = Value.lovelace(amount)
            )
        }

        // Build withdrawals (for withdraw zero trick)
        val withdrawals = SortedMap.fromList(
          SList((Credential.ScriptCredential(scriptHash), BigInt(0)))
        )

        // Build redeemers map
        // First script input is at index 1 if feePayer input exists, otherwise index 0
        val firstScriptInputIndex = if tc.feePayerInput._2 > 0 then 1 else 0
        val spendingRedeemer = SpendRedeemer(BigInt(firstScriptInputIndex)).toData
        val rewardingRedeemer = verification.toData

        val firstScriptOutRef = TxOutRef(lockTxId, 0)
        val stakingCredential = Credential.ScriptCredential(scriptHash)

        val redeemers = SortedMap.fromList(
          SList(
            (ScriptPurpose.Spending(firstScriptOutRef), spendingRedeemer),
            (ScriptPurpose.Rewarding(stakingCredential), rewardingRedeemer)
          )
        )

        val txInfo = TxInfo(
          inputs = allInputs,
          outputs = txOutputs,
          fee = tc.fee,
          withdrawals = withdrawals,
          redeemers = redeemers,
          id = txId
        )

        // Test reward endpoint (where actual validation happens)
        val rewardContext = ScriptContext(
          txInfo = txInfo,
          redeemer = rewardingRedeemer,
          scriptInfo = RewardingScript(stakingCredential)
        )
        val rewardResult = (applied $ rewardContext.toData).evaluateDebug

        if tc.expectedSuccess then
            assert(
              rewardResult.isSuccess,
              clue = s"Expected reward success but got failure: ${rewardResult.logs.mkString(", ")}"
            )

            // Also test spending endpoint if reward succeeded
            val spendContext = ScriptContext(
              txInfo = txInfo,
              redeemer = spendingRedeemer,
              scriptInfo = SpendingScript(txOutRef = firstScriptOutRef)
            )
            val spendResult = (applied $ spendContext.toData).evaluateDebug
            assert(
              spendResult.isSuccess,
              clue = s"Expected spend success but got failure: ${spendResult.logs.mkString(", ")}"
            )
        else
            assert(
              rewardResult.isFailure,
              clue = s"Expected failure but got success"
            )
            assert(
              tc.matchesError(rewardResult.logs),
              clue =
                  s"Expected error matching '${tc.expectedError.getOrElse("")}' but got: ${rewardResult.logs.mkString(", ")}"
            )
    }

    private def runTestCaseWithBudget(tc: PaymentSplitterTestCase): (ExUnits, ExUnits) = {
        val payeesList = tc.payees.map(_.pkh)
        val payeesData = payeesList.toData

        val applied = contract.program $ payeesData

        val sumContractInputs = tc.contractInputs.sum
        val nPayed = tc.outputs.asScala.map(_.payee).distinct.size

        val verification = SplitVerificationRedeemer(
          payeeWithChange = PubKeyHash(tc.feePayerInput._1.pkh),
          sumContractInputs = sumContractInputs,
          splitPerPayee = tc.splitPerPayee,
          nPayed = BigInt(nPayed)
        )

        // Build inputs
        val scriptInputs = tc.contractInputs.zipWithIndex.map { case (value, idx) =>
            TxInInfo(
              outRef = TxOutRef(lockTxId, idx),
              resolved = TxOut(
                address = Address(ScriptCredential(scriptHash), SOption.None),
                value = Value.lovelace(value)
              )
            )
        }

        val feePayerTxIn = TxInInfo(
          outRef = TxOutRef(payeesTxId, 0),
          resolved = TxOut(
            address = Address(PubKeyCredential(PubKeyHash(tc.feePayerInput._1.pkh)), SOption.None),
            value = Value.lovelace(tc.feePayerInput._2)
          )
        )

        val allInputs = SList.from(feePayerTxIn :: scriptInputs)

        // Build outputs
        val txOutputs = tc.outputs.map { case Output(payee, amount) =>
            TxOut(
              address = Address(PubKeyCredential(PubKeyHash(payee.pkh)), SOption.None),
              value = Value.lovelace(amount)
            )
        }

        // Build withdrawals
        val withdrawals = SortedMap.fromList(
          SList((Credential.ScriptCredential(scriptHash), BigInt(0)))
        )

        // Build redeemers
        // First script input is at index 1 (feePayer is at index 0)
        val spendingRedeemer = SpendRedeemer(BigInt(1)).toData
        val rewardingRedeemer = verification.toData

        val firstScriptOutRef = TxOutRef(lockTxId, 0)
        val stakingCredential = Credential.ScriptCredential(scriptHash)

        val redeemers = SortedMap.fromList(
          SList(
            (ScriptPurpose.Spending(firstScriptOutRef), spendingRedeemer),
            (ScriptPurpose.Rewarding(stakingCredential), rewardingRedeemer)
          )
        )

        val txInfo = TxInfo(
          inputs = allInputs,
          outputs = txOutputs,
          fee = tc.fee,
          withdrawals = withdrawals,
          redeemers = redeemers,
          id = txId
        )

        // Get reward endpoint budget
        val rewardContext = ScriptContext(
          txInfo = txInfo,
          redeemer = rewardingRedeemer,
          scriptInfo = RewardingScript(stakingCredential)
        )
        val rewardResult = (applied $ rewardContext.toData).evaluateDebug
        val rewardBudget = rewardResult.budget

        // Get spend endpoint budget
        val spendContext = ScriptContext(
          txInfo = txInfo,
          redeemer = spendingRedeemer,
          scriptInfo = SpendingScript(txOutRef = firstScriptOutRef)
        )
        val spendResult = (applied $ spendContext.toData).evaluateDebug
        val spendBudget = spendResult.budget

        (rewardBudget, spendBudget)
    }
}
