package scalus.examples.paymentsplitter

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.Data.toData
import scalus.cardano.ledger.ExUnits
import scalus.cardano.onchain.plutus.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2.TxOut
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.v3.ScriptInfo.{RewardingScript, SpendingScript}
import scalus.cardano.onchain.plutus.prelude.{List as SList, Option as SOption, SortedMap}
import scalus.testing.kit.ScalusTest

/** Tests for OptimizedPaymentSplitterValidator using shared test cases. */
class OptimizedPaymentSplitterValidatorTest
    extends AnyFunSuite
    with ScalusTest
    with PaymentSplitterTestCases {

    private val contract = OptimizedPaymentSplitterContract.compiled.withErrorTraces
    private val lockTxId = random[TxId]
    private val payeesTxId = random[TxId]
    private val txId = random[TxId]
    private val scriptHash = contract.script.scriptHash

    private val expectedRewardBudgets: Map[String, ExUnits] = ScalaCompilerVersion.baseline(
      pre38 = Map(
        "success when payments are correctly split for a single payee" -> ExUnits(
          memory = 221310L,
          steps = 71183860L
        ),
        "success when payments are correctly split between 2 payees" -> ExUnits(
          memory = 297952L,
          steps = 97573439L
        ),
        "success when payments are correctly split between 3 payees" -> ExUnits(
          memory = 378692L,
          steps = 126202370L
        ),
        "success when split equally and remainder compensates fee - o1" -> ExUnits(
          memory = 378692L,
          steps = 126202370L
        ),
        "success when split equally and remainder compensates fee - o2" -> ExUnits(
          memory = 378692L,
          steps = 126202370L
        ),
        "success when split equally and remainder compensates fee - o3" -> ExUnits(
          memory = 378692L,
          steps = 126202370L
        ),
        "success between 5 payees" -> ExUnits(memory = 552466L, steps = 190178288L),
        "success with multiple contract UTxOs" -> ExUnits(memory = 523938L, steps = 177448748L)
      ),
      since38 = Map(
        "success when payments are correctly split for a single payee" -> ExUnits(
          memory = 216394L,
          steps = 69441311L
        ),
        "success when payments are correctly split between 2 payees" -> ExUnits(
          memory = 293036L,
          steps = 95830890L
        ),
        "success when payments are correctly split between 3 payees" -> ExUnits(
          memory = 373776L,
          steps = 124459821L
        ),
        "success when split equally and remainder compensates fee - o1" -> ExUnits(
          memory = 373776L,
          steps = 124459821L
        ),
        "success when split equally and remainder compensates fee - o2" -> ExUnits(
          memory = 373776L,
          steps = 124459821L
        ),
        "success when split equally and remainder compensates fee - o3" -> ExUnits(
          memory = 373776L,
          steps = 124459821L
        ),
        "success between 5 payees" -> ExUnits(memory = 547550L, steps = 188435739L),
        "success with multiple contract UTxOs" -> ExUnits(memory = 519022L, steps = 175706199L)
      )
    )

    private val expectedSpendBudget: ExUnits = ExUnits(memory = 61924, steps = 19445022)

    // Run all shared test cases
    testCases.foreach { tc =>
        test(s"Optimized: ${tc.name}") {
            runTestCase(tc)
        }
    }

    test("Optimized: budget comparison with multiple UTxOs") {
        val tc = testCases.find(_.name.contains("multiple contract UTxOs")).get
        val (rewardBudget, spendBudget) = runTestCaseWithBudget(tc)
        assert(
          rewardBudget == ScalaCompilerVersion.baseline(
            pre38 = ExUnits(memory = 523938L, steps = 177448748L),
            since38 = ExUnits(memory = 519022L, steps = 175706199L)
          )
        )
        assert(spendBudget == ExUnits(memory = 61924, steps = 19445022))
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
            expectedRewardBudgets.get(tc.name).foreach { expected =>
                assert(rewardResult.budget == expected, s"Reward budget mismatch for '${tc.name}'")
            }
            assert(
              spendResult.budget == expectedSpendBudget,
              s"Spend budget mismatch for '${tc.name}'"
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
