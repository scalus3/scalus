package scalus.patterns

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.ledger.ExUnits
import scalus.examples.{SplitVerificationRedeemer, StakeValidatorPaymentSplitterContract}
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v1.{Address, Credential, PubKeyHash, Value}
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.ScriptInfo.{RewardingScript, SpendingScript}
import scalus.prelude.{List, Option as POption, SortedMap}
import scalus.testing.kit.ScalusTest

/** Tests for the Stake Validator Payment Splitter pattern.
  *
  * Demonstrates that the pattern correctly validates payment splits while running the heavy
  * computation only once (in the reward endpoint).
  */
class StakeValidatorPaymentSplitterTest extends AnyFunSuite with ScalusTest {
    import Payee.*

    private val contract = StakeValidatorPaymentSplitterContract.withErrorTraces
    private val lockTxId = random[TxId]
    private val payeesTxId = random[TxId]
    private val txId = random[TxId]
    private val scriptHash = contract.script.scriptHash

    test("success: single payee receives full amount") {
        TestCase(
          payees = scala.List(A),
          contractInputs = scala.List(30L),
          feePayerInput = (A, 10L),
          outputs = scala.List(A -> 38L),
          fee = 2L,
          splitPerPayee = 30L,
          expectedResult = success
        ).run()
    }

    test("success: two payees split equally") {
        TestCase(
          payees = scala.List(A, B),
          contractInputs = scala.List(30L),
          feePayerInput = (A, 10L),
          outputs = scala.List(A -> 23L, B -> 15L),
          fee = 2L,
          splitPerPayee = 15L,
          expectedResult = success
        ).run()
    }

    test("success: three payees split equally") {
        TestCase(
          payees = scala.List(A, B, C),
          contractInputs = scala.List(30L),
          feePayerInput = (A, 10L),
          outputs = scala.List(A -> 18L, B -> 10L, C -> 10L),
          fee = 2L,
          splitPerPayee = 10L,
          expectedResult = success
        ).run()
    }

    test("success: multiple contract UTxOs spent in one transaction") {
        // This is where the stake validator pattern shines - multiple UTxOs, one computation
        TestCase(
          payees = scala.List(A, B, C),
          contractInputs = scala.List(30L, 20L, 10L), // 3 contract UTxOs = 60 total
          feePayerInput = (A, 10L),
          outputs = scala.List(A -> 28L, B -> 20L, C -> 20L),
          fee = 2L,
          splitPerPayee = 20L,
          expectedResult = success
        ).run()
    }

    test("success: five payees with remainder going to change payee") {
        TestCase(
          payees = scala.List(A, B, C, D, E),
          contractInputs = scala.List(53L), // 53 / 5 = 10 remainder 3
          feePayerInput = (A, 10L),
          outputs = scala.List(A -> 21L, B -> 10L, C -> 10L, D -> 10L, E -> 10L),
          fee = 2L,
          splitPerPayee = 10L,
          expectedResult = success
        ).run()
    }

    test("failure: sumContractInputs mismatch") {
        TestCase(
          payees = scala.List(A, B),
          contractInputs = scala.List(30L),
          feePayerInput = (A, 10L),
          outputs = scala.List(A -> 23L, B -> 15L),
          fee = 2L,
          splitPerPayee = 15L,
          overrideSumContractInputs = scala.Some(50L), // Wrong value!
          expectedResult = failure("sumContractInputs mismatch")
        ).run()
    }

    test("failure: not all payees paid") {
        TestCase(
          payees = scala.List(A, B, C),
          contractInputs = scala.List(30L),
          feePayerInput = (A, 10L),
          outputs = scala.List(A -> 28L, B -> 10L), // C not paid!
          fee = 2L,
          splitPerPayee = 10L,
          overrideNPayed = scala.Some(2L),
          expectedResult = failure("Not all payees were paid")
        ).run()
    }

    test("failure: payee receives wrong split amount") {
        TestCase(
          payees = scala.List(A, B, C),
          contractInputs = scala.List(30L),
          feePayerInput = (A, 10L),
          outputs = scala.List(A -> 18L, B -> 15L, C -> 5L), // B and C wrong
          fee = 2L,
          splitPerPayee = 10L,
          expectedResult = failure("Payee must receive exact split")
        ).run()
    }

    test("failure: output to non-payee") {
        val nonPayee = Payee.F
        TestCase(
          payees = scala.List(A, B),
          contractInputs = scala.List(30L),
          feePayerInput = (A, 10L),
          outputs = scala.List(A -> 18L, B -> 10L, nonPayee -> 10L),
          fee = 2L,
          splitPerPayee = 10L,
          overrideNPayed = scala.Some(3L),
          expectedResult = failure("Output must be to a payee")
        ).run()
    }

    test("budget comparison: stake validator pattern with 3 UTxOs") {
        // This test demonstrates the budget savings of the stake validator pattern
        // when spending multiple UTxOs
        val testCase = TestCase(
          payees = scala.List(A, B, C),
          contractInputs = scala.List(30L, 20L, 10L), // 3 contract UTxOs = 60 total
          feePayerInput = (A, 10L),
          outputs = scala.List(A -> 28L, B -> 20L, C -> 20L),
          fee = 2L,
          splitPerPayee = 20L,
          expectedResult = success
        )

        // Run and get budget
        val (rewardBudget, spendBudget) = testCase.runWithBudget()

        println(s"\n=== Stake Validator Payment Splitter Budget (3 UTxOs) ===")
        println(
          s"Reward endpoint (runs once):    mem=${rewardBudget.memory}, cpu=${rewardBudget.steps}"
        )
        println(
          s"Spend endpoint (per UTxO):      mem=${spendBudget.memory}, cpu=${spendBudget.steps}"
        )
        println(
          s"Total for 3 UTxOs: mem=${rewardBudget.memory + 3 * spendBudget.memory}, cpu=${rewardBudget.steps + 3 * spendBudget.steps}"
        )
        println(s"Compare with original PaymentSplitter running full logic 3 times!")
        println()
    }

    // Test helpers

    enum Payee(val pkh: ByteString):
        case A extends Payee(genByteStringOfN(28).sample.get)
        case B extends Payee(genByteStringOfN(28).sample.get)
        case C extends Payee(genByteStringOfN(28).sample.get)
        case D extends Payee(genByteStringOfN(28).sample.get)
        case E extends Payee(genByteStringOfN(28).sample.get)
        case F extends Payee(genByteStringOfN(28).sample.get)

    case class TestCase(
        payees: scala.List[Payee],
        contractInputs: scala.List[Long],
        feePayerInput: (Payee, Long),
        outputs: scala.List[(Payee, Long)],
        fee: Long,
        splitPerPayee: Long,
        overrideSumContractInputs: scala.Option[Long] = scala.None,
        overrideNPayed: scala.Option[Long] = scala.None,
        expectedResult: (String | Unit, POption[ExUnits])
    ) {
        def run(): Unit = {
            val payeesList = List.from(payees.map(_.pkh))
            val payeesData = payeesList.toData

            val sumContractInputs =
                overrideSumContractInputs.getOrElse(contractInputs.sum)
            val nPayed = overrideNPayed.getOrElse(outputs.map(_._1).distinct.size.toLong)

            val verification = SplitVerificationRedeemer(
              payeeWithChange = PubKeyHash(feePayerInput._1.pkh),
              sumContractInputs = BigInt(sumContractInputs),
              splitPerPayee = BigInt(splitPerPayee),
              nPayed = BigInt(nPayed)
            )

            // Build inputs
            val scriptInputs = contractInputs.zipWithIndex.map { case (value, idx) =>
                TxInInfo(
                  outRef = TxOutRef(lockTxId, idx),
                  resolved = TxOut(
                    address = Address(ScriptCredential(scriptHash), scalus.prelude.Option.None),
                    value = Value.lovelace(BigInt(value))
                  )
                )
            }

            val feePayerTxIn = TxInInfo(
              outRef = TxOutRef(payeesTxId, 0),
              resolved = TxOut(
                address = Address(
                  PubKeyCredential(PubKeyHash(feePayerInput._1.pkh)),
                  scalus.prelude.Option.None
                ),
                value = Value.lovelace(BigInt(feePayerInput._2))
              )
            )

            val allInputs = List.from(feePayerTxIn :: scriptInputs)

            // Build outputs
            val txOutputs = List.from(outputs.map { case (payee, amount) =>
                TxOut(
                  address =
                      Address(PubKeyCredential(PubKeyHash(payee.pkh)), scalus.prelude.Option.None),
                  value = Value.lovelace(BigInt(amount))
                )
            })

            // Build withdrawals (for withdraw zero trick)
            val withdrawals = SortedMap.fromList(
              List((Credential.ScriptCredential(scriptHash), BigInt(0)))
            )

            // Build redeemers map - need both spending and rewarding
            val spendingRedeemer = Data.unit
            val rewardingRedeemer = verification.toData

            val firstScriptOutRef = TxOutRef(lockTxId, 0)
            val stakingCredential = Credential.ScriptCredential(scriptHash)

            val redeemers = SortedMap.fromList(
              List(
                (ScriptPurpose.Spending(firstScriptOutRef), spendingRedeemer),
                (ScriptPurpose.Rewarding(stakingCredential), rewardingRedeemer)
              )
            )

            val txInfo = TxInfo(
              inputs = allInputs,
              outputs = txOutputs,
              fee = BigInt(fee),
              withdrawals = withdrawals,
              redeemers = redeemers,
              id = txId
            )

            val applied = contract.program $ payeesData

            // Test reward endpoint (where actual validation happens)
            val rewardContext = ScriptContext(
              txInfo = txInfo,
              redeemer = rewardingRedeemer,
              scriptInfo = RewardingScript(stakingCredential)
            )
            val rewardResult = (applied $ rewardContext.toData).evaluateDebug
            checkResult(expectedResult, rewardResult)

            // Also test spending endpoint if reward succeeded
            if expectedResult._1.isInstanceOf[Unit] then {
                val spendContext = ScriptContext(
                  txInfo = txInfo,
                  redeemer = spendingRedeemer,
                  scriptInfo = SpendingScript(txOutRef = firstScriptOutRef)
                )
                val spendResult = (applied $ spendContext.toData).evaluateDebug
                checkResult(expectedResult, spendResult)
            }
        }

        def runWithBudget(): (ExUnits, ExUnits) = {
            val payeesList = List.from(payees.map(_.pkh))
            val payeesData = payeesList.toData

            val sumContractInputs =
                overrideSumContractInputs.getOrElse(contractInputs.sum)
            val nPayed = overrideNPayed.getOrElse(outputs.map(_._1).distinct.size.toLong)

            val verification = SplitVerificationRedeemer(
              payeeWithChange = PubKeyHash(feePayerInput._1.pkh),
              sumContractInputs = BigInt(sumContractInputs),
              splitPerPayee = BigInt(splitPerPayee),
              nPayed = BigInt(nPayed)
            )

            // Build inputs
            val scriptInputs = contractInputs.zipWithIndex.map { case (value, idx) =>
                TxInInfo(
                  outRef = TxOutRef(lockTxId, idx),
                  resolved = TxOut(
                    address = Address(ScriptCredential(scriptHash), scalus.prelude.Option.None),
                    value = Value.lovelace(BigInt(value))
                  )
                )
            }

            val feePayerTxIn = TxInInfo(
              outRef = TxOutRef(payeesTxId, 0),
              resolved = TxOut(
                address = Address(
                  PubKeyCredential(PubKeyHash(feePayerInput._1.pkh)),
                  scalus.prelude.Option.None
                ),
                value = Value.lovelace(BigInt(feePayerInput._2))
              )
            )

            val allInputs = List.from(feePayerTxIn :: scriptInputs)

            // Build outputs
            val txOutputs = List.from(outputs.map { case (payee, amount) =>
                TxOut(
                  address =
                      Address(PubKeyCredential(PubKeyHash(payee.pkh)), scalus.prelude.Option.None),
                  value = Value.lovelace(BigInt(amount))
                )
            })

            // Build withdrawals (for withdraw zero trick)
            val withdrawals = SortedMap.fromList(
              List((Credential.ScriptCredential(scriptHash), BigInt(0)))
            )

            // Build redeemers map
            val spendingRedeemer = Data.unit
            val rewardingRedeemer = verification.toData

            val firstScriptOutRef = TxOutRef(lockTxId, 0)
            val stakingCredential = Credential.ScriptCredential(scriptHash)

            val redeemers = SortedMap.fromList(
              List(
                (ScriptPurpose.Spending(firstScriptOutRef), spendingRedeemer),
                (ScriptPurpose.Rewarding(stakingCredential), rewardingRedeemer)
              )
            )

            val txInfo = TxInfo(
              inputs = allInputs,
              outputs = txOutputs,
              fee = BigInt(fee),
              withdrawals = withdrawals,
              redeemers = redeemers,
              id = txId
            )

            val applied = contract.program $ payeesData

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
}
