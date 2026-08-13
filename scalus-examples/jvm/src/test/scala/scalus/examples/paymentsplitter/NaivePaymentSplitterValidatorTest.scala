package scalus.examples.paymentsplitter

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.cardano.onchain.plutus.v1.{Address, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2.TxOut
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.v3.ScriptInfo.SpendingScript
import scalus.cardano.onchain.plutus.prelude.{List as SList, Option as SOption, SortedMap}
import scalus.cardano.ledger.ExUnits
import scalus.testing.kit.ScalusTest

/** Tests for NaivePaymentSplitterValidator using shared test cases. */
class NaivePaymentSplitterValidatorTest
    extends AnyFunSuite
    with ScalusTest
    with PaymentSplitterTestCases {

    private val contract = NaivePaymentSplitterContract.compiled.withErrorTraces
    private val lockTxId = random[TxId]
    private val payeesTxId = random[TxId]
    private val txId = random[TxId]
    private val scriptHash = contract.script.scriptHash

    private val expectedBudgets: Map[String, ExUnits] = ScalaCompilerVersion.baseline(
      pre38 = Map(
        "success when payments are correctly split for a single payee" -> ExUnits(
          memory = 204886,
          steps = 65_033072
        ),
        "success when payments are correctly split between 2 payees" -> ExUnits(
          memory = 313118,
          steps = 100_912529
        ),
        "success when payments are correctly split between 3 payees" -> ExUnits(
          memory = 436554,
          steps = 143_746565
        ),
        "success when split equally and remainder compensates fee - o1" -> ExUnits(
          memory = 436554,
          steps = 143_746565
        ),
        "success when split equally and remainder compensates fee - o2" -> ExUnits(
          memory = 436554,
          steps = 143_746565
        ),
        "success when split equally and remainder compensates fee - o3" -> ExUnits(
          memory = 436554,
          steps = 143_746565
        ),
        "success between 5 payees" -> ExUnits(memory = 739235, steps = 254_134967),
        "success with multiple contract UTxOs" -> ExUnits(memory = 539962, steps = 181_238237)
      ),
      since38 = Map(
        "success when payments are correctly split for a single payee" -> ExUnits(
          memory = 196250,
          steps = 61_961149
        ),
        "success when payments are correctly split between 2 payees" -> ExUnits(
          memory = 302622,
          steps = 97_175919
        ),
        "success when payments are correctly split between 3 payees" -> ExUnits(
          memory = 424198,
          steps = 139_345268
        ),
        "success when split equally and remainder compensates fee - o1" -> ExUnits(
          memory = 424198,
          steps = 139_345268
        ),
        "success when split equally and remainder compensates fee - o2" -> ExUnits(
          memory = 424198,
          steps = 139_345268
        ),
        "success when split equally and remainder compensates fee - o3" -> ExUnits(
          memory = 424198,
          steps = 139_345268
        ),
        "success between 5 payees" -> ExUnits(memory = 723159, steps = 248_404296),
        "success with multiple contract UTxOs" -> ExUnits(memory = 527606, steps = 176_836940)
      )
    )

    // Run all shared test cases
    testCases.foreach { tc =>
        test(s"Naive: ${tc.name}") {
            runTestCase(tc)
        }
    }

    test("Naive: rejects a contract input carrying native tokens (no token skim)") {
        import Payee.{A, B}
        val payeesData = SList(A.pkh, B.pkh).toData
        val applied = contract.program $ payeesData

        // One contract UTxO holds 30 ADA *plus* a native token. The outputs split only the ADA
        // (A gets 23, B gets 15) and never account for the token — pre-fix the validator ignored it,
        // letting the fee payer skim the token. The ADA-only guard must reject this.
        val tokenPolicy = genByteStringOfN(28).sample.get
        val contractInput = TxInInfo(
          outRef = TxOutRef(lockTxId, 0),
          resolved = TxOut(
            address = Address(ScriptCredential(scriptHash), SOption.None),
            value = Value.lovelace(30) + Value(tokenPolicy, ByteString.fromString("SKIM"), 1)
          )
        )
        val feePayerInput = TxInInfo(
          outRef = TxOutRef(payeesTxId, 0),
          resolved = TxOut(
            address = Address(PubKeyCredential(PubKeyHash(A.pkh)), SOption.None),
            value = Value.lovelace(10)
          )
        )
        val outputs = SList(
          TxOut(Address(PubKeyCredential(PubKeyHash(A.pkh)), SOption.None), Value.lovelace(23)),
          TxOut(Address(PubKeyCredential(PubKeyHash(B.pkh)), SOption.None), Value.lovelace(15))
        )

        val txOutRef = TxOutRef(lockTxId, 0)
        val context = ScriptContext(
          txInfo = TxInfo(
            inputs = SList(feePayerInput, contractInput),
            outputs = outputs,
            fee = BigInt(2),
            redeemers = SortedMap.fromList(
              SList((ScriptPurpose.Spending(txOutRef), Data.unit))
            ),
            id = txId
          ),
          redeemer = Data.unit,
          scriptInfo = SpendingScript(txOutRef = txOutRef)
        )

        val result = (applied $ context.toData).evaluateDebug
        assert(
          result.isFailure,
          s"Expected rejection of token-bearing contract input: ${result.logs}"
        )
        assert(
          result.logs.exists(_.contains("only ADA")),
          s"Expected 'only ADA' error, got: ${result.logs.mkString(", ")}"
        )
    }

    private def runTestCase(tc: PaymentSplitterTestCase): Unit = {
        val payeesList = tc.payees.map(_.pkh)
        val payeesData = payeesList.toData

        val applied = contract.program $ payeesData

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

        val txOutRef = TxOutRef(lockTxId, 0)
        val redeemer = Data.unit

        val context = ScriptContext(
          txInfo = TxInfo(
            inputs = allInputs,
            outputs = txOutputs,
            fee = tc.fee,
            redeemers = SortedMap.fromList(
              SList((ScriptPurpose.Spending(txOutRef), redeemer))
            ),
            id = txId
          ),
          redeemer = redeemer,
          scriptInfo = SpendingScript(txOutRef = txOutRef)
        )

        val programWithContext = applied $ context.toData
        val result = programWithContext.evaluateDebug

        if tc.expectedSuccess then
            assert(
              result.isSuccess,
              clue = s"Expected success but got failure: ${result.logs.mkString(", ")}"
            )
            expectedBudgets.get(tc.name).foreach { expected =>
                assert(result.budget == expected, s"Budget mismatch for '${tc.name}'")
            }
        else
            assert(
              result.isFailure,
              clue = s"Expected failure but got success"
            )
            assert(
              tc.matchesError(result.logs),
              clue =
                  s"Expected error matching '${tc.expectedError.getOrElse("")}' but got: ${result.logs.mkString(", ")}"
            )
    }
}
