package scalus.examples.paymentsplitter

import scalus.compiler.Options
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.Data
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

    private val contract = NaivePaymentSplitterContract.withErrorTraces
    private val lockTxId = random[TxId]
    private val payeesTxId = random[TxId]
    private val txId = random[TxId]
    private val scriptHash = contract.script.scriptHash

    private val expectedBudgets: Map[String, ExUnits] = Map(
      "success when payments are correctly split for a single payee" ->
          (if Options.default.nativeListElements then ExUnits(memory = 402635, steps = 117_768375)
           else ExUnits(memory = 314055, steps = 91_706696)),
      "success when payments are correctly split between 2 payees" ->
          (if Options.default.nativeListElements then ExUnits(memory = 636156, steps = 185_321621)
           else ExUnits(memory = 522404, steps = 151_871157)),
      "success when payments are correctly split between 3 payees" ->
          (if Options.default.nativeListElements then ExUnits(memory = 918191, steps = 267_143433)
           else ExUnits(memory = 785051, steps = 227_899845)),
      "success when split equally and remainder compensates fee - o1" ->
          (if Options.default.nativeListElements then ExUnits(memory = 918191, steps = 267_143433)
           else ExUnits(memory = 785051, steps = 227_899845)),
      "success when split equally and remainder compensates fee - o2" ->
          (if Options.default.nativeListElements then ExUnits(memory = 918191, steps = 267_143433)
           else ExUnits(memory = 785051, steps = 227_899845)),
      "success when split equally and remainder compensates fee - o3" ->
          (if Options.default.nativeListElements then ExUnits(memory = 918191, steps = 267_143433)
           else ExUnits(memory = 785051, steps = 227_899845)),
      "success between 5 payees" -> (if Options.default.nativeListElements then
                                         ExUnits(memory = 1638003, steps = 477515765L)
                                     else ExUnits(memory = 1_483439, steps = 431_660130)),
      "success with multiple contract UTxOs" -> (if Options.default.nativeListElements then
                                                     ExUnits(memory = 1180517, steps = 343222425L)
                                                 else ExUnits(memory = 976681, steps = 283_832909))
    )

    // Run all shared test cases
    testCases.foreach { tc =>
        test(s"Naive: ${tc.name}") {
            runTestCase(tc)
        }
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
