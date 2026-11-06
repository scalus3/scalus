package scalus.examples.cape.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, RefScriptFee, Script}
import scalus.examples.cape.{CapeHarness, CapeTestSuite}
import scalus.testing.budget.{BudgetPinsSuite, BudgetTable}
import scalus.testing.kit.ScalusTest

/** CAPE test harness for the HTLC (Hashed Time-Locked Contract) benchmark.
  *
  * Loads the v3.0.0 `cape-tests.json` fixtures via the shared `CapeTestSuite` loader and runs all
  * cases against the compiled `HtlcValidator`.
  */
class HtlcCapeTest extends AnyFunSuite with ScalusTest with BudgetPinsSuite {
    override protected def budgetTable: BudgetTable = HtlcCapeTestBudgets
    private given CardanoInfo = CardanoInfo.mainnet
    private val program = HtlcContract.program
    private val suite = CapeTestSuite.load("/cape/htlc/cape-tests.json")
    private val protocolParams = summon[CardanoInfo].protocolParams
    private val scriptSize = program.cborByteString.size
    private val evaluatedScriptSize = Script.PlutusV3(program).script.size
    // Price it as the transaction's only reference script, using the repository's parameter snapshot.
    private val referenceScriptFee = new RefScriptFee().calculate(scriptSize).value

    test("Script size") {
        info(s"$scriptSize bytes")
        pinSize(scriptSize)
    }

    for c <- suite.cases do
        test(s"CAPE: ${c.name}") {
            CapeHarness.run(program, c).foreach { budget =>
                val executionFee = budget.fee(protocolParams.executionUnitPrices).value
                val combinedFee = referenceScriptFee + executionFee
                info(
                  s"${c.name}: $budget; fees (lovelace): reference-script=$referenceScriptFee, " +
                      s"execution=$executionFee, combined=$combinedFee"
                )
                pinEval(budget, evaluatedScriptSize)
            }
        }
}
