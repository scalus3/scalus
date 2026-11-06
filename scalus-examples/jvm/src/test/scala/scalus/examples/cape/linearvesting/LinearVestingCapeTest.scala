package scalus.examples.cape.linearvesting

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, Script}
import scalus.examples.cape.{CapeHarness, CapeTestSuite}
import scalus.testing.budget.{BudgetPinsSuite, BudgetTable}
import scalus.testing.kit.ScalusTest

/** CAPE test harness for the Linear Vesting benchmark.
  *
  * Loads the v3.0.0 `cape-tests.json` fixtures via the shared `CapeTestSuite` loader and runs all
  * cases against the compiled `LinearVestingValidator`.
  */
class LinearVestingCapeTest extends AnyFunSuite with ScalusTest with BudgetPinsSuite {
    override protected def budgetTable: BudgetTable = LinearVestingCapeTestBudgets
    private given CardanoInfo = CardanoInfo.mainnet
    private val program = LinearVestingContract.program
    private val suite = CapeTestSuite.load("/cape/linear_vesting/cape-tests.json")
    private val scriptSize = Script.PlutusV3(program).script.size

    test("Script size") {
        info(s"${program.cborByteString.length} bytes")
        pinSize(program.cborEncoded.length)
    }

    // Pins for the default CIP-0153 build (Options.releaseUntagged): Value.quantityOf lowers to
    // the lookupCoin/unValueData builtins. Requires canonically ordered fixture Values, which the
    // JVM harness (CapeTestSuite) always builds and CAPE's upstream fixture builder emits since
    // the canonical-Value-ordering fix (see docs/internal/UPLC_CAPE_VALUE_CANONICALITY_PR.md).
    for c <- suite.cases do
        test(s"CAPE: ${c.name}") {
            CapeHarness.run(program, c).foreach { budget =>
                info(s"${c.name}: $budget")
                pinEval(budget, scriptSize)
            }
        }
}
