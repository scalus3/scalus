package scalus.examples.cape.linearvesting

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, ExUnits}
import scalus.examples.cape.{CapeHarness, CapeTestSuite}
import scalus.testing.kit.ScalusTest

/** CAPE test harness for the Linear Vesting benchmark.
  *
  * Loads the v3.0.0 `cape-tests.json` fixtures via the shared `CapeTestSuite` loader and runs all
  * cases against the compiled `LinearVestingValidator`.
  */
class LinearVestingCapeTest extends AnyFunSuite with ScalusTest {
    private given CardanoInfo = CardanoInfo.mainnet
    private val program = LinearVestingContract.program
    private val suite = CapeTestSuite.load("/cape/linear_vesting/cape-tests.json")

    test(s"Script size: ${program.cborByteString.length} bytes") {
        assert(program.cborByteString.length == 660)
    }

    // Pins for the default CIP-0153 build (Options.releaseUntagged): Value.quantityOf lowers to
    // the lookupCoin/unValueData builtins. Requires canonically ordered fixture Values, which the
    // JVM harness (CapeTestSuite) always builds and CAPE's upstream fixture builder emits since
    // the canonical-Value-ordering fix (see docs/internal/UPLC_CAPE_VALUE_CANONICALITY_PR.md).
    private val expectedBudgets: Map[String, ExUnits] = Map(
      "partial_unlock_first_installment" -> ExUnits(memory = 63888, steps = 31013223),
      "partial_unlock_mid_vesting" -> ExUnits(memory = 63888, steps = 31013223),
      "partial_unlock_near_end" -> ExUnits(memory = 63888, steps = 31013223),
      "partial_unlock_between_installments" -> ExUnits(memory = 63888, steps = 31013223),
      "full_unlock_after_period_end" -> ExUnits(memory = 23422, steps = 8983746),
      "full_unlock_well_after" -> ExUnits(memory = 23422, steps = 8983746)
    )

    for c <- suite.cases do
        test(s"CAPE: ${c.name}") {
            CapeHarness.run(program, c).foreach { budget =>
                info(s"${c.name}: $budget")
                expectedBudgets.get(c.name).foreach(exp => assert(budget == exp))
            }
        }
}
