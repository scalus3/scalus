package scalus.examples.cape.twopartyescrow

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, Coin, ExUnits}
import scalus.examples.cape.{CapeHarness, CapeTestSuite}
import scalus.testing.kit.ScalusTest

/** CAPE test harness for the Two-Party Escrow benchmark.
  *
  * Loads the revision-2 `cape-tests.json` fixtures (UPLC-CAPE #275) via the shared `CapeTestSuite`
  * loader and runs all cases, under the minting and the spending purpose, against the compiled
  * `TwoPartyEscrowValidator`.
  */
class TwoPartyEscrowCapeTest extends AnyFunSuite with ScalusTest {
    private given CardanoInfo = CardanoInfo.mainnet
    private val compiled = TwoPartyEscrowContract.compiled
    private val program = compiled.program

    private val suite = CapeTestSuite.load("/cape/two_party_escrow/cape-tests.json")

    test(s"Script size: ${compiled.script.script.size} bytes") {
        assert(compiled.script.script.size == 1040)
    }

    for c <- suite.cases do
        test(s"CAPE: ${c.name}") {
            CapeHarness.run(program, c).foreach { budget =>
                val fee = budget.fee
                info(s"${c.name}: $budget fee=$fee")
                expectedBudgets.get(c.name).foreach { exp =>
                    assert(budget == exp, s"${c.name} budget: expected $exp but got $budget")
                }
                expectedFees.get(c.name).foreach { exp =>
                    assert(fee == exp, s"${c.name} fee: expected $exp but got $fee")
                }
            }
        }

    // Expected execution budgets for the five measured executions of the lifecycle.
    private val expectedBudgets: Map[String, ExUnits] = Map(
      "deposit_successful" -> ExUnits(memory = 49109, steps = 21_771069),
      "accept_successful" -> ExUnits(memory = 59845, steps = 27_612452),
      "accept_burn" -> ExUnits(memory = 18844, steps = 6_367844),
      "refund_successful" -> ExUnits(memory = 65564, steps = 28_795232),
      "refund_burn" -> ExUnits(memory = 19045, steps = 6_452177)
    )

    private val expectedFees: Map[String, Coin] = Map(
      "deposit_successful" -> Coin(4404),
      "accept_successful" -> Coin(5444),
      "accept_burn" -> Coin(1547),
      "refund_successful" -> Coin(5860),
      "refund_burn" -> Coin(1565)
    )
}
