package scalus.examples.cape.twopartyescrow

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, Coin, ExUnits}
import scalus.examples.cape.{CapeHarness, CapeTestSuite}
import scalus.testing.kit.ScalusTest

/** CAPE test harness for the Two-Party Escrow benchmark.
  *
  * Loads the v3.0.0 `cape-tests.json` fixtures via the shared `CapeTestSuite` loader and runs all
  * cases against the compiled `TwoPartyEscrowValidator`.
  */
class TwoPartyEscrowCapeTest extends AnyFunSuite with ScalusTest {
    private given CardanoInfo = CardanoInfo.mainnet
    private val compiled = TwoPartyEscrowContract.compiled
    private val program = compiled.program

    private val suite = CapeTestSuite.load("/cape/two_party_escrow/cape-tests.json")

    test(s"Script size: ${compiled.script.script.size} bytes") {
        assert(compiled.script.script.size == 1009)
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

    // Expected execution budgets for measurement cases only
    private val expectedBudgets: Map[String, ExUnits] = Map(
      "deposit_successful" -> ExUnits(memory = 33790, steps = 14_104_304),
      "accept_successful" -> ExUnits(memory = 41362, steps = 17_178_503),
      "accept_with_multiple_inputs" -> ExUnits(memory = 41362, steps = 17_178_503),
      "accept_with_datum_attached" -> ExUnits(memory = 41362, steps = 17_178_503),
      "accept_with_multiple_outputs_to_seller" -> ExUnits(memory = 48743, steps = 22_270_548),
      "refund_successful" -> ExUnits(memory = 50442, steps = 20_530_156),
      "refund_after_exact_deadline" -> ExUnits(memory = 50442, steps = 20_530_156),
      "refund_with_multiple_inputs" -> ExUnits(memory = 50442, steps = 20_530_156),
      "refund_with_datum_attached" -> ExUnits(memory = 50442, steps = 20_530_156),
      "refund_with_multiple_outputs_to_buyer" -> ExUnits(memory = 57823, steps = 25_622_201)
    )

    private val expectedFees: Map[String, Coin] = Map(
      "deposit_successful" -> Coin(2967),
      "accept_successful" -> Coin(3626),
      "accept_with_multiple_inputs" -> Coin(3626),
      "accept_with_datum_attached" -> Coin(3626),
      "accept_with_multiple_outputs_to_seller" -> Coin(4419),
      "refund_successful" -> Coin(4391),
      "refund_after_exact_deadline" -> Coin(4391),
      "refund_with_multiple_inputs" -> Coin(4391),
      "refund_with_datum_attached" -> Coin(4391),
      "refund_with_multiple_outputs_to_buyer" -> Coin(5184)
    )
}
