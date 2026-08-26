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
        assert(compiled.script.script.size == 1174)
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
      "deposit_successful" -> ExUnits(memory = 38536, steps = 15_312352),
      "accept_successful" -> ExUnits(memory = 42061, steps = 17_202055),
      "accept_with_multiple_inputs" -> ExUnits(memory = 42061, steps = 17_202055),
      "accept_with_datum_attached" -> ExUnits(memory = 42061, steps = 17_202055),
      "accept_with_multiple_outputs_to_seller" -> ExUnits(memory = 51937, steps = 22_745892),
      "refund_successful" -> ExUnits(memory = 51305, steps = 20_309462),
      "refund_after_exact_deadline" -> ExUnits(memory = 51305, steps = 20_309462),
      "refund_with_multiple_inputs" -> ExUnits(memory = 51305, steps = 20_309462),
      "refund_with_datum_attached" -> ExUnits(memory = 51305, steps = 20_309462),
      "refund_with_multiple_outputs_to_buyer" -> ExUnits(memory = 61181, steps = 25_853299)
    )

    private val expectedFees: Map[String, Coin] = Map(
      "deposit_successful" -> Coin(3328),
      "accept_successful" -> Coin(3668),
      "accept_with_multiple_inputs" -> Coin(3668),
      "accept_with_datum_attached" -> Coin(3668),
      "accept_with_multiple_outputs_to_seller" -> Coin(4637),
      "refund_successful" -> Coin(4425),
      "refund_after_exact_deadline" -> Coin(4425),
      "refund_with_multiple_inputs" -> Coin(4425),
      "refund_with_datum_attached" -> Coin(4425),
      "refund_with_multiple_outputs_to_buyer" -> Coin(5395)
    )
}
