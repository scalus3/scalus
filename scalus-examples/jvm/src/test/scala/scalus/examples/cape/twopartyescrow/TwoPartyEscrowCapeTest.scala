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
        assert(compiled.script.script.size == 1007)
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
      "accept_successful" -> ExUnits(memory = 41130, steps = 17_004_511),
      "accept_with_multiple_inputs" -> ExUnits(memory = 41130, steps = 17_004_511),
      "accept_with_datum_attached" -> ExUnits(memory = 41130, steps = 17_004_511),
      "accept_with_multiple_outputs_to_seller" -> ExUnits(memory = 48511, steps = 22_096_556),
      "refund_successful" -> ExUnits(memory = 50210, steps = 20_356_164),
      "refund_after_exact_deadline" -> ExUnits(memory = 50210, steps = 20_356_164),
      "refund_with_multiple_inputs" -> ExUnits(memory = 50210, steps = 20_356_164),
      "refund_with_datum_attached" -> ExUnits(memory = 50210, steps = 20_356_164),
      "refund_with_multiple_outputs_to_buyer" -> ExUnits(memory = 57591, steps = 25_448_209)
    )

    private val expectedFees: Map[String, Coin] = Map(
      "deposit_successful" -> Coin(2967),
      "accept_successful" -> Coin(3600),
      "accept_with_multiple_inputs" -> Coin(3600),
      "accept_with_datum_attached" -> Coin(3600),
      "accept_with_multiple_outputs_to_seller" -> Coin(4393),
      "refund_successful" -> Coin(4365),
      "refund_after_exact_deadline" -> Coin(4365),
      "refund_with_multiple_inputs" -> Coin(4365),
      "refund_with_datum_attached" -> Coin(4365),
      "refund_with_multiple_outputs_to_buyer" -> Coin(5158)
    )
}
