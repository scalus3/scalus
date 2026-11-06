package scalus.examples.cape.twopartyescrow

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.CardanoInfo
import scalus.examples.cape.{CapeHarness, CapeTestSuite}
import scalus.testing.budget.{BudgetPinsSuite, BudgetTable}
import scalus.testing.kit.ScalusTest

/** CAPE test harness for the Two-Party Escrow benchmark.
  *
  * Loads the revision-2 `cape-tests.json` fixtures (UPLC-CAPE #275) via the shared `CapeTestSuite`
  * loader and runs all cases, under the minting and the spending purpose, against the compiled
  * `TwoPartyEscrowValidator`.
  */
class TwoPartyEscrowCapeTest extends AnyFunSuite with ScalusTest with BudgetPinsSuite {
    override protected def budgetTable: BudgetTable = TwoPartyEscrowCapeTestBudgets
    private given CardanoInfo = CardanoInfo.mainnet
    private val compiled = TwoPartyEscrowContract.compiled
    private val program = compiled.program

    private val suite = CapeTestSuite.load("/cape/two_party_escrow/cape-tests.json")

    test("Script size") {
        info(s"${compiled.script.script.size} bytes")
        pinSize(compiled.script.script.size)
    }

    for c <- suite.cases do
        test(s"CAPE: ${c.name}") {
            CapeHarness.run(program, c).foreach { budget =>
                val fee = budget.fee
                info(s"${c.name}: $budget fee=$fee")
                pinEval(budget, compiled.script.script.size)
            }
        }
}
