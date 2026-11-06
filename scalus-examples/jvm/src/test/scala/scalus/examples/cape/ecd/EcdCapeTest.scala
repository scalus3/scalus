package scalus.examples.cape.ecd

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, Script}
import scalus.examples.cape.{CapeHarness, CapeTestSuite}
import scalus.testing.budget.{BudgetPinsSuite, BudgetTable}
import scalus.testing.kit.ScalusTest
import scalus.uplc.DefaultFun

class EcdCapeTest extends AnyFunSuite with ScalusTest with BudgetPinsSuite {
    override protected def budgetTable: BudgetTable = EcdCapeTestBudgets
    private given CardanoInfo = CardanoInfo.mainnet
    private val program = EcdContract.program
    private val suite = CapeTestSuite.load("/cape/ecd/cape-tests.json")
    private val scriptSize = Script.PlutusV3(program).script.size

    // The spec recurses on `a `mod` b`, and CAPE rules that `rem` changes the recursive arguments
    // for negative inputs: ecd(-12, 8) must recurse into ecd(8, 4), not ecd(8, -4). The results
    // agree either way, so only the builtin used shows the difference.
    test("recursion uses modInteger, as the prescribed algorithm does") {
        val used = program.term.collectBuiltins
        assert(used.contains(DefaultFun.ModInteger))
        assert(!used.contains(DefaultFun.RemainderInteger))
    }

    test("Script size") {
        info(s"${program.cborByteString.length} bytes")
        pinSize(program.cborEncoded.length)
    }

    for c <- suite.cases do
        test(s"CAPE: ${c.name}") {
            CapeHarness.run(program, c).foreach { budget =>
                info(s"${c.name}: $budget")
                pinEval(budget, scriptSize)
            }
        }
}
