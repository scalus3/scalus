package scalus.cardano.onchain.plutus.prelude

import scalus.testing.budget.BudgetTable
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.PairList
import scalus.cardano.onchain.plutus.prelude.PairList.*
import scalus.testing.kit.EvalTestKit

class PairListTest extends AnyFunSuite with EvalTestKit {
    override protected def budgetTable: BudgetTable = PairListTestBudgets

    test("head") {
        assertEvalEq((BigInt(1), BigInt(2))): (pl: PairList[BigInt, BigInt]) =>
            pl.head
        .at(PairList.singleton(BigInt(1), BigInt(2)))
    }

    test("tail") {
        assertEvalEq(PairCons((BigInt(3), BigInt(4)), PairNil)): (pl: PairList[BigInt, BigInt]) =>
            pl.tail
        .at(PairCons((BigInt(1), BigInt(2)), PairCons((BigInt(3), BigInt(4)), PairNil)))
    }

    test("isEmpty") {
        assertEvalEq(true): (pl: PairList[BigInt, BigInt]) =>
            pl.isEmpty
        .at(PairList.empty[BigInt, BigInt])
        assertEvalEq(false): (pl: PairList[BigInt, BigInt]) =>
            pl.isEmpty
        .at(PairList.singleton(BigInt(1), BigInt(2)))
    }
}
