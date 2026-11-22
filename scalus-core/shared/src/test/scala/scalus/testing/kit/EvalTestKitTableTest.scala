package scalus.testing.kit

import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite
import scalus.testing.budget.BudgetPinsTest.failuresOf

/** The `Table` form of `checkEval`: spec `[gen.runs.checkeval-table]` and
  * `[gen.runs.checkeval-row-report]` of docs/internal/test-refactor/d5-generators.md.
  */
class EvalTestKitTableTest extends AnyFunSuite {
    import EvalTestKitTableTest.*

    test("rows and random cases that hold pass, for 1, 2 and 3 arguments") {
        assert(failuresOf(new Holds).isEmpty)
    }

    test("every failing row is reported with its heading names and values") {
        val failures = failuresOf(new FailingRows)
        assert(failures.size == 2, failures)
        val oneArgument =
            failures.find(_.contains("2 of 3 edge rows failed")).getOrElse(fail(failures.toString))
        assert(oneArgument.contains("row 1 (x = -1): false"), oneArgument)
        assert(oneArgument.contains("row 2 (x = -2): false"), oneArgument)
        assert(!oneArgument.contains("row 0"), oneArgument)
        val twoArguments =
            failures.find(_.contains("1 of 2 edge rows failed")).getOrElse(fail(failures.toString))
        assert(twoArguments.contains("row 1 (a = 2, b = 1): false"), twoArguments)
    }

    test("a row whose evaluation throws is reported as a failing row") {
        val failures = failuresOf(new ThrowingRow)
        assert(failures.size == 1, failures)
        assert(failures.head.contains("row 0 (x = 0): threw"), failures.head)
    }

    test("the random cases run after the rows") {
        val failures = failuresOf(new RandomCaseFails)
        assert(failures.size == 1, failures)
        assert(!failures.head.contains("edge rows"), failures.head)
    }
}

object EvalTestKitTableTest {

    @DoNotDiscover
    final class Holds extends AnyFunSuite, EvalTestKit {
        test("one argument") {
            checkEval(Table("x", BigInt(0), BigInt(-1), BigInt(2).pow(64))) { (x: BigInt) =>
                x * x >= 0
            }
        }
        test("two arguments") {
            checkEval(Table(("a", "b"), (BigInt(0), BigInt(1)), (BigInt(-3), BigInt(2).pow(64)))) {
                (a: BigInt, b: BigInt) => a + b == b + a
            }
        }
        test("three arguments") {
            checkEval(Table(("a", "b", "c"), (BigInt(1), BigInt(2), BigInt(3)))) {
                (a: BigInt, b: BigInt, c: BigInt) => (a + b) + c == a + (b + c)
            }
        }
    }

    @DoNotDiscover
    final class FailingRows extends AnyFunSuite, EvalTestKit {
        test("one argument") {
            checkEval(Table("x", BigInt(1), BigInt(-1), BigInt(-2))) { (x: BigInt) => x > 0 }
        }
        test("two arguments") {
            checkEval(Table(("a", "b"), (BigInt(1), BigInt(2)), (BigInt(2), BigInt(1)))) {
                (a: BigInt, b: BigInt) => a <= b
            }
        }
    }

    @DoNotDiscover
    final class ThrowingRow extends AnyFunSuite, EvalTestKit {
        test("division by zero") {
            checkEval(Table("x", BigInt(0))) { (x: BigInt) => x / x == BigInt(1) }
        }
    }

    @DoNotDiscover
    final class RandomCaseFails extends AnyFunSuite, EvalTestKit {
        test("only the row holds") {
            checkEval(Table("x", BigInt(0))) { (x: BigInt) => x == BigInt(0) }
        }
    }
}
