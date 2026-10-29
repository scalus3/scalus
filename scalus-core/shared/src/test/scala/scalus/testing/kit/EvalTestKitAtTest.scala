package scalus.testing.kit

import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.Math
import scalus.testing.budget.BudgetPinsTest.failuresOf

/** The `.at` form of `assertEvalEq`: it checks only when applied, and a missing `.at` fails. */
class EvalTestKitAtTest extends AnyFunSuite {
    import EvalTestKitAtTest.*

    test(".at checks the result on the JVM and the PlutusVM") {
        assert(failuresOf(Fixture(wrongExpected = false, applyAt = true)).isEmpty)
    }

    test(".at with a wrong expected value fails") {
        val failures = failuresOf(Fixture(wrongExpected = true, applyAt = true))
        assert(failures.size == 1, failures)
    }

    test("a check without .at fails the test instead of passing silently") {
        val failures = failuresOf(Fixture(wrongExpected = false, applyAt = false))
        assert(failures.size == 1, failures)
        assert(failures.head.contains("without .at(...)"), failures.head)
    }
}

object EvalTestKitAtTest {

    @DoNotDiscover
    final class Fixture(wrongExpected: Boolean, applyAt: Boolean) extends AnyFunSuite, EvalTestKit {
        test("min") {
            // The expected value is compiled to UPLC, so it must be a literal, not a class field.
            val check =
                if wrongExpected then assertEvalEq(BigInt(2))(Math.min)
                else assertEvalEq(BigInt(1))(Math.min)
            if applyAt then check.at(1, 2)
        }
    }
}
