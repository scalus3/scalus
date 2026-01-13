package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.*
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.Constant.given
import scalus.uplc.eval.*

// Test case class
case class EqTestPoint(x: BigInt, y: BigInt)

@Compile
object EqTestPoint:
    given Eq[EqTestPoint] = Eq.derived

// Test case class with no fields
case class EqTestEmpty()

@Compile
object EqTestEmpty:
    given Eq[EqTestEmpty] = Eq.derived

// Test enum
enum EqTestStatus:
    case Pending
    case Done(result: BigInt)
    case Failed(code: BigInt, message: String)

@Compile
object EqTestStatus:
    given Eq[EqTestStatus] = Eq.derived

// Test sealed trait hierarchy
sealed trait EqTestExpr

@Compile
object EqTestExpr:
    given Eq[EqTestExpr] = Eq.derived

case class EqTestLit(value: BigInt) extends EqTestExpr

@Compile
object EqTestLit:
    given Eq[EqTestLit] = Eq.derived

case class EqTestAdd(left: BigInt, right: BigInt) extends EqTestExpr

@Compile
object EqTestAdd:
    given Eq[EqTestAdd] = Eq.derived

case object EqTestZero extends EqTestExpr

// Note: Nested sealed traits (sealed trait child of sealed trait) are not yet
// supported by the Scalus compiler plugin due to type test pattern limitations.

// Test case class for budget comparison: Eq.derived vs manual unapply
case class EqBudgetPair(x: BigInt, y: BigInt)

@Compile
object EqBudgetPair:
    // Eq.derived uses direct field access: lhs.x === rhs.x && lhs.y === rhs.y
    given eqDerived: Eq[EqBudgetPair] = Eq.derived

    // Manual implementation using pattern matching with unapply (for comparison)
    given eqManualUnapply: Eq[EqBudgetPair] = (lhs: EqBudgetPair, rhs: EqBudgetPair) =>
        (lhs, rhs) match
            case (EqBudgetPair(x1, y1), EqBudgetPair(x2, y2)) => x1 === x2 && y1 === y2

class EqDerivingTest extends AnyFunSuite {

    test("Eq.derived for case class - equal values") {
        val eq = summon[Eq[EqTestPoint]]
        assert(eq(EqTestPoint(1, 2), EqTestPoint(1, 2)))
    }

    test("Eq.derived for case class - different values") {
        val eq = summon[Eq[EqTestPoint]]
        assert(!eq(EqTestPoint(1, 2), EqTestPoint(1, 3)))
        assert(!eq(EqTestPoint(1, 2), EqTestPoint(2, 2)))
    }

    test("Eq.derived for empty case class") {
        val eq = summon[Eq[EqTestEmpty]]
        assert(eq(EqTestEmpty(), EqTestEmpty()))
    }

    test("Eq.derived for enum - same singleton") {
        val eq = summon[Eq[EqTestStatus]]
        assert(eq(EqTestStatus.Pending, EqTestStatus.Pending))
    }

    test("Eq.derived for enum - different singletons") {
        val eq = summon[Eq[EqTestStatus]]
        assert(!eq(EqTestStatus.Pending, EqTestStatus.Done(0)))
    }

    test("Eq.derived for enum - same case class variant") {
        val eq = summon[Eq[EqTestStatus]]
        assert(eq(EqTestStatus.Done(42), EqTestStatus.Done(42)))
        assert(eq(EqTestStatus.Failed(1, "err"), EqTestStatus.Failed(1, "err")))
    }

    test("Eq.derived for enum - different case class variants") {
        val eq = summon[Eq[EqTestStatus]]
        assert(!eq(EqTestStatus.Done(42), EqTestStatus.Done(43)))
        assert(!eq(EqTestStatus.Done(42), EqTestStatus.Failed(42, "x")))
    }

    test("Eq.derived for sealed trait - same variants") {
        val eq = summon[Eq[EqTestExpr]]
        assert(eq(EqTestLit(1), EqTestLit(1)))
        assert(eq(EqTestAdd(1, 2), EqTestAdd(1, 2)))
        assert(eq(EqTestZero, EqTestZero))
    }

    test("Eq.derived for sealed trait - different variants") {
        val eq = summon[Eq[EqTestExpr]]
        assert(!eq(EqTestLit(1), EqTestLit(2)))
        assert(!eq(EqTestLit(1), EqTestAdd(1, 0)))
        assert(!eq(EqTestZero, EqTestLit(0)))
    }

    test("Eq.derived with === extension") {
        import scalus.prelude.===
        val p1 = EqTestPoint(1, 2)
        val p2 = EqTestPoint(1, 2)
        val p3 = EqTestPoint(1, 3)
        assert(p1 === p2)
        assert(!(p1 === p3))
        assert(p1 !== p3)
    }

    test("Eq.derived compiles to UPLC and evaluates correctly") {
        given PlutusVM = PlutusVM.makePlutusV3VM()
        given scalus.compiler.Options = scalus.compiler.Options(
          targetLoweringBackend = scalus.compiler.sir.TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true,
          optimizeUplc = false,
          debug = false
        )

        val sir = scalus.compiler.compile { (x1: BigInt, y1: BigInt, x2: BigInt, y2: BigInt) =>
            val p1 = EqTestPoint(x1, y1)
            val p2 = EqTestPoint(x2, y2)
            summon[Eq[EqTestPoint]](p1, p2)
        }

        val uplc = sir.toUplc(generateErrorTraces = true)
        val program = uplc.plutusV3

        // Test equal points
        val result1 =
            (program $ BigInt(1).asTerm $ BigInt(2).asTerm $ BigInt(1).asTerm $ BigInt(
              2
            ).asTerm).term.evaluateDebug
        result1 match
            case Result.Success(t, _, _, _) =>
                assert(t == Term.Const(Constant.Bool(true)))
            case Result.Failure(ex, _, _, l) =>
                fail(s"Expected success for equal points, got failure: $ex, logs=$l")

        // Test different points
        val result2 =
            (program $ BigInt(1).asTerm $ BigInt(2).asTerm $ BigInt(1).asTerm $ BigInt(
              3
            ).asTerm).term.evaluateDebug
        result2 match
            case Result.Success(t, _, _, _) =>
                assert(t == Term.Const(Constant.Bool(false)))
            case Result.Failure(ex, _, _, l) =>
                fail(s"Expected success for different points, got failure: $ex, logs=$l")
    }

    test("Eq.derived vs manual for case class - budget comparison") {
        given PlutusVM = PlutusVM.makePlutusV3VM()
        given scalus.compiler.Options = scalus.compiler.Options(
          targetLoweringBackend = scalus.compiler.sir.TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = false,
          optimizeUplc = true,
          debug = false
        )

        // Compile using Eq.derived (direct field access)
        val sirDerived = scalus.compiler.compile {
            (x1: BigInt, y1: BigInt, x2: BigInt, y2: BigInt) =>
                val p1 = EqBudgetPair(x1, y1)
                val p2 = EqBudgetPair(x2, y2)
                EqBudgetPair.eqDerived(p1, p2)
        }

        // Compile using manual implementation (pattern matching with unapply)
        val sirManual = scalus.compiler.compile {
            (x1: BigInt, y1: BigInt, x2: BigInt, y2: BigInt) =>
                val p1 = EqBudgetPair(x1, y1)
                val p2 = EqBudgetPair(x2, y2)
                EqBudgetPair.eqManualUnapply(p1, p2)
        }

        val programDerived = sirDerived.toUplcOptimized(false).plutusV3
        val programManual = sirManual.toUplcOptimized(false).plutusV3

        // Test with equal Pair values
        val resultDerived =
            (programDerived $ BigInt(1).asTerm $ BigInt(2).asTerm $ BigInt(1).asTerm $ BigInt(
              2
            ).asTerm).term.evaluateDebug
        val resultManual =
            (programManual $ BigInt(1).asTerm $ BigInt(2).asTerm $ BigInt(1).asTerm $ BigInt(
              2
            ).asTerm).term.evaluateDebug

        // Both should succeed with true
        assert(resultDerived.isSuccess, s"Derived failed: $resultDerived")
        assert(resultManual.isSuccess, s"Manual failed: $resultManual")

        val budgetDerived = resultDerived.budget
        val budgetManual = resultManual.budget

        println(s"Case class Eq budget comparison (equal values):")
        println(
          s"  Derived (direct field access):      memory=${budgetDerived.memory}, steps=${budgetDerived.steps}"
        )
        println(
          s"  Manual (pattern match with unapply): memory=${budgetManual.memory}, steps=${budgetManual.steps}"
        )
        println(
          s"  Difference: memory=${budgetDerived.memory - budgetManual.memory}, steps=${budgetDerived.steps - budgetManual.steps}"
        )

        // Test with different values to exercise the comparison logic
        val resultDerived2 =
            (programDerived $ BigInt(1).asTerm $ BigInt(2).asTerm $ BigInt(1).asTerm $ BigInt(
              3
            ).asTerm).term.evaluateDebug
        val resultManual2 =
            (programManual $ BigInt(1).asTerm $ BigInt(2).asTerm $ BigInt(1).asTerm $ BigInt(
              3
            ).asTerm).term.evaluateDebug

        assert(resultDerived2.isSuccess)
        assert(resultManual2.isSuccess)

        println(s"Case class Eq budget comparison (different values):")
        println(
          s"  Derived: memory=${resultDerived2.budget.memory}, steps=${resultDerived2.budget.steps}"
        )
        println(
          s"  Manual:  memory=${resultManual2.budget.memory}, steps=${resultManual2.budget.steps}"
        )
        println(
          s"  Difference: memory=${resultDerived2.budget.memory - resultManual2.budget.memory}, steps=${resultDerived2.budget.steps - resultManual2.budget.steps}"
        )

        // For case classes, derived should be equal or better
        // (direct field access should be at least as efficient as pattern matching)
    }

}
