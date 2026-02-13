package scalus.uplc.transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.Word64
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.PlutusVM

import scala.language.implicitConversions

/** Tests for the StrictIf optimization that converts lazy if-then-else to strict when safe.
  *
  * The optimization targets the pattern:
  * {{{
  * Force(Apply(Apply(Apply(Force(Builtin(IfThenElse)), c), Delay(t)), Delay(f)))
  * }}}
  *
  * And converts it to:
  * {{{
  * Apply(Apply(Apply(Force(Builtin(IfThenElse)), c), t), f)
  * }}}
  *
  * when both t and f are "simple values" (evaluate to exactly 1 term).
  */
class StrictIfTest extends AnyFunSuite:
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    // Helper to create the lazy if-then-else pattern
    private def lazyIf(c: Term, t: Term, f: Term): Term =
        Force(Apply(Apply(Apply(Force(Builtin(IfThenElse)), c), Delay(t)), Delay(f)))

    // Helper to create the strict if-then-else pattern
    private def strictIf(c: Term, t: Term, f: Term): Term =
        Apply(Apply(Apply(Force(Builtin(IfThenElse)), c), t), f)

    // ======================================================================================
    // POSITIVE CASES: Transformations that should be applied
    // ======================================================================================

    test("transforms lazy if with Var branches to strict if") {
        val input = lazyIf(true, vr"x", vr"y")
        val expected = strictIf(true, vr"x", vr"y")
        assert(StrictIf(input) == expected)
    }

    test("transforms lazy if with Const branches to strict if") {
        val input = lazyIf(true, 42, 100)
        val expected = strictIf(true, 42, 100)
        assert(StrictIf(input) == expected)
    }

    test("transforms lazy if with LamAbs branches to strict if") {
        val thenBranch = λ(x => x)
        val elseBranch = λ(y => y)
        val input = lazyIf(false, thenBranch, elseBranch)
        val expected = strictIf(false, thenBranch, elseBranch)
        assert(StrictIf(input) == expected)
    }

    test("transforms lazy if with Builtin branches to strict if") {
        val input = lazyIf(true, AddInteger, SubtractInteger)
        val expected = strictIf(true, AddInteger, SubtractInteger)
        assert(StrictIf(input) == expected)
    }

    test("transforms lazy if with Delay branches to strict if") {
        val thenBranch = Delay(42)
        val elseBranch = Delay(100)
        val input = lazyIf(true, thenBranch, elseBranch)
        val expected = strictIf(true, thenBranch, elseBranch)
        assert(StrictIf(input) == expected)
    }

    test("transforms lazy if with empty Constr branches to strict if") {
        val thenBranch = Constr(Word64(0), List.empty)
        val elseBranch = Constr(Word64(1), List.empty)
        val input = lazyIf(false, thenBranch, elseBranch)
        val expected = strictIf(false, thenBranch, elseBranch)
        assert(StrictIf(input) == expected)
    }

    test("transforms lazy if with mixed simple value branches to strict if") {
        val input = lazyIf(true, 42, vr"x")
        val expected = strictIf(true, 42, vr"x")
        assert(StrictIf(input) == expected)
    }

    // ======================================================================================
    // NEGATIVE CASES: No transformation should be applied
    // ======================================================================================

    test("does not transform if with Apply in then branch") {
        val thenBranch = AddInteger $ 1
        val elseBranch = 100
        val input = lazyIf(true, thenBranch, elseBranch)
        assert(StrictIf(input) == input)
    }

    test("does not transform if with Apply in else branch") {
        val thenBranch = 42
        val elseBranch = SubtractInteger $ 1
        val input = lazyIf(false, thenBranch, elseBranch)
        assert(StrictIf(input) == input)
    }

    test("does not transform if with Error in then branch") {
        val input = lazyIf(true, Error(), 100)
        assert(StrictIf(input) == input)
    }

    test("does not transform if with Error in else branch") {
        val input = lazyIf(false, 42, Error())
        assert(StrictIf(input) == input)
    }

    test("does not transform if with Force in then branch") {
        val thenBranch = Force(Delay(42))
        val elseBranch = 100
        val input = lazyIf(true, thenBranch, elseBranch)
        assert(StrictIf(input) == input)
    }

    test("does not transform if with Force in else branch") {
        val thenBranch = 42
        val elseBranch = Force(Delay(100))
        val input = lazyIf(false, thenBranch, elseBranch)
        assert(StrictIf(input) == input)
    }

    test("does not transform if with Case in then branch") {
        val thenBranch = Case(Constr(Word64(0), List.empty), List(42))
        val elseBranch = 100
        val input = lazyIf(true, thenBranch, elseBranch)
        assert(StrictIf(input) == input)
    }

    test("does not transform if with Case in else branch") {
        val thenBranch = 42
        val elseBranch = Case(Constr(Word64(1), List.empty), List(100))
        val input = lazyIf(false, thenBranch, elseBranch)
        assert(StrictIf(input) == input)
    }

    test("does not transform if with non-empty Constr in then branch") {
        val thenBranch = Constr(Word64(0), List(1))
        val elseBranch = 100
        val input = lazyIf(true, thenBranch, elseBranch)
        assert(StrictIf(input) == input)
    }

    test("does not transform if with non-empty Constr in else branch") {
        val thenBranch = 42
        val elseBranch = Constr(Word64(1), List(2))
        val input = lazyIf(false, thenBranch, elseBranch)
        assert(StrictIf(input) == input)
    }

    test("does not transform if both branches are not simple") {
        val thenBranch = AddInteger $ 1
        val elseBranch = SubtractInteger $ 2
        val input = lazyIf(true, thenBranch, elseBranch)
        assert(StrictIf(input) == input)
    }

    test("does not transform already strict if-then-else (no delays)") {
        // Already in strict form - no Delay wrappers
        val input = strictIf(true, 42, 100)
        assert(StrictIf(input) == input)
    }

    // ======================================================================================
    // RECURSION TESTS: Transformation in nested structures
    // ======================================================================================

    test("transforms nested lazy if-then-else in then branch") {
        val innerIf = lazyIf(false, 1, 2)
        val outerLambda = λ("x")(innerIf)
        val elseLambda = λ(y => y)
        val input = lazyIf(true, outerLambda, elseLambda)

        val expectedInner = strictIf(false, 1, 2)
        val expectedOuter = λ("x")(expectedInner)
        val expected = strictIf(true, expectedOuter, elseLambda)

        assert(StrictIf(input) == expected)
    }

    test("transforms lazy if inside lambda body") {
        val innerIf = lazyIf(true, 42, 100)
        val input = λ("x")(innerIf)

        val expectedInner = strictIf(true, 42, 100)
        val expected = λ("x")(expectedInner)

        assert(StrictIf(input) == expected)
    }

    test("transforms lazy if inside Apply function position") {
        val ifInFunction = lazyIf(true, λ(x => x), λ(y => y))
        val input = ifInFunction $ 42

        val expectedIf = strictIf(true, λ(x => x), λ(y => y))
        val expected = expectedIf $ 42

        assert(StrictIf(input) == expected)
    }

    test("transforms lazy if inside Apply argument position") {
        val ifInArg = lazyIf(false, 1, 2)
        val input = λ(x => x) $ ifInArg

        val expectedIf = strictIf(false, 1, 2)
        val expected = λ(x => x) $ expectedIf

        assert(StrictIf(input) == expected)
    }

    test("transforms lazy if inside Constr arguments") {
        val ifInConstr = lazyIf(true, 42, 100)
        val input = Constr(Word64(0), List(ifInConstr))

        val expectedIf = strictIf(true, 42, 100)
        val expected = Constr(Word64(0), List(expectedIf))

        assert(StrictIf(input) == expected)
    }

    test("transforms lazy if inside Case scrutinee") {
        val ifInScrutinee =
            lazyIf(
              true,
              Constr(Word64(0), List.empty),
              Constr(Word64(1), List.empty)
            )
        val input = Case(ifInScrutinee, List(42))

        val expectedIf =
            strictIf(
              true,
              Constr(Word64(0), List.empty),
              Constr(Word64(1), List.empty)
            )
        val expected = Case(expectedIf, List(42))

        assert(StrictIf(input) == expected)
    }

    test("transforms lazy if inside Case branches") {
        val ifInBranch = lazyIf(false, 1, 2)
        val input = Case(Constr(Word64(0), List.empty), List(ifInBranch))

        val expectedIf = strictIf(false, 1, 2)
        val expected = Case(Constr(Word64(0), List.empty), List(expectedIf))

        assert(StrictIf(input) == expected)
    }

    test("transforms lazy if inside Force") {
        val ifInForce = lazyIf(true, Delay(42), Delay(100))
        val input = Force(ifInForce)

        val expected = Force(strictIf(true, Delay(42), Delay(100)))

        assert(StrictIf(input) == expected)
    }

    test("transforms lazy if inside Delay") {
        val ifInDelay = lazyIf(false, 1, 2)
        val input = Delay(ifInDelay)

        val expectedIf = strictIf(false, 1, 2)
        val expected = Delay(expectedIf)

        assert(StrictIf(input) == expected)
    }

    // ======================================================================================
    // EDGE CASES
    // ======================================================================================

    test("preserves terms without if-then-else") {
        val input = λ(x => x $ 42)
        assert(StrictIf(input) == input)
    }

    test("preserves simple constants") {
        val input: Term = 42
        assert(StrictIf(input) == input)
    }

    test("preserves variables") {
        val input = vr"x"
        assert(StrictIf(input) == input)
    }

    test("preserves Error") {
        val input = Error()
        assert(StrictIf(input) == input)
    }

    test("handles deeply nested structure with multiple if-then-else") {
        val if1 = lazyIf(true, 1, 2)
        val if2 = lazyIf(false, 3, 4)
        val input = λ("x")(AddInteger $ if1 $ if2)

        val expectedIf1 = strictIf(true, 1, 2)
        val expectedIf2 = strictIf(false, 3, 4)
        val expected = λ("x")(AddInteger $ expectedIf1 $ expectedIf2)

        assert(StrictIf(input) == expected)
    }

    // ======================================================================================
    // SEMANTIC PRESERVATION: Verify optimization preserves behavior
    // ======================================================================================

    test("strict if produces same result as lazy if for true condition") {
        val lazyTerm = lazyIf(true, 42, 100)
        val strictTerm = strictIf(true, 42, 100)

        (lazyTerm.evaluateDebug.success.term, strictTerm.evaluateDebug.success.term)
    }

    test("strict if produces same result as lazy if for false condition") {
        val lazyTerm = lazyIf(false, 42, 100)
        val strictTerm = strictIf(false, 42, 100)

        (lazyTerm.evaluateDebug.success.term, strictTerm.evaluateDebug.success.term)
    }

    test("optimized term evaluates to same result as original") {
        val original = lazyIf(true, 42, 100)
        val optimized = StrictIf(original)

        assert(original.evaluateDebug.success.term == optimized.evaluateDebug.success.term)
    }

    // ======================================================================================
    // LOGGING TESTS
    // ======================================================================================

    test("logs optimization when transformation is applied") {
        var loggedMessages = List.empty[String]
        val logger = (msg: String) => loggedMessages = loggedMessages :+ msg

        val input = lazyIf(true, 42, 100)
        StrictIf(input, logger)

        assert(loggedMessages.nonEmpty)
        assert(loggedMessages.head.contains("StrictIf"))
    }

    test("does not log when no transformation is applied") {
        var loggedMessages = List.empty[String]
        val logger = (msg: String) => loggedMessages = loggedMessages :+ msg

        val input = lazyIf(true, Error(), 100)
        StrictIf(input, logger)

        assert(loggedMessages.isEmpty)
    }

    // ======================================================================================
    // IDEMPOTENCE: Applying optimization multiple times should be the same as once
    // ======================================================================================

    test("optimization is idempotent") {
        val input = lazyIf(true, 42, 100)
        val once = StrictIf(input)
        val twice = StrictIf(once)

        assert(once == twice)
    }

    test("optimization is idempotent for complex nested structure") {
        val innerIf = lazyIf(false, vr"x", vr"y")
        val outerIf = lazyIf(true, innerIf, 100)
        val input = λ("z")(outerIf)

        val once = StrictIf(input)
        val twice = StrictIf(once)
        val thrice = StrictIf(twice)

        assert(once == twice)
        assert(twice == thrice)
    }
