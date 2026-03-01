package scalus.uplc
package transform

import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.Constant.given
import scalus.uplc.transform.TermAnalysis.freeVars
import DefaultFun.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Word64
import scalus.uplc.eval.PlutusVM

import scala.language.implicitConversions

class InlinerTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("inliner should inline identity Var") {
        val term = LamAbs("x", vr"x" $ vr"x") $ vr"y"
        val expected = vr"y" $ vr"y"
        assert(Inliner(term) == expected)
    }

    test("constants should remain unchanged") {
        val constTerm: Term = 42
        assert(Inliner(constTerm) == constTerm)

        val strTerm: Term = "hello"
        assert(Inliner(strTerm) == strTerm)
    }

    test("builtins should remain unchanged") {
        val addTerm: Term = AddInteger
        assert(Inliner(addTerm) == addTerm)

        val mulTerm: Term = MultiplyInteger
        assert(Inliner(mulTerm) == mulTerm)
    }

    test("identity function should be eliminated") {
        // (λx.x) 42 => 42
        val term = λ("x")(vr"x") $ 42
        assert(Inliner(term) == 42.asTerm)

        // (λx.x) "hello" => "hello"
        val strTerm = λ("x")(vr"x") $ "hello"
        assert(Inliner(strTerm) == "hello".asTerm)
    }

    test("nested identity functions should all be eliminated") {
        // (λx.x) ((λy.y) 42) => 42
        val term = λ("x")(vr"x") $ (λ("y")(vr"y") $ 42)
        assert(Inliner(term) == 42.asTerm)
    }

    test("variable substitution should work correctly") {
        // (λx. x + x) y => y + y (free variable prevents constant folding, tests pure substitution)
        val term = λ("x")(AddInteger $ vr"x" $ vr"x") $ vr"y"

        val expected = AddInteger $ vr"y" $ vr"y"

        assert(Inliner(term) == expected)
    }

    test("should avoid name capture through alpha-renaming") {
        // (λx. λy. x) y
        // Should alpha-rename y in the inner lambda to avoid capture
        val term = λ("x", "y")(vr"x") $ vr"y"

        val result = Inliner(term)

        // The result should be λy_1. y where y_1 is a fresh name
        result match
            case LamAbs(newName, Var(NamedDeBruijn("y", 0), _), _) =>
                assert(newName != "y")
            case _ =>
                fail(s"Unexpected result: $result")
    }

    test("should handle Constr and Case") {
        val constr = Constr(Word64.Zero, List(42))
        assert(Inliner(constr) == constr)

        val caseExpr = Case(
          constr,
          List(
            λ("x")(vr"x") $ vr"y", // Identity function application
            0
          )
        )

        val expectedCase = Case(
          constr,
          List(
            vr"y", // Identity function eliminated
            0
          )
        )

        assert(Inliner(caseExpr) == expectedCase)
    }

    test("should not inline non-pure terms") {
        // (λx. x + x) Error => (λx. x + x) Error
        val termWithError = λ("x")(AddInteger $ vr"x" $ vr"x") $ Error()
        assert(Inliner(termWithError) == termWithError)
    }

    test("should handle complex arithmetic expressions") {
        // (λx. λy. x + y) a b => a + b (tests multi-arg substitution without constant folding)
        val term = λ("x", "y")(AddInteger $ vr"x" $ vr"y") $ vr"a" $ vr"b"
        val expected = AddInteger $ vr"a" $ vr"b"
        assert(Inliner(term) == expected)
    }

    test("should properly handle substitution with potential capture") {
        // (λx.λy.x) y => λy'.y
        val term = λ("x")(λ("y")(vr"x")) $ vr"y"
        val result = Inliner(term)

        result match
            case LamAbs(newName, Var(NamedDeBruijn("y", 0), _), _) =>
                assert(newName != "y") // Should be renamed to avoid capture
            case _ => fail(s"Unexpected result: $result")
    }

    test("should handle substitution with multiple bound variables") {
        // (λx.λy.x y) y => λy'.y y
        val term = λ("x")(λ("y")(vr"x" $ vr"y")) $ vr"y"
        val result = Inliner(term)

        result match
            case LamAbs(
                  newName,
                  Apply(Var(NamedDeBruijn("y", 0), _), Var(NamedDeBruijn(y2, 0), _), _),
                  _
                ) =>
                assert(newName != "y") // Should be renamed
                assert(y2 == newName) // The bound y should refer to the new name
            case _ => fail(s"Unexpected result: $result")
    }

    test("should respect shadowing in substitution") {
        // (λx.λx.x) y => λx.x
        val term = λ("x")(λ("x")(vr"x")) $ vr"y"
        val result = Inliner(term)

        assert(
          result == λ("x")(vr"x")
        ) // The inner x shadows outer x, so y shouldn't be substituted
    }

    test("should handle multiple variable references") {
        // (λx. x + (x * x)) y => y + (y * y) (tests duplicating a variable reference)
        val term = λ("x")(
          AddInteger $ vr"x" $ (MultiplyInteger $ vr"x" $ vr"x")
        ) $ vr"y"

        val expected = AddInteger $ vr"y" $ (MultiplyInteger $ vr"y" $ vr"y")
        assert(Inliner(term) == expected)
    }

    test("should eliminate Force(Delay(t))") {
        // !(~42) => 42
        val term = !(~(42: Term))
        val expected: Term = 42
        assert(Inliner(term) == expected)
    }

    // Tests for Term.isPure extension method from TermAnalysis.
    // These tests verify that Force terms are correctly identified as impure
    // to prevent incorrect dead code elimination.
    test("should not eliminate Force of constant as dead code - it will error") {
        // (λx. 42) (Force (Const 1))
        // Force of non-delayed term errors at runtime, must be preserved
        // TermAnalysis.isPure correctly identifies Force(Const) as impure
        val term = λ("x")(42) $ Force(1.asTerm)
        val expected = λ("x")(42) $ Force(1.asTerm)
        assert(Inliner(term) == expected)
    }

    test("should not eliminate Force of builtin as dead code") {
        // (λx. 100) (Force AddInteger)
        // Forcing a builtin (not delayed) will error
        // TermAnalysis.isPure correctly identifies this as impure
        val term = λ("x")(100) $ Force(AddInteger)
        val expected = λ("x")(100) $ Force(AddInteger)
        assert(Inliner(term) == expected)
    }

    test("should not eliminate Force of variable as dead code") {
        // (λy. (λx. 42) (Force y))
        // We don't know if y is delayed, so Force y could error
        // TermAnalysis.isPure conservatively treats Force as impure
        val term = λ("y")(λ("x")(42) $ Force(vr"y"))
        val expected = λ("y")(λ("x")(42) $ Force(vr"y"))
        assert(Inliner(term) == expected)
    }

    test("should inline small constant with multiple occurrences") {
        // (λx. x + x) 42 => 42 + 42
        // Small integer (≤64 bits flat-encoded) is safe to duplicate
        val term = λ("x")(AddInteger $ vr"x" $ vr"x") $ 42
        val expected = 84.asTerm
        assert(Inliner(term) == expected)
    }

    test("should not inline large constant with multiple occurrences") {
        // (λx. pair x x) "a long string..." => (λx. pair x x) "a long string..."
        // Large constant (>64 bits) should not be duplicated
        val largeStr: Term = "this is a long string that exceeds 64 bits in flat encoding"
        val term = λ("x")(Constr(Word64.Zero, List(vr"x", vr"x"))) $ largeStr
        assert(Inliner(term) == term)
    }

    test("should inline large constant with single occurrence") {
        // (λx. x) "a long string..." => "a long string..."
        // Even large constants are safe when used only once (identity is a special case,
        // so use a non-identity single-occurrence body)
        val largeStr: Term = "this is a long string that exceeds 64 bits in flat encoding"
        val term = λ("x")(Constr(Word64.Zero, List(vr"x"))) $ largeStr
        val expected = Constr(Word64.Zero, List(largeStr))
        assert(Inliner(term) == expected)
    }

    test("should inline builtin with multiple occurrences") {
        // (λx. x y (x z)) AddInteger => AddInteger y (AddInteger z)
        // Builtins are always safe to duplicate
        val term = λ("x")(vr"x" $ vr"y" $ (vr"x" $ vr"z")) $ AddInteger
        val expected = AddInteger $ vr"y" $ (AddInteger $ vr"z")
        assert(Inliner(term) == expected)
    }

    test("should eliminate unused pure Delay as dead code") {
        // (λx. 42) (Delay (Const 1))
        // Delay IS pure - can be safely eliminated
        // TermAnalysis.isPure correctly identifies Delay as pure
        val term = λ("x")(42) $ Delay(1.asTerm)
        val expected = 42.asTerm
        assert(Inliner(term) == expected)
    }

    // ========================================================================
    // Partial evaluation integration
    // ========================================================================

    test("should partially evaluate closed builtin application") {
        // addInteger 2 3 => 5 (closed term, folded by tryPartialEval)
        val term = AddInteger $ 2 $ 3
        assert(Inliner(term) == 5.asTerm)
    }

    test("should partially evaluate Case on known Constr") {
        // case (constr 0 [1, 2]) of [λa.λb. addInteger a b] => 3
        val term = Case(
          Constr(Word64.Zero, List(1, 2)),
          List(λ("a", "b")(AddInteger $ vr"a" $ vr"b"))
        )
        assert(Inliner(term) == 3.asTerm)
    }

    test("should optimize subexpressions inside Constr args") {
        // Constr(0, [(λx.x) 42]) => Constr(0, [42])
        val term = Constr(Word64.Zero, List(λ("x")(vr"x") $ 42))
        val expected = Constr(Word64.Zero, List[Term](42))
        assert(Inliner(term) == expected)
    }

    test("should cascade optimizations after substitution") {
        // (λf. f 2 3) addInteger => addInteger 2 3 => 5
        val term = λ("f")(vr"f" $ 2 $ 3) $ AddInteger
        assert(Inliner(term) == 5.asTerm)
    }

    // ========================================================================
    // Force/Delay interactions
    // ========================================================================

    test("should eliminate nested Force(Delay)") {
        // Force(Force(Delay(Delay(42)))) => 42
        val term = Force(Force(Delay(Delay(42.asTerm))))
        assert(Inliner(term) == 42.asTerm)
    }

    test("Force(Delay) revealed after optimization should be eliminated") {
        // Force((λx. Delay(x)) y) => after go: Force(Delay(y)) => y
        val term = Force(λ("x")(Delay(vr"x")) $ vr"y")
        assert(Inliner(term) == vr"y")
    }

    // ========================================================================
    // shouldInline: single-occurrence non-trivial terms
    // ========================================================================

    test("should inline single-occurrence Delay argument in direct position") {
        // x occurs once in direct position (OnceDirect) → safe to inline any term.
        // Delay(42) is the argument; after substitution: addInteger(Delay(42), 1)
        val term = λ("x")(AddInteger $ vr"x" $ 1) $ Delay(42.asTerm)
        val expected = AddInteger $ Delay(42.asTerm) $ 1.asTerm
        assert(Inliner(term) == expected)
    }

    test("should inline single-occurrence LamAbs argument in direct position") {
        // (λf. f 1) (λy. addInteger y 2)
        // f occurs once in direct position (OnceDirect) → inline.
        // After substitution: (λy. addInteger y 2) 1 → folds to 3.
        val term = λ("f")(vr"f" $ 1) $ λ("y")(AddInteger $ vr"y" $ 2)
        assert(Inliner(term) == 3.asTerm)
    }

    // ========================================================================
    // Guarded occurrence tests
    // ========================================================================

    test("should not inline non-value in guarded (Delay) position") {
        // λx. Delay(x) applied to a non-value (addInteger 1 2 → 3, but consider Error)
        // x occurs once under Delay → OnceGuarded. Error is not a value → not inlined.
        val term = λ("x")(Delay(vr"x")) $ Error()
        assert(Inliner(term) == term)
    }

    test("should inline value in guarded (Delay) position") {
        // λx. Delay(x) applied to Delay(42) — a value.
        // x occurs once under Delay → OnceGuarded. Delay(42) is a value → inlined.
        val term = λ("x")(Delay(vr"x")) $ Delay(42.asTerm)
        val expected = Delay(Delay(42.asTerm))
        assert(Inliner(term) == expected)
    }

    test("should not inline non-value in guarded (Case branch) position") {
        // x occurs only inside a Case branch → OnceGuarded.
        // (addInteger 1 2) is not a value → not inlined.
        // But the whole expression is closed, so tryPartialEval may fold it.
        val term = λ("x")(Case(Constr(Word64.Zero, Nil), List(vr"x"))) $ (AddInteger $ 1 $ 2)
        val result = Inliner(term)
        // After go: arg becomes 3 (small const, which IS a value), so OnceGuarded + value → inlined
        assert(result == 3.asTerm)
    }

    test("should inline value in guarded (LamAbs body) position") {
        // λx. λy. x applied to 42 — x occurs once under LamAbs → OnceGuarded.
        // 42 (small const) is a value → inlined.
        val term = λ("x")(λ("y")(vr"x")) $ 42
        val expected = λ("y")(42.asTerm)
        assert(Inliner(term) == expected)
    }

    test("should not inline non-value in guarded (LamAbs body) position") {
        // λx. λy. x applied to Error — x under LamAbs → OnceGuarded.
        // Error is not a value → not inlined. Also not pure → not dead code eliminated.
        val term = λ("x")(λ("y")(vr"x")) $ Error()
        assert(Inliner(term) == term)
    }

    // ========================================================================
    // Non-inlinable Apply not passed to tryPartialEval
    // ========================================================================

    // BUG: When shouldInline returns false, the Apply is returned without
    // calling tryPartialEval, even though the whole term may be closed.
    test("should partially evaluate non-inlinable closed Apply") {
        val longA: Term = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        val longB: Term = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
        val arg = AppendString $ longA $ longB
        // After go(arg), arg folds to a large string constant (>64 bits).
        // shouldInline(largeConst, 2) = false, but tryPartialEval should fold the Apply
        // since the whole expression is closed and evaluates to a Const.
        val term = λ("x")(AppendString $ vr"x" $ vr"x") $ arg
        val result = Inliner(term)
        result match
            case Const(_, _) => succeed
            case _           => fail(s"Expected Const, got: ${result.show}")
    }

    // ========================================================================
    // Substitute correctness: freshName collision with body free vars
    // ========================================================================

    test("substitute should not capture body free vars during alpha-renaming") {
        val inliner = new Inliner()
        // substitute(λy. (y_0, x), "x", y)
        // replacement = Var("y"), so alpha-rename "y" must avoid both {"y"} and {"y_0"}
        val term = LamAbs("y", Constr(Word64.Zero, List(vr"y_0", vr"x")))
        val result = inliner.substitute(term, "x", vr"y")
        val freeVarsResult = result.freeVars
        assert(
          freeVarsResult.contains("y_0"),
          s"Free variable y_0 was captured! Result: $result, freeVars: $freeVarsResult"
        )
    }

    // ========================================================================
    // Substitution into Constr and Case
    // ========================================================================

    test("should substitute into Constr arguments") {
        // (λx. constr 0 [x, 1]) y => constr 0 [y, 1]
        val term = λ("x")(Constr(Word64.Zero, List(vr"x", 1))) $ vr"y"
        val expected = Constr(Word64.Zero, List(vr"y", 1.asTerm))
        assert(Inliner(term) == expected)
    }

    test("should substitute into Case branches") {
        // (λx. case scrutinee of [x, 0]) y => case scrutinee of [y, 0]
        val term = λ("x")(Case(vr"scrutinee", List(vr"x", 0))) $ vr"y"
        val expected = Case(vr"scrutinee", List(vr"y", 0.asTerm))
        assert(Inliner(term) == expected)
    }

    // ========================================================================
    // Semantic preservation for closed terms
    // ========================================================================

    test("optimization preserves semantics of closed arithmetic") {
        val terms = List(
          AddInteger $ 10 $ (MultiplyInteger $ 3 $ 4),
          λ("x")(AddInteger $ vr"x" $ vr"x") $ 21,
          λ("x", "y")(MultiplyInteger $ vr"x" $ vr"y") $ 6 $ 7,
          Case(
            Constr(Word64.Zero, List[Term](5, 10)),
            List(λ("a", "b")(AddInteger $ vr"a" $ vr"b"))
          )
        )
        terms.foreach { term =>
            val optimized = Inliner(term)
            val origResult = term.evaluate
            val optResult = optimized.evaluate
            assert(
              origResult α_== optResult,
              s"Semantics differ for ${term.show}: original=$origResult, optimized=$optResult"
            )
        }
    }

    test("optimization is idempotent") {
        val terms = List[Term](
          AddInteger $ 2 $ 3,
          λ("x")(vr"x") $ 42,
          λ("x")(AddInteger $ vr"x" $ vr"x") $ vr"y",
          Force(Delay(1.asTerm)),
          Case(Constr(Word64.Zero, List[Term](1)), List(λ("a")(vr"a"))),
          λ("x")(Constr(Word64.Zero, List(vr"x", vr"x"))) $ vr"z"
        )
        terms.foreach { term =>
            val once = Inliner(term)
            val twice = Inliner(once)
            assert(once == twice, s"Not idempotent for ${term.show}: once=$once, twice=$twice")
        }
    }

    test("optimization does not introduce new free variables") {
        val terms = List[Term](
          λ("x")(vr"x") $ vr"y",
          λ("x")(AddInteger $ vr"x" $ vr"z") $ vr"y",
          λ("x", "y")(vr"x") $ vr"y",
          Force(Delay(vr"a")),
          Case(vr"s", List(λ("x")(vr"x") $ vr"b", vr"c"))
        )
        terms.foreach { term =>
            val optimized = Inliner(term)
            val origFree = term.freeVars
            val optFree = optimized.freeVars
            assert(
              optFree.subsetOf(origFree),
              s"New free vars ${optFree -- origFree} in ${term.show} => ${optimized.show}"
            )
        }
    }

    // ========================================================================
    // Logging
    // ========================================================================

    // ========================================================================
    // DCE with builtin totality
    // ========================================================================

    test("should eliminate dead saturated total builtin") {
        // (λx. 42) (AddInteger $ 1 $ 2) => 42
        // AddInteger is total, so the saturated application is pure and can be eliminated
        val term = λ("x")(42) $ (AddInteger $ 1 $ 2)
        assert(Inliner(term) == 42.asTerm)
    }

    test("should not eliminate dead saturated partial builtin") {
        // (λx. 42) (DivideInteger $ 1 $ 0) stays unchanged
        // DivideInteger is partial, so the saturated application is impure
        val term = λ("x")(42) $ (DivideInteger $ 1 $ 0)
        assert(Inliner(term) == term)
    }

    test("should not eliminate dead Trace (side effect)") {
        // (λx. 42) (Force(Trace) $ "hello" $ 1) stays unchanged
        // Trace has a side effect (logging), must be preserved
        val term = λ("x")(42) $ (Force(Builtin(Trace)) $ "hello" $ 1)
        assert(Inliner(term) == term)
    }

    test("should produce log entries for optimizations") {
        val inliner = new Inliner()
        inliner(λ("x")(vr"x") $ (AddInteger $ 1 $ 2))
        val logs = inliner.logs
        assert(logs.nonEmpty, "Expected log entries for optimizations")
        assert(logs.exists(_.contains("Inlining identity function")))
    }
}
