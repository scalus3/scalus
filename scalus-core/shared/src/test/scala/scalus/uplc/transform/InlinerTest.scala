package scalus.uplc
package transform

import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.Constant.given
import DefaultFun.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Word64

import scala.language.implicitConversions

class InlinerTest extends AnyFunSuite {
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
        assert(Inliner(term) == Const(Constant.Integer(42)))

        // (λx.x) "hello" => "hello"
        val strTerm = λ("x")(vr"x") $ "hello"
        assert(Inliner(strTerm) == Const(Constant.String("hello")))
    }

    test("nested identity functions should all be eliminated") {
        // (λx.x) ((λy.y) 42) => 42
        val term = λ("x")(vr"x") $ (λ("y")(vr"y") $ 42)
        assert(Inliner(term) == Const(Constant.Integer(42)))
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
        val term = λ("x")(42) $ Force(Const(Constant.Integer(1)))
        val expected = λ("x")(42) $ Force(Const(Constant.Integer(1)))
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
        val expected = Const(Constant.Integer(84))
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
        val term = λ("x")(42) $ Delay(Const(Constant.Integer(1)))
        val expected = Const(Constant.Integer(42))
        assert(Inliner(term) == expected)
    }
}
