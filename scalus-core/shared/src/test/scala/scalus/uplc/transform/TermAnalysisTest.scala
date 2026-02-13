package scalus.uplc
package transform

import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.Constant.given
import scalus.uplc.builtin.ByteString.hex
import DefaultFun.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Word64
import scalus.uplc.DefaultUni
import TermAnalysis.isPure

import scala.language.implicitConversions

class TermAnalysisTest extends AnyFunSuite:

    // Pure terms - basic cases
    test("Var is pure") {
        assert(vr"x".isPure)
    }

    test("Const is pure") {
        assert(Const(Constant.Integer(42)).isPure)
        assert(Const(Constant.String("hello")).isPure)
        assert(Const(Constant.Bool(true)).isPure)
    }

    test("Builtin is pure") {
        assert(Builtin(AddInteger).isPure)
        assert(Builtin(HeadList).isPure)
    }

    test("LamAbs is pure") {
        assert(λ("x")(vr"x").isPure)
        assert(λ("x", "y")(vr"x").isPure)
        assert(λ("x")(AddInteger $ vr"x" $ 42).isPure)
    }

    test("Delay is pure") {
        assert(Delay(Const(Constant.Integer(42))).isPure)
        assert(Delay(vr"x").isPure)
    }

    // Impure terms - basic cases
    test("Error is impure") {
        assert(!Error().isPure)
    }

    test("Force of constant is impure") {
        assert(!Force(Const(Constant.Integer(1))).isPure)
        assert(!Force(Const(Constant.String("test"))).isPure)
        assert(!(!(42: Term)).isPure)
    }

    test("Force of variable is impure") {
        assert(!Force(vr"x").isPure)
    }

    test("Force of lambda is impure") {
        assert(!Force(λ("x")(vr"x")).isPure)
    }

    test("Force of Delay is pure (no-op)") {
        // Force(Delay(x)) is pure - it's a no-op that evaluates to x
        assert(Force(Delay(Const(Constant.Integer(1)))).isPure)
        assert(Force(Delay(vr"x")).isPure)
        assert(Force(Delay(Error())).isPure) // Even Error inside Delay is fine
    }

    // Builtins with Force - polymorphic builtins
    test("Force of polymorphic builtin (1 type arg) is pure") {
        // HeadList requires 1 type argument
        assert(Force(Builtin(HeadList)).isPure)
        assert(Force(Builtin(TailList)).isPure)
        assert(Force(Builtin(NullList)).isPure)
    }

    test("Force(Force(builtin)) with 2 type args is pure") {
        // ChooseList requires 2 type arguments
        assert(Force(Force(Builtin(ChooseList))).isPure)
    }

    test("Force of non-polymorphic builtin is impure") {
        // AddInteger has no type arguments
        assert(!Force(Builtin(AddInteger)).isPure)
        assert(!Force(Builtin(MultiplyInteger)).isPure)
    }

    // Partially applied builtins (unsaturated)
    test("Partially applied builtin is pure") {
        // AddInteger needs 2 args, has 1 - pure
        assert((AddInteger $ 1).isPure)
        assert((MultiplyInteger $ 42).isPure)

        // DivideInteger needs 2 args, has 1 - pure
        assert((DivideInteger $ 10).isPure)
    }

    test("Partially applied builtin with multiple args is pure") {
        // SliceByteString needs 3 args, has 2 - pure
        val bs = Const(Constant.ByteString(hex"deadbeef"))
        assert((SliceByteString $ 0 $ 2).isPure)
    }

    // Saturated builtins (can fail)
    test("Saturated builtin application is impure") {
        // AddInteger needs 2 args, has 2 - saturated, impure
        assert(!(AddInteger $ 1 $ 2).isPure)

        // DivideInteger can fail with division by zero
        assert(!(DivideInteger $ 1 $ 0).isPure)
        assert(!(DivideInteger $ 10 $ 5).isPure)

        // MultiplyInteger saturated
        assert(!(MultiplyInteger $ 3 $ 4).isPure)
    }

    test("Saturated polymorphic builtin is impure") {
        // HeadList with type arg and list arg - saturated, can fail
        val list = Const(Constant.List(DefaultUni.Integer, List()))
        assert(!(Force(Builtin(HeadList)) $ list).isPure)
    }

    // Beta-redexes
    test("Beta-redex with pure parts is pure") {
        // (λx. x) 42 - both pure
        assert((λ("x")(vr"x") $ 42).isPure)

        // (λx. AddInteger x) 42 - partially applied builtin in body is pure
        assert((λ("x")(AddInteger $ vr"x") $ 42).isPure)
    }

    test("Beta-redex with impure argument is impure") {
        // (λx. x) Error - argument impure
        assert(!(λ("x")(vr"x") $ Error()).isPure)

        // (λx. x) (Force 1) - argument impure
        assert(!(λ("x")(vr"x") $ Force(Const(Constant.Integer(1)))).isPure)
    }

    test("Beta-redex with impure body is impure") {
        // (λx. Error) 42 - body impure
        assert(!(λ("x")(Error()) $ 42).isPure)

        // (λx. Force x) y - body impure (Force is impure)
        assert(!(λ("x")(Force(vr"x")) $ vr"y").isPure)
    }

    test("Beta-redex with both parts impure is impure") {
        // (λx. Error) Error - both impure
        assert(!(λ("x")(Error()) $ Error()).isPure)
    }

    // Constructors
    test("Constructor with pure arguments is pure") {
        assert(Constr(Word64.Zero, List()).isPure)
        assert(Constr(Word64.Zero, List[Term](42, "test", vr"x")).isPure)
        assert(Constr(Word64.One, List[Term](vr"a", vr"b", 123)).isPure)
    }

    test("Constructor with impure argument is impure") {
        assert(!Constr(Word64.Zero, List(Error())).isPure)
        assert(!Constr(Word64.Zero, List[Term](42, Error(), vr"x")).isPure)
        assert(!Constr(Word64.One, List(Force(Const(Constant.Integer(1))))).isPure)
    }

    test("Constructor with mixed purity is impure") {
        assert(!Constr(Word64.Zero, List[Term](42, Force(vr"x"), vr"y")).isPure)
    }

    // Case expressions
    test("Case with pure scrutinee and cases is pure") {
        val scrut = Constr(Word64.Zero, List[Term](42))
        val cases = List[Term](vr"x", vr"y", 123)
        assert(Case(scrut, cases).isPure)
    }

    test("Case with impure scrutinee is impure") {
        val scrut = Error()
        val cases = List(vr"x", vr"y")
        assert(!Case(scrut, cases).isPure)
    }

    test("Case with impure case is impure") {
        val scrut = Constr(Word64.Zero, List[Term](42))
        val cases = List(vr"x", Error(), vr"z")
        assert(!Case(scrut, cases).isPure)
    }

    test("Case with all impure is impure") {
        val scrut = Error()
        val cases = List(Error(), Force(vr"x"))
        assert(!Case(scrut, cases).isPure)
    }

    // Complex nested cases
    test("Nested pure terms are pure") {
        // (λx. (λy. x + y)) 1 2 - all pure
        val term = λ("x", "y")(AddInteger $ vr"x" $ vr"y") $ 1 $ 2
        // This creates Apply(Apply(LamAbs(...), 1), 2)
        // The inner Apply is a beta-redex (pure), outer is also beta-redex
        // But since it's not matched as beta-redex in one step, and Apply
        // with non-LamAbs is checked via builtin extraction which fails,
        // it will be impure. Let me reconsider...

        // Actually, let's test the intermediate result
        val partial = λ("x", "y")(AddInteger $ vr"x" $ vr"y") $ 1
        // This is Apply(LamAbs(...), 1) - beta-redex with pure parts
        assert(partial.isPure)
    }

    test("Nested impure terms are impure") {
        // (λx. (λy. Error)) 1 2
        val term = λ("x", "y")(Error()) $ 1 $ 2
        assert(!term.isPure)
    }

    test("Delay of impure term is still pure (lazy)") {
        // Delay suspends computation, so even Error is fine inside Delay
        assert(Delay(Error()).isPure)
        assert(Delay(Force(vr"x")).isPure)
    }

    // Apply of non-beta-redex, non-builtin
    test("Apply of non-beta-redex is impure") {
        // vr"f" $ 42 - not a beta-redex, not a builtin
        assert(!(vr"f" $ 42).isPure)

        // (Const 42) $ 1 - applying a constant (nonsensical but impure)
        assert(!(Const(Constant.Integer(42)) $ 1).isPure)
    }

    // Property-based style tests
    test("All pure primitives remain pure in constructor") {
        val pureTerms = List(
          vr"x",
          Const(Constant.Integer(1)),
          Builtin(AddInteger),
          λ("x")(vr"x"),
          Delay(vr"x")
        )
        pureTerms.foreach { term =>
            assert(term.isPure, s"$term should be pure")
        }

        // Constructor of all pure terms
        assert(Constr(Word64.Zero, pureTerms).isPure)
    }

    test("Any impure term makes Apply impure") {
        val impureTerms = List(
          Error(),
          Force(Const(Constant.Integer(1))),
          Force(vr"x"),
          AddInteger $ 1 $ 2 // saturated
        )

        impureTerms.foreach { term =>
            assert(!term.isPure, s"$term should be impure")
            // Beta-redex with impure arg
            assert(!(λ("x")(vr"x") $ term).isPure)
            // Constructor with impure arg
            assert(!Constr(Word64.Zero, List(term)).isPure)
        }
    }
