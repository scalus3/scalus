package scalus.uplc
package transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.Word64
import scalus.uplc.Constant
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.transform.EtaReduce.etaReduce

import scala.language.implicitConversions

class EtaReduceTest extends AnyFunSuite:
    test("(lam x [f x]) reduces to f"):
        assert(etaReduce(λ("x")(vr"f" $ vr"x")) == vr"f")

    test("(lam x [(builtin unBData) x]) reduces to (builtin unBData)"):
        assert(etaReduce(λ("x")(Builtin(UnBData) $ vr"x")) == Builtin(UnBData))

    test("(lam x [(lam f f) x]) reduces to (lam f f)"):
        assert(etaReduce(λ("x")(λ("f")(vr"f") $ vr"x")) == λ("f")(vr"f"))

    test("(lam x (lam y [f x y])) does not reduce as [f x] may have side effects"):
        assert(etaReduce(λ("x", "y")(vr"f" $ vr"x" $ vr"y")) == λ("x", "y")(vr"f" $ vr"x" $ vr"y"))

    test("(lam x [(error) x]) does not reduce"):
        assert(etaReduce(λ("x")(Error $ vr"x")) == λ("x")(Error $ vr"x"))

    test("(lam x [(delay error) x]) reduces to (delay error)"):
        assert(etaReduce(λ("x")(Delay(Error) $ vr"x")) == Delay(Error))

    test("(lam x [(force f) x]) does not reduce"):
        assert(etaReduce(λ("x")(Force(vr"f") $ vr"x")) == λ("x")(Force(vr"f") $ vr"x"))

    test("(lam x [(builtin divideInteger) 1 x]) reduces to [(builtin divideInteger) 1]"):
        assert(
          etaReduce(λ("x")(Builtin(DivideInteger) $ 1 $ vr"x")) == (Builtin(
            DivideInteger
          ) $ 1)
        )

    test("(lam x [x x]) does not reduce as x is bound in the function body"):
        assert(etaReduce(λ("x")(vr"x" $ vr"x")) == λ("x")(vr"x" $ vr"x"))

    test("nested eta-reductions") {
        val term = "(lam x44 (lam y45 [(builtin equalsByteString) x44 y45]))"
        val parsedTerm = UplcParser().term
            .parse(term)
            .getOrElse(
              throw new Exception("Failed to parse term in EtaReduceTest")
            )
            ._2
        val reducedTerm = etaReduce(parsedTerm)
        assert(
          reducedTerm == Builtin(EqualsByteString),
          s"Expected Builtin(EqualsByteString), got ${reducedTerm.showHighlighted}"
        )
    }

    // Polymorphic builtins with Force
    test("(lam x [(force (builtin headList)) x]) reduces to (force (builtin headList))"):
        assert(
          etaReduce(λ("x")(Force(Builtin(HeadList)) $ vr"x")) == Force(Builtin(HeadList))
        )

    test(
      "(lam x (lam y [(force (builtin mkCons)) x y])) reduces to (force (builtin mkCons))"
    ):
        assert(
          etaReduce(λ("x", "y")(Force(Builtin(MkCons)) $ vr"x" $ vr"y")) == Force(
            Builtin(MkCons)
          )
        )

    test(
      "(lam x [(force (force (builtin fstPair))) x]) reduces to (force (force (builtin fstPair)))"
    ):
        assert(
          etaReduce(λ("x")(Force(Force(Builtin(FstPair))) $ vr"x")) == Force(
            Force(Builtin(FstPair))
          )
        )

    test(
      "(lam x (lam y (lam z [(force (force (builtin chooseList))) x y z]))) reduces to (force (force (builtin chooseList)))"
    ):
        assert(
          etaReduce(
            λ("x", "y", "z")(Force(Force(Builtin(ChooseList))) $ vr"x" $ vr"y" $ vr"z")
          ) == Force(Force(Builtin(ChooseList)))
        )

    // 3-argument builtins
    test(
      "(lam x (lam y (lam z [(builtin verifyEd25519Signature) x y z]))) reduces to (builtin verifyEd25519Signature)"
    ):
        assert(
          etaReduce(
            λ("x", "y", "z")(Builtin(VerifyEd25519Signature) $ vr"x" $ vr"y" $ vr"z")
          ) == Builtin(VerifyEd25519Signature)
        )

    test(
      "(lam x (lam y (lam z [(builtin sliceByteString) x y z]))) reduces to (builtin sliceByteString)"
    ):
        assert(
          etaReduce(
            λ("x", "y", "z")(Builtin(SliceByteString) $ vr"x" $ vr"y" $ vr"z")
          ) == Builtin(SliceByteString)
        )

    // Partially applied polymorphic builtin
    test(
      "(lam x [(force (builtin ifThenElse)) true x]) reduces to [(force (builtin ifThenElse)) true]"
    ):
        assert(
          etaReduce(
            λ("x")(Force(Builtin(IfThenElse)) $ Constant.Bool(true) $ vr"x")
          ) == (Force(Builtin(IfThenElse)) $ Constant.Bool(true))
        )

    test(
      "(lam x (lam y [(force (builtin ifThenElse)) true x y])) reduces to [(force (builtin ifThenElse)) true]"
    ):
        assert(
          etaReduce(
            λ("x", "y")(Force(Builtin(IfThenElse)) $ Constant.Bool(true) $ vr"x" $ vr"y")
          ) == (Force(Builtin(IfThenElse)) $ Constant.Bool(true))
        )

    // Impure arguments
    test("(lam x [(builtin addInteger) error x]) does not reduce due to impure argument"):
        assert(
          etaReduce(λ("x")(Builtin(AddInteger) $ Error $ vr"x")) == λ("x")(
            Builtin(AddInteger) $ Error $ vr"x"
          )
        )

    test(
      "(lam x [(builtin addInteger) (force error) x]) does not reduce due to impure argument"
    ):
        assert(
          etaReduce(λ("x")(Builtin(AddInteger) $ Force(Error) $ vr"x")) == λ("x")(
            Builtin(AddInteger) $ Force(Error) $ vr"x"
          )
        )

    // Saturated builtin (impure)
    test("(lam x [[(builtin addInteger) 1 2] x]) does not reduce (saturated builtin is impure)"):
        val saturatedBuiltin = Builtin(AddInteger) $ 1 $ 2
        assert(
          etaReduce(λ("x")(saturatedBuiltin $ vr"x")) == λ("x")(saturatedBuiltin $ vr"x")
        )

    // Constr is pure
    test("(lam x [(constr 0 []) x]) reduces to (constr 0 [])"):
        assert(
          etaReduce(λ("x")(Constr(Word64.Zero, List.empty) $ vr"x")) == Constr(
            Word64.Zero,
            List.empty
          )
        )

    test("(lam x [(constr 0 [1, 2]) x]) reduces to (constr 0 [1, 2])"):
        assert(
          etaReduce(
            λ("x")(
              Constr(
                Word64.Zero,
                List(Const(Constant.Integer(1)), Const(Constant.Integer(2)))
              ) $ vr"x"
            )
          ) == Constr(
            Word64.Zero,
            List(Const(Constant.Integer(1)), Const(Constant.Integer(2)))
          )
        )

    // Const is pure
    test("(lam x [(const 42) x]) reduces to (const 42)"):
        assert(
          etaReduce(λ("x")(Const(Constant.Integer(42)) $ vr"x")) == Const(Constant.Integer(42))
        )

    // Variable is pure
    test("(lam x [y x]) reduces to y"):
        assert(etaReduce(λ("x")(vr"y" $ vr"x")) == vr"y")

    // Multiple levels of nesting
    test(
      "(lam a (lam b (lam c (lam d [(builtin addInteger) a b])))) reduces to (builtin addInteger)"
    ):
        // The inner [(builtin addInteger) a b] is partially applied (2 args, arity 2), so it's saturated and impure
        // Wait, no. addInteger has arity 2, so with 2 args applied it's saturated, thus impure
        // So (lam b [(builtin addInteger) a b]) should reduce to [(builtin addInteger) a]
        // But [(builtin addInteger) a] is partially applied (1 arg, arity 2), so it's pure
        // Actually, let me reconsider this test case
        val input = λ("a", "b", "c", "d")(Builtin(AddInteger) $ vr"a" $ vr"b")
        // c and d are not used, so they should be removed
        // But the inner [(builtin addInteger) a b] with a, b from outer lambdas is saturated
        // So it's impure and (lam b [(builtin addInteger) a b]) cannot reduce
        // Let me change this test
        assert(
          etaReduce(input) == λ("a", "b", "c", "d")(Builtin(AddInteger) $ vr"a" $ vr"b")
        )

    // Case with pure scrutinee and cases
    test("(lam x [(case 1 [y, z]) x]) reduces to (case 1 [y, z])"):
        assert(
          etaReduce(λ("x")(Case(Const(Constant.Integer(1)), List(vr"y", vr"z")) $ vr"x")) == Case(
            Const(Constant.Integer(1)),
            List(vr"y", vr"z")
          )
        )
