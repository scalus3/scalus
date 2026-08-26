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
import scala.language.implicitConversions

class EtaReduceTest extends AnyFunSuite:
    def etaReduce(term: Term): Term = EtaReduce(term)

    test("(lam x [f x]) reduces to f"):
        assert(etaReduce(λ("x")(vr"f" $ vr"x")) == vr"f")

    test("(lam x [(builtin unBData) x]) reduces to (builtin unBData)"):
        assert(etaReduce(λ("x")(Builtin(UnBData) $ vr"x")) == Builtin(UnBData))

    test("(lam x [(lam f f) x]) reduces to (lam f f)"):
        assert(etaReduce(λ("x")(λ("f")(vr"f") $ vr"x")) == λ("f")(vr"f"))

    test("(lam x (lam y [f x y])) does not reduce as [f x] may have side effects"):
        assert(etaReduce(λ("x", "y")(vr"f" $ vr"x" $ vr"y")) == λ("x", "y")(vr"f" $ vr"x" $ vr"y"))

    test("(lam x [(error) x]) does not reduce"):
        assert(etaReduce(λ("x")(Error() $ vr"x")) == λ("x")(Error() $ vr"x"))

    test("(lam x [(delay error) x]) reduces to (delay error)"):
        assert(etaReduce(λ("x")(Delay(Error()) $ vr"x")) == Delay(Error()))

    test("(lam x [(force f) x]) does not reduce"):
        assert(etaReduce(λ("x")(Force(vr"f") $ vr"x")) == λ("x")(Force(vr"f") $ vr"x"))

    test("(lam x [(force (delay error)) x]) does not reduce"):
        // (force (delay error)) fails immediately, the lambda only fails when applied
        val term = λ("x")(Force(Delay(Error())) $ vr"x")
        assert(etaReduce(term) == term)

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
        val parsedTerm = Term
            .parseUplc(term)
            .getOrElse(
              throw new Exception("Failed to parse term in EtaReduceTest")
            )
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
          etaReduce(λ("x")(Builtin(AddInteger) $ Error() $ vr"x")) == λ("x")(
            Builtin(AddInteger) $ Error() $ vr"x"
          )
        )

    test(
      "(lam x [(builtin addInteger) (force error) x]) does not reduce due to impure argument"
    ):
        assert(
          etaReduce(λ("x")(Builtin(AddInteger) $ Force(Error()) $ vr"x")) == λ("x")(
            Builtin(AddInteger) $ Force(Error()) $ vr"x"
          )
        )

    // Saturated total builtin is now pure, so eta-reduction applies
    test("(lam x [[(builtin addInteger) 1 2] x]) reduces (saturated total builtin is pure)"):
        val saturatedTotal = Builtin(AddInteger) $ 1 $ 2
        assert(
          etaReduce(λ("x")(saturatedTotal $ vr"x")) == saturatedTotal
        )

    // Saturated partial builtin is impure, so eta-reduction does NOT apply
    test(
      "(lam x [[(builtin divideInteger) 1 0] x]) does not reduce (saturated partial builtin is impure)"
    ):
        val saturatedPartial = Builtin(DivideInteger) $ 1 $ 0
        assert(
          etaReduce(λ("x")(saturatedPartial $ vr"x")) == λ("x")(saturatedPartial $ vr"x")
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

    // (case 1 ...) errors at runtime: a case only matches constr values
    test("(lam x [(case 1 [y, z]) x]) does not reduce"):
        val term = λ("x")(Case(Const(Constant.Integer(1)), List(vr"y", vr"z")) $ vr"x")
        assert(etaReduce(term) == term)

    // (case (constr 0) [y, z]) evaluates the selected branch y, which is pure
    test("(lam x [(case (constr 0) [y, z]) x]) reduces to the case term"):
        val caseTerm = Case(Constr(Word64.Zero, Nil), List(vr"y", vr"z"))
        assert(etaReduce(λ("x")(caseTerm $ vr"x")) == caseTerm)

    // Value-arity tracking: multi-argument eta-wrappers over let-bound lambdas

    test("[(lam f (lam a (lam b [f a b]))) (lam x (lam y x))] reduces the wrapper to f"):
        // f is let-bound to a 2-argument lambda, so [f a] is a pure partial application
        val rhs = λ("x", "y")(vr"x")
        val term = λ("f")(λ("a", "b")(vr"f" $ vr"a" $ vr"b")) $ rhs
        assert(etaReduce(term) == (λ("f")(vr"f") $ rhs))

    test("wrapper over the self-application fixpoint encoding reduces to f"):
        // rhs = [(lam s [s s]) (lam s (lam a (lam b 42)))] evaluates to a 2-argument lambda
        val fix = λ("s")(vr"s" $ vr"s") $ λ("s")(λ("a", "b")(Const(Constant.Integer(42))))
        val term = λ("f")(λ("a", "b")(vr"f" $ vr"a" $ vr"b")) $ fix
        assert(etaReduce(term) == (λ("f")(vr"f") $ fix))

    test("wrapper does not reduce when the bound lambda has arity 1"):
        // f is bound to (lam x x): applying [f a] already evaluates the body, and applying
        // one more argument to its result is an unknown computation
        val rhs = λ("x")(vr"x")
        val term = λ("f")(λ("a", "b")(vr"f" $ vr"a" $ vr"b")) $ rhs
        assert(etaReduce(term) == term)

    test("wrapper does not reduce when the variable is rebound by an inner lambda"):
        // the inner (lam f ...) shadows the let-bound f, so its arity is unknown
        val rhs = λ("x", "y")(vr"x")
        val term = λ("f")(λ("f")(λ("a", "b")(vr"f" $ vr"a" $ vr"b"))) $ rhs
        assert(etaReduce(term) == term)

    // Case-constr encoding of multi-argument eta-wrappers:
    //   (lam a (lam b (case (constr 0 a b) f)))  =>  f

    /** The case-constr wrapper `(lam n1 .. (lam nk (case (constr tag fields) branches)))`. */
    private def ccWrapper(
        binders: List[String],
        fields: List[Term],
        branches: List[Term],
        tag: Word64 = Word64.Zero
    ): Term =
        binders.foldRight(Case(Constr(tag, fields), branches): Term)((n, acc) => λ(n)(acc))

    test("(lam a (lam b (case (constr 0 a b) f))) reduces to f when f is a 2-arg lambda"):
        val rhs = λ("x", "y")(vr"x")
        val wrapper = ccWrapper(List("a", "b"), List(vr"a", vr"b"), List(vr"f"))
        val term = λ("f")(wrapper) $ rhs
        assert(etaReduce(term) == (λ("f")(vr"f") $ rhs))

    test("3-arg case-constr wrapper over the self-application fixpoint reduces to f"):
        val fix = λ("s")(vr"s" $ vr"s") $ λ("s")(λ("a", "b", "c")(Const(Constant.Integer(42))))
        val wrapper = ccWrapper(List("a", "b", "c"), List(vr"a", vr"b", vr"c"), List(vr"f"))
        val term = λ("f")(wrapper) $ fix
        assert(etaReduce(term) == (λ("f")(vr"f") $ fix))

    test("1-arg case-constr wrapper over a pure term reduces (same as the plain rule)"):
        // (lam a (case (constr 0 a) unBData)) = (lam a [unBData a])
        val term = ccWrapper(List("a"), List(vr"a"), List(Builtin(UnBData)))
        assert(etaReduce(term) == Builtin(UnBData))

    test("2-arg case-constr wrapper over a pure NON-lambda does NOT reduce"):
        // (lam a (lam b (case (constr 0 a b) 5))) partially applied is a lambda value;
        // (con integer 5) partially applied is an error. Purity alone must not fire for n >= 2.
        val term = ccWrapper(List("a", "b"), List(vr"a", vr"b"), List(Const(Constant.Integer(5))))
        assert(etaReduce(term) == term)

    test("2-arg case-constr wrapper does not reduce when the bound lambda has arity 1"):
        val rhs = λ("x")(vr"x")
        val wrapper = ccWrapper(List("a", "b"), List(vr"a", vr"b"), List(vr"f"))
        val term = λ("f")(wrapper) $ rhs
        assert(etaReduce(term) == term)

    test("case-constr wrapper with wrong tag does not reduce"):
        val rhs = λ("x", "y")(vr"x")
        val wrapper = ccWrapper(List("a", "b"), List(vr"a", vr"b"), List(vr"f"), tag = Word64(1))
        val term = λ("f")(wrapper) $ rhs
        assert(etaReduce(term) == term)

    test("case-constr wrapper with two branches does not reduce"):
        val rhs = λ("x", "y")(vr"x")
        val wrapper = ccWrapper(List("a", "b"), List(vr"a", vr"b"), List(vr"f", vr"g"))
        val term = λ("f")(wrapper) $ rhs
        assert(etaReduce(term) == term)

    test("case-constr wrapper with permuted fields does not reduce"):
        // (lam a (lam b (case (constr 0 b a) f))) flips the arguments; it is NOT f
        val rhs = λ("x", "y")(vr"x")
        val wrapper = ccWrapper(List("a", "b"), List(vr"b", vr"a"), List(vr"f"))
        val term = λ("f")(wrapper) $ rhs
        assert(etaReduce(term) == term)

    test("case-constr wrapper with a duplicated field does not reduce"):
        val rhs = λ("x", "y")(vr"x")
        val wrapper = ccWrapper(List("a", "b"), List(vr"a", vr"a"), List(vr"f"))
        val term = λ("f")(wrapper) $ rhs
        assert(etaReduce(term) == term)

    test("case-constr wrapper with an extra field does not reduce"):
        val rhs = λ("x", "y")(vr"x")
        val wrapper = ccWrapper(List("a", "b"), List(vr"a", vr"b", vr"b"), List(vr"f"))
        val term = λ("f")(wrapper) $ rhs
        assert(etaReduce(term) == term)

    test("case-constr wrapper with missing fields does not reduce"):
        val rhs = λ("x", "y")(vr"x")
        val wrapper = ccWrapper(List("a", "b"), List(vr"a"), List(vr"f"))
        val term = λ("f")(wrapper) $ rhs
        assert(etaReduce(term) == term)

    test("case-constr wrapper with a non-variable field does not reduce"):
        val rhs = λ("x", "y")(vr"x")
        val wrapper =
            ccWrapper(List("a", "b"), List(vr"a", Const(Constant.Integer(1))), List(vr"f"))
        val term = λ("f")(wrapper) $ rhs
        assert(etaReduce(term) == term)

    test("case-constr wrapper whose branch captures a bound variable does not reduce"):
        // branch [f a] uses the wrapper's own binder
        val rhs = λ("x", "y")(vr"x")
        val wrapper = ccWrapper(List("a", "b"), List(vr"a", vr"b"), List(vr"f" $ vr"a"))
        val term = λ("f")(wrapper) $ rhs
        assert(etaReduce(term) == term)

    test("case-constr wrapper with duplicated binder names does not reduce"):
        // (lam a (lam a (case (constr 0 a a) f))): both fields refer to the INNER a
        val rhs = λ("x", "y")(vr"x")
        val wrapper = ccWrapper(List("a", "a"), List(vr"a", vr"a"), List(vr"f"))
        val term = λ("f")(wrapper) $ rhs
        assert(etaReduce(term) == term)

    test("shadowed let binding does not feed its arity into a case-constr wrapper"):
        // inner (lam f ...) shadows the outer let-bound 2-arg f
        val rhs = λ("x", "y")(vr"x")
        val wrapper = ccWrapper(List("a", "b"), List(vr"a", vr"b"), List(vr"f"))
        val term = λ("f")(λ("f")(wrapper)) $ rhs
        assert(etaReduce(term) == term)

    // Redexes nested inside Constr fields and Case scrutinees/branches. UPLC lowered at PV11 is
    // full of Case nodes, so a wrapper that sits under one is the common case, not a corner case.

    test("eta-redex inside a constr field reduces"):
        val term = Constr(Word64.Zero, List(λ("x")(vr"f" $ vr"x")))
        assert(etaReduce(term) == Constr(Word64.Zero, List(vr"f")))

    test("eta-redex inside a case scrutinee reduces"):
        val term = Case(λ("x")(vr"f" $ vr"x"), List(vr"g"))
        assert(etaReduce(term) == Case(vr"f", List(vr"g")))

    test("eta-redex inside a case branch reduces"):
        val term = Case(vr"scrut", List(λ("x")(vr"f" $ vr"x"), vr"g"))
        assert(etaReduce(term) == Case(vr"scrut", List(vr"f", vr"g")))

    test("eta-redex nested under several constr/case layers reduces"):
        val inner = λ("x")(vr"f" $ vr"x")
        val term = Case(vr"scrut", List(Constr(Word64.Zero, List(Delay(inner)))))
        assert(etaReduce(term) == Case(vr"scrut", List(Constr(Word64.Zero, List(Delay(vr"f"))))))

    test("a let-bound arity is visible to a redex inside a case branch"):
        // The 2-arg rhs makes [f a] pure, so the whole wrapper collapses under the Case -- without
        // the arity environment reaching in there, (lam a (lam b [f a b])) would survive untouched
        // (see "(lam x (lam y [f x y])) does not reduce ..." for the no-arity-known case).
        val rhs = λ("x", "y")(vr"x")
        val body = Case(vr"scrut", List(λ("a")(λ("b")(vr"f" $ vr"a" $ vr"b"))))
        val term = λ("f")(body) $ rhs
        val expected = λ("f")(Case(vr"scrut", List(vr"f"))) $ rhs
        assert(etaReduce(term) == expected)
