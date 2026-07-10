package scalus.uplc
package transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Word64
import scalus.uplc.Constant.given
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.PlutusVM
import scalus.uplc.transform.TermAnalysis.{freeVars, isPure, isValueForm}

import scala.language.implicitConversions

/** Differential testing of [[TermAnalysis]] against the CEK machine.
  *
  * The analysis is deliberately conservative, so the checkable invariants are one-directional:
  *   - `isPure(t)` implies evaluation succeeds (for closed `t`); the converse does not hold (e.g.
  *     `Trace` succeeds but is impure by design, `DivideInteger $ 1 $ 1` succeeds but is
  *     conservatively impure)
  *   - `isValueForm(t)` implies evaluation succeeds and returns `t` itself (values evaluate to
  *     themselves)
  *   - `Force(Delay(t))` evaluates exactly like `t` (same value or same failure)
  *
  * The corpus is a curated seed set closed under a fixed set of wrappers to depth 2 — a
  * deterministic, bounded-exhaustive enumeration rather than random generation.
  */
class TermAnalysisCekConformanceTest extends AnyFunSuite:

    private given vm: PlutusVM = PlutusVM.makePlutusV3VM()

    private def evalEither(t: Term): Either[Throwable, Term] =
        try Right(t.evaluate)
        catch case e: Exception => Left(e)

    /** deBruijn round-trip normalizes binder names so evaluation results compare structurally */
    private def normalized(t: Term): Term = DeBruijn.fromDeBruijnTerm(DeBruijn.deBruijnTerm(t))

    // ------------------------------------------------------------------
    // Corpus
    // ------------------------------------------------------------------

    private val emptyIntList = Const(Constant.List(DefaultUni.Integer, List()))

    private val seeds: List[Term] = List(
      Const(Constant.Integer(1)),
      Error(),
      Delay(Const(Constant.Integer(1))),
      Delay(Error()),
      λ("x")(vr"x"),
      Builtin(AddInteger),
      Force(Builtin(HeadList)),
      AddInteger $ 1,
      AddInteger $ 1 $ 2,
      DivideInteger $ 1 $ 0,
      Force(Builtin(IfThenElse)) $ true $ 1,
      Force(Builtin(HeadList)) $ emptyIntList,
      Constr(Word64.Zero, Nil),
      Case(Constr(Word64.Zero, Nil), List[Term](42))
    )

    private def wrappers(t: Term): List[Term] = List(
      Force(t),
      Delay(t),
      λ("x")(vr"x") $ t, // strict use of t
      λ("x")(Const(Constant.Integer(0))) $ t, // dead argument (DCE shape)
      Constr(Word64.Zero, List(t)),
      Case(Constr(Word64.Zero, Nil), List(t)), // t as zero-arg branch
      Case(Constr(Word64.Zero, List(t)), List(λ("a")(vr"a"))), // t as constructor arg
      t $ 42 // t in function position
    )

    private val corpus: List[Term] = {
        val level1 = seeds.flatMap(wrappers)
        val level2 = level1.flatMap(wrappers)
        (seeds ++ level1 ++ level2).distinct
    }

    test("corpus is closed and non-trivial") {
        assert(corpus.forall(_.freeVars.isEmpty))
        assert(corpus.size > 500)
    }

    // ------------------------------------------------------------------
    // Soundness properties
    // ------------------------------------------------------------------

    test("isPure(t) implies CEK evaluation succeeds") {
        for t <- corpus if t.isPure do
            evalEither(t) match
                case Right(_) => ()
                case Left(e) =>
                    fail(s"isPure=true but CEK failed with '$e' for term: ${t.show}")
    }

    test("isValueForm(t) implies CEK succeeds and returns the term itself") {
        for t <- corpus if t.isValueForm do
            evalEither(t) match
                case Right(v) =>
                    assert(
                      v ~=~ normalized(t),
                      s"isValueForm=true but CEK returned ${v.show} for term: ${t.show}"
                    )
                case Left(e) =>
                    fail(s"isValueForm=true but CEK failed with '$e' for term: ${t.show}")
    }

    test("isValueForm implies isPure (values require no computation)") {
        for t <- corpus if t.isValueForm do
            assert(t.isPure, s"isValueForm=true but isPure=false for term: ${t.show}")
    }

    test("Force(Delay(t)) evaluates exactly like t") {
        for t <- corpus do
            (evalEither(Force(Delay(t))), evalEither(t)) match
                case (Right(a), Right(b)) =>
                    assert(a ~=~ b, s"Force(Delay(t)) returned ${a.show}, t returned ${b.show}")
                case (Left(_), Left(_)) => ()
                case (fd, plain) =>
                    fail(
                      s"Force(Delay(t)) and t diverge for ${t.show}: " +
                          s"Force(Delay(t))=$fd, t=$plain"
                    )
    }

    // ------------------------------------------------------------------
    // Shapes the analysis claims must fail: verify the CEK agrees
    // ------------------------------------------------------------------

    test("shapes classified impure because they always fail do fail on the CEK") {
        val alwaysFailing: List[Term] = List(
          Error(),
          Force(Const(Constant.Integer(1))), // forcing a non-delayed term
          Force(λ("x")(vr"x")), // forcing a lambda
          Force(Builtin(AddInteger)), // forcing a non-polymorphic builtin
          Force(Force(Builtin(HeadList))), // over-forced builtin
          Force(Delay(Error())), // force evaluates the delayed body
          Builtin(HeadList) $ emptyIntList, // under-forced: type arg expected first
          Force(Force(Builtin(MkCons))) $ 1, // over-forced with value arg
          AddInteger $ 1 $ 2 $ 3, // over-applied: result is not a function
          Force(Force(Builtin(ChooseList)) $ emptyIntList), // interleaved Force/Apply
          Const(Constant.Integer(1)) $ 2, // applying a constant
          Delay(Error()) $ 1, // applying a delay
          Case(Const(Constant.Integer(1)), List[Term](42)), // non-constr scrutinee
          Case(Constr(Word64(2), Nil), List[Term](42, 43)), // out-of-range tag
          Case(Constr(Word64.Zero, List[Term](42)), List[Term](1)) // non-lambda branch applied
        )
        for t <- alwaysFailing do
            assert(!t.isPure, s"expected impure: ${t.show}")
            assert(!t.isValueForm, s"expected non-value: ${t.show}")
            assert(evalEither(t).isLeft, s"expected CEK failure for: ${t.show}")
    }
