package scalus.uplc
package transform

import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.cardano.ledger.{ExUnits, Word64}
import scalus.uplc.Constant.given
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.{OutOfExBudgetError, PlutusVM, RestrictingBudgetSpender}

import scala.language.implicitConversions

/** Differential testing of [[LetChainRegroup]] against the CEK machine.
  *
  * Random closed UPLC terms built around let chains — with dependencies, repeated binder names,
  * failing right-hand sides, and chains nested inside `Delay`/`Constr`/`Case` — are regrouped, and
  * both terms are evaluated under a budget cap.
  *
  * ==Two properties==
  *
  *   - '''Semantics''': the pass preserves the result exactly. Unlike eta-reduction it does not
  *     change the shape of anything, so results are compared with `~=~`, not "compatible modulo the
  *     transformation".
  *   - '''Budget''': `CaseConstrApply(LetChainRegroup(t))` never costs more than
  *     `CaseConstrApply(t)` — the real before/after, since [[LetChainRegroup]] always runs
  *     immediately before [[CaseConstrApply]] in [[V3Optimizer]].
  *
  * The budget property is deliberately stated on the '''pair''' of passes. Re-association alone can
  * cost more when a right-hand side fails: `[[[F e1] e2] e3]` enters all three `Apply` nodes before
  * evaluating `e1`, where the nested form enters one. The `case (constr 0 [...])` encoding removes
  * that — `Case` + `Constr` is two steps whatever the arity — which is why the pass must never ship
  * without [[CaseConstrApply]] after it.
  */
class LetChainRegroupCekConformanceTest extends AnyFunSuite with ScalaCheckPropertyChecks:

    private given vm: PlutusVM = PlutusVM.makePlutusV3VM()

    private val budgetLimit = ExUnits(memory = 1_000_000L, steps = 500_000_000L)

    private enum Outcome:
        case Ok(result: Term)
        case Failed
        case OutOfBudget

    private def normalized(t: Term): Term = DeBruijn.fromDeBruijnTerm(DeBruijn.deBruijnTerm(t))

    private def run(t: Term): (Outcome, ExUnits) =
        val spender = new RestrictingBudgetSpender(budgetLimit)
        try
            val r = vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(t), spender)
            (Outcome.Ok(normalized(r)), spender.getSpentBudget)
        catch
            case _: OutOfExBudgetError          => (Outcome.OutOfBudget, spender.getSpentBudget)
            case scala.util.control.NonFatal(_) => (Outcome.Failed, spender.getSpentBudget)

    private def agree(a: Outcome, b: Outcome): Boolean = (a, b) match
        case (Outcome.Ok(r1), Outcome.Ok(r2))           => r1 ~=~ r2
        case (Outcome.Failed, Outcome.Failed)           => true
        case (Outcome.OutOfBudget, Outcome.OutOfBudget) => true
        case _                                          => false

    // ------------------------------------------------------------------
    // Generators
    // ------------------------------------------------------------------

    /** Big enough that runs can reach [[LetChainRegroup.MinRunSize]], small enough that binder
      * names still repeat within a chain and exercise shadowing.
      */
    private val binderPool = List("a", "b", "c", "d", "e", "f", "g", "x")

    private def genName: Gen[String] = Gen.oneOf(binderPool)

    private def genConst: Gen[Term] = Gen.oneOf(
      Gen.chooseNum(-3L, 12L).map(i => Const(Constant.Integer(BigInt(i)))),
      Gen.oneOf(true, false).map(b => Const(Constant.Bool(b))),
      Gen.const(Const(Constant.Unit))
    )

    /** Constants are favoured over in-scope variables so that independent runs — the case the pass
      * acts on — occur often enough to measure; variables still appear often enough to split runs.
      */
    private def genLeaf(env: List[String]): Gen[Term] =
        if env.isEmpty then genConst
        else Gen.frequency(3 -> genConst, 2 -> Gen.oneOf(env).map(vr))

    /** Expressions used as right-hand sides and bodies. Includes shapes that fail at runtime
      * (`Error`, ill-typed builtin arguments, division by zero) so the order in which failures
      * happen is exercised, and shapes that nest another chain.
      */
    private def genExpr(env: List[String], depth: Int): Gen[Term] =
        val leaf = genLeaf(env)
        val flat: List[Gen[Term]] = List(
          leaf,
          leaf,
          leaf,
          leaf,
          Gen.const(Error()),
          leaf.map(t => Delay(t)),
          leaf.map(t => Force(Delay(t))),
          for a <- leaf; b <- leaf yield AddInteger $ a $ b,
          for a <- leaf; b <- leaf yield DivideInteger $ a $ b,
          for a <- leaf; b <- leaf yield Constr(Word64.Zero, List(a, b)),
          for
              s <- leaf
              t <- leaf
              f <- leaf
          yield Force(IfThenElse $ s $ Delay(t) $ Delay(f)),
          for
              a <- leaf
              n <- genName
              b <- leaf
          yield Case(Constr(Word64.Zero, List(a)), List(LamAbs(n, b)))
        )
        if depth <= 0 then anyOf(flat)
        else
            Gen.frequency(
              (flat.size, anyOf(flat)),
              (4, genChain(env, depth - 1)),
              (1, genName.flatMap(n => genExpr(n :: env, depth - 1).map(LamAbs(n, _))))
            )

    /** `Gen.oneOf` over a list of generators (the built-in overload takes a list of values). */
    private def anyOf(gs: List[Gen[Term]]): Gen[Term] = Gen.choose(0, gs.size - 1).flatMap(gs(_))

    /** A chain of 1..10 nested lets — long enough that runs regularly reach
      * [[LetChainRegroup.MinRunSize]]. Right-hand sides are generated in the environment *before*
      * their binder, so dependencies on earlier bindings arise naturally and split the runs.
      */
    private def genChain(env: List[String], depth: Int): Gen[Term] =
        def loop(env: List[String], remaining: Int): Gen[Term] =
            if remaining == 0 then genExpr(env, depth)
            else
                for
                    name <- genName
                    rhs <- genExpr(env, depth)
                    body <- loop(name :: env, remaining - 1)
                yield Apply(LamAbs(name, body), rhs)
        Gen.choose(1, 10).flatMap(loop(env, _))

    private def genProgram: Gen[Term] = genChain(Nil, 2)

    // ------------------------------------------------------------------
    // Properties
    // ------------------------------------------------------------------

    test("regrouping preserves the result exactly") {
        forAll(genProgram) { (t: Term) =>
            val regrouped = LetChainRegroup(t)
            val (before, _) = run(t)
            val (after, _) = run(regrouped)
            assert(
              agree(before, after),
              s"\noriginal:  ${t.showHighlighted}\nregrouped: ${regrouped.showHighlighted}\n$before vs $after"
            )
        }
    }

    /** Guards the two properties above against vacuity: a generator that never produced a groupable
      * run would satisfy them trivially. Seeded, because the proportion varies a lot between runs.
      */
    test("the generator produces terms the pass actually regroups") {
        val terms = Gen.listOfN(200, genProgram).pureApply(Gen.Parameters.default, Seed(20260901))
        val changed = terms.count(t => t ~!=~ LetChainRegroup(t))
        assert(changed >= 20, s"only $changed/200 generated terms were regrouped")
    }

    /** Pins the cost model the whole design rests on: a run of five collapses five `Apply` + five
      * `LamAbs` (10 steps) into `Case` + `Constr` + five `LamAbs` (7 steps). One step is 100 memory
      * and 16,000 cpu, so grouping five bindings must save exactly 300 / 48,000.
      */
    test("a run of five saves exactly three machine steps") {
        val chain = (1 to 5).foldRight(Const(Constant.Integer(BigInt(0))): Term) { (i, body) =>
            Apply(LamAbs(s"v$i", body), Const(Constant.Integer(BigInt(i))))
        }
        val regrouped = LetChainRegroup(chain)
        assert(chain ~!=~ regrouped, "the fixture must actually be regrouped")
        val (before, beforeBudget) = run(CaseConstrApply(chain))
        val (after, afterBudget) = run(CaseConstrApply(regrouped))
        assert(agree(before, after))
        assert(beforeBudget.memory - afterBudget.memory == 300, s"$beforeBudget -> $afterBudget")
        assert(beforeBudget.steps - afterBudget.steps == 48000, s"$beforeBudget -> $afterBudget")
    }

    test("regrouping followed by CaseConstrApply never costs more budget") {
        forAll(genProgram) { (t: Term) =>
            val baseline = CaseConstrApply(t)
            val optimized = CaseConstrApply(LetChainRegroup(t))
            val (before, beforeBudget) = run(baseline)
            val (after, afterBudget) = run(optimized)
            assert(agree(before, after), s"\n$before vs $after\n${t.showHighlighted}")
            assert(
              afterBudget.fitsWithin(beforeBudget),
              s"\nbudget grew: $beforeBudget -> $afterBudget\n${t.showHighlighted}\n${optimized.showHighlighted}"
            )
        }
    }
