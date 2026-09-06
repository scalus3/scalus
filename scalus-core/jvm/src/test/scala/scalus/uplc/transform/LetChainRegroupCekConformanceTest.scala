package scalus.uplc
package transform

import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.cardano.ledger.{ExUnits, Word64}
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.{Log, MachineError, OutOfExBudgetError, PlutusVM, RestrictingBudgetSpender}

import scala.language.implicitConversions

/** Differential testing of [[LetChainRegroup]] against the CEK machine.
  *
  * Random closed UPLC terms built around let chains — with dependencies, repeated binder names,
  * failing right-hand sides, and chains nested inside `Delay`/`Constr`/`Case` — are regrouped, and
  * both terms are evaluated under a budget cap.
  *
  * ==Two properties==
  *
  *   - '''Semantics''': ground results, failure classes and trace logs agree. Returned delays are
  *     forced and lambdas are applied to a fixed set of probes: their unevaluated bodies may
  *     legitimately change shape. These bounded observations are not a proof of equivalence.
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
        case Ok(result: Term, logs: List[String])
        case Failed(errorClass: Class[?], logs: List[String])
        case OutOfBudget(logs: List[String])

    private def run(t: Term): (Outcome, ExUnits) = {
        val spender = new RestrictingBudgetSpender(budgetLimit)
        val logger = new Log()
        val outcome =
            try
                val r = vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(t), spender, logger)
                Outcome.Ok(r, logger.getLogs.toList)
            catch
                case _: OutOfExBudgetError => Outcome.OutOfBudget(logger.getLogs.toList)
                // Unexpected host exceptions must fail the test rather than masquerade as UPLC failure.
                case e: MachineError => Outcome.Failed(e.getClass, logger.getLogs.toList)
        (outcome, spender.getSpentBudget)
    }

    private val probes: List[Term] = List(
      Const(Constant.Integer(BigInt(0))),
      Const(Constant.Integer(BigInt(1))),
      Const(Constant.Bool(true)),
      Const(Constant.Bool(false)),
      Const(Constant.Unit)
    )

    private def agree(a: Outcome, b: Outcome, depth: Int = 8): Boolean = (a, b) match
        case (Outcome.Ok(r1, l1), Outcome.Ok(r2, l2)) =>
            l1 == l2 && agreeValues(r1, r2, depth)
        case (Outcome.Failed(e1, l1), Outcome.Failed(e2, l2))   => e1 == e2 && l1 == l2
        case (Outcome.OutOfBudget(l1), Outcome.OutOfBudget(l2)) => l1 == l2
        case _                                                  => false

    private def agreeValues(a: Term, b: Term, depth: Int): Boolean = {
        def observe(x: Term, y: Term): Boolean = {
            assert(depth > 0, "closure observation depth exhausted")
            agree(run(x)._1, run(y)._1, depth - 1)
        }
        (a, b) match
            case (_: Delay, _: Delay) => observe(Force(a), Force(b))
            case (_: LamAbs, _: LamAbs) =>
                probes.forall(arg => observe(Apply(a, arg), Apply(b, arg)))
            case (Constr(t1, xs, _), Constr(t2, ys, _)) =>
                t1 == t2 && xs.size == ys.size &&
                xs.zip(ys).forall((x, y) => agreeValues(x, y, depth))
            case _ => a ~=~ b
    }

    private def trace(label: String, value: Term): Term =
        Apply(Apply(Force(Builtin(Trace)), Const(Constant.String(label))), value)

    private def fiveLets(rhs: List[Term], body: Term): Term = {
        require(rhs.size == 5)
        rhs.zipWithIndex.foldRight(body) { case ((value, i), acc) =>
            Apply(LamAbs(s"v$i", acc), value)
        }
    }

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
          leaf.map(t => trace("rhs", t)),
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
              (1, genChain(env, depth - 1).map(Delay(_))),
              (1, genName.flatMap(n => genExpr(n :: env, depth - 1).map(LamAbs(n, _)))),
              // a chain in FUNCTION position: CaseConstrApply flattens the whole left spine, so
              // this is the shape where a grouped outermost run could swallow the outer argument
              (
                2,
                for
                    ch <- genChain(env, depth - 1)
                    a <- genLeaf(env)
                yield Apply(ch, a)
              )
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

    test("regrouping preserves observed results and trace logs") {
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

    test("returned delays, lambdas and constructor fields are compared by observation") {
        val chain = fiveLets(probes, vr"v0")
        val fixtures = List(
          Delay(chain),
          LamAbs("p", fiveLets(probes, vr"p")),
          Constr(Word64.Zero, List(Delay(chain), LamAbs("p", chain)))
        )
        for fixture <- fixtures do
            val regrouped = LetChainRegroup(fixture)
            assert(fixture ~!=~ regrouped)
            assert(agree(run(fixture)._1, run(regrouped)._1))
            assert(agree(run(CaseConstrApply(fixture))._1, run(CaseConstrApply(regrouped))._1))
        // Different behavior must still be rejected, even when both results are closures.
        assert(!agree(run(Delay(probes.head))._1, run(Delay(probes(1)))._1))
        assert(!agree(run(LamAbs("p", vr"p"))._1, run(LamAbs("p", probes.head))._1))
    }

    test("independent effectful bindings retain trace order and stop at the first failure") {
        for failureAt <- List(None, Some(0), Some(1), Some(4)) do
            val rhs = (0 until 5).map { i =>
                if failureAt.contains(i) then Error()
                else trace(s"rhs$i", probes.head)
            }.toList
            val chain = fiveLets(rhs, trace("body", probes.head))
            val regrouped = LetChainRegroup(chain)
            assert(chain ~!=~ regrouped)
            val expectedLogs = (0 until failureAt.getOrElse(5)).map(i => s"rhs$i").toList ++
                (if failureAt.isEmpty then List("body") else Nil)
            for encode <- List[Term => Term](identity, CaseConstrApply.apply) do
                val before = run(encode(chain))._1
                val after = run(encode(regrouped))._1
                before match
                    case Outcome.Ok(_, logs) =>
                        assert(failureAt.isEmpty)
                        assert(logs == expectedLogs)
                    case Outcome.Failed(_, logs) =>
                        assert(failureAt.nonEmpty)
                        assert(logs == expectedLogs)
                    case other => fail(s"unexpected outcome: $other")
                assert(agree(before, after), s"$before vs $after")
    }

    test("the oracle rejects an extra trace before failure") {
        val quiet = run(Error())._1
        val noisy = run(Force(trace("unexpected", Delay(Error()))))._1
        assert(!agree(quiet, noisy))
    }

    /** Guards the two properties above against vacuity: a generator that never produced a groupable
      * run would satisfy them trivially. Seeded, because the proportion varies a lot between runs.
      */
    test("the generator produces terms the pass actually regroups") {
        val terms = Gen.listOfN(200, genProgram).pureApply(Gen.Parameters.default, Seed(20260901))
        val changed = terms.count(t => t ~!=~ LetChainRegroup(t))
        assert(changed >= 20, s"only $changed/200 generated terms were regrouped")
    }

    /** A grouped chain sitting in the FUNCTION position of an enclosing `Apply` must not merge with
      * that application's own argument.
      *
      * `CaseConstrApply` flattens the whole left spine, so grouping the outermost run would put the
      * outer argument into the same `constr` as the chain's bindings — and `Case` evaluates every
      * field before entering the branch. That moves the outer argument ahead of the chain body,
      * which is observable: here the body fails, so the argument is never reached in the original
      * and diverges in the regrouped form.
      */
    test("a chain in function position does not swallow the outer argument") {
        val omega = Apply(
          LamAbs("x", Apply(vr"x", vr"x")),
          LamAbs("x", Apply(vr"x", vr"x"))
        )
        val chain = (1 to 5).foldRight(Error(): Term) { (i, body) =>
            Apply(LamAbs(s"v$i", body), Const(Constant.Integer(BigInt(i))))
        }
        val t = Apply(chain, omega)
        val (before, beforeBudget) = run(CaseConstrApply(t))
        val (after, afterBudget) = run(CaseConstrApply(LetChainRegroup(t)))
        assert(agree(before, after), s"$before vs $after")
        assert(afterBudget.fitsWithin(beforeBudget), s"$beforeBudget -> $afterBudget")
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
