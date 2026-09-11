package scalus.uplc.transform

import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.{ExUnits, Word64}
import scalus.uplc.*
import scalus.uplc.Term.*
import scalus.uplc.DefaultFun.*
import scalus.uplc.TermDSL.given
import scalus.uplc.transform.TermAnalysis.freeVars
import scalus.uplc.eval.{Log, MachineError, OutOfExBudgetError, PlutusVM, RestrictingBudgetSpender}

import scala.language.implicitConversions

class CseCekConformanceTest extends AnyFunSuite with ScalaCheckPropertyChecks {
    override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 200)

    private val vm = PlutusVM.makePlutusV3VM()
    private val limit = ExUnits(memory = 1_000_000L, steps = 500_000_000L)

    private enum Observation {
        case Value(term: Term)
        case Failed
        case OutOfBudget
    }

    private def observe(t: Term): Observation = {
        val spender = new RestrictingBudgetSpender(limit)
        try
            Observation.Value(
              vm.evaluateDeBruijnedTerm(
                DeBruijn.deBruijnTerm(t, throwOnFreeVariable = true),
                spender,
                new Log()
              )
            )
        catch
            case _: OutOfExBudgetError => Observation.OutOfBudget
            case _: MachineError       => Observation.Failed
    }

    private val probes: List[Term] = List(0, 1, true, Const(Constant.Unit))

    private def agree(before: Term, after: Term, depth: Int = 6): Unit = {
        (observe(before), observe(after)) match
            case (Observation.Value(a), Observation.Value(b)) =>
                (a, b) match
                    case (_: Delay, _: Delay) =>
                        assert(depth > 0, "delay observation depth exhausted")
                        agree(Force(a), Force(b), depth - 1)
                    case (_: LamAbs, _: LamAbs) =>
                        assert(depth > 0, "lambda observation depth exhausted")
                        probes.foreach(p => agree(Apply(a, p), Apply(b, p), depth - 1))
                    case (Constr(i, as, _), Constr(j, bs, _)) =>
                        assert(i == j && as.size == bs.size)
                        as.zip(bs).foreach((a, b) => agree(a, b, depth))
                    case _ => assert(a ~=~ b, s"${a.show} != ${b.show}")
            case (Observation.Failed, Observation.Failed) => ()
            // Budget exhaustion is inconclusive, not a semantic mismatch or evidence of equality.
            // These generators are bounded and nonrecursive; exhaustion fails their coverage gate.
            case (a, b) => fail(s"${before.show}\n${after.show}\n$a != $b")
    }

    private def encodedBits(t: Term): Int = {
        val closed = t.freeVars.toList.sorted.foldRight(t)((n, body) => LamAbs(n, body))
        val db = DeBruijn.deBruijnTerm(closed, true)
        val flat = summon[scalus.serialization.flat.Flat[Term]]
        val encoder = new scalus.serialization.flat.EncoderState((flat.bitSize(db) + 7) / 8)
        flat.encode(db, encoder)
        encoder.bitPosition()
    }

    private def check(t: Term): Term = {
        val cse = new CommonSubexpressionElimination()
        val result = cse(t)
        assert(
          CseTranslationValidation.validate(t, result),
          s"Invalid CSE:\n${t.show}\n${result.show}"
        )
        assert(cse.logs.nonEmpty == (t ~!=~ result))
        assert(
          CommonSubexpressionElimination.termBits(result) <= CommonSubexpressionElimination
              .termBits(t)
        )
        if cse.logs.nonEmpty then
            assert(
              CommonSubexpressionElimination.termBits(result) < CommonSubexpressionElimination
                  .termBits(t)
            )
        assert(CommonSubexpressionElimination(result) ~=~ result, "CSE did not reach a fixed point")
        DeBruijn.deBruijnTerm(result, throwOnFreeVariable = true)
        agree(t, result)
        result
    }

    test(
      "candidate selection shares larger terms and rejects tiny duplicates"
    ) {
        val fields = Constr(Word64.Zero, List[Term](1, 2))
        val constructors = Constr(Word64.Zero, List(fields, fields))
        assert(check(constructors) ~!=~ constructors)
        val guarded = Case(Constr(Word64.Zero, Nil), List[Term](42, Error()))
        val branches = Constr(Word64.Zero, List(guarded, guarded, guarded))
        assert(check(branches) ~!=~ branches)
        val errors = Constr(Word64.Zero, List[Term](Error(), Error()))
        assert(check(errors) ~=~ errors)
        val forced = Force(Builtin(HeadList))
        val workFree = Constr(Word64.Zero, List(forced, forced))
        assert(check(workFree) ~=~ workFree)
    }

    test("division stays in the two branches that evaluate it") {
        val div = DivideInteger $ vr"a" $ vr"b"
        val body = Case(Constr(Word64(2), Nil), List(div, div, 42))
        val t = LamAbs("a", LamAbs("b", body)) $ 1 $ 0
        assert(observe(t) == Observation.Value(42))
        check(t)
    }

    test("CSE and inliner agree on both sides of the constant-sharing threshold") {
        val value = Const(Constant.Data(scalus.uplc.builtin.Data.I(0)))
        for n <- List(2, 3, 20) do
            val original = Constr(Word64.Zero, List.fill(n)(value))
            val shared = check(original)
            assert((shared ~!=~ original) == (n >= 3))
            assert(Inliner(shared) ~=~ shared)
    }

    test("forced builtins use the same profitability estimate as other terms") {
        for (builtin, minimumUses) <- List(
              (Force(Builtin(HeadList)), 12),
              (Force(Force(Builtin(FstPair))), 6)
            )
        do
            val tooFew = Constr(Word64.Zero, List.fill(minimumUses - 1)(builtin))
            assert(check(tooFew) ~=~ tooFew)
            val enough = Constr(Word64.Zero, List.fill(minimumUses)(builtin))
            assert(check(enough) ~!=~ enough)
    }

    test("size estimates may accept growth when variable indices widen") {
        val forced = Force(Builtin(HeadList))
        val body = Constr(Word64.Zero, List.fill(12)(forced) ++ List.fill(3)(vr"x0"))
        val shallow = LamAbs("x0", body)
        assert(CommonSubexpressionElimination(shallow) ~!=~ shallow)
        val deep = (0 until 127).foldRight[Term](body)((i, t) => LamAbs(s"x$i", t))
        val result = CommonSubexpressionElimination(deep)
        assert(result ~!=~ deep)
        assert(encodedBits(result) > encodedBits(deep))
        assert(CseTranslationValidation.validate(deep, result))
    }

    test("the greatest bit saving wins over the largest AST") {
        val large = Const(
          Constant.ByteString(scalus.uplc.builtin.ByteString.fromArray(Array.fill[Byte](128)(1)))
        )
        val call = AddInteger $ 1 $ 2
        val term = Constr(Word64.Zero, List(large, large, call, call))
        val cse = new CommonSubexpressionElimination()
        cse(term)
        assert(cse.logs.head.endsWith(s"as __cse_${TermNaming.termDescription(large)}"))
        check(term)
    }

    test("size-only sharing can trade execution budget for a smaller constant representation") {
        val large = Const(
          Constant.ByteString(scalus.uplc.builtin.ByteString.fromArray(Array.fill[Byte](64)(1)))
        )
        val term = Constr(Word64.Zero, List(large, large))
        val shared = check(term)
        def spent(t: Term): ExUnits = {
            val spender = new RestrictingBudgetSpender(limit)
            vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(t, true), spender, new Log())
            spender.getSpentBudget
        }
        assert(encodedBits(shared) < encodedBits(term))
        assert(spent(shared).memory > spent(term).memory)
        assert(spent(shared).steps > spent(term).steps)
        val smallCall = vr"f" $ vr"x"
        val calls = LamAbs("f", LamAbs("x", Constr(Word64.Zero, List(smallCall, smallCall))))
        assert(check(calls) ~=~ calls)
    }

    test("IfThenElse shares identical delays without forcing their bodies early") {
        val div = DivideInteger $ 1 $ 0
        val delayed = Delay(div)
        val choice = Force(Builtin(IfThenElse)) $ true $ delayed $ delayed
        val result = check(choice)
        assert(result ~!=~ choice)
        assert(observe(choice).isInstanceOf[Observation.Value])
        // The shared value is still a delay: only forcing the selected result fails.
        assert(observe(Force(result)) == Observation.Failed)
        val distinct = Force(Force(Builtin(IfThenElse)) $ true $ Delay(42) $ delayed)
        assert(observe(check(distinct)) == Observation.Value(42))
    }

    test("the default V3 pipeline shares local work and leaves other branches guarded") {
        val div = DivideInteger $ 20 $ vr"d"
        val branch = AddInteger $ div $ div
        val term = LamAbs("s", LamAbs("d", Case(vr"s", List(branch, branch, 42))))
        val optimizer = new V3Optimizer()
        val optimized = optimizer(term)
        assert(optimizer.logs.exists(_.startsWith("CSE:")), "pipeline must exercise CSE")
        for (tag, divisor) <- List((2, 0), (0, 2), (1, 0)) do
            val scrutinee = Constr(Word64(tag), Nil)
            agree(term $ scrutinee $ divisor, optimized $ scrutinee $ divisor)
    }

    test("unknown helper names cannot bypass branch placement") {
        for name <- List("helper", "__helper", "__cce_helper", "__cse_helper") do
            val call = Var(NamedDeBruijn(name)) $ 0
            val body = Case(Constr(Word64(2), Nil), List(call, call, 42))
            check(LamAbs(name, body) $ LamAbs("x", DivideInteger $ 1 $ vr"x"))
    }

    test("placement does not assume variables passed to total builtins have the right type") {
        val add = AddInteger $ vr"x" $ 1
        val body = Case(Constr(Word64(2), Nil), List(add, add, 42))
        check(LamAbs("x", body) $ true)
    }

    test("the CEK budget bounds divergence and unused divergent branches stay deferred") {
        val self = LamAbs("x", vr"x" $ vr"x")
        val omega = self $ self
        assert(observe(omega) == Observation.OutOfBudget)
        assert(!CseTranslationValidation.validate(42, omega), "oracle must reject a cyclic proof")
        assert(CseTranslationValidation.validate(omega, CommonSubexpressionElimination(omega)))
        val force = Force(vr"d")
        val body = Case(Constr(Word64(2), Nil), List(force, force, 42))
        check(LamAbs("d", body) $ Delay(omega))
    }

    test("duplicates in unforced delays and unapplied lambdas stay deferred") {
        val div = DivideInteger $ 1 $ 0
        check(Constr(Word64.Zero, List(Delay(div), Delay(div))))
        check(Constr(Word64.Zero, List(LamAbs("x", div), LamAbs("y", div))))
    }

    test("local duplicates still share inside separate branches") {
        val div = DivideInteger $ 20 $ 2
        val branch = AddInteger $ div $ div
        val t = Case(Constr(Word64.Zero, Nil), List(branch, branch))
        val result = check(t)
        assert(result ~!=~ t)
        assert(result.isInstanceOf[Case])
    }

    test("an ancestor occurrence can share with a deferred occurrence") {
        val div = DivideInteger $ 20 $ 2
        val t = Constr(Word64.Zero, List(div, Delay(div)))
        assert(check(t) ~!=~ t)
    }

    test("sharing a trace preserves its result but may reduce the number of logs") {
        val trace = Force(Builtin(Trace)) $ "shared" $ 42
        val t = Constr(Word64.Zero, List(trace, trace))
        val result = check(t)
        def logs(term: Term): List[String] = {
            val logger = new Log()
            vm.evaluateDeBruijnedTerm(
              DeBruijn.deBruijnTerm(term, throwOnFreeVariable = true),
              new RestrictingBudgetSpender(limit),
              logger
            )
            logger.getLogs.toList
        }
        assert(logs(t) == List("shared", "shared"))
        assert(logs(result) == List("shared"))
    }

    test("a scrutinee occurrence anchors sharing into its Case branches") {
        val value = Constr(Word64.Zero, List(AddInteger $ 1 $ 2))
        val t = Case(value, List(LamAbs("field", Constr(Word64.Zero, List(value, vr"field")))))
        assert(check(t) ~!=~ t)
    }

    test("a late ancestor absorbs occurrences collected in earlier descendant regions") {
        val div = DivideInteger $ 20 $ 2
        val t = Constr(Word64.Zero, List(Delay(div), LamAbs("x", div), div))
        val result = check(t)
        assert(result ~!=~ t)
        assert(CommonSubexpressionElimination(result) ~=~ result)
    }

    test("partial builtin sharing still reduces execution budget") {
        val div = DivideInteger $ 20 $ 2
        val t = Constr(Word64.Zero, List(div, div, div))
        val result = check(t)
        def spent(term: Term): ExUnits = {
            val spender = new RestrictingBudgetSpender(limit)
            vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(term, true), spender, new Log())
            spender.getSpentBudget
        }
        val before = spent(t)
        val after = spent(result)
        assert(after.memory < before.memory && after.steps < before.steps)
    }

    test("immediately applied lambdas share without escaping their binders") {
        val add = AddInteger $ vr"x" $ 1
        val t = LamAbs("x", Constr(Word64.Zero, List(add, add))) $ 3
        val result = check(t)
        result match
            case Apply(LamAbs("x", Apply(LamAbs(_, _, _), rhs, _), _), _, _) =>
                assert(rhs ~=~ add)
            case _ => fail(s"binding escaped x: ${result.show}")
    }

    test("shadowed variables and generated-name collisions retain their binding") {
        val add = AddInteger $ vr"x" $ 1
        val t = LamAbs(
          "x",
          Constr(
            Word64.Zero,
            List(
              add,
              LamAbs("x", Constr(Word64.Zero, List(add, add))) $ 9,
              add
            )
          )
        ) $ 3
        check(t)
        check(LamAbs("__cse_AddInteger", Constr(Word64.Zero, List(t, t))) $ 0)
    }

    test("the translation oracle rejects speculation, capture and an incorrect RHS") {
        val e = DivideInteger $ 1 $ 0
        val before = Constr(Word64.Zero, List(Delay(e), Delay(e)))
        val bad = LamAbs("c", Constr(Word64.Zero, List(Delay(vr"c"), Delay(vr"c")))) $ e
        assert(!CseTranslationValidation.validate(before, bad))
        assert(!CseTranslationValidation.validate(LamAbs("x", vr"x"), LamAbs("x", vr"free")))
        assert(
          !CseTranslationValidation.validate(
            LamAbs("x", LamAbs("y", vr"x")),
            LamAbs("x", LamAbs("y", vr"y"))
          )
        )
        val good = Constr(Word64.Zero, List(AddInteger $ 1 $ 2, AddInteger $ 1 $ 2))
        val wrong = LamAbs("c", Constr(Word64.Zero, List(vr"c", vr"c"))) $ (AddInteger $ 1 $ 3)
        assert(!CseTranslationValidation.validate(good, wrong))
        assert(CseTranslationValidation.validate(good, CommonSubexpressionElimination(good)))
    }

    private val names = List("a", "b", "c")
    private def genTerm(env: List[String], size: Int): Gen[Term] = {
        val constant: Gen[Term] = Gen.choose(-3, 5).map(n => Const(Constant.Integer(n)))
        val leaf =
            if env.isEmpty then constant
            else Gen.frequency(3 -> constant, 1 -> Gen.oneOf(env).map(n => Var(NamedDeBruijn(n))))
        if size <= 0 then leaf
        else {
            val child = genTerm(env, size / 2)
            Gen.frequency(
              3 -> leaf,
              3 -> (for
                  a <- child
                  b <- child
                  op <- Gen.oneOf(AddInteger, MultiplyInteger, DivideInteger)
              yield Builtin(op) $ a $ b),
              4 -> child.map(e => Constr(Word64.Zero, List(e, e))),
              2 -> (for
                  name <- Gen.oneOf(names)
                  rhs <- child
                  body <- genTerm(name :: env, size / 2)
              yield LamAbs(name, body) $ rhs),
              2 -> (for
                  n <- Gen.choose(0, 4 min size)
                  tag <- Gen.choose(0, n)
                  fields <- Gen.choose(0, 4 min size).flatMap(Gen.listOfN(_, child))
                  branches <- Gen.listOfN(n, child)
              yield Case(Constr(Word64(tag), fields), branches)),
              1 -> child.map(Delay(_)),
              1 -> child.map(e => Force(Delay(e))),
              1 -> child.map(e => Force(Builtin(Trace)) $ "generated" $ e),
              1 -> (for
                  name <- Gen.oneOf(names)
                  body <- genTerm(name :: env, size / 2)
              yield LamAbs(name, body)),
              1 -> Gen.const(Error(): Term)
            )
        }
    }

    private val genProgram = genTerm(Nil, 8)

    test("rich closed terms satisfy translation validation and bounded CEK observations") {
        forAll(genProgram)(check(_))
    }

    test("open terms preserve free variables and satisfy translation validation") {
        forAll(genTerm(names, 8)) { t =>
            val result = CommonSubexpressionElimination(t)
            assert(result.freeVars == t.freeVars)
            assert(CseTranslationValidation.validate(t, result))
        }
    }

    test("the generator exercises successful, failing and rewritten programs") {
        val terms = Gen.listOfN(200, genProgram).pureApply(Gen.Parameters.default, Seed(20260910))
        val changed = terms.count(t => t ~!=~ CommonSubexpressionElimination(t))
        val values = terms.count(t => observe(t).isInstanceOf[Observation.Value])
        val failures = terms.count(t => observe(t) == Observation.Failed)
        assert(changed >= 20, s"only $changed/200 rewrites")
        assert(values >= 20 && failures >= 20, s"$values values, $failures failures")
        assert(values + failures == terms.size, "budget exhaustion in generator")
    }
}
