package scalus.uplc.jit

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.{ExUnits, Language, MajorProtocolVersion}
import scalus.compiler.compile
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.{platform, Builtins}
import scalus.uplc.eval.*
import scalus.uplc.{BuiltinSemanticsVariant, Constant, DeBruijn, Term}

import scala.util.Try

/** [[UplcCompiler]] evaluators return what the CEK machine returns: the result term or the
  * exception, the budget per category and the logs; `withFallback` also past the call-depth limit
  * (spec [EV-3] to [EV-9]).
  */
class UplcCompilerTest extends AnyFunSuite {

    private given vm: PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)
    private val budget = ExUnits(memory = 10_000_000_000L, steps = 10_000_000_000_000L)

    /** `sumToN(n)` by plain recursion, which traces "start" first: one level of UPLC calls per
      * element.
      */
    private val sumTo: Term = compile { (n: BigInt) =>
        def sumToN(n: BigInt): BigInt =
            if n <= 0 then 0
            else n + sumToN(n - 1)
        sumToN(Builtins.trace("start")(n))
    }.toUplc(true)

    /** One evaluation: its result, and every spender and logger it asked for, in order. */
    private final case class Run(
        result: Try[Term],
        spenders: List[TallyingBudgetSpender],
        logs: List[Log]
    ) {
        def spender: TallyingBudgetSpender = spenders.last
        def log: Log = logs.last
    }

    /** Evaluates `term` with `run`, giving it a new spender and a new log each time it asks. */
    private def track(limit: ExUnits)(run: (=> BudgetSpender, => Logger) => Term): Run = {
        var spenders = List.empty[TallyingBudgetSpender]
        var logs = List.empty[Log]
        def newSpender = {
            val s = TallyingBudgetSpender(RestrictingBudgetSpender(limit))
            spenders :+= s
            s
        }
        def newLog = {
            val l = Log()
            logs :+= l
            l
        }
        val result = Try(run(newSpender, newLog))
        Run(result, spenders, logs)
    }

    /** A low depth limit, so the tests reach it with shallow scripts. */
    private val MaxDepth = 1000

    private def jit(term: Term, limit: ExUnits = budget): Run =
        val evaluator = UplcCompiler.compile(term, MaxDepth).withFallback()
        track(limit)((s, l) => evaluator.evaluate(s, l))

    private def cek(term: Term, limit: ExUnits = budget): Run =
        track(limit)((s, l) => vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(term), s, l))

    /** The last spender and log of `jit` hold exactly what the CEK machine spent and logged. */
    private def assertSame(jit: Run, expected: Run): Unit = {
        assert(jit.result.isSuccess == expected.result.isSuccess, s"JIT: $jit\nCEK: $expected")
        assert(jit.result.toOption == expected.result.toOption)
        (jit.result.failed.toOption zip expected.result.failed.toOption).foreach { (j, c) =>
            assert(JitCekDiff.sameFailure(c, j), s"JIT: $j\nCEK: $c")
        }
        assert(jit.spender.getSpentBudget == expected.spender.getSpentBudget)
        assert(jit.spender.costs == expected.spender.costs)
        assert(jit.log.getLogs.toSeq == expected.log.getLogs.toSeq)
    }

    test("a shallow script: one run, with the CEK result, budget, costs and logs") {
        val term = sumTo $ BigInt(100).asTerm
        val run = jit(term)
        assertSame(run, cek(term))
        assert(run.spenders.size == 1 && run.logs.size == 1)
        assert(run.result.toOption.contains(Term.Const(Constant.Integer(5050))))
    }

    test("Compiled alone: past the call-depth limit it throws StackThresholdException") {
        val compiled = UplcCompiler.compile(sumTo $ BigInt(MaxDepth + 100).asTerm, MaxDepth)
        assertThrows[nativestack.StackThresholdException](
          compiled.evaluate(NoBudgetSpender, NoLogger)
        )
        // below the limit it evaluates
        val shallow = UplcCompiler.compile(sumTo $ BigInt(MaxDepth / 2).asTerm, MaxDepth)
        assert(
          shallow.evaluate(NoBudgetSpender, NoLogger) == cek(
            sumTo $ BigInt(MaxDepth / 2).asTerm
          ).result.get
        )
    }

    test(
      "a loop through case alone is depth-counted: StackThresholdException, not a JVM overflow"
    ) {
        // the loop applies each branch to its field; only those calls recurse
        val loop = Term
            .parseUplc("[(lam s (case (constr 0 s) s)) (lam s (case (constr 0 s) s))]")
            .toOption
            .get
        val compiled = UplcCompiler.compile(loop, MaxDepth)
        assertThrows[nativestack.StackThresholdException](
          compiled.evaluate(NoBudgetSpender, NoLogger)
        )
    }

    test("withFallback past the call-depth limit: a second spender and log for the CEK run") {
        val term = sumTo $ BigInt(MaxDepth + 100).asTerm
        val run = jit(term)
        assertSame(run, cek(term))
        assert(run.spenders.size == 2 && run.logs.size == 2)
        // the compiled code spent and traced before it stopped; none of it reaches the result
        assert(run.spenders.head.getSpentBudget.steps > 0)
        assert(run.logs.head.getLogs.toSeq == Seq("start"))
        assert(run.log.getLogs.toSeq == Seq("start"))
    }

    test("out of budget: the CEK exception at the same point") {
        val term = sumTo $ BigInt(100).asTerm
        val small = ExUnits(memory = 20_000, steps = 5_000_000)
        val run = jit(term, small)
        assert(run.result.isFailure)
        assertSame(run, cek(term, small))
    }

    test("a failing script: the CEK exception, budget and logs") {
        val term = compile { (n: BigInt) =>
            Builtins.trace("before")(if n > 0 then throw new Exception("boom") else n)
        }.toUplc(true) $ BigInt(1).asTerm
        val run = jit(term)
        assert(run.result.isFailure)
        assertSame(run, cek(term))
    }

    test("a constr result is the CEK term") {
        val term = Term.parseUplc("(constr 1 (con integer 2) (constr 0))").toOption.get
        assertSame(jit(term), cek(term))
    }

    test("a function result: FunctionResultError from evaluate, no CEK run [EV-8]") {
        val term = Term.parseUplc("[(lam x (lam y x)) (con integer 1)]").toOption.get
        val run = jit(term)
        assert(run.result.failed.toOption.exists(_.isInstanceOf[FunctionResultError]))
        assert(run.spenders.size == 1)
    }

    test("a function result: InvalidReturnValue from evaluateScript, as on the CEK machine") {
        val compiled = UplcCompiler.compile(
          Term.parseUplc("(lam x x)").toOption.get,
          MaxDepth
        )
        assertThrows[InvalidReturnValue](compiled.evaluateScript(NoBudgetSpender, NoLogger))
    }

    test("one Compiled evaluated from 8 threads at once gives the CEK result every time [CT-2]") {
        val term = sumTo $ BigInt(50).asTerm
        val compiled = UplcCompiler.compile(term, MaxDepth)
        val expected = cek(term)
        val pool = java.util.concurrent.Executors.newFixedThreadPool(8)
        try
            val runs = (1 to 400).map { _ =>
                pool.submit(() => track(budget)((s, l) => compiled.evaluate(s, l)))
            }
            runs.foreach(r => assertSame(r.get(), expected))
        finally pool.shutdown()
    }

    test("evaluateScript: a non-unit result of a V3 script throws InvalidReturnValue") {
        val compiled = UplcCompiler.compile(sumTo $ BigInt(3).asTerm, MaxDepth)
        assertThrows[InvalidReturnValue](compiled.evaluateScript(NoBudgetSpender, NoLogger))
    }

    test("evaluateScript: a unit result passes") {
        val compiled = UplcCompiler.compile(Term.parseUplc("(con unit ())").toOption.get, MaxDepth)
        compiled.evaluateScript(NoBudgetSpender, NoLogger)
        compiled.withFallback().evaluateScript(NoBudgetSpender, NoLogger)
    }

    test(
      "rejects a machine of another language, semantics variant or protocol version [EV-1, EV-2]"
    ) {
        val compiled = UplcCompiler.compile(Term.parseUplc("(con unit ())").toOption.get, MaxDepth)
        for other <- List(
              PlutusVM.makePlutusV2VM(),
              PlutusVM.makePlutusV3VM(MajorProtocolVersion.changPV),
              PlutusVM.makePlutusV3VM(MajorProtocolVersion.dijkstraPV),
              // variant E at PV11, but a PlutusV1 machine: its scripts may return a function
              PlutusVM(
                Language.PlutusV1,
                vm.machineParams,
                BuiltinSemanticsVariant.E,
                platform,
                MajorProtocolVersion.vanRossemPV
              )
            )
        do
            assertThrows[IllegalArgumentException](
              compiled.evaluate(NoBudgetSpender, NoLogger)(using other)
            )
    }
}
