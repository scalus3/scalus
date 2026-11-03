package scalus.uplc.jit

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.{ExUnits, Language, MajorProtocolVersion}
import scalus.compiler.compile
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.{platform, Builtins, Data}
import scalus.uplc.eval.*
import scalus.uplc.{BuiltinSemanticsVariant, Constant, DeBruijn, Term}

import scala.util.Try

/** [[UplcCompiler]] evaluators return what the CEK machine returns: the result term or the
  * exception, the budget per category and the logs; `withFallback` also past the call-depth limit
  * (spec [EV-3] to [EV-9]).
  */
class UplcCompilerTest extends AnyFunSuite {

    private given vm: PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)
    private val dijkstraVM: PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.dijkstraPV)
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

    /** `evaluator.run`, tracked as `track` does; its result is unit, as `run` returns nothing. */
    private def runOnly(evaluator: UplcEvaluator)(using PlutusVM): Run =
        track(budget) { (s, l) =>
            evaluator.run(s, l)
            Term.Const(Constant.Unit)
        }

    /** The last spender and log of `run` hold what the CEK machine spent and logged. */
    private def assertSameCost(run: Run, expected: Run): Unit = {
        assert(run.result.isSuccess, run)
        assert(run.spender.getSpentBudget == expected.spender.getSpentBudget)
        assert(run.spender.costs == expected.spender.costs)
        assert(run.log.getLogs.toSeq == expected.log.getLogs.toSeq)
    }

    test("run: a function result returns, with the budget of the CEK machine [EV-15, EV-16]") {
        val term = Term.parseUplc("[(lam x (lam y x)) (con integer 1)]").toOption.get
        val expected = cek(term)
        assert(expected.result.isSuccess) // not vacuous: the CEK machine evaluates it
        for evaluator <- List(
              UplcCompiler.compile(term, MaxDepth),
              UplcCompiler.compile(term, MaxDepth).withFallback()
            )
        do assertSameCost(runOnly(evaluator), expected)
    }

    test("run with withFallback past the call-depth limit: the CEK budget and logs [EV-17]") {
        val term = sumTo $ BigInt(MaxDepth + 100).asTerm
        val run = runOnly(UplcCompiler.compile(term, MaxDepth).withFallback())
        assertSameCost(run, cek(term))
        assert(run.spenders.size == 2 && run.logs.size == 2)
    }

    test("run: a failing script throws what evaluate throws") {
        val term = compile { (n: BigInt) =>
            Builtins.trace("before")(if n > 0 then throw new Exception("boom") else n)
        }.toUplc(true) $ BigInt(1).asTerm
        val run = runOnly(UplcCompiler.compile(term, MaxDepth).withFallback())
        val expected = cek(term)
        assert(run.result.isFailure)
        assert(JitCekDiff.sameFailure(expected.result.failed.get, run.result.failed.get))
        assert(run.spender.getSpentBudget == expected.spender.getSpentBudget)
    }

    test("run rejects a vm of another protocol version, before it spends [EV-14]") {
        val compiled = UplcCompiler.compile(Term.parseUplc("(lam x x)").toOption.get, MaxDepth)
        for evaluator <- List(compiled, compiled.withFallback()) do
            val run = runOnly(evaluator)(using dijkstraVM)
            assert(run.result.failed.toOption.exists(_.isInstanceOf[IllegalArgumentException]))
            assert(run.spenders.isEmpty && run.logs.isEmpty)
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
              // compiled for PV11: the code rejects case on Data, which this machine allows
              PlutusVM.makePlutusV3VM(MajorProtocolVersion.dijkstraPV),
              // a protocol version after Dijkstra may change the machine again
              PlutusVM.makePlutusV3VM(MajorProtocolVersion(13)),
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

    private val dijkstra = PlutusVM.makePlutusV3VM(MajorProtocolVersion.dijkstraPV)

    /** Selects the branch of tag 1, which returns the fields: `(con (list data) [I 7])` at PV12. */
    private val dataCase = Term
        .parseUplc("(case (con data (Constr 1 [I 7])) (con integer 0) (lam fs fs))")
        .toOption
        .get

    test("compiled for PV12, case on Data gives the result and budget of the PV12 CEK [SC-3]") {
        val compiled =
            UplcCompiler.compile(dataCase, MaxDepth, MajorProtocolVersion.dijkstraPV)
        assert(compiled.protocolVersion == MajorProtocolVersion.dijkstraPV)
        val expected = track(budget)((s, l) =>
            dijkstra.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(dataCase), s, l)
        )
        assert(expected.result.isSuccess) // not vacuous: the CEK machine of PV12 cases on Data
        assertSame(track(budget)((s, l) => compiled.evaluate(s, l)(using dijkstra)), expected)
        val fallback = compiled.withFallback()
        assertSame(track(budget)((s, l) => fallback.evaluate(s, l)(using dijkstra)), expected)
    }

    test("compiled for PV11 (the default), case on Data fails as on the PV11 CEK [SC-3]") {
        val compiled = UplcCompiler.compile(dataCase, MaxDepth)
        assert(compiled.protocolVersion == MajorProtocolVersion.vanRossemPV)
        val expected = cek(dataCase)
        assert(
          expected.result.failed.toOption.exists(_.isInstanceOf[CaseDataNotSupportedError]),
          expected
        )
        assertSame(jit(dataCase), expected)
    }

    test(
      "rejects a vm of another protocol version than the compile, before it runs [EV-2, EV-13]"
    ) {
        val forPV11 = UplcCompiler.compile(dataCase, MaxDepth)
        val forPV12 = UplcCompiler.compile(dataCase, MaxDepth, MajorProtocolVersion.dijkstraPV)
        for (compiled, other) <- List(forPV11 -> dijkstra, forPV12 -> vm) do
            for evaluator <- List(compiled, compiled.withFallback()) do
                val run = track(budget)((s, l) => evaluator.evaluate(s, l)(using other))
                val error = run.result.failed.get
                assert(error.isInstanceOf[IllegalArgumentException], error)
                assert(
                  error.getMessage.contains(
                    s"compiled for protocol version ${compiled.protocolVersion.version}"
                  ) && error.getMessage.contains(
                    s"protocol version ${other.protocolVersion.version}"
                  ),
                  error.getMessage
                )
                // no spender and no logger asked for: neither the JIT nor the CEK machine ran
                assert(run.spenders.isEmpty && run.logs.isEmpty)
    }

    test("compiles only for protocol versions 11 and 12 [SC-2, EV-12]") {
        for pv <- List(MajorProtocolVersion.changPV, MajorProtocolVersion(13)) do
            assertThrows[IllegalArgumentException](UplcCompiler.compile(dataCase, MaxDepth, pv))
    }

    // applyTo: one compile, evaluated with new arguments ([EV-18] to [EV-24])

    private def parse(uplc: String): Term = Term.parseUplc(uplc).toOption.get

    /** `[[term (con a1)] .. (con an)]`: the applied term, for the CEK machine. */
    private def applied(term: Term, args: Constant*): Term =
        args.foldLeft(term)((f, a) => Term.Apply(f, Term.Const(a)))

    /** `a + b - c`: the result tells if the arguments were applied in order. Its body makes no
      * counted call: a saturated builtin is a static call.
      */
    private val threeArgs = parse(
      "(lam a (lam b (lam c [[(builtin subtractInteger) [[(builtin addInteger) a] b]] c])))"
    )
    private val tenFiveThree = List(10, 5, 3).map(n => Constant.Integer(n))

    test("applyTo: one compile, a new argument per run, each as the CEK machine [EV-18, EV-19]") {
        val compiled = UplcCompiler.compile(sumTo, MaxDepth)
        for n <- List(0, 1, 100) do
            val arg = Constant.Integer(n)
            val run = track(budget)((s, l) => compiled.applyTo(arg).evaluate(s, l))
            assertSame(run, cek(applied(sumTo, arg)))
            assert(run.spenders.size == 1)
            // not vacuous: the result depends on the argument
            assert(run.result.toOption.contains(Term.Const(Constant.Integer(n * (n + 1) / 2))))
    }

    test("applyTo three arguments: the term applied to them in order [EV-18, EV-19]") {
        val compiled = UplcCompiler.compile(threeArgs, MaxDepth)
        val run = track(budget)((s, l) => compiled.applyTo(tenFiveThree*).evaluate(s, l))
        assertSame(run, cek(applied(threeArgs, tenFiveThree*)))
        assert(run.result.toOption.contains(Term.Const(Constant.Integer(12))))
    }

    test("applyTo on code compiled for PV12: the result and budget of the PV12 CEK [EV-19, SC-3]") {
        // case on Data: the PV12 machine selects the branch of tag 1, the PV11 machine fails
        val caseOnArg = parse("(lam d (case d (con integer 0) (lam fs fs)))")
        val Term.Const(arg, _) = parse("(con data (Constr 1 [I 7]))"): @unchecked
        val compiled = UplcCompiler.compile(caseOnArg, MaxDepth, MajorProtocolVersion.dijkstraPV)
        val expected = track(budget)((s, l) =>
            dijkstra.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(applied(caseOnArg, arg)), s, l)
        )
        assert(expected.result.isSuccess, expected) // not vacuous: the PV12 CEK cases on Data
        for evaluator <- List(compiled, compiled.withFallback()) do
            val run = track(budget)((s, l) => evaluator.applyTo(arg).evaluate(s, l)(using dijkstra))
            assertSame(run, expected)
    }

    test("applyTo(a).applyTo(b, c) evaluates as applyTo(a, b, c) [EV-21]") {
        val compiled = UplcCompiler.compile(threeArgs, MaxDepth)
        val List(a, b, c) = tenFiveThree: @unchecked
        val expected = cek(applied(threeArgs, a, b, c))
        for evaluator <- List(compiled, compiled.withFallback()) do
            val run = track(budget)((s, l) => evaluator.applyTo(a).applyTo(b, c).evaluate(s, l))
            assertSame(run, expected)
    }

    test("applyTo a validator: evaluateScript passes or fails per context [EV-18, EV-19]") {
        // passes for the context I 42 only
        val validator = parse(
          "(lam ctx (case [[(builtin equalsInteger) [(builtin unIData) ctx]] (con integer 42)]" +
              " (error) (con unit ())))"
        )
        val compiled = UplcCompiler.compile(validator, MaxDepth).withFallback()
        for n <- List(42, 7) do
            val ctx = Constant.Data(Data.I(BigInt(n)))
            val run = track(budget) { (s, l) =>
                compiled.applyTo(ctx).evaluateScript(s, l)
                Term.Const(Constant.Unit)
            }
            assertSame(run, cek(applied(validator, ctx)))
            assert(run.result.isSuccess == (n == 42))
    }

    test("applyTo a term that is not a function: the CEK failure and budget [EV-19]") {
        for term <- List("(con integer 1)", "(delay (con integer 1))", "(constr 0)").map(parse) do
            val arg = Constant.Integer(2)
            val run = track(budget) { (s, l) =>
                UplcCompiler.compile(term, MaxDepth).applyTo(arg).evaluate(s, l)
            }
            val expected = cek(applied(term, arg))
            assert(expected.result.isFailure, expected) // not vacuous: the CEK machine fails
            assertSame(run, expected)
    }

    test("applyTo a builtin: the CEK result and budget, saturated or not [EV-19]") {
        val compiled = UplcCompiler.compile(parse("(builtin addInteger)"), MaxDepth)
        val (two, three) = (Constant.Integer(2), Constant.Integer(3))
        val add = parse("(builtin addInteger)")
        val run = track(budget)((s, l) => compiled.applyTo(two, three).evaluate(s, l))
        assertSame(run, cek(applied(add, two, three)))
        assert(run.result.toOption.contains(Term.Const(Constant.Integer(5))))
        // one argument: a function result, which run accepts as the CEK machine does
        assertSameCost(runOnly(compiled.applyTo(two)), cek(applied(add, two)))
    }

    test("applyTo, out of budget in the body: the CEK exception at the same point [EV-19]") {
        val small = ExUnits(memory = 20_000, steps = 5_000_000)
        val arg = Constant.Integer(100)
        val compiled = UplcCompiler.compile(sumTo, MaxDepth)
        val run = track(small)((s, l) => compiled.applyTo(arg).evaluate(s, l))
        assert(run.result.isFailure)
        assertSame(run, cek(applied(sumTo, arg), small))
    }

    test(
      "applyTo, out of budget in the first n + 1 spends: same total, Apply spent first [EV-20]"
    ) {
        // 150 memory: the CEK machine spends Startup (100) and fails on Apply (100); the applied
        // evaluator spends Apply and fails on Startup. Both spend 200 memory and 16 100 CPU.
        val tight = ExUnits(memory = 150, steps = 1_000_000)
        val arg = Constant.Integer(1)
        val compiled = UplcCompiler.compile(sumTo, MaxDepth)
        val run = track(tight)((s, l) => compiled.applyTo(arg).evaluate(s, l))
        val expected = cek(applied(sumTo, arg), tight)
        for r <- List(run, expected) do
            assert(r.result.failed.toOption.exists(_.isInstanceOf[OutOfExBudgetError]), r)
        assert(run.spender.getSpentBudget == expected.spender.getSpentBudget)
        assert(expected.spender.costs.keySet == Set(ExBudgetCategory.Startup))
        assert(run.spender.costs.keySet == Set(ExBudgetCategory.Step(StepKind.Apply)))
    }

    test("withFallback, applied, past the call-depth limit: the CEK run of [term arg] [EV-23]") {
        val arg = Constant.Integer(MaxDepth + 100)
        val compiled = UplcCompiler.compile(sumTo, MaxDepth)
        assertThrows[nativestack.StackThresholdException](
          compiled.applyTo(arg).evaluate(NoBudgetSpender, NoLogger)
        )
        val evaluator = compiled.withFallback().applyTo(arg)
        val expected = cek(applied(sumTo, arg))
        val run = track(budget)((s, l) => evaluator.evaluate(s, l))
        assertSame(run, expected)
        assert(run.spenders.size == 2 && run.logs.size == 2)
        val runOnlyRun = runOnly(evaluator)
        assertSameCost(runOnlyRun, expected)
        assert(runOnlyRun.spenders.size == 2)
    }

    test("applyTo rejects a vm of another language or protocol version, before it spends [EV-22]") {
        val compiled = UplcCompiler.compile(sumTo, MaxDepth)
        for
            evaluator <- List(compiled, compiled.withFallback())
            other <- List(dijkstraVM, PlutusVM.makePlutusV2VM())
        do
            val appliedOne = evaluator.applyTo(Constant.Integer(1))
            val run = track(budget)((s, l) => appliedOne.evaluate(s, l)(using other))
            val runOnlyRun = runOnly(appliedOne)(using other)
            for r <- List(run, runOnlyRun) do
                assert(r.result.failed.toOption.exists(_.isInstanceOf[IllegalArgumentException]))
                assert(r.spenders.isEmpty && r.logs.isEmpty)
    }

    test("applyTo: the calls that apply the arguments are not depth-counted [EV-24]") {
        // maxDepth 1: the first counted call throws
        assertThrows[nativestack.StackThresholdException](
          UplcCompiler
              .compile(applied(threeArgs, tenFiveThree*), 1)
              .evaluate(NoBudgetSpender, NoLogger)
        )
        val result = UplcCompiler
            .compile(threeArgs, 1)
            .applyTo(tenFiveThree*)
            .evaluate(NoBudgetSpender, NoLogger)
        assert(result == Term.Const(Constant.Integer(12)))
    }
}
