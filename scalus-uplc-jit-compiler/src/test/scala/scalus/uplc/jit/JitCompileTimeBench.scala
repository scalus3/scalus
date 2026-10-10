package scalus.uplc.jit

import scalus.cardano.ledger.{ExUnits, MajorProtocolVersion}
import scalus.uplc.eval.*
import scalus.uplc.jit.nativestack.StackTresholdException
import scalus.uplc.{DeBruijn, DeBruijnedProgram, Term}

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{ExecutionException, ExecutorService, Executors, Future, TimeUnit, TimeoutException}
import scala.collection.mutable.ListBuffer
import scala.quoted.staging

/** Measures the compile cost of the Hybrid (NativeStack + CEK fallback) UPLC JIT on real scripts.
  *
  * Splits compilation into codegen (UPLC -> quoted `Expr`, timed inside `staging.run`) and staging
  * (the rest of `staging.run`: pickling, scalac, bytecode, classloading). Also times the first and
  * the steady-state evaluation, for both the JIT and the CEK machine.
  *
  * Usage: `JitCompileTimeBench <dir with .flat files> <output csv> [hanging script name...]`. The
  * scripts named after the csv run last, because a compile that times out cannot be stopped and
  * keeps a core busy.
  */
object JitCompileTimeBench {

    private val CompileTimeoutMs = 120_000L
    private val EvalTimeoutMs = 300_000L
    private val StackSize = 64L << 20
    // The JIT-generated code needs 5-20 s of C2 time to reach steady state (auction_1-1: 385 us
    // after 1 s of warm-up, 103 us after 5 s, 17 us after 20 s). JMH in the benchmark warms up 5 s.
    private val JitWarmupNanos = 20_000_000_000L
    private val CekWarmupNanos = 5_000_000_000L
    private val EarlyNanos = 1_000_000_000L
    private val MinWarmupRuns = 50
    private val MeasuredRuns = 200

    private val jitParams = MachineParams.defaultPlutusV2PostConwayParams
    private val cekVm = PlutusVM.makePlutusV1VM(MajorProtocolVersion.vanRossemPV)

    private type JitFn = (Logger, BudgetSpender, MachineParams) => Any

    private final case class Compiled(fn: JitFn, codegenNs: Long, totalNs: Long)

    private enum Outcome[+A]:
        case Done(value: A)
        case Failed(error: Throwable)
        case TimedOut

    /** Runs `body` on a fresh thread with a 64 MB stack, like `-Xss64m` in the benchmark. */
    private def onThread[A](name: String, timeoutMs: Long)(body: => A): Outcome[A] = {
        @volatile var result: Outcome[A] = Outcome.TimedOut
        val task: Runnable = () => {
            result =
                try Outcome.Done(body)
                catch case e: Throwable => Outcome.Failed(e)
        }
        val thread = new Thread(null, task, name, StackSize)
        thread.setDaemon(true)
        thread.start()
        thread.join(timeoutMs)
        if thread.isAlive then Outcome.TimedOut else result
    }

    /** A staging compiler with its own thread.
      *
      * A staging compiler may only be used from the thread that first ran it. It also keeps every
      * symbol of every run alive (25 M `SymDenotation`s after ~50 compiles, a full 4 GB heap), so
      * each script gets a fresh compiler. `submit` shuts the thread down after its one task, and
      * the compiler becomes garbage when the task ends, even after a timeout.
      */
    private final class CompileWorker {
        private val executor: ExecutorService = Executors.newSingleThreadExecutor { r =>
            val t = new Thread(null, r, "jit-compile", StackSize)
            t.setDaemon(true)
            t
        }
        private given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

        def compile(term: Term): Compiled = {
            var codegenNs = 0L
            val start = System.nanoTime()
            val fn = staging.run {
                val t0 = System.nanoTime()
                val expr = nativestack.JIT.genCodeFromTerm(term)
                codegenNs = System.nanoTime() - t0
                expr
            }
            Compiled(fn, codegenNs, System.nanoTime() - start)
        }

        /** Runs `terms` one after another on this compiler, then stops the thread. */
        def submit(terms: List[Term]): Future[List[Compiled]] = {
            val future = executor.submit(() => terms.map(compile))
            executor.shutdown()
            future
        }
    }

    /** Compiles that exceeded the timeout, still running: script name and future. */
    private val stragglers = ListBuffer.empty[(String, Future[List[Compiled]])]

    private def compileWithTimeout(name: String, term: Term): Outcome[Compiled] = {
        val future = CompileWorker().submit(List(term))
        try Outcome.Done(future.get(CompileTimeoutMs, TimeUnit.MILLISECONDS).head)
        catch
            case _: TimeoutException =>
                stragglers += name -> future
                Outcome.TimedOut
            case e: ExecutionException => Outcome.Failed(e.getCause)
    }

    private def termNodes(term: Term): Long = term match
        case Term.LamAbs(_, body, _)  => 1 + termNodes(body)
        case Term.Apply(f, arg, _)    => 1 + termNodes(f) + termNodes(arg)
        case Term.Force(t, _)         => 1 + termNodes(t)
        case Term.Delay(t, _)         => 1 + termNodes(t)
        case Term.Constr(_, args, _)  => 1 + args.map(termNodes).sum
        case Term.Case(arg, cases, _) => 1 + termNodes(arg) + cases.map(termNodes).sum
        case _                        => 1

    /** The evaluation wrapper of [[hybrid.HybridJIT]], around an already compiled function. */
    private final class HybridEval(fn: JitFn, term: Term) {
        private val vm = PlutusVM.makePlutusV3VM()
        private val djTerm = DeBruijn.deBruijnTerm(term, true)
        @volatile var fellBack = false

        def apply(): Any =
            try fn(NoLogger, NoBudgetSpender, jitParams)
            catch
                case _: StackTresholdException =>
                    fellBack = true
                    vm.evaluateDeBruijnedTerm(djTerm, NoBudgetSpender, NoLogger)
    }

    private final case class EvalTimes(firstNs: Long, earlyMedianNs: Long, medianNs: Long)

    private def medianOf(run: () => Any): Long = {
        val samples = Array.fill(MeasuredRuns) {
            val s = System.nanoTime()
            run()
            System.nanoTime() - s
        }
        java.util.Arrays.sort(samples)
        samples(MeasuredRuns / 2)
    }

    private def warmUp(run: () => Any, untilNanos: Long, start: Long): Unit = {
        var runs = 0
        while runs < MinWarmupRuns || System.nanoTime() - start < untilNanos do
            run()
            runs += 1
    }

    /** First call, median after 1 s of warm-up, median after `warmupNanos` of warm-up. */
    private def timeEval(run: () => Any, warmupNanos: Long): EvalTimes = {
        val t0 = System.nanoTime()
        run()
        val firstNs = System.nanoTime() - t0
        val start = System.nanoTime()
        warmUp(run, EarlyNanos, start)
        val early = medianOf(run)
        warmUp(run, warmupNanos, start)
        EvalTimes(firstNs, early, medianOf(run))
    }

    private def ms(ns: Long): String = f"${ns / 1e6}%.3f"
    private def us(ns: Long): String = f"${ns / 1e3}%.3f"

    private def oneLine(e: Throwable): String =
        s"${e.getClass.getSimpleName}: ${String.valueOf(e.getMessage)}"
            .replaceAll("[\\r\\n,\"]+", " ")
            .take(200)

    private val header = List(
      "script",
      "flat_bytes",
      "term_nodes",
      "status",
      "codegen_ms",
      "staging_ms",
      "compile_ms",
      "jit_first_eval_us",
      "jit_eval_after_1s_us",
      "jit_steady_eval_us",
      "jit_fallback_to_cek",
      "cek_first_eval_us",
      "cek_steady_eval_us",
      "error"
    ).mkString(",")

    private def measure(file: Path): String = {
        val name = file.getFileName.toString.stripSuffix(".flat")
        val bytes = Files.readAllBytes(file)
        val program = DeBruijnedProgram.fromFlatEncoded(bytes)
        val term = program.toProgram.term
        val nodes = onThread(s"size-$name", EvalTimeoutMs)(termNodes(term)) match
            case Outcome.Done(n) => n.toString
            case _               => ""

        val cek = onThread(s"cek-$name", EvalTimeoutMs) {
            timeEval(
              () =>
                  cekVm.evaluateScript(
                    program,
                    RestrictingBudgetSpender(ExUnits.enormous),
                    NoLogger
                  ),
              CekWarmupNanos
            )
        }
        val (cekFirst, cekSteady, cekError) = cek match
            case Outcome.Done(t)   => (us(t.firstNs), us(t.medianNs), "")
            case Outcome.Failed(e) => ("", "", s"cek: ${oneLine(e)}")
            case Outcome.TimedOut  => ("", "", "cek: timeout")

        System.err.println(s"[$name] compiling (${bytes.length} bytes, $nodes nodes)")
        val compiled = compileWithTimeout(name, term)
        val (status, compileCols, evalCols, error) = compiled match
            case Outcome.TimedOut =>
                ("timeout", List("", "", ""), List("", "", "", ""), "compile exceeded 120 s")
            case Outcome.Failed(e) =>
                ("compile_error", List("", "", ""), List("", "", "", ""), oneLine(e))
            case Outcome.Done(c) =>
                val cols = List(ms(c.codegenNs), ms(c.totalNs - c.codegenNs), ms(c.totalNs))
                val hybrid = HybridEval(c.fn, term)
                onThread(s"jit-$name", EvalTimeoutMs)(
                  timeEval(() => hybrid(), JitWarmupNanos)
                ) match
                    case Outcome.Done(t) =>
                        val ev = List(
                          us(t.firstNs),
                          us(t.earlyMedianNs),
                          us(t.medianNs),
                          hybrid.fellBack.toString
                        )
                        ("ok", cols, ev, "")
                    case Outcome.Failed(e) =>
                        ("eval_error", cols, List("", "", "", ""), oneLine(e))
                    case Outcome.TimedOut =>
                        ("eval_timeout", cols, List("", "", "", ""), "jit eval exceeded 120 s")
        System.err.println(s"[$name] $status compile=${compileCols(2)} ms $error")
        val err = List(error, cekError).filter(_.nonEmpty).mkString("; ")
        (List(name, bytes.length.toString, nodes, status) ++ compileCols ++ evalCols ++
            List(cekFirst, cekSteady, err)).mkString(",")
    }

    private def ofFile(file: Path): Term =
        DeBruijnedProgram.fromFlatEncoded(Files.readAllBytes(file)).toProgram.term

    /** Total compile times of `terms`, run in order on one compiler. */
    private def compileAll(terms: List[Term]): List[Long] =
        CompileWorker()
            .submit(terms)
            .get(CompileTimeoutMs * terms.size, TimeUnit.MILLISECONDS)
            .map(_.totalNs)

    def main(args: Array[String]): Unit = {
        val dir = Paths.get(args(0))
        val out = new PrintWriter(new File(args(1)))
        val deferred = args.drop(2).toSet
        val files = Files
            .list(dir)
            .toArray
            .map(_.asInstanceOf[Path])
            .filter(_.toString.endsWith(".flat"))
            .toList
        def scriptName(f: Path) = f.getFileName.toString.stripSuffix(".flat")
        // Smallest terms first, so that a timed-out compile still running in the background
        // can only overlap the largest scripts.
        val bySize = files.map { f =>
            val n = onThread("size", EvalTimeoutMs)(termNodes(ofFile(f))) match
                case Outcome.Done(n) => n
                case _               => Long.MaxValue
            f -> n
        }
        val ordered = bySize
            .sortBy((f, n) => (deferred.contains(scriptName(f)), n, scriptName(f)))
            .map(_._1)
        val probe = files.minBy(f => Files.size(f))
        val probeTerm = ofFile(probe)

        val coldNs = compileAll(List(probeTerm)).head
        System.err.println(s"cold compile of ${scriptName(probe)}: ${ms(coldNs)} ms")

        out.println(header)
        out.flush()
        for f <- ordered do
            out.println(measure(f))
            out.flush()

        // The rows use a fresh compiler per script. JIT.scala reuses one compiler: measure both.
        val fresh = List.fill(5)(compileAll(List(probeTerm)).head).sorted
        val reused = compileAll(List.fill(6)(probeTerm)).tail.sorted
        System.err.println(s"warm fresh-compiler compiles: ${fresh.map(ms).mkString(" ")} ms")
        System.err.println(s"warm reused-compiler compiles: ${reused.map(ms).mkString(" ")} ms")
        out.close()

        val summary = new PrintWriter(new File(args(1).stripSuffix(".csv") + "-coldwarm.txt"))
        summary.println(s"probe_script=${scriptName(probe)}")
        summary.println(s"probe_flat_bytes=${Files.size(probe)}")
        summary.println(s"cold_compile_ms=${ms(coldNs)}")
        summary.println(s"warm_fresh_compiler_ms_runs=${fresh.map(ms).mkString(" ")}")
        summary.println(s"warm_fresh_compiler_ms_median=${ms(fresh(2))}")
        summary.println(s"warm_reused_compiler_ms_runs=${reused.map(ms).mkString(" ")}")
        summary.println(s"warm_reused_compiler_ms_median=${ms(reused(2))}")
        summary.flush()
        // Report how long the timed-out compiles really took, if they end within 20 more minutes.
        val deadline = System.nanoTime() + 20L * 60 * 1_000_000_000L
        for (name, future) <- stragglers do
            val left = math.max(0L, deadline - System.nanoTime())
            val line =
                try
                    s"late_finish_ms.$name=${ms(future.get(left, TimeUnit.NANOSECONDS).head.totalNs)}"
                catch
                    case _: TimeoutException => s"late_finish_ms.$name=unfinished"
                    case e: ExecutionException =>
                        s"late_finish_ms.$name=failed ${oneLine(e.getCause)}"
            System.err.println(line)
            summary.println(line)
            summary.flush()
        summary.close()
        System.exit(0)
    }
}
