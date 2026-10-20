package scalus.uplc.jit

import scalus.cardano.ledger.ExUnits
import scalus.uplc.eval.*

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** Measures the JIT, or the CEK machine, on the bench scripts (spec section 4.3).
  *
  * Usage: `JitBench <jit|cek> [--label <name>]`. Run each variant in its own JVM ([MS-8]) with
  * `-XX:CICompilerCount=12` ([MS-9]); quick mode ([MS-13]) is a matter of JVM flags only, which
  * `scripts/jit-bench.sh` passes. Writes `<label>.csv` (one row per script) and `<label>.summary`
  * to `-Djit.bench.out` (default `target/jit-bench`).
  *
  * Steps:
  *   1. Compile every script with the variant, in corpus order, as `jitUplc` does in production (on
  *      the staging compiler thread), and record the compile time and the class-file bytes
  *      ([MS-11]); a script that does not compile is recorded and left out. The first
  *      [[CompilerWarmupScripts]] scripts are compiled once untimed before.
  *   1. Check that every compiled script gives CEK semantics ([MS-6], [[JitCekDiff.same]]): result,
  *      failure, budget per category. If one does not, the variant is broken: write that and stop,
  *      untimed ([MS-7]).
  *   1. Warm up all scripts round-robin in windows of 5 s, until the mean round time of two
  *      consecutive windows differs by less than 3 % ([MS-10]), or [[MaxWarmupNanos]].
  *   1. Time every script round-robin for [[MeasureNanos]], at least [[MinMeasureRounds]] rounds,
  *      and record its median.
  *
  * With `-Djit.bench.isolated=true` steps 3 and 4 run one script at a time instead
  * ([[measureIsolated]]): round-robin does not settle on this code (spec, implementation status).
  * The scripts are [[JitCorpus.benchSubset]]; `-Djit.bench.all=true` times the whole corpus.
  *
  * Every evaluation gets a new `RestrictingBudgetSpender` with an unlimited budget and `NoLogger`,
  * the CEK machine and the JIT alike. The whole bench runs on a thread with a 1 GB stack: the JIT
  * counts its call depth for a 64 MB stack and does not catch a stack overflow.
  */
object JitBench {

    /** `-Djit.bench.minWarmupS=<s>`: warm up at least this long before the steady test applies. 0,
      * the default, is [MS-10]; a long minimum checks whether [MS-10] stops too early.
      */
    private val MinWarmupNanos =
        (sys.props.getOrElse("jit.bench.minWarmupS", "0").toDouble * 1e9).toLong

    private val WindowNanos = 5_000_000_000L
    private val SteadyTolerance = 0.03
    private val MaxWarmupNanos = 900_000_000_000L
    private val MeasureNanos = 20_000_000_000L
    private val MinMeasureRounds = 50

    /** `-Djit.bench.isolated=true`: warm up and time one script at a time, not round-robin. */
    private val isolated = sys.props.get("jit.bench.isolated").contains("true")
    private val IsolatedWindowNanos = 1_000_000_000L
    private val IsolatedMinWarmupNanos = 5_000_000_000L
    private val IsolatedMaxWarmupNanos = 60_000_000_000L
    private val IsolatedMeasureNanos = 3_000_000_000L

    /** Scripts compiled untimed before the timed compiles. */
    private val CompilerWarmupScripts = 10

    /** A script's time-to-steady-state: the first window after which its window mean stays within
      * this fraction of its measured median.
      */
    private val ScriptSteadyTolerance = 0.10

    private val Unlimited = ExUnits(Long.MaxValue / 2, Long.MaxValue / 2)

    /** What the bench records for one script ([MS-11]). */
    final case class Row(
        script: String,
        compiled: Boolean,
        compileS: Double,
        classBytes: Long,
        steadyS: Double,
        medianUs: Double,
        error: String
    )

    /** A script ready to run: one evaluation per call; `compiled` is the JIT code, if any. */
    private final case class Prepared(name: String, run: () => Any, compiled: CompiledTerm | Null)

    def main(args: Array[String]): Unit = {
        val variant = args(0)
        val label = args.indexOf("--label") match
            case -1 => variant
            case i  => args(i + 1)
        val out = Paths.get(sys.props.getOrElse("jit.bench.out", "target/jit-bench"))
        Files.createDirectories(out)
        var failure: Throwable | Null = null
        val body: Runnable = () =>
            try run(variant, label, out)
            catch case e: Throwable => failure = e
        val thread = new Thread(null, body, "jit-bench", 1L << 30)
        thread.start()
        thread.join()
        if failure != null then throw failure.nn
    }

    private def run(variant: String, label: String, out: Path): Unit = {
        val vm = JitCorpus.vm
        // the bench subset; `-Djit.bench.all=true` the whole corpus, `-Djit.bench.filter=<regex>`
        // a part of it, for diagnostics
        val filter = sys.props.get("jit.bench.filter").map(_.r)
        val corpus =
            if sys.props.get("jit.bench.all").contains("true") || filter.nonEmpty then JitCorpus.all
            else JitCorpus.benchSubset
        val scripts = corpus.filter(s => filter.forall(_.findFirstIn(s.name).isDefined))
        val start = System.nanoTime()
        log(s"$label: ${scripts.size} scripts, variant $variant")

        // 1. compile
        val rows = collection.mutable.LinkedHashMap.empty[String, Row]
        val prepared =
            if variant == "cek" then
                scripts.map { s =>
                    rows(s.name) = Row(s.name, true, 0, 0, 0, 0, "")
                    val term = s.program.term
                    Prepared(
                      s.name,
                      () =>
                          vm.evaluateDeBruijnedTerm(
                            term,
                            RestrictingBudgetSpender(Unlimited),
                            NoLogger
                          ),
                      null
                    )
                }
            else
                val classDir = sys.props.get("scalus.jit.outDir").map(Paths.get(_))
                // untimed: the first compiles of a JVM also compile the staging compiler itself
                for s <- scripts.take(CompilerWarmupScripts) do
                    try nativestack.JIT.jitUplc(s.term)
                    catch case NonFatal(_) => ()
                scripts.flatMap { s =>
                    classDir.foreach(clear)
                    val t0 = System.nanoTime()
                    try
                        val compiled = nativestack.JIT.jitUplc(s.term)
                        val compileS = (System.nanoTime() - t0) / 1e9
                        val bytes = classDir.map(size).getOrElse(0L)
                        rows(s.name) = Row(s.name, true, compileS, bytes, 0, 0, "")
                        log(f"compiled ${s.name}%-40s $compileS%6.2f s $bytes%9d bytes")
                        val params = vm.machineParams
                        Some(
                          Prepared(
                            s.name,
                            () => compiled(NoLogger, RestrictingBudgetSpender(Unlimited), params),
                            compiled
                          )
                        )
                    catch
                        case NonFatal(e) =>
                            val msg = e.getMessage.linesIterator.toList.lastOption.getOrElse("")
                            rows(s.name) = Row(s.name, false, 0, 0, 0, 0, msg.replace(',', ';'))
                            log(s"DID NOT COMPILE ${s.name}: $msg")
                            None
                }

        // 2. CEK semantics
        if variant != "cek" then
            val terms = scripts.map(s => s.name -> s.term).toMap
            val broken = prepared.filterNot { p =>
                val cek = JitCekDiff.runCek(vm, terms(p.name))
                JitCekDiff.same(cek, JitCekDiff.runJit(p.compiled.nn, vm.machineParams))
            }
            if broken.nonEmpty then
                val text = s"variant $variant BROKEN: no CEK semantics on ${broken.map(_.name)}"
                log(text)
                Files.writeString(out.resolve(s"$label.summary"), text + "\n")
                return

        val runs: Array[() => Any] = prepared.map(_.run).toArray
        val (warmupS, windows, steady, rounds) =
            if isolated then measureIsolated(prepared.map(_.name), runs, rows)
            else measureRoundRobin(prepared.map(_.name), runs, rows)

        // report
        val csv = "script,compiled,compile_s,class_bytes,steady_s,median_us,error" ::
            rows.values.toList.map(r =>
                f"${r.script},${r.compiled},${r.compileS}%.3f,${r.classBytes},${r.steadyS}%.0f,${r.medianUs}%.2f,${r.error}"
            )
        Files.write(out.resolve(s"$label.csv"), csv.asJava)
        val ok = rows.values.filter(_.compiled).toList
        val summary = List(
          s"variant $variant",
          s"label $label",
          s"jvm ${java.lang.management.ManagementFactory.getRuntimeMXBean.getInputArguments.asScala.mkString(" ")}",
          s"scripts ${rows.size}",
          s"compiled ${ok.size}",
          f"geomean_eval_us ${geoMean(ok.map(_.medianUs))}%.3f",
          f"geomean_compile_s ${if variant == "cek" then 0.0 else geoMean(ok.map(_.compileS))}%.4f",
          s"total_class_bytes ${ok.map(_.classBytes).sum}",
          s"mode ${if isolated then "isolated" else "round-robin"}",
          f"warmup_s $warmupS%.0f",
          s"warmup_windows $windows",
          s"steady $steady",
          s"measure_rounds $rounds",
          f"total_s ${(System.nanoTime() - start) / 1e9}%.0f"
        )
        Files.write(out.resolve(s"$label.summary"), summary.asJava)
        summary.foreach(log)
    }

    /** [MS-9], [MS-10]: warms up all scripts round-robin, then times them round-robin. Returns the
      * warm-up seconds, the number of windows, whether it reached the steady test, and the measured
      * rounds.
      */
    private def measureRoundRobin(
        names: List[String],
        runs: Array[() => Any],
        rows: collection.mutable.Map[String, Row]
    ): (Double, Int, Boolean, Int) = {
        val n = runs.length
        val windowMeans = collection.mutable.ArrayBuffer.empty[Array[Double]] // per script, us
        val windowRound = collection.mutable.ArrayBuffer.empty[Double] // mean round time, us
        val warmStart = System.nanoTime()
        var steady = false
        while !steady && System.nanoTime() - warmStart < MaxWarmupNanos do
            val sums = new Array[Long](n)
            var rounds = 0
            val w0 = System.nanoTime()
            while System.nanoTime() - w0 < WindowNanos do
                var i = 0
                while i < n do
                    val t = System.nanoTime()
                    runs(i)()
                    sums(i) += System.nanoTime() - t
                    i += 1
                rounds += 1
            windowMeans += sums.map(_ / 1e3 / rounds)
            windowRound += sums.sum / 1e3 / rounds
            val k = windowRound.size
            if k >= 2 && System.nanoTime() - warmStart >= MinWarmupNanos then
                val (a, b) = (windowRound(k - 2), windowRound(k - 1))
                steady = math.abs(a - b) / b < SteadyTolerance
            log(f"warm-up window $k: $rounds rounds, mean round ${windowRound.last}%.0f us")
        val warmupS = (System.nanoTime() - warmStart) / 1e9

        // 4. measure, round-robin
        val samples = Array.fill(n)(collection.mutable.ArrayBuffer.empty[Double])
        val m0 = System.nanoTime()
        var rounds = 0
        while System.nanoTime() - m0 < MeasureNanos || rounds < MinMeasureRounds do
            var i = 0
            while i < n do
                val t = System.nanoTime()
                runs(i)()
                samples(i) += (System.nanoTime() - t) / 1e3
                i += 1
            rounds += 1
        for (name, i) <- names.zipWithIndex do
            val sorted = samples(i).sorted
            val median = sorted(sorted.size / 2)
            // the end of the first window from which the script stays near its median
            val firstSteady = windowMeans.indices
                .find(k =>
                    windowMeans
                        .drop(k)
                        .forall(w => math.abs(w(i) - median) / median <= ScriptSteadyTolerance)
                )
                .getOrElse(windowMeans.size)
            val steadyS = (firstSteady + 1) * WindowNanos / 1e9
            rows(name) = rows(name).copy(steadyS = steadyS, medianUs = median)
        (warmupS, windowRound.size, steady, rounds)
    }

    /** Warms up and times one script at a time: windows of [[IsolatedWindowNanos]] until two
      * consecutive ones differ by less than 3 %, at least [[IsolatedMinWarmupNanos]], then the
      * median of [[IsolatedMeasureNanos]] of runs. Returns the total warm-up seconds, the number of
      * windows, whether every script reached the steady test, and the measured runs.
      */
    private def measureIsolated(
        names: List[String],
        runs: Array[() => Any],
        rows: collection.mutable.Map[String, Row]
    ): (Double, Int, Boolean, Int) = {
        var warmupNanos = 0L
        var windows = 0
        var allSteady = true
        var measured = 0
        for (name, i) <- names.zipWithIndex do
            val run = runs(i)
            val w0 = System.nanoTime()
            var steady = false
            var last = -1.0
            while !steady && System.nanoTime() - w0 < IsolatedMaxWarmupNanos do
                val t0 = System.nanoTime()
                var n = 0
                while System.nanoTime() - t0 < IsolatedWindowNanos do
                    run()
                    n += 1
                val mean = (System.nanoTime() - t0) / 1e3 / n
                windows += 1
                steady = last > 0 && math.abs(mean - last) / mean < SteadyTolerance &&
                    System.nanoTime() - w0 >= IsolatedMinWarmupNanos
                last = mean
            if !steady then allSteady = false
            val steadyNanos = System.nanoTime() - w0
            warmupNanos += steadyNanos
            val samples = collection.mutable.ArrayBuffer.empty[Double]
            val m0 = System.nanoTime()
            while System.nanoTime() - m0 < IsolatedMeasureNanos do
                val t = System.nanoTime()
                run()
                samples += (System.nanoTime() - t) / 1e3
            measured += samples.size
            val sorted = samples.sorted
            val median = sorted(sorted.size / 2)
            rows(name) = rows(name).copy(steadyS = steadyNanos / 1e9, medianUs = median)
            log(
              f"isolated $name%-40s steady after ${steadyNanos / 1e9}%5.1f s, median $median%9.2f us"
            )
        (warmupNanos / 1e9, windows, allSteady, measured)
    }

    def geoMean(xs: Seq[Double]): Double =
        if xs.isEmpty then 0 else math.exp(xs.map(math.log).sum / xs.size)

    private def clear(dir: Path): Unit =
        if Files.exists(dir) then
            Files.list(dir).iterator().asScala.filter(Files.isRegularFile(_)).foreach(Files.delete)

    private def size(dir: Path): Long =
        Files.walk(dir).iterator().asScala.filter(Files.isRegularFile(_)).map(Files.size).sum

    private def log(msg: String): Unit = println(s"[bench] $msg")
}
