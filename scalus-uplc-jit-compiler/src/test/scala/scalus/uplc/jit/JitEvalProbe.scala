package scalus.uplc.jit

import scalus.uplc.DeBruijnedProgram
import scalus.uplc.eval.*

import java.nio.file.{Files, Paths}
import scala.quoted.staging

/** Times steady-state evaluation of one script: nativestack JIT vs CEK.
  *
  * Usage: `JitEvalProbe <script.flat> [warmupSeconds] [runs]`. Compiles once, warms each side up
  * for `warmupSeconds` (default 20), then prints the median of `runs` (default 200) evaluations.
  * Both sides use `NoBudgetSpender` and the V3 VM machine parameters. `-Dprobe.outDir=<dir>` writes
  * the generated classes to `<dir>`, for `javap`.
  */
object JitEvalProbe {

    def main(args: Array[String]): Unit = {
        val program = DeBruijnedProgram.fromFlatEncoded(Files.readAllBytes(Paths.get(args(0))))
        val warmupS = if args.length > 1 then args(1).toDouble else 20.0
        val runs = if args.length > 2 then args(2).toInt else 200
        val term = program.toProgram.term
        val vm = PlutusVM.makePlutusV3VM()
        val params = vm.machineParams
        given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)(using
          staging.Compiler.Settings.make(outDir = sys.props.get("probe.outDir"))
        )
        val t0 = System.nanoTime()
        val fn = staging.run(nativestack.JIT.genCodeFromTerm(term))
        println(f"compile ${(System.nanoTime() - t0) / 1e9}%.2f s")
        val jit = median(warmupS, runs)(fn(NoLogger, NoBudgetSpender, params))
        val cek = median(warmupS, runs)(
          vm.evaluateDeBruijnedTerm(program.term, NoBudgetSpender, NoLogger)
        )
        println(f"RESULT jit $jit%.1f us cek $cek%.1f us ratio ${jit / cek}%.2f")
    }

    private def median(warmupS: Double, runs: Int)(body: => Any): Double = {
        val end = System.nanoTime() + (warmupS * 1e9).toLong
        while System.nanoTime() < end do body
        val times = Array.fill(runs) {
            val s = System.nanoTime()
            body
            (System.nanoTime() - s) / 1e3
        }
        java.util.Arrays.sort(times)
        times(runs / 2)
    }
}
