package scalus.uplc.jit

import scala.util.Try

/** Compiles every corpus script ([[JitCorpus]]) with the JIT and compares the result, success or
  * failure, and the spent budget per category with the CEK machine, both with the machine
  * parameters of [[JitCorpus.vm]] ([[JitCekDiff.same]]).
  *
  * Usage: `JitVsCekCheck [name filter]`. Prints one line per script and the number of mismatches.
  */
object JitVsCekCheck {

    def main(args: Array[String]): Unit = {
        val filter = args.headOption.getOrElse("")
        val scripts = JitCorpus.all.filter(_.name.contains(filter))
        val vm = JitCorpus.vm
        var mismatches = 0
        for script <- scripts do
            val cek = JitCekDiff.runCek(vm, script.term)
            val start = System.nanoTime()
            val compiled = Try(nativestack.JIT.jitUplc(script.term))
            val compileS = (System.nanoTime() - start) / 1e9
            val jit = compiled.map(fn => JitCekDiff.runJit(fn, vm.machineParams))
            val same = jit.toOption.exists(JitCekDiff.same(cek, _))
            if !same then mismatches += 1
            val jitDesc = jit.fold(
              e => s"COMPILE FAILED $e",
              j => s"${j.result.map(short).getOrElse("FAIL")} ${j.total}"
            )
            println(
              f"${if same then "OK  " else "DIFF"} ${script.name}%-40s compile $compileS%6.2f s " +
                  s"cek=${cek.result.map(short).getOrElse("FAIL")} ${cek.total} jit=$jitDesc"
            )
        println(s"scripts=${scripts.size} mismatches=$mismatches")
    }

    private def short(x: Any): String = x.toString.take(40)
}
