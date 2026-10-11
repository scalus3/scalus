package scalus.uplc.jit

import scalus.uplc.eval.*
import scalus.uplc.{DeBruijnedProgram, Term}

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.quoted.staging
import scala.util.Try

/** Compiles every script of a directory with the nativestack JIT and compares the result and the
  * spent budget with the CEK machine, both with the same machine parameters.
  *
  * Usage: `JitVsCekCheck <dir with .flat files> [name filter]`.
  */
object JitVsCekCheck {

    def main(args: Array[String]): Unit = {
        val filter = args.lift(1).getOrElse("")
        val files = Files
            .list(Paths.get(args(0)))
            .iterator()
            .asScala
            .filter(f => f.toString.endsWith(".flat") && f.getFileName.toString.contains(filter))
            .toList
            .sortBy(_.toString)
        val vm = PlutusVM.makePlutusV3VM()
        val params = vm.machineParams
        var mismatches = 0
        for file <- files do
            val name = file.getFileName.toString.stripSuffix(".flat")
            val program = DeBruijnedProgram.fromFlatEncoded(Files.readAllBytes(file))
            val term = program.toProgram.term
            val cekBudget = CountingBudgetSpender()
            val cek = Try(vm.evaluateDeBruijnedTerm(program.term, cekBudget, NoLogger))
            given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
            val start = System.nanoTime()
            val fn = staging.run(nativestack.JIT.genCodeFromTerm(term))
            val compileS = (System.nanoTime() - start) / 1e9
            val jitBudget = CountingBudgetSpender()
            val jit = Try(fn(NoLogger, jitBudget, params))
            val same = cek.isSuccess == jit.isSuccess &&
                cekBudget.getSpentBudget == jitBudget.getSpentBudget
            if !same then mismatches += 1
            println(
              f"${if same then "OK  " else "DIFF"} $name%-28s compile $compileS%6.2f s " +
                  s"cek=${cek.map(short).getOrElse("FAIL")} ${cekBudget.getSpentBudget} " +
                  s"jit=${jit.map(short).getOrElse("FAIL")} ${jitBudget.getSpentBudget}"
            )
        println(s"scripts=${files.size} mismatches=$mismatches")
    }

    private def short(x: Any): String = x.toString.take(40)
}
