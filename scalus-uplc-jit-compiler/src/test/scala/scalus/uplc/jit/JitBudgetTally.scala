package scalus.uplc.jit

import scalus.cardano.ledger.ExUnits
import scalus.uplc.DeBruijnedProgram
import scalus.uplc.eval.*

import java.nio.file.{Files, Paths}
import scala.quoted.staging
import scala.util.Try

/** Runs one script with the CEK machine and the nativestack JIT, tallies the spent budget per
  * `ExBudgetCategory`, and prints the categories that differ.
  *
  * Usage: `JitBudgetTally <script.flat>`.
  */
object JitBudgetTally {

    def main(args: Array[String]): Unit = {
        val program = DeBruijnedProgram.fromFlatEncoded(Files.readAllBytes(Paths.get(args(0))))
        val vm = PlutusVM.makePlutusV3VM()
        val cek = TallyingBudgetSpender(CountingBudgetSpender())
        val cekResult = Try(vm.evaluateDeBruijnedTerm(program.term, cek, NoLogger))
        given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
        val fn = staging.run(nativestack.JIT.genCodeFromTerm(program.toProgram.term))
        val jit = TallyingBudgetSpender(CountingBudgetSpender())
        val jitResult = Try(fn(NoLogger, jit, vm.machineParams))
        println(s"cek ${cekResult.isSuccess} ${cek.getSpentBudget}")
        println(s"jit ${jitResult.isSuccess} ${jit.getSpentBudget}")
        val cats = (cek.costs.keySet ++ jit.costs.keySet).toList.sortBy(_.toString)
        for cat <- cats do
            val c = cek.costs.getOrElse(cat, ExUnits.zero)
            val j = jit.costs.getOrElse(cat, ExUnits.zero)
            val mark = if c == j then "    " else "DIFF"
            println(s"$mark $cat cek=$c jit=$j")
    }
}
