package scalus.uplc.eval.hybrid

import scalus.uplc.{DeBruijn, Term}
import scalus.uplc.eval.{BudgetSpender, CekMachine, Logger, MachineParams, PlutusVM}
import scalus.uplc.eval.jitcommon.JitRunner
import scalus.uplc.eval.nativestack.StackTresholdException

object HybridJIT extends JitRunner {

    override def isStackSafe: Boolean = true

    override def jitUplc(term: Term): (Logger, BudgetSpender, MachineParams) => Any = {
        val deBruijnedTerm = DeBruijn.deBruijnTerm(term, true)
        val nativeStackFun = scalus.uplc.eval.nativestack.JIT.jitUplc(term)

        (logger, budgetSpender, machineParams) =>
            try {
                nativeStackFun(logger, budgetSpender, machineParams)
            } catch {
                case ex: StackTresholdException =>
                    // Fallback to stack-safe mincont JIT
                    val mincontFun = scalus.uplc.eval.mincont.JIT.jitUplc(term)
                    mincontFun(logger, budgetSpender, machineParams)
            }
    }

}
