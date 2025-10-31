package scalus.uplc.eval.hybrid

import scalus.uplc.{DeBruijn, Term}
import scalus.uplc.eval.{BudgetSpender, CekMachine, EvaluationFailure, Logger, MachineError, MachineParams, PlutusVM}
import scalus.uplc.eval.jitcommon.{JitRunner, RuntimeHelper}
import scalus.uplc.eval.nativestack.StackTresholdException

object HybridJIT extends JitRunner {

    override def isStackSafe: Boolean = true

    enum FailBackStrategy {
        case Cek, Mincont
    }

    val config = FailBackStrategy.Cek

    override def jitUplc(term: Term): (Logger, BudgetSpender, MachineParams) => Any = {
        val backupEvaluator: (Logger, BudgetSpender, MachineParams) => Any = config match {
            case FailBackStrategy.Cek =>
                val vm = PlutusVM.makePlutusV3VM()
                val djTerm = DeBruijn.deBruijnTerm(term, true)
                (logger: Logger, budgetSpender: BudgetSpender, machineParams: MachineParams) =>
                    val term = vm.evaluateDeBruijnedTerm(djTerm, budgetSpender, logger)
                    term match
                        case Term.Const(v) => RuntimeHelper.uplcToJitAny(v)
                        case _             =>
                            throw MachineError(
                              "HybridJIT: Expected constant result from Cek fallback",
                            )

            case FailBackStrategy.Mincont =>
                val mincontFun = scalus.uplc.eval.mincont.JIT.jitUplc(term)
                (logger: Logger, budgetSpender: BudgetSpender, machineParams: MachineParams) =>
                    mincontFun(logger, budgetSpender, machineParams)
        }
        val deBruijnedTerm = DeBruijn.deBruijnTerm(term, true)
        val nativeStackFun = scalus.uplc.eval.nativestack.JIT.jitUplc(term)

        (logger, budgetSpender, machineParams) =>
            try {
                nativeStackFun(logger, budgetSpender, machineParams)
            } catch {
                case ex: StackTresholdException =>
                    // Fallback to stack-safe Evaluator (Cek or minicont JIT)
                    backupEvaluator(logger, budgetSpender, machineParams)
            }
    }

}
