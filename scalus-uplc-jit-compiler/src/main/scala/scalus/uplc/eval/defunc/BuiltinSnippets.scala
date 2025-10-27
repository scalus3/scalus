package scalus.uplc.eval.defunc

import scalus.builtin.Data
import scalus.uplc.eval.*

/** Pre-compiled Snippet instances for UPLC builtins.
  * These are created once and reused - no need for staging since builtins are static.
  */
object BuiltinSnippets {
    
    val addInteger: Snippet = new Snippet {
        def execute(
            acc: Any,
            dataStack: DataStack,
            budget: BudgetSpender,
            logger: Logger,
            params: MachineParams
        ): Any = {
            (x: Any) => (y: Any) =>
                {
                    val xv = x.asInstanceOf[BigInt]
                    val yv = y.asInstanceOf[BigInt]
                    budget.spendBudget(
                      ExBudgetCategory.Step(StepKind.Builtin),
                      params.builtinCostModel.addInteger
                          .calculateCostFromMemory(
                            Seq(
                              MemoryUsageJit.memoryUsage(xv),
                              MemoryUsageJit.memoryUsage(yv)
                            )
                          ),
                      Nil
                    )
                    xv + yv
                }
        }
    }
    
    val subtractInteger: Snippet = new Snippet {
        def execute(
            acc: Any,
            dataStack: DataStack,
            budget: BudgetSpender,
            logger: Logger,
            params: MachineParams
        ): Any = {
            (x: Any) => (y: Any) =>
                {
                    val xv = x.asInstanceOf[BigInt]
                    val yv = y.asInstanceOf[BigInt]
                    budget.spendBudget(
                      ExBudgetCategory.Step(StepKind.Builtin),
                      params.builtinCostModel.subtractInteger
                          .calculateCostFromMemory(
                            Seq(
                              MemoryUsageJit.memoryUsage(xv),
                              MemoryUsageJit.memoryUsage(yv)
                            )
                          ),
                      Nil
                    )
                    xv - yv
                }
        }
    }
    
    val multiplyInteger: Snippet = new Snippet {
        def execute(
            acc: Any,
            dataStack: DataStack,
            budget: BudgetSpender,
            logger: Logger,
            params: MachineParams
        ): Any = {
            (x: Any) => (y: Any) =>
                {
                    val xv = x.asInstanceOf[BigInt]
                    val yv = y.asInstanceOf[BigInt]
                    budget.spendBudget(
                      ExBudgetCategory.Step(StepKind.Builtin),
                      params.builtinCostModel.multiplyInteger
                          .calculateCostFromMemory(
                            Seq(
                              MemoryUsageJit.memoryUsage(xv),
                              MemoryUsageJit.memoryUsage(yv)
                            )
                          ),
                      Nil
                    )
                    xv * yv
                }
        }
    }
    
    val equalsData: Snippet = new Snippet {
        def execute(
            acc: Any,
            dataStack: DataStack,
            budget: BudgetSpender,
            logger: Logger,
            params: MachineParams
        ): Any = {
            (x: Any) => (y: Any) =>
                {
                    val xv = x.asInstanceOf[Data]
                    val yv = y.asInstanceOf[Data]
                    budget.spendBudget(
                      ExBudgetCategory.Step(StepKind.Builtin),
                      params.builtinCostModel.equalsData
                          .calculateCostFromMemory(
                            Seq(
                              MemoryUsageJit.memoryUsage(xv),
                              MemoryUsageJit.memoryUsage(yv)
                            )
                          ),
                      Nil
                    )
                    xv == yv
                }
        }
    }
}
