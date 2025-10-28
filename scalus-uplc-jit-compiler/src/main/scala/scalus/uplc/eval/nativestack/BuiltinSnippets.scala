package scalus.uplc.eval.nativestack

import scalus.builtin.{Builtins, ByteString, Data}
import scalus.uplc.eval.{BudgetSpender, ExBudgetCategory, Logger, MachineParams, StepKind}
import scalus.uplc.eval.ExBudgetCategory.Step

/** Pre-compiled builtin function implementations for NativeStack JIT.
  *
  * These are regular Scala functions (not staged code) that can be directly referenced during JIT
  * compilation. This eliminates the overhead of staging/quoting for builtins, which are constant
  * code that never changes.
  *
  * Benefits:
  *   - Faster JIT compilation (no staging overhead for builtins)
  *   - Simpler code (regular Scala functions)
  *   - Better testability (can unit test snippets independently)
  *   - More efficient bytecode (pre-compiled, not generated)
  */
object BuiltinSnippets {

    // Binary integer operations

    @inline def addInteger(
        budget: BudgetSpender,
        params: MachineParams
    ): (BigInt, BigInt) => BigInt =
        (x: BigInt, y: BigInt) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.addInteger.calculateCostFromMemory(
                Seq(
                  MemoryUsage.memoryUsage(x),
                  MemoryUsage.memoryUsage(y)
                )
              ),
              Nil
            )
            x + y
        }

    @inline def subtractInteger(
        budget: BudgetSpender,
        params: MachineParams
    ): (BigInt, BigInt) => BigInt =
        (x: BigInt, y: BigInt) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.subtractInteger.calculateCostFromMemory(
                Seq(
                  MemoryUsage.memoryUsage(x),
                  MemoryUsage.memoryUsage(y)
                )
              ),
              Nil
            )
            x - y
        }

    @inline def multiplyInteger(
        budget: BudgetSpender,
        params: MachineParams
    ): (BigInt, BigInt) => BigInt =
        (x: BigInt, y: BigInt) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.multiplyInteger.calculateCostFromMemory(
                Seq(
                  MemoryUsage.memoryUsage(x),
                  MemoryUsage.memoryUsage(y)
                )
              ),
              Nil
            )
            x * y
        }

    @inline def lessThanInteger(
        budget: BudgetSpender,
        params: MachineParams
    ): (BigInt, BigInt) => Boolean =
        (x: BigInt, y: BigInt) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.lessThanInteger.calculateCostFromMemory(
                Seq(
                  MemoryUsage.memoryUsage(x),
                  MemoryUsage.memoryUsage(y)
                )
              ),
              Nil
            )
            x < y
        }

    @inline def lessThanEqualsInteger(
        budget: BudgetSpender,
        params: MachineParams
    ): (BigInt, BigInt) => Boolean =
        (x: BigInt, y: BigInt) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.lessThanEqualsInteger.calculateCostFromMemory(
                Seq(
                  MemoryUsage.memoryUsage(x),
                  MemoryUsage.memoryUsage(y)
                )
              ),
              Nil
            )
            x <= y
        }

    @inline def equalsInteger(
        budget: BudgetSpender,
        params: MachineParams
    ): (BigInt, BigInt) => Boolean =
        (x: BigInt, y: BigInt) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.equalsInteger.calculateCostFromMemory(
                Seq(
                  MemoryUsage.memoryUsage(x),
                  MemoryUsage.memoryUsage(y)
                )
              ),
              Nil
            )
            x == y
        }

    // ByteString operations

    @inline def equalsByteString(
        budget: BudgetSpender,
        params: MachineParams
    ): (ByteString, ByteString) => Boolean =
        (x: ByteString, y: ByteString) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.equalsByteString.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage(x),
                  MemoryUsageJit.memoryUsage(y)
                )
              ),
              Nil
            )
            x == y
        }

    @inline def sha2_256(budget: BudgetSpender, params: MachineParams): ByteString => ByteString =
        (bs: ByteString) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.sha2_256.calculateCostFromMemory(
                Seq(MemoryUsage.memoryUsage(bs))
              ),
              Nil
            )
            Builtins.sha2_256(bs)
        }

    // Data operations

    @inline def equalsData(budget: BudgetSpender, params: MachineParams): (Data, Data) => Boolean =
        (x: Data, y: Data) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.equalsData.calculateCostFromMemory(
                Seq(
                  MemoryUsage.memoryUsage(x),
                  MemoryUsage.memoryUsage(y)
                )
              ),
              Nil
            )
            Builtins.equalsData(x, y)
        }

    @inline def unConstrData(budget: BudgetSpender, params: MachineParams): Data => Any =
        (x: Data) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.unConstrData.constantCost,
              Nil
            )
            RuntimeHelper.unConstrData(x)
        }

    @inline def unListData(budget: BudgetSpender, params: MachineParams): Data => List[Data] =
        (x: Data) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.unListData.constantCost,
              Nil
            )
            RuntimeHelper.unListData(x)
        }

    @inline def unIData(budget: BudgetSpender, params: MachineParams): Data => BigInt =
        (x: Data) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.unIData.constantCost,
              Nil
            )
            Builtins.unIData(x)
        }

    @inline def unBData(budget: BudgetSpender, params: MachineParams): Data => ByteString =
        (x: Data) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.unBData.constantCost,
              Nil
            )
            Builtins.unBData(x)
        }

    // Control flow

    @inline def ifThenElse(budget: BudgetSpender, params: MachineParams): () => Any =
        () =>
            (c: Boolean) =>
                (t: Any) =>
                    (f: Any) => {
                        budget.spendBudget(
                          Step(StepKind.Builtin),
                          params.builtinCostModel.ifThenElse.constantCost,
                          Nil
                        )
                        if c then t else f
                    }

    @inline def trace(logger: Logger, budget: BudgetSpender, params: MachineParams): () => Any =
        () =>
            (s: String) =>
                (a: Any) => {
                    budget.spendBudget(
                      Step(StepKind.Builtin),
                      params.builtinCostModel.trace.constantCost,
                      Nil
                    )
                    logger.log(s)
                    a
                }

    // Pair operations

    @inline def fstPair(budget: BudgetSpender, params: MachineParams): () => Any =
        () =>
            () =>
                (x: Any) => {
                    import scalus.builtin.BuiltinPair
                    budget.spendBudget(
                      Step(StepKind.Builtin),
                      params.builtinCostModel.fstPair.constantCost,
                      Nil
                    )
                    Builtins.fstPair(x.asInstanceOf[BuiltinPair[?, ?]])
                }

    @inline def sndPair(budget: BudgetSpender, params: MachineParams): () => Any =
        () =>
            () =>
                (x: Any) => {
                    import scalus.builtin.BuiltinPair
                    budget.spendBudget(
                      Step(StepKind.Builtin),
                      params.builtinCostModel.sndPair.constantCost,
                      Nil
                    )
                    Builtins.sndPair(x.asInstanceOf[BuiltinPair[?, ?]])
                }

    // List operations

    @inline def chooseList(budget: BudgetSpender, params: MachineParams): () => Any =
        () =>
            () =>
                (l: Any) =>
                    (e: Any) =>
                        (ne: Any) => {
                            val lv = l.asInstanceOf[List[?]]
                            budget.spendBudget(
                              Step(StepKind.Builtin),
                              params.builtinCostModel.chooseList.constantCost,
                              Nil
                            )
                            if lv.isEmpty then e else ne
                        }

    @inline def headList(budget: BudgetSpender, params: MachineParams): () => Any =
        () =>
            (y: Any) => {
                val yv = y.asInstanceOf[List[?]]
                budget.spendBudget(
                  Step(StepKind.Builtin),
                  params.builtinCostModel.headList.constantCost,
                  Nil
                )
                yv.head
            }

    @inline def tailList(budget: BudgetSpender, params: MachineParams): () => Any =
        () =>
            (x: Any) => {
                val xv = x.asInstanceOf[List[?]]
                budget.spendBudget(
                  Step(StepKind.Builtin),
                  params.builtinCostModel.tailList.constantCost,
                  Nil
                )
                xv.tail
            }
}
