package scalus.uplc.eval.jitcommon

import scalus.builtin.{BuiltinPair, Builtins, ByteString, Data}
import scalus.uplc.eval.{BudgetSpender, ExBudgetCategory, Logger, MachineParams, StepKind}
import scalus.uplc.eval.ExBudgetCategory.Step
import scalus.uplc.eval.jitcommon.*

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
    ): BigInt => BigInt => BigInt =
        (x: BigInt) =>
            (y: BigInt) => {
                budget.spendBudget(
                  Step(StepKind.Builtin),
                  params.builtinCostModel.addInteger.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(x),
                      MemoryUsageJit.memoryUsage(y)
                    )
                  ),
                  Nil
                )
                x + y
            }

    @inline def subtractInteger(
        budget: BudgetSpender,
        params: MachineParams
    ): BigInt => BigInt => BigInt =
        (x: BigInt) =>
            (y: BigInt) => {
                budget.spendBudget(
                  Step(StepKind.Builtin),
                  params.builtinCostModel.subtractInteger.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(x),
                      MemoryUsageJit.memoryUsage(y)
                    )
                  ),
                  Nil
                )
                x - y
            }

    @inline def multiplyInteger(
        budget: BudgetSpender,
        params: MachineParams
    ): BigInt => BigInt => BigInt =
        (x: BigInt) =>
            (y: BigInt) => {
                budget.spendBudget(
                  Step(StepKind.Builtin),
                  params.builtinCostModel.multiplyInteger.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(x),
                      MemoryUsageJit.memoryUsage(y)
                    )
                  ),
                  Nil
                )
                x * y
            }

    @inline def lessThanInteger(
        budget: BudgetSpender,
        params: MachineParams
    ): BigInt => BigInt => Boolean =
        (x: BigInt) =>
            (y: BigInt) => {
                budget.spendBudget(
                  Step(StepKind.Builtin),
                  params.builtinCostModel.lessThanInteger.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(x),
                      MemoryUsageJit.memoryUsage(y)
                    )
                  ),
                  Nil
                )
                x < y
            }

    @inline def lessThanEqualsInteger(
        budget: BudgetSpender,
        params: MachineParams
    ): BigInt => BigInt => Boolean =
        (x: BigInt) =>
            (y: BigInt) => {
                budget.spendBudget(
                  Step(StepKind.Builtin),
                  params.builtinCostModel.lessThanEqualsInteger.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(x),
                      MemoryUsageJit.memoryUsage(y)
                    )
                  ),
                  Nil
                )
                x <= y
            }

    @inline def equalsInteger(
        budget: BudgetSpender,
        params: MachineParams
    ): BigInt => BigInt => Boolean =
        (x: BigInt) =>
            (y: BigInt) => {
                budget.spendBudget(
                  Step(StepKind.Builtin),
                  params.builtinCostModel.equalsInteger.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(x),
                      MemoryUsageJit.memoryUsage(y)
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
    ): ByteString => ByteString => Boolean =
        (x: ByteString) =>
            (y: ByteString) => {
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

    @inline def consByteString(
        budget: BudgetSpender,
        params: MachineParams
    ): BigInt => ByteString => ByteString =
        (char: BigInt) =>
            (bs: ByteString) => {
                budget.spendBudget(
                  Step(StepKind.Builtin),
                  params.builtinCostModel.consByteString.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(char),
                      MemoryUsageJit.memoryUsage(bs)
                    )
                  ),
                  Nil
                )
                Builtins.consByteString(char, bs)
            }

    @inline def sliceByteString(
        budget: BudgetSpender,
        params: MachineParams
    ): BigInt => BigInt => ByteString => ByteString =
        (start: BigInt) =>
            (n: BigInt) =>
                (bs: ByteString) => {
                    budget.spendBudget(
                      Step(StepKind.Builtin),
                      params.builtinCostModel.sliceByteString.calculateCostFromMemory(
                        Seq(
                          MemoryUsageJit.memoryUsage(start),
                          MemoryUsageJit.memoryUsage(n),
                          MemoryUsageJit.memoryUsage(bs)
                        )
                      ),
                      Nil
                    )
                    Builtins.sliceByteString(start, n, bs)
                }

    @inline def indexByteString(
        budget: BudgetSpender,
        params: MachineParams
    ): ByteString => BigInt => BigInt =
        (bs: ByteString) =>
            (i: BigInt) => {
                budget.spendBudget(
                  Step(StepKind.Builtin),
                  params.builtinCostModel.indexByteString.constantCost,
                  Nil
                )
                Builtins.indexByteString(bs, i)
            }

    @inline def sha2_256(budget: BudgetSpender, params: MachineParams): ByteString => ByteString =
        (bs: ByteString) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.sha2_256.calculateCostFromMemory(
                Seq(MemoryUsageJit.memoryUsage(bs))
              ),
              Nil
            )
            Builtins.sha2_256(bs)
        }

    // Data operations

    @inline def equalsData(budget: BudgetSpender, params: MachineParams): Data => Data => Boolean =
        (x: Data) =>
            (y: Data) => {
                budget.spendBudget(
                  Step(StepKind.Builtin),
                  params.builtinCostModel.equalsData.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(x),
                      MemoryUsageJit.memoryUsage(y)
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

    // Data constructors

    @inline def constrData(
        budget: BudgetSpender,
        params: MachineParams
    ): BigInt => List[Data] => Data =
        (tag: BigInt) =>
            (fields: List[Data]) => {
                budget.spendBudget(
                  Step(StepKind.Builtin),
                  params.builtinCostModel.constrData.constantCost,
                  Nil
                )
                Data.Constr(tag.longValue, fields)
            }

    @inline def iData(budget: BudgetSpender, params: MachineParams): BigInt => Data =
        (x: BigInt) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.iData.constantCost,
              Nil
            )
            Data.I(x)
        }

    @inline def bData(budget: BudgetSpender, params: MachineParams): ByteString => Data =
        (x: ByteString) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.bData.constantCost,
              Nil
            )
            Data.B(x)
        }

    @inline def listData(budget: BudgetSpender, params: MachineParams): List[Data] => Data =
        (x: List[Data]) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.listData.constantCost,
              Nil
            )
            Data.List(x)
        }

    @inline def mapData(
        budget: BudgetSpender,
        params: MachineParams
    ): List[BuiltinPair[Data, Data]] => Data =
        (x: List[BuiltinPair[Data, Data]]) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.mapData.constantCost,
              Nil
            )
            Data.Map(x.map(p => (p.fst, p.snd)))
        }

    @inline def unMapData(
        budget: BudgetSpender,
        params: MachineParams
    ): Data => List[BuiltinPair[Data, Data]] =
        (x: Data) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.unMapData.constantCost,
              Nil
            )
            RuntimeHelper.unMapData(x)
        }

    @inline def serialiseData(budget: BudgetSpender, params: MachineParams): Data => ByteString =
        (x: Data) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.serialiseData.calculateCostFromMemory(
                Seq(MemoryUsageJit.memoryUsage(x))
              ),
              Nil
            )
            Builtins.serialiseData(x)
        }

    @inline def mkPairData(
        budget: BudgetSpender,
        params: MachineParams
    ): Data => Data => BuiltinPair[Data, Data] =
        (fst: Data) =>
            (snd: Data) => {
                budget.spendBudget(
                  Step(StepKind.Builtin),
                  params.builtinCostModel.mkPairData.constantCost,
                  Nil
                )
                BuiltinPair(fst, snd)
            }

    @inline def chooseData(budget: BudgetSpender, params: MachineParams): () => Any =
        () =>
            (d: Data) =>
                (constrCase: Any) =>
                    (mapCase: Any) =>
                        (listCase: Any) =>
                            (iCase: Any) =>
                                (bCase: Any) => {
                                    budget.spendBudget(
                                      Step(StepKind.Builtin),
                                      params.builtinCostModel.chooseData.constantCost,
                                      Nil
                                    )
                                    Builtins.chooseData(
                                      d,
                                      constrCase,
                                      mapCase,
                                      listCase,
                                      iCase,
                                      bCase
                                    )
                                }

    // List operations

    @inline def mkCons(budget: BudgetSpender, params: MachineParams): () => Any =
        () =>
            (head: Any) =>
                (tail: List[Any]) => {
                    budget.spendBudget(
                      Step(StepKind.Builtin),
                      params.builtinCostModel.mkCons.constantCost,
                      Nil
                    )
                    head :: tail
                }

    @inline def nullList(budget: BudgetSpender, params: MachineParams): () => Any =
        () =>
            (list: List[Any]) => {
                budget.spendBudget(
                  Step(StepKind.Builtin),
                  params.builtinCostModel.nullList.constantCost,
                  Nil
                )
                list.isEmpty
            }

    @inline def mkNilData(budget: BudgetSpender, params: MachineParams): Unit => List[Data] =
        (_: Unit) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.mkNilData.constantCost,
              Nil
            )
            Nil
        }

    @inline def mkNilPairData(
        budget: BudgetSpender,
        params: MachineParams
    ): Unit => List[BuiltinPair[Data, Data]] =
        (_: Unit) => {
            budget.spendBudget(
              Step(StepKind.Builtin),
              params.builtinCostModel.mkNilPairData.constantCost,
              Nil
            )
            Nil
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

    @inline def fstPair(
        budget: BudgetSpender,
        params: MachineParams
    ): () => () => BuiltinPair[?, ?] => Any =
        () =>
            () =>
                (x: BuiltinPair[?, ?]) => {
                    import scalus.builtin.BuiltinPair
                    budget.spendBudget(
                      Step(StepKind.Builtin),
                      params.builtinCostModel.fstPair.constantCost,
                      Nil
                    )
                    Builtins.fstPair(x.asInstanceOf[BuiltinPair[?, ?]])
                }

    @inline def sndPair(
        budget: BudgetSpender,
        params: MachineParams
    ): () => () => BuiltinPair[?, ?] => Any =
        () =>
            () =>
                (x: BuiltinPair[?, ?]) => {
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
