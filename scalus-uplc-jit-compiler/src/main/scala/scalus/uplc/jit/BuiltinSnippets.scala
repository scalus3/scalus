package scalus.uplc.jit

import scalus.uplc.builtin.{BuiltinPair, Builtins, ByteString, Data}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.onchain.plutus.prelude.List.toScalaList
import scalus.uplc.DefaultFun.*
import scalus.uplc.eval.ExBudgetCategory.BuiltinApp
import scalus.uplc.eval.{BudgetSpender, ExBudgetCategory, Logger, MachineParams}

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
                  BuiltinApp(AddInteger),
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
                  BuiltinApp(SubtractInteger),
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
                  BuiltinApp(MultiplyInteger),
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
                  BuiltinApp(LessThanInteger),
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
                  BuiltinApp(LessThanEqualsInteger),
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
                  BuiltinApp(EqualsInteger),
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
                  BuiltinApp(EqualsByteString),
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
                  BuiltinApp(ConsByteString),
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
                      BuiltinApp(SliceByteString),
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
                  BuiltinApp(IndexByteString),
                  params.builtinCostModel.indexByteString.constantCost,
                  Nil
                )
                Builtins.indexByteString(bs, i)
            }

    @inline def sha2_256(budget: BudgetSpender, params: MachineParams): ByteString => ByteString =
        (bs: ByteString) => {
            budget.spendBudget(
              BuiltinApp(Sha2_256),
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
                  BuiltinApp(EqualsData),
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
              BuiltinApp(UnConstrData),
              params.builtinCostModel.unConstrData.constantCost,
              Nil
            )
            RuntimeHelper.unConstrData(x)
        }

    @inline def unListData(budget: BudgetSpender, params: MachineParams): Data => List[Data] =
        (x: Data) => {
            budget.spendBudget(
              BuiltinApp(UnListData),
              params.builtinCostModel.unListData.constantCost,
              Nil
            )
            RuntimeHelper.unListData(x)
        }

    @inline def unIData(budget: BudgetSpender, params: MachineParams): Data => BigInt =
        (x: Data) => {
            budget.spendBudget(
              BuiltinApp(UnIData),
              params.builtinCostModel.unIData.constantCost,
              Nil
            )
            Builtins.unIData(x)
        }

    @inline def unBData(budget: BudgetSpender, params: MachineParams): Data => ByteString =
        (x: Data) => {
            budget.spendBudget(
              BuiltinApp(UnBData),
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
                  BuiltinApp(ConstrData),
                  params.builtinCostModel.constrData.constantCost,
                  Nil
                )
                Data.Constr(tag.longValue, PList.from(fields))
            }

    @inline def iData(budget: BudgetSpender, params: MachineParams): BigInt => Data =
        (x: BigInt) => {
            budget.spendBudget(
              BuiltinApp(IData),
              params.builtinCostModel.iData.constantCost,
              Nil
            )
            Data.I(x)
        }

    @inline def bData(budget: BudgetSpender, params: MachineParams): ByteString => Data =
        (x: ByteString) => {
            budget.spendBudget(
              BuiltinApp(BData),
              params.builtinCostModel.bData.constantCost,
              Nil
            )
            Data.B(x)
        }

    @inline def listData(budget: BudgetSpender, params: MachineParams): List[Data] => Data =
        (x: List[Data]) => {
            budget.spendBudget(
              BuiltinApp(ListData),
              params.builtinCostModel.listData.constantCost,
              Nil
            )
            Data.List(PList.from(x))
        }

    @inline def mapData(
        budget: BudgetSpender,
        params: MachineParams
    ): List[BuiltinPair[Data, Data]] => Data =
        (x: List[BuiltinPair[Data, Data]]) => {
            budget.spendBudget(
              BuiltinApp(MapData),
              params.builtinCostModel.mapData.constantCost,
              Nil
            )
            Data.Map(PList.from(x.map(p => (p.fst, p.snd))))
        }

    @inline def unMapData(
        budget: BudgetSpender,
        params: MachineParams
    ): Data => List[BuiltinPair[Data, Data]] =
        (x: Data) => {
            budget.spendBudget(
              BuiltinApp(UnMapData),
              params.builtinCostModel.unMapData.constantCost,
              Nil
            )
            x match
                case Data.Map(values) => values.toScalaList.map(BuiltinPair.apply)
                case _                => throw new Exception(s"not a map but $x")
        }

    @inline def serialiseData(budget: BudgetSpender, params: MachineParams): Data => ByteString =
        (x: Data) => {
            budget.spendBudget(
              BuiltinApp(SerialiseData),
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
                  BuiltinApp(MkPairData),
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
                                      BuiltinApp(ChooseData),
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
                      BuiltinApp(MkCons),
                      params.builtinCostModel.mkCons.constantCost,
                      Nil
                    )
                    head :: tail
                }

    @inline def nullList(budget: BudgetSpender, params: MachineParams): () => Any =
        () =>
            (list: List[Any]) => {
                budget.spendBudget(
                  BuiltinApp(NullList),
                  params.builtinCostModel.nullList.constantCost,
                  Nil
                )
                list.isEmpty
            }

    @inline def mkNilData(budget: BudgetSpender, params: MachineParams): Unit => List[Data] =
        (_: Unit) => {
            budget.spendBudget(
              BuiltinApp(MkNilData),
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
              BuiltinApp(MkNilPairData),
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
                          BuiltinApp(IfThenElse),
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
                      BuiltinApp(Trace),
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
                    budget.spendBudget(
                      BuiltinApp(FstPair),
                      params.builtinCostModel.fstPair.constantCost,
                      Nil
                    )
                    Builtins.fstPair(x)
                }

    @inline def sndPair(
        budget: BudgetSpender,
        params: MachineParams
    ): () => () => BuiltinPair[?, ?] => Any =
        () =>
            () =>
                (x: BuiltinPair[?, ?]) => {
                    budget.spendBudget(
                      BuiltinApp(SndPair),
                      params.builtinCostModel.sndPair.constantCost,
                      Nil
                    )
                    Builtins.sndPair(x)
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
                              BuiltinApp(ChooseList),
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
                  BuiltinApp(HeadList),
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
                  BuiltinApp(TailList),
                  params.builtinCostModel.tailList.constantCost,
                  Nil
                )
                xv.tail
            }

    def verifyEd25519Signature(
        budget: BudgetSpender,
        params: MachineParams
    ): ByteString => ByteString => ByteString => Boolean =
        (pk: ByteString) =>
            (msg: ByteString) =>
                (sig: ByteString) => {
                    budget.spendBudget(
                      BuiltinApp(VerifyEd25519Signature),
                      params.builtinCostModel.verifyEd25519Signature.calculateCostFromMemory(
                        Seq(
                          MemoryUsageJit.memoryUsage(pk),
                          MemoryUsageJit.memoryUsage(msg),
                          MemoryUsageJit.memoryUsage(sig)
                        )
                      ),
                      Nil
                    )
                    Builtins.verifyEd25519Signature(pk, msg, sig)
                }

    def verifyEcdsaSecp256k1Signature(
        budget: BudgetSpender,
        params: MachineParams
    ): ByteString => ByteString => ByteString => Boolean =
        (pk: ByteString) =>
            (msg: ByteString) =>
                (sig: ByteString) => {
                    budget.spendBudget(
                      BuiltinApp(VerifyEcdsaSecp256k1Signature),
                      params.builtinCostModel.verifyEcdsaSecp256k1Signature.constantCost,
                      Nil
                    )
                    Builtins.verifyEcdsaSecp256k1Signature(pk, msg, sig)
                }

    def verifySchnorrSecp256k1Signature(
        budget: BudgetSpender,
        params: MachineParams
    ): ByteString => ByteString => ByteString => Boolean =
        (pk: ByteString) =>
            (msg: ByteString) =>
                (sig: ByteString) => {
                    budget.spendBudget(
                      BuiltinApp(VerifySchnorrSecp256k1Signature),
                      params.builtinCostModel.verifySchnorrSecp256k1Signature
                          .calculateCostFromMemory(
                            Seq(
                              MemoryUsageJit.memoryUsage(pk),
                              MemoryUsageJit.memoryUsage(msg),
                              MemoryUsageJit.memoryUsage(sig)
                            )
                          ),
                      Nil
                    )
                    Builtins.verifySchnorrSecp256k1Signature(pk, msg, sig)
                }

    def chooseUnit(budget: BudgetSpender, params: MachineParams): () => Unit => Any =
        () =>
            (_: Unit) =>
                (caseUnit: Any) => {
                    budget.spendBudget(
                      BuiltinApp(ChooseUnit),
                      params.builtinCostModel.chooseUnit.constantCost,
                      Nil
                    )
                    caseUnit
                }

    // Additional integer operations (unapplied forms)

    @inline def divideInteger(
        budget: BudgetSpender,
        params: MachineParams
    ): BigInt => BigInt => BigInt =
        (x: BigInt) =>
            (y: BigInt) => {
                budget.spendBudget(
                  BuiltinApp(DivideInteger),
                  params.builtinCostModel.divideInteger.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(x),
                      MemoryUsageJit.memoryUsage(y)
                    )
                  ),
                  Nil
                )
                Builtins.divideInteger(x, y)
            }

    @inline def quotientInteger(
        budget: BudgetSpender,
        params: MachineParams
    ): BigInt => BigInt => BigInt =
        (x: BigInt) =>
            (y: BigInt) => {
                budget.spendBudget(
                  BuiltinApp(QuotientInteger),
                  params.builtinCostModel.quotientInteger.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(x),
                      MemoryUsageJit.memoryUsage(y)
                    )
                  ),
                  Nil
                )
                Builtins.quotientInteger(x, y)
            }

    @inline def remainderInteger(
        budget: BudgetSpender,
        params: MachineParams
    ): BigInt => BigInt => BigInt =
        (x: BigInt) =>
            (y: BigInt) => {
                budget.spendBudget(
                  BuiltinApp(RemainderInteger),
                  params.builtinCostModel.remainderInteger.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(x),
                      MemoryUsageJit.memoryUsage(y)
                    )
                  ),
                  Nil
                )
                Builtins.remainderInteger(x, y)
            }

    @inline def modInteger(
        budget: BudgetSpender,
        params: MachineParams
    ): BigInt => BigInt => BigInt =
        (x: BigInt) =>
            (y: BigInt) => {
                budget.spendBudget(
                  BuiltinApp(ModInteger),
                  params.builtinCostModel.modInteger.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(x),
                      MemoryUsageJit.memoryUsage(y)
                    )
                  ),
                  Nil
                )
                Builtins.modInteger(x, y)
            }

    // Additional ByteString operations (unapplied forms)

    @inline def appendByteString(
        budget: BudgetSpender,
        params: MachineParams
    ): ByteString => ByteString => ByteString =
        (x: ByteString) =>
            (y: ByteString) => {
                budget.spendBudget(
                  BuiltinApp(AppendByteString),
                  params.builtinCostModel.appendByteString.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(x),
                      MemoryUsageJit.memoryUsage(y)
                    )
                  ),
                  Nil
                )
                Builtins.appendByteString(x, y)
            }

    @inline def lessThanByteString(
        budget: BudgetSpender,
        params: MachineParams
    ): ByteString => ByteString => Boolean =
        (x: ByteString) =>
            (y: ByteString) => {
                budget.spendBudget(
                  BuiltinApp(LessThanByteString),
                  params.builtinCostModel.lessThanByteString.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(x),
                      MemoryUsageJit.memoryUsage(y)
                    )
                  ),
                  Nil
                )
                Builtins.lessThanByteString(x, y)
            }

    @inline def lessThanEqualsByteString(
        budget: BudgetSpender,
        params: MachineParams
    ): ByteString => ByteString => Boolean =
        (x: ByteString) =>
            (y: ByteString) => {
                budget.spendBudget(
                  BuiltinApp(LessThanEqualsByteString),
                  params.builtinCostModel.lessThanEqualsByteString.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(x),
                      MemoryUsageJit.memoryUsage(y)
                    )
                  ),
                  Nil
                )
                Builtins.lessThanEqualsByteString(x, y)
            }

    @inline def lengthOfByteString(
        budget: BudgetSpender,
        params: MachineParams
    ): ByteString => BigInt =
        (bs: ByteString) => {
            budget.spendBudget(
              BuiltinApp(LengthOfByteString),
              params.builtinCostModel.lengthOfByteString.constantCost,
              Nil
            )
            Builtins.lengthOfByteString(bs)
        }

    // Additional hash functions (unapplied forms)

    @inline def sha3_256(
        budget: BudgetSpender,
        params: MachineParams
    ): ByteString => ByteString =
        (bs: ByteString) => {
            budget.spendBudget(
              BuiltinApp(Sha3_256),
              params.builtinCostModel.sha3_256.calculateCostFromMemory(
                Seq(MemoryUsageJit.memoryUsage(bs))
              ),
              Nil
            )
            Builtins.sha3_256(bs)
        }

    @inline def blake2b_256(
        budget: BudgetSpender,
        params: MachineParams
    ): ByteString => ByteString =
        (bs: ByteString) => {
            budget.spendBudget(
              BuiltinApp(Blake2b_256),
              params.builtinCostModel.blake2b_256.calculateCostFromMemory(
                Seq(MemoryUsageJit.memoryUsage(bs))
              ),
              Nil
            )
            Builtins.blake2b_256(bs)
        }

    // Additional String operations (unapplied forms)

    @inline def appendString(
        budget: BudgetSpender,
        params: MachineParams
    ): String => String => String =
        (x: String) =>
            (y: String) => {
                budget.spendBudget(
                  BuiltinApp(AppendString),
                  params.builtinCostModel.appendString.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(x),
                      MemoryUsageJit.memoryUsage(y)
                    )
                  ),
                  Nil
                )
                Builtins.appendString(x, y)
            }

    @inline def equalsString(
        budget: BudgetSpender,
        params: MachineParams
    ): String => String => Boolean =
        (x: String) =>
            (y: String) => {
                budget.spendBudget(
                  BuiltinApp(EqualsString),
                  params.builtinCostModel.equalsString.calculateCostFromMemory(
                    Seq(
                      MemoryUsageJit.memoryUsage(x),
                      MemoryUsageJit.memoryUsage(y)
                    )
                  ),
                  Nil
                )
                Builtins.equalsString(x, y)
            }

    @inline def encodeUtf8(
        budget: BudgetSpender,
        params: MachineParams
    ): String => ByteString =
        (s: String) => {
            budget.spendBudget(
              BuiltinApp(EncodeUtf8),
              params.builtinCostModel.encodeUtf8.calculateCostFromMemory(
                Seq(MemoryUsageJit.memoryUsage(s))
              ),
              Nil
            )
            Builtins.encodeUtf8(s)
        }

    @inline def decodeUtf8(
        budget: BudgetSpender,
        params: MachineParams
    ): ByteString => String =
        (bs: ByteString) => {
            budget.spendBudget(
              BuiltinApp(DecodeUtf8),
              params.builtinCostModel.decodeUtf8.calculateCostFromMemory(
                Seq(MemoryUsageJit.memoryUsage(bs))
              ),
              Nil
            )
            Builtins.decodeUtf8(bs)
        }

}
