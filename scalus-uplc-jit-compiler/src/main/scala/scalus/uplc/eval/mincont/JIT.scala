package scalus.uplc.eval.mincont

import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, BuiltinPair, Builtins, ByteString, Data}
import scalus.uplc.eval.*
import scalus.uplc.{Constant, DefaultFun, Term}
import scalus.uplc.DefaultUni.asConstant
import scalus.*
import scalus.uplc.eval.ExBudgetCategory.{Startup, Step}
import scalus.uplc.eval.jitcommon.MemoryUsageJit

import scala.quoted.*

/** Just-In-Time compiler for UPLC (Untyped Plutus Core) terms.
  *
  * This object provides functionality to compile UPLC terms into optimized JVM bytecode at runtime
  * using Scala 3's staging capabilities. The JIT compilation can significantly improve execution
  * performance for repeatedly evaluated UPLC programs by eliminating the interpretation overhead.
  *
  * The JIT compiler transforms UPLC terms into native Scala functions that can be executed directly
  * on the JVM, while maintaining compatibility with the Plutus VM evaluation semantics including
  * budget tracking and logging.
  *
  * @note
  *   This is an experimental feature that requires scala3-staging and scala3-compiler dependencies.
  */
object JIT {
    private given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

    private given ByteStringToExpr: ToExpr[ByteString] with {
        def apply(x: ByteString)(using Quotes): Expr[ByteString] =
            '{ ByteString.fromArray(${ Expr(x.bytes) }) }
    }

    private given DataToExpr: ToExpr[Data] with {
        def apply(x: Data)(using Quotes): Expr[Data] = x match
            case Data.Constr(tag, args) =>
                val tagExpr = Expr(tag)
                val argsExpr = Expr.ofList(args.map(apply))
                '{ Data.Constr($tagExpr, $argsExpr) }
            case Data.List(value) =>
                val valueExpr = Expr.ofList(value.map(apply))
                '{ Data.List($valueExpr) }
            case Data.Map(values) =>
                val argsListOfExprTuple = values.map { case (k, v) =>
                    Expr.ofTuple(apply(k), apply(v))
                }
                val argsExpr = Expr.ofList(argsListOfExprTuple)
                '{ Data.Map($argsExpr) }
            case Data.I(value) => '{ Data.I(${ Expr(value) }) }
            case Data.B(value) => '{ Data.B(${ Expr(value) }) }
    }

    private def constantToExpr(const: Constant)(using Quotes): Expr[Any] = {
        const match
            case Constant.Integer(value)        => Expr(value)
            case Constant.ByteString(value)     => Expr(value)
            case Constant.String(value)         => Expr(value)
            case Constant.Unit                  => '{ () }
            case Constant.Bool(value)           => Expr(value)
            case Constant.Data(value)           => Expr(value)
            case Constant.List(elemType, value) =>
                // Lists are represented as plain Scala List[Any] at runtime
                // No need to track element type - only used for serialization
                '{ ${ Expr.ofList(value.map(constantToExpr)) } }
            case Constant.Pair(a, b) =>
                '{ BuiltinPair(${ constantToExpr(a) }, ${ constantToExpr(b) }) }
                // Expr.ofTuple(constantToExpr(a), constantToExpr(b))
            case Constant.BLS12_381_G1_Element(value) =>
                '{ BLS12_381_G1_Element(${ Expr(value.toCompressedByteString) }) }
            case Constant.BLS12_381_G2_Element(value) =>
                '{ BLS12_381_G2_Element(${ Expr(value.toCompressedByteString) }) }
            case Constant.BLS12_381_MlResult(value) =>
                sys.error("BLS12_381_MlResult values cannot be serialized as constants in UPLC")
    }

    enum FunType:
        case Lam(f: Any => Any)
        case Builtin(f: Any => Any)

    private def genCodeFromTerm(
        term: Term
    )(using Quotes): Expr[(Logger, BudgetSpender, MachineParams) => Any] = {
        import quotes.reflect.asTerm
        import ContinuationJitRepr.{Apply, Force, Return}

        val debugMode = false // Set to true to enable exception tracking

        def wrapWithDebug(
            code: Expr[ContinuationJitRepr],
            termDescription: String
        ): Expr[ContinuationJitRepr] = {
            if debugMode then
                val desc = Expr(termDescription)
                '{
                    try {
                        $code
                    } catch {
                        case e: ClassCastException =>
                            println("[DEBUG] ClassCastException in generated code for: " + $desc)
                            println("[DEBUG] Exception: " + e.getMessage)
                            println("[DEBUG] Stack trace:")
                            e.getStackTrace().take(10).foreach(st => println("  " + st))
                            throw e
                        case e: Throwable =>
                            println("[DEBUG] Exception in generated code for: " + $desc)
                            println(
                              "[DEBUG] Exception: " + e.getClass.getName + ": " + e.getMessage
                            )
                            throw e
                    }
                }
            else code
        }

        def genCode(
            term: Term,
            env: List[(String, quotes.reflect.Term)],
            logger: Expr[Logger],
            budget: Expr[BudgetSpender],
            params: Expr[MachineParams]
        ): Expr[ContinuationJitRepr] = {
            val termDesc = term.getClass.getSimpleName match {
                case "Var"     => s"Var(${term.asInstanceOf[Term.Var].name})"
                case "Builtin" => s"Builtin(${term.asInstanceOf[Term.Builtin].bn})"
                case other     => other
            }
            val result = term match
                case Term.Var(name) =>
                    val vr = env.find(_._1 == name.name).get._2.asExprOf[Any]
                    '{
                        $budget.spendBudget(Step(StepKind.Var), $params.machineCosts.varCost, Nil)
                        Return($vr)
                    }

                case Term.LamAbs(name, term) =>
                    // Lambda returns a function wrapped in Return
                    // No stack overflow check needed - continuations are on the heap!
                    '{
                        Return((arg: Any) =>
                            ${ genCode(term, (name -> 'arg.asTerm) :: env, logger, budget, params) }
                        )
                    }

                case Term.Apply(fun, arg) =>
                    // Create Apply continuation - flattened to the top-level loop
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Apply),
                          $params.machineCosts.applyCost,
                          Nil
                        )
                        Apply(
                          ${ genCode(fun, env, logger, budget, params) },
                          ${ genCode(arg, env, logger, budget, params) }
                        )
                    }

                case Term.Force(term) =>
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Force),
                          $params.machineCosts.forceCost,
                          Nil
                        )
                        Force(${ genCode(term, env, logger, budget, params) })
                    }

                case Term.Delay(term) =>
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Delay),
                          $params.machineCosts.delayCost,
                          Nil
                        )
                        Return.delayed(${ genCode(term, env, logger, budget, params) })
                    }

                case Term.Const(const) =>
                    val expr = constantToExpr(const)
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Const),
                          $params.machineCosts.constCost,
                          Nil
                        )
                        Return($expr)
                    }
                case Term.Builtin(DefaultFun.AddInteger) =>
                    '{
                        Return((x: Any) =>
                            (y: Any) => {
                                val xv = x.asInstanceOf[BigInt]
                                val yv = y.asInstanceOf[BigInt]
                                $budget.spendBudget(
                                  Step(StepKind.Builtin),
                                  $params.builtinCostModel.addInteger
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
                        )
                    }
                case Term.Builtin(DefaultFun.SubtractInteger) =>
                    '{
                        Return((x: Any) =>
                            (y: Any) => {
                                val xv = x.asInstanceOf[BigInt]
                                val yv = y.asInstanceOf[BigInt]
                                $budget.spendBudget(
                                  Step(StepKind.Builtin),
                                  $params.builtinCostModel.subtractInteger
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
                        )
                    }
                case Term.Builtin(DefaultFun.MultiplyInteger) =>
                    '{
                        Return((x: Any) =>
                            (y: Any) => {
                                val xv = x.asInstanceOf[BigInt]
                                val yv = y.asInstanceOf[BigInt]
                                $budget.spendBudget(
                                  Step(StepKind.Builtin),
                                  $params.builtinCostModel.multiplyInteger
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
                        )
                    }
                case Term.Builtin(DefaultFun.EqualsData) =>
                    '{
                        Return((x: Any) =>
                            (y: Any) => {
                                val xv = x.asInstanceOf[Data]
                                val yv = y.asInstanceOf[Data]
                                $budget.spendBudget(
                                  Step(StepKind.Builtin),
                                  $params.builtinCostModel.equalsData
                                      .calculateCostFromMemory(
                                        Seq(
                                          MemoryUsageJit.memoryUsage(xv),
                                          MemoryUsageJit.memoryUsage(yv)
                                        )
                                      ),
                                  Nil
                                )
                                Builtins.equalsData(xv, yv)
                            }
                        )
                    }
                case Term.Builtin(DefaultFun.LessThanInteger) =>
                    '{
                        Return((x: Any) =>
                            (y: Any) => {
                                val xv = x.asInstanceOf[BigInt]
                                val yv = y.asInstanceOf[BigInt]
                                $budget.spendBudget(
                                  Step(StepKind.Builtin),
                                  $params.builtinCostModel.lessThanInteger
                                      .calculateCostFromMemory(
                                        Seq(
                                          MemoryUsageJit.memoryUsage(xv),
                                          MemoryUsageJit.memoryUsage(yv)
                                        )
                                      ),
                                  Nil
                                )
                                xv < yv
                            }
                        )
                    }
                case Term.Builtin(DefaultFun.LessThanEqualsInteger) =>
                    '{
                        Return((x: Any) =>
                            (y: Any) => {
                                val xv = x.asInstanceOf[BigInt]
                                val yv = y.asInstanceOf[BigInt]
                                $budget.spendBudget(
                                  Step(StepKind.Builtin),
                                  $params.builtinCostModel.lessThanEqualsInteger
                                      .calculateCostFromMemory(
                                        Seq(
                                          MemoryUsageJit.memoryUsage(xv),
                                          MemoryUsageJit.memoryUsage(yv)
                                        )
                                      ),
                                  Nil
                                )
                                xv <= yv
                            }
                        )
                    }
                case Term.Builtin(DefaultFun.EqualsInteger) =>
                    '{
                        Return((x: Any) =>
                            (y: Any) => {
                                val xv = x.asInstanceOf[BigInt]
                                val yv = y.asInstanceOf[BigInt]
                                $budget.spendBudget(
                                  Step(StepKind.Builtin),
                                  $params.builtinCostModel.equalsInteger
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
                        )
                    }
                case Term.Builtin(DefaultFun.EqualsByteString) =>
                    '{
                        Return((x: Any) =>
                            (y: Any) => {
                                val xv = x.asInstanceOf[ByteString]
                                val yv = y.asInstanceOf[ByteString]
                                $budget.spendBudget(
                                  Step(StepKind.Builtin),
                                  $params.builtinCostModel.equalsByteString
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
                        )
                    }
                case Term.Builtin(DefaultFun.IfThenElse) =>
                    '{
                        Return(() =>
                            (c: Any) =>
                                (t: Any) =>
                                    (f: Any) => {
                                        val cv = c.asInstanceOf[Boolean]
                                        $budget.spendBudget(
                                          Step(StepKind.Builtin),
                                          $params.builtinCostModel.ifThenElse.constantCost,
                                          Nil
                                        )
                                        if cv then t else f
                                    }
                        )
                    }
                case Term.Builtin(DefaultFun.Trace) =>
                    '{
                        Return(() =>
                            (s: Any) =>
                                (a: Any) => {
                                    val sv = s.asInstanceOf[String]
                                    $budget.spendBudget(
                                      Step(StepKind.Builtin),
                                      $params.builtinCostModel.trace.constantCost,
                                      Nil
                                    )
                                    ${ logger }.log(sv)
                                    a
                                }
                        )
                    }
                case Term.Builtin(DefaultFun.FstPair) =>
                    '{
                        Return(() =>
                            () =>
                                (x: Any) => {
                                    val xv = x.asInstanceOf[BuiltinPair[?, ?]]
                                    $budget.spendBudget(
                                      Step(StepKind.Builtin),
                                      $params.builtinCostModel.fstPair.constantCost,
                                      Nil
                                    )
                                    Builtins.fstPair(xv)
                                }
                        )
                    }
                case Term.Builtin(DefaultFun.SndPair) =>
                    '{
                        Return(() =>
                            () =>
                                (x: Any) => {
                                    val xv = x.asInstanceOf[BuiltinPair[?, ?]]
                                    $budget.spendBudget(
                                      Step(StepKind.Builtin),
                                      $params.builtinCostModel.sndPair.constantCost,
                                      Nil
                                    )
                                    Builtins.sndPair(xv)
                                }
                        )
                    }
                case Term.Builtin(DefaultFun.ChooseList) =>
                    '{
                        Return(() =>
                            () =>
                                (l: Any) =>
                                    (e: Any) =>
                                        (ne: Any) => {
                                            val lv = l.asInstanceOf[List[?]]
                                            $budget.spendBudget(
                                              Step(StepKind.Builtin),
                                              $params.builtinCostModel.chooseList.constantCost,
                                              Nil
                                            )
                                            if lv.isEmpty then e else ne
                                        }
                        )
                    }
                case Term.Builtin(DefaultFun.Sha2_256) =>
                    '{
                        Return((bs: Any) => {
                            val bsv = bs.asInstanceOf[ByteString]
                            $budget.spendBudget(
                              Step(StepKind.Builtin),
                              $params.builtinCostModel.sha2_256
                                  .calculateCostFromMemory(
                                    Seq(MemoryUsageJit.memoryUsage(bsv))
                                  ),
                              Nil
                            )
                            Builtins.sha2_256(bsv)
                        })
                    }
                case Term.Builtin(DefaultFun.HeadList) =>
                    '{
                        Return(() =>
                            (y: Any) => {
                                val yv = y.asInstanceOf[List[?]]
                                $budget.spendBudget(
                                  Step(StepKind.Builtin),
                                  $params.builtinCostModel.headList.constantCost,
                                  Nil
                                )
                                yv.head
                            }
                        )
                    }
                case Term.Builtin(DefaultFun.TailList) =>
                    '{
                        Return(() =>
                            (x: Any) => {
                                val xv = x.asInstanceOf[List[?]]
                                $budget.spendBudget(
                                  Step(StepKind.Builtin),
                                  $params.builtinCostModel.tailList.constantCost,
                                  Nil
                                )
                                xv.tail
                            }
                        )
                    }
                case Term.Builtin(DefaultFun.UnConstrData) =>
                    '{
                        Return((x: Any) => {
                            val xv = x.asInstanceOf[Data]
                            $budget.spendBudget(
                              Step(StepKind.Builtin),
                              $params.builtinCostModel.unConstrData.constantCost,
                              Nil
                            )
                            RuntimeHelper.unConstrData(xv)
                        })
                    }
                case Term.Builtin(DefaultFun.UnListData) =>
                    '{
                        Return((x: Any) => {
                            val xv = x.asInstanceOf[Data]
                            $budget.spendBudget(
                              Step(StepKind.Builtin),
                              $params.builtinCostModel.unListData.constantCost,
                              Nil
                            )
                            RuntimeHelper.unListData(xv)
                        })
                    }
                case Term.Builtin(DefaultFun.UnIData) =>
                    '{
                        Return((x: Any) => {
                            val xv = x.asInstanceOf[Data]
                            $budget.spendBudget(
                              Step(StepKind.Builtin),
                              $params.builtinCostModel.unIData.constantCost,
                              Nil
                            )
                            Builtins.unIData(xv)
                        })
                    }
                case Term.Builtin(DefaultFun.UnBData) =>
                    '{
                        Return((x: Any) => {
                            val xv = x.asInstanceOf[Data]
                            $budget.spendBudget(
                              Step(StepKind.Builtin),
                              $params.builtinCostModel.unBData.constantCost,
                              Nil
                            )
                            Builtins.unBData(xv)
                        })
                    }
                case Term.Builtin(bi) =>
                    sys.error(
                      s"Builtin $bi is not yet supported by the JIT compiler. Please add implementation in the Builtin pattern matching section."
                    )
                case Term.Error =>
                    '{ throw new RuntimeException("UPLC Error term evaluated") }
                case Term.Constr(tag, args) =>
                    // TODO: Implement Constr with continuation support
                    // For now, fall back to simple evaluation
                    val argsExprs = args.map(a => genCode(a, env, logger, budget, params))
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Constr),
                          $params.machineCosts.constrCost,
                          Nil
                        )
                        // Evaluate all args first, then construct
                        val evaluatedArgs = List(${
                            Expr.ofSeq(argsExprs.map(e => '{ ContinuationJitRepr.eval($e) }))
                        }*)
                        Return((${ Expr(tag.value) }, evaluatedArgs))
                    }
                case Term.Case(arg, cases) =>
                    // TODO: Implement Case with continuation support
                    // For now, fall back to simple evaluation
                    val constrCont = genCode(arg, env, logger, budget, params)
                    val caseFuncs = Expr.ofList(
                      cases.map(c => genCode(c, env, logger, budget, params))
                    )
                    '{
                        $budget.spendBudget(Step(StepKind.Case), $params.machineCosts.caseCost, Nil)
                        val constr =
                            ContinuationJitRepr.eval($constrCont).asInstanceOf[(Long, List[Any])]
                        val (tag, args) = constr
                        val caseFunc = $caseFuncs(tag.toInt)
                        // Apply all args to the case function
                        args.foldLeft[ContinuationJitRepr](caseFunc)((f, a) => Apply(f, Return(a)))
                    }
            wrapWithDebug(result, termDesc)
        }

        val retval = '{ (logger: Logger, budget: BudgetSpender, params: MachineParams) =>
            budget.spendBudget(Startup, params.machineCosts.startupCost, Nil)
            val initialCont = ${ genCode(term, Nil, 'logger, 'budget, 'params) }
            ContinuationJitRepr.eval(initialCont)
        }

        retval
    }

    /** Compiles a UPLC term into an optimized JVM function using JIT compilation.
      *
      * This method takes a UPLC term and generates optimized JVM bytecode that can be executed
      * directly without interpretation overhead. The resulting function maintains full
      * compatibility with Plutus VM semantics including proper budget tracking, logging, and error
      * handling.
      *
      * @param term
      *   The UPLC term to compile
      * @return
      *   A function that takes a Logger, BudgetSpender, and MachineParams and returns the
      *   evaluation result. The function signature is:
      *   `(Logger, BudgetSpender, MachineParams) => Any`
      *
      * @example
      *   {{{
      * val term: Term = ... // some UPLC term
      * val jittedFunction = JIT.jitUplc(term)
      * val result = jittedFunction(logger, budgetSpender, machineParams)
      *   }}}
      *
      * @note
      *   The compilation happens at runtime and may take some time for complex terms. The compiled
      *   function can then be cached and reused for better performance when evaluating the same
      *   term multiple times.
      *
      * @throws RuntimeException
      *   if the term contains unsupported constructs or if compilation fails
      */
    def jitUplc(term: Term): (Logger, BudgetSpender, MachineParams) => Any =
        val result = staging.run { (quotes: Quotes) ?=>
            val expr = genCodeFromTerm(term)
            expr
        }
        result
}
