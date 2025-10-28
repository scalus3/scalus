package scalus.uplc.eval.mincont

import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, BuiltinPair, Builtins, ByteString, Data}
import scalus.uplc.eval.*
import scalus.uplc.{Constant, DefaultFun, Term}
import scalus.uplc.DefaultUni.asConstant
import scalus.*
import scalus.uplc.DefaultFun.AddInteger
import scalus.uplc.eval.ExBudgetCategory.{Startup, Step}
import scalus.uplc.eval.jitcommon.*

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
object JIT extends JitRunner {
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
                    '{
                        Return((arg: Any) =>
                            ${ genCode(term, (name -> 'arg.asTerm) :: env, logger, budget, params) }
                        )
                    }

                case Term.Apply(fun, arg) =>
                    // Optimize: detect fully-applied 1-argument builtins with simple arg
                    if UplcTermHelper.isApplyBuiltin1WithSimpleArg(term) then {
                        fun match
                            case Term.Builtin(bn) if BuiltinAppliedGenerator.isSupported1(bn) =>
                                val argCode = genCode(arg, env, logger, budget, params)
                                // Generate inline code for one-argument builtins
                                val result = bn match
                                    case DefaultFun.Sha2_256 =>
                                        BuiltinAppliedGenerator.sha2_256(
                                          '{ ${ argCode }.asInstanceOf[Return].value },
                                          budget,
                                          params
                                        )
                                    case DefaultFun.Sha3_256 =>
                                        BuiltinAppliedGenerator.sha3_256(
                                          '{ ${ argCode }.asInstanceOf[Return].value },
                                          budget,
                                          params
                                        )
                                    case DefaultFun.Blake2b_256 =>
                                        BuiltinAppliedGenerator.blake2b_256(
                                          '{ ${ argCode }.asInstanceOf[Return].value },
                                          budget,
                                          params
                                        )
                                    case DefaultFun.UnConstrData =>
                                        BuiltinAppliedGenerator.unConstrData(
                                          '{ ${ argCode }.asInstanceOf[Return].value },
                                          budget,
                                          params
                                        )
                                    case DefaultFun.UnListData =>
                                        BuiltinAppliedGenerator.unListData(
                                          '{ ${ argCode }.asInstanceOf[Return].value },
                                          budget,
                                          params
                                        )
                                    case DefaultFun.UnIData =>
                                        BuiltinAppliedGenerator.unIData(
                                          '{ ${ argCode }.asInstanceOf[Return].value },
                                          budget,
                                          params
                                        )
                                    case DefaultFun.UnBData =>
                                        BuiltinAppliedGenerator.unBData(
                                          '{ ${ argCode }.asInstanceOf[Return].value },
                                          budget,
                                          params
                                        )
                                    case DefaultFun.LengthOfByteString =>
                                        BuiltinAppliedGenerator.lengthOfByteString(
                                          '{ ${ argCode }.asInstanceOf[Return].value },
                                          budget,
                                          params
                                        )
                                    case DefaultFun.EncodeUtf8 =>
                                        BuiltinAppliedGenerator.encodeUtf8(
                                          '{ ${ argCode }.asInstanceOf[Return].value },
                                          budget,
                                          params
                                        )
                                    case DefaultFun.DecodeUtf8 =>
                                        BuiltinAppliedGenerator.decodeUtf8(
                                          '{ ${ argCode }.asInstanceOf[Return].value },
                                          budget,
                                          params
                                        )
                                    case DefaultFun.HeadList =>
                                        BuiltinAppliedGenerator.headList(
                                          '{ ${ argCode }.asInstanceOf[Return].value },
                                          budget,
                                          params
                                        )
                                    case DefaultFun.TailList =>
                                        BuiltinAppliedGenerator.tailList(
                                          '{ ${ argCode }.asInstanceOf[Return].value },
                                          budget,
                                          params
                                        )
                                    case _ =>
                                        throw IllegalStateException(
                                          s"Short circuit optimization for 1-arg builtin $bn not implemented"
                                        )
                                '{ Return($result) }
                            case _ =>
                                throw IllegalStateException(
                                  s"isApplyBuiltin1WithSimpleArg returned true but term has wrong shape"
                                )
                    }
                    // Optimize: if both fun and arg are simple (Return-producing),
                    // generate direct application instead of Apply continuation
                    else if UplcTermHelper.isSimpleTerm(fun) && UplcTermHelper.isSimpleTerm(arg)
                    then {
                        val funCode = genCode(fun, env, logger, budget, params)
                        val argCode = genCode(arg, env, logger, budget, params)
                        '{
                            $budget.spendBudget(
                              Step(StepKind.Apply),
                              $params.machineCosts.applyCost,
                              Nil
                            )
                            // Both are Return values, apply directly
                            val f = $funCode.asInstanceOf[Return].value.asInstanceOf[Any => Any]
                            val a = $argCode.asInstanceOf[Return].value
                            val result = f(a)
                            // Result might be continuation or value
                            result match {
                                case cont: ContinuationJitRepr => cont
                                case v                         => Return(v)
                            }
                        }
                    } else if UplcTermHelper.isApplyBuiltin2WithSimpleArgs(term) then {
                        val argCode = genCode(arg, env, logger, budget, params)
                        fun match
                            case Term.Apply(Term.Builtin(bn), arg1)
                                if BuiltinAppliedGenerator.isSupported(bn) =>
                                val arg1Code = genCode(arg1, env, logger, budget, params)
                                bn match
                                    case DefaultFun.AddInteger =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.addInteger(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    case DefaultFun.SubtractInteger =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.subtractInteger(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    case DefaultFun.MultiplyInteger =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.multiplyInteger(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    case DefaultFun.EqualsInteger =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.equalsInteger(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    case DefaultFun.LessThanInteger =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.lessThanInteger(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    case DefaultFun.LessThanEqualsInteger =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.lessThanEqualsInteger(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    case DefaultFun.EqualsByteString =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.equalsByteString(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    case DefaultFun.EqualsData =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.equalsData(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    // Additional integer operations
                                    case DefaultFun.DivideInteger =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.divideInteger(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    case DefaultFun.QuotientInteger =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.quotientInteger(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    case DefaultFun.RemainderInteger =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.remainderInteger(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    case DefaultFun.ModInteger =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.modInteger(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    // ByteString operations
                                    case DefaultFun.AppendByteString =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.appendByteString(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    case DefaultFun.LessThanByteString =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.lessThanByteString(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    case DefaultFun.LessThanEqualsByteString =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.lessThanEqualsByteString(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    // String operations
                                    case DefaultFun.AppendString =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.appendString(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    case DefaultFun.EqualsString =>
                                        '{
                                            Return(${
                                                BuiltinAppliedGenerator.equalsString(
                                                  '{ ${ arg1Code }.asInstanceOf[Return].value },
                                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                                  budget,
                                                  params
                                                )
                                            })
                                        }
                                    case _ => // impossible
                                        throw IllegalStateException(
                                          s"Short circuit optimization for builtin $bn not implemented"
                                        )
                            case _ => // imoissibke, we have check
                                throw IllegalStateException(
                                  s"isApplyBuiltin2 for ${term} returns true but term is wrong shape"
                                )
                    } else {
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
                    '{ Return(BuiltinSnippets.addInteger($budget, $params)) }
                case Term.Builtin(DefaultFun.SubtractInteger) =>
                    '{ Return(BuiltinSnippets.subtractInteger($budget, $params)) }
                case Term.Builtin(DefaultFun.MultiplyInteger) =>
                    '{ Return(BuiltinSnippets.multiplyInteger($budget, $params)) }
                case Term.Builtin(DefaultFun.EqualsData) =>
                    '{ Return(BuiltinSnippets.equalsData($budget, $params)) }
                case Term.Builtin(DefaultFun.LessThanInteger) =>
                    '{ Return(BuiltinSnippets.lessThanInteger($budget, $params)) }
                case Term.Builtin(DefaultFun.LessThanEqualsInteger) =>
                    '{ Return(BuiltinSnippets.lessThanEqualsInteger($budget, $params)) }
                case Term.Builtin(DefaultFun.EqualsInteger) =>
                    '{ Return(BuiltinSnippets.equalsInteger($budget, $params)) }
                case Term.Builtin(DefaultFun.EqualsByteString) =>
                    '{ Return(BuiltinSnippets.equalsByteString($budget, $params)) }
                case Term.Builtin(DefaultFun.IfThenElse) =>
                    '{ Return(BuiltinSnippets.ifThenElse($budget, $params)) }
                case Term.Builtin(DefaultFun.Trace) =>
                    '{ Return(BuiltinSnippets.trace($logger, $budget, $params)) }
                case Term.Builtin(DefaultFun.FstPair) =>
                    '{ Return(BuiltinSnippets.fstPair($budget, $params)) }
                case Term.Builtin(DefaultFun.SndPair) =>
                    '{ Return(BuiltinSnippets.sndPair($budget, $params)) }
                case Term.Builtin(DefaultFun.ChooseList) =>
                    '{ Return(BuiltinSnippets.chooseList($budget, $params)) }
                case Term.Builtin(DefaultFun.Sha2_256) =>
                    '{ Return(BuiltinSnippets.sha2_256($budget, $params)) }
                case Term.Builtin(DefaultFun.HeadList) =>
                    '{ Return(BuiltinSnippets.headList($budget, $params)) }
                case Term.Builtin(DefaultFun.TailList) =>
                    '{ Return(BuiltinSnippets.tailList($budget, $params)) }
                case Term.Builtin(DefaultFun.UnConstrData) =>
                    '{ Return(BuiltinSnippets.unConstrData($budget, $params)) }
                case Term.Builtin(DefaultFun.UnListData) =>
                    '{ Return(BuiltinSnippets.unListData($budget, $params)) }
                case Term.Builtin(DefaultFun.UnIData) =>
                    '{ Return(BuiltinSnippets.unIData($budget, $params)) }
                case Term.Builtin(DefaultFun.UnBData) =>
                    '{ Return(BuiltinSnippets.unBData($budget, $params)) }
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

    override def jitUplc(term: Term): (Logger, BudgetSpender, MachineParams) => Any =
        val result = staging.run { (quotes: Quotes) ?=>
            val expr = genCodeFromTerm(term)
            expr
        }
        result

    override def isStackSafe = true
}
