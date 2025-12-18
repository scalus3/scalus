package scalus.uplc.jit.mincont

import scalus.*
import scalus.builtin.*
import scalus.prelude.List as PList
import scalus.prelude.List.toScalaList
import scalus.uplc.eval.*
import scalus.uplc.eval.ExBudgetCategory.{Startup, Step}
import scalus.uplc.jit.*
import scalus.uplc.{Constant, Term}

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
                val argsExpr = Expr.ofList(args.toScalaList.map(apply))
                '{ Data.Constr($tagExpr, PList.from($argsExpr)) }
            case Data.List(value) =>
                val valueExpr = Expr.ofList(value.toScalaList.map(apply))
                '{ Data.List(PList.from($valueExpr)) }
            case Data.Map(values) =>
                val argsListOfExprTuple = values.toScalaList.map { case (k, v) =>
                    Expr.ofTuple(apply(k), apply(v))
                }
                val argsExpr = Expr.ofList(argsListOfExprTuple)
                '{ Data.Map(PList.from($argsExpr)) }
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
                Expr.ofList(value.map(constantToExpr))
            case Constant.Array(elemType, value) =>
                // Arrays are represented as Vector[Any] at runtime
                val elements = value.map(constantToExpr)
                '{ Vector(${ Varargs(elements) }*) }
            case Constant.Pair(a, b) =>
                '{ BuiltinPair(${ constantToExpr(a) }, ${ constantToExpr(b) }) }
                // Expr.ofTuple(constantToExpr(a), constantToExpr(b))
            case Constant.BLS12_381_G1_Element(value) =>
                '{ BLS12_381_G1_Element(${ Expr(value.toCompressedByteString) }) }
            case Constant.BLS12_381_G2_Element(value) =>
                '{ BLS12_381_G2_Element(${ Expr(value.toCompressedByteString) }) }
            case Constant.BLS12_381_MlResult(value) =>
                throw JitEvaluationFailure(
                  "BLS12_381_MlResult values cannot be serialized as constants in UPLC"
                )
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
                                val result = BuiltinEmitter.emitAppliedBuiltin1(
                                  bn,
                                  '{ ${ argCode }.asInstanceOf[Return].value },
                                  budget,
                                  params
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
                                '{
                                    Return(${
                                        BuiltinEmitter.emitAppliedBuiltin2(
                                          bn,
                                          '{ ${ arg1Code }.asInstanceOf[Return].value },
                                          '{ ${ argCode }.asInstanceOf[Return].value },
                                          budget,
                                          params
                                        )
                                    })
                                }
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
                case Term.Builtin(bn) =>
                    '{ Return(${ BuiltinEmitter.emitBuiltin(bn, logger, budget, params) }) }
                case Term.Error =>
                    '{ throw new JitEvaluationFailure("UPLC Error term evaluated") }
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
                    val scrutineeCont = genCode(arg, env, logger, budget, params)
                    val branchExprs: List[Expr[ContinuationJitRepr]] =
                        cases.map(c => genCode(c, env, logger, budget, params))
                    '{
                        $budget.spendBudget(Step(StepKind.Case), $params.machineCosts.caseCost, Nil)
                        ${
                            CaseHelper.genCaseDispatchCont(
                              scrutineeCont,
                              branchExprs
                            )
                        }
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
