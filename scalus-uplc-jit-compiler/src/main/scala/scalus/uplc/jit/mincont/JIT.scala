package scalus.uplc.jit.mincont

import scalus.*
import scalus.uplc.builtin.*
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.onchain.plutus.prelude.List.toScalaList
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
                '{
                    scalus.uplc.builtin.bls12_381.G1Element(${ Expr(value.toCompressedByteString) })
                }
            case Constant.BLS12_381_G2_Element(value) =>
                '{
                    scalus.uplc.builtin.bls12_381.G2Element(${ Expr(value.toCompressedByteString) })
                }
            case Constant.BLS12_381_MlResult(value) =>
                throw JitEvaluationFailure(
                  "BLS12_381_MlResult values cannot be serialized as constants in UPLC"
                )
            case Constant.BuiltinValue(value) =>
                val dataExpr: Expr[Data] = Expr(BuiltinValue.toData(value))
                '{ BuiltinValue.fromData($dataExpr) }
    }

    enum FunType:
        case Lam(f: Any => Any)
        case Builtin(f: Any => Any)

    private[jit] def genCodeFromTerm(
        term: Term
    )(using Quotes): Expr[(Logger, BudgetSpender, MachineParams) => Any] = {
        import ContinuationJitRepr.{Apply, Force, Return}

        val debugMode = false // Set to true to enable exception tracking

        def wrapWithDebug(
            code: Expr[ContinuationJitRepr],
            termDescription: String
        )(using Quotes): Expr[ContinuationJitRepr] = {
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

        /** Generates code for `term`.
          *
          * Takes the `Quotes` of the splice it is called from, and every recursive call sits inside
          * the splice of its result. So each piece of code is created with its final owner. A
          * finished `Expr` spliced under a `val` or a lambda is copied to change its owner, which
          * makes the generation quadratic in the nesting depth of the term.
          */
        def genCode(
            term: Term,
            env: List[(String, Expr[Any])],
            logger: Expr[Logger],
            budget: Expr[BudgetSpender],
            params: Expr[MachineParams]
        )(using Quotes): Expr[ContinuationJitRepr] = {
            def gen(t: Term)(using Quotes): Expr[ContinuationJitRepr] =
                genCode(t, env, logger, budget, params)

            /** The value of a simple (Return-producing) term. */
            def genValue(t: Term)(using Quotes): Expr[Any] =
                '{ ${ gen(t) }.asInstanceOf[Return].value }

            val termDesc = term.getClass.getSimpleName match {
                case "Var"     => s"Var(${term.asInstanceOf[Term.Var].name})"
                case "Builtin" => s"Builtin(${term.asInstanceOf[Term.Builtin].bn})"
                case other     => other
            }
            val result = term match
                case Term.Var(name, _) =>
                    val vr = env.find(_._1 == name.name).get._2
                    '{
                        $budget.spendBudget(Step(StepKind.Var), $params.machineCosts.varCost, Nil)
                        Return($vr)
                    }

                case Term.LamAbs(name, body, _) =>
                    '{
                        $budget.spendBudget(
                          Step(StepKind.LamAbs),
                          $params.machineCosts.lamCost,
                          Nil
                        )
                        Return((arg: Any) =>
                            ${ genCode(body, (name -> 'arg) :: env, logger, budget, params) }
                        )
                    }

                case Term.Apply(fun, arg, _) =>
                    // Optimize: detect fully-applied 1-argument builtins with simple arg
                    if UplcTermHelper.isApplyBuiltin1WithSimpleArg(term) then {
                        fun match
                            case Term.Builtin(bn, _) if BuiltinAppliedGenerator.isSupported1(bn) =>
                                // Generate inline code for one-argument builtins
                                '{
                                    Return(${
                                        BuiltinEmitter
                                            .emitAppliedBuiltin1(bn, genValue(arg), budget, params)
                                    })
                                }
                            case _ =>
                                throw IllegalStateException(
                                  s"isApplyBuiltin1WithSimpleArg returned true but term has wrong shape"
                                )
                    }
                    // Optimize: if both fun and arg are simple (Return-producing),
                    // generate direct application instead of Apply continuation
                    else if UplcTermHelper.isSimpleTerm(fun) && UplcTermHelper.isSimpleTerm(arg)
                    then {
                        '{
                            $budget.spendBudget(
                              Step(StepKind.Apply),
                              $params.machineCosts.applyCost,
                              Nil
                            )
                            // Both are Return values, apply directly
                            val f = ${ genValue(fun) }.asInstanceOf[Any => Any]
                            val a = ${ genValue(arg) }
                            val result = f(a)
                            // Result might be continuation or value
                            result match {
                                case cont: ContinuationJitRepr => cont
                                case v                         => Return(v)
                            }
                        }
                    } else if UplcTermHelper.isApplyBuiltin2WithSimpleArgs(term) then {
                        fun match
                            case Term.Apply(Term.Builtin(bn, _), arg1, _)
                                if BuiltinAppliedGenerator.isSupported(bn) =>
                                '{
                                    Return(${
                                        BuiltinEmitter.emitAppliedBuiltin2(
                                          bn,
                                          genValue(arg1),
                                          genValue(arg),
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
                            Apply(${ gen(fun) }, ${ gen(arg) })
                        }
                    }

                case Term.Force(term, _) =>
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Force),
                          $params.machineCosts.forceCost,
                          Nil
                        )
                        Force(${ gen(term) })
                    }

                case Term.Delay(term, _) =>
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Delay),
                          $params.machineCosts.delayCost,
                          Nil
                        )
                        Return.delayed(${ gen(term) })
                    }

                case Term.Const(const, _) =>
                    val expr = constantToExpr(const)
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Const),
                          $params.machineCosts.constCost,
                          Nil
                        )
                        Return($expr)
                    }
                case Term.Builtin(bn, _) =>
                    '{ Return(${ BuiltinEmitter.emitBuiltin(bn, logger, budget, params) }) }
                case _: Term.Error =>
                    '{ throw new JitEvaluationFailure("UPLC Error term evaluated") }
                case Term.Constr(tag, args, _) =>
                    // TODO: Implement Constr with continuation support
                    // For now, fall back to simple evaluation
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Constr),
                          $params.machineCosts.constrCost,
                          Nil
                        )
                        // Evaluate all args first, then construct
                        val evaluatedArgs = List(${
                            Expr.ofSeq(args.map(a => '{ ContinuationJitRepr.eval(${ gen(a) }) }))
                        }*)
                        Return((${ Expr(tag.value) }, evaluatedArgs))
                    }
                case Term.Case(arg, cases, _) =>
                    '{
                        $budget.spendBudget(Step(StepKind.Case), $params.machineCosts.caseCost, Nil)
                        ${
                            CaseHelper.genCaseDispatchCont(
                              gen(arg),
                              cases.map(c => (q: Quotes) ?=> gen(c)(using q))
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
        StagingCompilerThread.run(term)(genCodeFromTerm(term))

    override def isStackSafe = true
}
