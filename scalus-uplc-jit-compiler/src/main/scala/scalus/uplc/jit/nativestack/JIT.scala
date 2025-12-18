package scalus.uplc.jit.nativestack

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
                // No need to track element type - list operations have constant cost functions
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
            case Constant.BuiltinValue(value) =>
                val dataExpr: Expr[Data] = Expr(BuiltinValue.toData(value))
                '{ BuiltinValue.fromData($dataExpr) }
    }

    enum FunType:
        case Lam(f: Any => Any)
        case Builtin(f: Any => Any)

    private def genCodeFromTerm(
        term: Term
    )(using Quotes): Expr[(Logger, BudgetSpender, MachineParams) => Any] = {
        import quotes.reflect.{Lambda, MethodType, Symbol, TypeRepr, asTerm}

        val nativeStackContext = '{ new NativeStackContext() }

        def genCode(
            term: Term,
            env: List[(String, quotes.reflect.Term)],
            logger: Expr[Logger],
            budget: Expr[BudgetSpender],
            params: Expr[MachineParams],
            nativeStackContext: Expr[NativeStackContext]
        ): Expr[Any] = {
            term match
                case Term.Var(name) =>
                    val vr = env.find(_._1 == name.name).get._2.asExprOf[Any]
                    '{
                        $budget.spendBudget(Step(StepKind.Var), $params.machineCosts.varCost, Nil)
                        $vr
                    }
                case Term.LamAbs(name, term) =>
                    val mtpe =
                        MethodType(List(name))(_ => List(TypeRepr.of[Any]), _ => TypeRepr.of[Any])
                    val lambda = Lambda(
                      Symbol.spliceOwner,
                      mtpe,
                      { case (methSym, args) =>
                          genCode(
                            term,
                            (name -> args.head.asInstanceOf[quotes.reflect.Term]) :: env,
                            logger,
                            budget,
                            params,
                            nativeStackContext
                          ).asTerm
                              .changeOwner(methSym)
                      }
                    ).asExprOf[Any]
                    '{
                        $budget.spendBudget(
                          Step(StepKind.LamAbs),
                          $params.machineCosts.varCost,
                          Nil
                        )
                        $lambda
                    }
                case Term.Apply(f, arg) =>
                    // Optimize: detect fully-applied 1-argument builtins with simple arg
                    if UplcTermHelper.isApplyBuiltin1WithSimpleArg(term) then {
                        f match
                            case Term.Builtin(bn) if BuiltinAppliedGenerator.isSupported1(bn) =>
                                val argCode =
                                    genCode(arg, env, logger, budget, params, nativeStackContext)
                                // Generate inline code for one-argument builtins
                                BuiltinEmitter.emitAppliedBuiltin1(bn, argCode, budget, params)
                            case _ =>
                                throw IllegalStateException(
                                  s"isApplyBuiltin1WithSimpleArg returned true but term has wrong shape"
                                )
                    }
                    // Optimize: detect fully-applied 2-argument builtins with simple args
                    else if UplcTermHelper.isApplyBuiltin2WithSimpleArgs(term) then {
                        f match
                            case Term.Apply(Term.Builtin(bn), arg1) =>
                                val arg1Code =
                                    genCode(arg1, env, logger, budget, params, nativeStackContext)
                                val arg2Code =
                                    genCode(arg, env, logger, budget, params, nativeStackContext)
                                BuiltinEmitter.emitAppliedBuiltin2(
                                  bn,
                                  arg1Code,
                                  arg2Code,
                                  budget,
                                  params
                                )
                            case _ =>
                                throw IllegalStateException(
                                  s"isApplyBuiltin2WithSimpleArgs returned true but term has wrong shape"
                                )
                    } else {
                        val func = genCode(f, env, logger, budget, params, nativeStackContext)
                        val a = genCode(arg, env, logger, budget, params, nativeStackContext)
                        if UplcTermHelper.isSimpleTerm(f) && UplcTermHelper.isSimpleTerm(arg) then {
                            // can not check stack-overflow
                            '{
                                $budget.spendBudget(
                                  Step(StepKind.Apply),
                                  $params.machineCosts.applyCost,
                                  Nil
                                )
                                ${ func }.asInstanceOf[Any => Any].apply($a)
                            }
                        } else
                            '{
                                $budget.spendBudget(
                                  Step(StepKind.Apply),
                                  $params.machineCosts.applyCost,
                                  Nil
                                )
                                ${ nativeStackContext }.incr()
                                val r = ${ func }.asInstanceOf[Any => Any].apply($a)
                                ${ nativeStackContext }.decr()
                                r
                            }
                    }

                case Term.Force(term) =>
                    val expr = genCode(term, env, logger, budget, params, nativeStackContext)
                    '{
                        val forceTerm = ${ expr }.asInstanceOf[() => Any]
                        $budget.spendBudget(
                          Step(StepKind.Force),
                          $params.machineCosts.forceCost,
                          Nil
                        )
                        forceTerm.apply()
                    }
                case Term.Delay(term) =>
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Delay),
                          $params.machineCosts.delayCost,
                          Nil
                        )
                        () => ${ genCode(term, env, logger, budget, params, nativeStackContext) }
                    }
                case Term.Const(const) =>
                    val expr = constantToExpr(const)
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Const),
                          $params.machineCosts.constCost,
                          Nil
                        )
                        $expr
                    }
                case Term.Builtin(bn) =>
                    BuiltinEmitter.emitBuiltin(bn, logger, budget, params)
                case Term.Error =>
                    '{ throw new JitEvaluationFailure("UPLC Error term evaluated") }
                case Term.Constr(tag, args) =>
                    val expr = Expr.ofTuple(
                      Expr(tag.value) -> Expr.ofList(
                        args.map(a => genCode(a, env, logger, budget, params, nativeStackContext))
                      )
                    )
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Constr),
                          $params.machineCosts.constrCost,
                          Nil
                        )
                        $expr
                    }
                case Term.Case(arg, cases) =>
                    val scrutinee =
                        genCode(arg, env, logger, budget, params, nativeStackContext)
                    val branchExprs: List[Expr[Any]] =
                        cases.map(c => genCode(c, env, logger, budget, params, nativeStackContext))
                    '{
                        $budget.spendBudget(Step(StepKind.Case), $params.machineCosts.caseCost, Nil)
                        ${
                            CaseHelper.genCaseDispatch(
                              scrutinee,
                              branchExprs
                            )
                        }
                    }
        }

        val retval = '{ (logger: Logger, budget: BudgetSpender, params: MachineParams) =>
            val nativeStackContext = new NativeStackContext()
            budget.spendBudget(Startup, params.machineCosts.startupCost, Nil)
            ${ genCode(term, Nil, 'logger, 'budget, 'params, 'nativeStackContext) }
        }

        // println(retval.show)

        retval
    }

    /** Compiles a UPLC term into an optimized JVM function using JIT compilation.
      */
    override def jitUplc(term: Term): (Logger, BudgetSpender, MachineParams) => Any =
        staging.run { (quotes: Quotes) ?=>
            val expr = genCodeFromTerm(term)
            expr
        }

    override def isStackSafe: Boolean = false

}
