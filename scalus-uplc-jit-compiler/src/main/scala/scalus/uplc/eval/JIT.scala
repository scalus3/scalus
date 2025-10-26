package scalus.uplc.eval

import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, BuiltinPair, Builtins, ByteString, Data}
import scalus.uplc.{Constant, DefaultFun, Term}
import scalus.uplc.DefaultUni.asConstant
import scalus.*
import scalus.uplc.eval.ExBudgetCategory.{Startup, Step}

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
            case Constant.Integer(value)    => Expr(value)
            case Constant.ByteString(value) => Expr(value)
            case Constant.String(value)     => Expr(value)
            case Constant.Unit              => '{ () }
            case Constant.Bool(value)       => Expr(value)
            case Constant.Data(value)       => Expr(value)
            case Constant.List(elemType, value) =>
                '{ ListJitRepr(${ Expr(elemType) }, ${ Expr.ofList(value.map(constantToExpr)) }) }
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
        import quotes.reflect.{Lambda, MethodType, Symbol, TypeRepr, asTerm}

        val debugMode = false // Set to true to enable exception tracking

        def wrapWithDebug(code: Expr[Any], termDescription: String): Expr[Any] = {
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
            params: Expr[MachineParams],
            runtimeJitContext: Expr[RuntimeJitContext]
        ): Expr[Any] = {
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
                        $vr
                    }
                case Term.LamAbs(name, term) =>
                    // auction:  177653 ms
                    '{ (arg: Any) =>
                        ${ runtimeJitContext }.withCheckOverflow(
                          ${
                              genCode(
                                term,
                                (name -> 'arg.asTerm) :: env,
                                logger,
                                budget,
                                params,
                                runtimeJitContext
                              )
                          }
                        )
                    }
                case Term.Apply(fun, arg) =>
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Apply),
                          $params.machineCosts.applyCost,
                          Nil
                        )
                        ${ runtimeJitContext }.trampolinedApply(
                          ${ genCode(fun, env, logger, budget, params, runtimeJitContext) },
                          ${ genCode(arg, env, logger, budget, params, runtimeJitContext) }
                        )
                    }
                case Term.Force(term) =>
                    val in = genCode(term, env, logger, budget, params, runtimeJitContext)
                    '{
                        RuntimeHelper.unaryOp[() => Any](
                          ${ in.asExprOf[Any] },
                          { forceTerm =>
                              $budget.spendBudget(
                                Step(StepKind.Force),
                                $params.machineCosts.forceCost,
                                Nil
                              )
                              forceTerm.apply()
                          }
                        )
                    }
                case Term.Delay(term) =>
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Delay),
                          $params.machineCosts.delayCost,
                          Nil
                        )
                        () =>
                            ${
                                genCode(term, env, logger, budget, params, runtimeJitContext)
                            }
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
                case Term.Builtin(DefaultFun.AddInteger) =>
                    '{ (xIn: Any) => (yIn: Any) =>
                        RuntimeHelper.integerOp(
                          xIn,
                          yIn,
                          { (x: BigInt, y: BigInt) =>
                              $budget.spendBudget(
                                Step(StepKind.Builtin),
                                $params.builtinCostModel.addInteger
                                    .calculateCost(
                                      CekValue.VCon(asConstant(x)),
                                      CekValue.VCon(asConstant(y))
                                    ),
                                Nil
                              )
                              x + y
                          }
                        )
                    }
                case Term.Builtin(DefaultFun.SubtractInteger) =>
                    '{ (xIn: Any) => (yIn: Any) =>
                        RuntimeHelper.integerOp(
                          xIn,
                          yIn,
                          { (x: BigInt, y: BigInt) =>
                              $budget.spendBudget(
                                Step(StepKind.Builtin),
                                $params.builtinCostModel.subtractInteger
                                    .calculateCost(
                                      CekValue.VCon(asConstant(x)),
                                      CekValue.VCon(asConstant(y))
                                    ),
                                Nil
                              )
                              x - y
                          }
                        )
                    }
                case Term.Builtin(DefaultFun.MultiplyInteger) =>
                    '{ (xIn: Any) => (yIn: Any) =>
                        RuntimeHelper.integerOp(
                          xIn,
                          yIn,
                          { (x: BigInt, y: BigInt) =>
                              $budget.spendBudget(
                                Step(StepKind.Builtin),
                                $params.builtinCostModel.multiplyInteger
                                    .calculateCost(
                                      CekValue.VCon(asConstant(x)),
                                      CekValue.VCon(asConstant(y))
                                    ),
                                Nil
                              )
                              x * y
                          }
                        )
                    }
                case Term.Builtin(DefaultFun.EqualsData) =>
                    '{ (xIn: Any) => (yIn: Any) =>
                        RuntimeHelper.binaryOp[Data, Data](
                          xIn,
                          yIn,
                          { (x: Data, y: Data) =>
                              $budget.spendBudget(
                                Step(StepKind.Builtin),
                                $params.builtinCostModel.equalsData
                                    .calculateCost(
                                      CekValue.VCon(asConstant(x)),
                                      CekValue.VCon(asConstant(y))
                                    ),
                                Nil
                              )
                              Builtins.equalsData(x, y)
                          }
                        )
                    }
                case Term.Builtin(DefaultFun.LessThanInteger) =>
                    '{ (xIn: Any) => (yIn: Any) =>
                        RuntimeHelper.integerOp(
                          xIn,
                          yIn,
                          { (x: BigInt, y: BigInt) =>
                              $budget.spendBudget(
                                Step(StepKind.Builtin),
                                $params.builtinCostModel.lessThanInteger
                                    .calculateCost(
                                      CekValue.VCon(asConstant(x)),
                                      CekValue.VCon(asConstant(y))
                                    ),
                                Nil
                              )
                              x < y
                          }
                        )
                    }
                case Term.Builtin(DefaultFun.LessThanEqualsInteger) =>
                    '{ (xIn: Any) => (yIn: Any) =>
                        RuntimeHelper.integerOp(
                          xIn,
                          yIn,
                          { (x: BigInt, y: BigInt) =>
                              $budget.spendBudget(
                                Step(StepKind.Builtin),
                                $params.builtinCostModel.lessThanEqualsInteger
                                    .calculateCost(
                                      CekValue.VCon(asConstant(x)),
                                      CekValue.VCon(asConstant(y))
                                    ),
                                Nil
                              )
                              x <= y
                          }
                        )
                    }
                case Term.Builtin(DefaultFun.EqualsInteger) =>
                    '{ (xIn: Any) => (yIn: Any) =>
                        RuntimeHelper.integerOp(
                          xIn,
                          yIn,
                          { (x: BigInt, y: BigInt) =>
                              $budget.spendBudget(
                                Step(StepKind.Builtin),
                                $params.builtinCostModel.equalsInteger
                                    .calculateCost(
                                      CekValue.VCon(asConstant(x)),
                                      CekValue.VCon(asConstant(y))
                                    ),
                                Nil
                              )
                              x == y
                          }
                        )
                    }
                case Term.Builtin(DefaultFun.EqualsByteString) =>
                    '{ (xIn: Any) => (yIn: Any) =>
                        RuntimeHelper.binaryOp[ByteString, ByteString](
                          xIn,
                          yIn,
                          { (x: ByteString, y: ByteString) =>
                              $budget.spendBudget(
                                Step(StepKind.Builtin),
                                $params.builtinCostModel.equalsByteString
                                    .calculateCost(
                                      CekValue.VCon(asConstant(x)),
                                      CekValue.VCon(asConstant(y))
                                    ),
                                Nil
                              )
                              x == y
                          }
                        )
                    }
                case Term.Builtin(DefaultFun.IfThenElse) =>
                    '{ () => (cIn: Any) => (t: Any) => (f: Any) =>
                        RuntimeHelper.unaryOp[Boolean](
                          cIn,
                          { c =>
                              $budget.spendBudget(
                                Step(StepKind.Builtin),
                                $params.builtinCostModel.ifThenElse.constantCost,
                                Nil
                              )
                              if c then t else f
                          }
                        )
                    }
                case Term.Builtin(DefaultFun.Trace) =>
                    '{ () => (sIn: Any) => (a: Any) =>
                        RuntimeHelper.unaryOp[String](
                          sIn,
                          { s =>
                              $budget.spendBudget(
                                Step(StepKind.Builtin),
                                $params.builtinCostModel.trace.constantCost,
                                Nil
                              )
                              ${ logger }.log(s); a
                          }
                        )
                    }
                case Term.Builtin(DefaultFun.FstPair) =>
                    '{ () => () =>
                        { (xIn: Any) =>
                            RuntimeHelper.unaryOp[BuiltinPair[?, ?]](
                              xIn,
                              { x =>
                                  $budget.spendBudget(
                                    Step(StepKind.Builtin),
                                    $params.builtinCostModel.fstPair.constantCost,
                                    Nil
                                  )
                                  Builtins.fstPair(x)
                              }
                            )
                        }
                    }
                case Term.Builtin(DefaultFun.SndPair) =>
                    '{ () => () =>
                        { (xIn: Any) =>
                            RuntimeHelper.unaryOp[BuiltinPair[?, ?]](
                              xIn,
                              { x =>
                                  $budget.spendBudget(
                                    Step(StepKind.Builtin),
                                    $params.builtinCostModel.sndPair.constantCost,
                                    Nil
                                  )
                                  Builtins.sndPair(x)
                              }
                            )
                        }
                    }
                case Term.Builtin(DefaultFun.ChooseList) =>
                    '{ () => () => (laIn: Any) => (e: Any) => (ne: Any) =>
                        RuntimeHelper.unaryOp[ListJitRepr](
                          laIn,
                          { l =>
                              $budget.spendBudget(
                                Step(StepKind.Builtin),
                                $params.builtinCostModel.chooseList.constantCost,
                                Nil
                              )
                              if l.elements.isEmpty then e else ne
                          }
                        )
                    }
                case Term.Builtin(DefaultFun.Sha2_256) =>
                    '{ (bsIn: Any) =>
                        RuntimeHelper.unaryOp[ByteString](
                          bsIn,
                          { bs =>
                              $budget.spendBudget(
                                Step(StepKind.Builtin),
                                $params.builtinCostModel.sha2_256
                                    .calculateCost(CekValue.VCon(asConstant(bs))),
                                Nil
                              )
                              Builtins.sha2_256(bs)
                          }
                        )
                    }
                case Term.Builtin(DefaultFun.HeadList) =>
                    '{ () =>
                        { (yIn: Any) =>
                            RuntimeHelper.unaryOp[ListJitRepr](
                              yIn,
                              { y =>
                                  $budget.spendBudget(
                                    Step(StepKind.Builtin),
                                    $params.builtinCostModel.headList.constantCost,
                                    Nil
                                  )
                                  y.elements.head
                              }
                            )
                        }
                    }
                case Term.Builtin(DefaultFun.TailList) =>
                    '{ () =>
                        { (xIn: Any) =>
                            RuntimeHelper.unaryOp[ListJitRepr](
                              xIn,
                              { x =>
                                  $budget.spendBudget(
                                    Step(StepKind.Builtin),
                                    $params.builtinCostModel.tailList.constantCost,
                                    Nil
                                  )
                                  ListJitRepr(x.elementType, x.elements.tail)
                              }
                            )
                        }
                    }
                case Term.Builtin(DefaultFun.UnConstrData) =>
                    '{ (xIn: Any) =>
                        RuntimeHelper.unaryOp[Data](
                          xIn,
                          { x =>
                              $budget.spendBudget(
                                Step(StepKind.Builtin),
                                $params.builtinCostModel.unConstrData.constantCost,
                                Nil
                              )
                              RuntimeHelper.unConstrData(x)
                          }
                        )
                    }
                case Term.Builtin(DefaultFun.UnListData) =>
                    '{ (xIn: Any) =>
                        RuntimeHelper.unaryOp[Data](
                          xIn,
                          { x =>
                              $budget.spendBudget(
                                Step(StepKind.Builtin),
                                $params.builtinCostModel.unListData.constantCost,
                                Nil
                              )
                              RuntimeHelper.unListData(x)
                          }
                        )
                    }
                case Term.Builtin(DefaultFun.UnIData) =>
                    '{ (xIn: Any) =>
                        RuntimeHelper.unaryOp[Data](
                          xIn,
                          { x =>
                              $budget.spendBudget(
                                Step(StepKind.Builtin),
                                $params.builtinCostModel.unIData.constantCost,
                                Nil
                              )
                              Builtins.unIData(x)
                          }
                        )
                    }
                case Term.Builtin(DefaultFun.UnBData) =>
                    '{ (xIn: Any) =>
                        RuntimeHelper.unaryOp[Data](
                          xIn,
                          { x =>
                              $budget.spendBudget(
                                Step(StepKind.Builtin),
                                $params.builtinCostModel.unBData.constantCost,
                                Nil
                              )
                              Builtins.unBData(x)
                          }
                        )
                    }
                case Term.Builtin(bi) =>
                    sys.error(
                      s"Builtin $bi is not yet supported by the JIT compiler. Please add implementation in the Builtin pattern matching section."
                    )
                case Term.Error =>
                    '{ throw new RuntimeException("UPLC Error term evaluated") }
                case Term.Constr(tag, args) =>
                    val argsExprs = args.map(a =>
                        genCode(a, env, logger, budget, params, runtimeJitContext).asExprOf[Any]
                    )
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Constr),
                          $params.machineCosts.constrCost,
                          Nil
                        )
                        RuntimeHelper.unwrapList(List(${ Expr.ofSeq(argsExprs) }*)) match
                            case unwrappedArgs: List[Any] =>
                                (${ Expr(tag.value) }, unwrappedArgs)
                            case cc: CallCCTrampoline =>
                                cc.map { unwrappedArgs =>
                                    (${ Expr(tag.value) }, unwrappedArgs.asInstanceOf[List[Any]])
                                }
                    }
                case Term.Case(arg, cases) =>
                    val constrIn =
                        genCode(arg, env, logger, budget, params, runtimeJitContext)
                    val caseFuncs =
                        Expr.ofList(
                          cases.map(c =>
                              genCode(c, env, logger, budget, params, runtimeJitContext)
                                  .asExprOf[Any => Any]
                          )
                        )
                    '{
                        $budget.spendBudget(Step(StepKind.Case), $params.machineCosts.caseCost, Nil)
                        RuntimeHelper.unaryOp[(Long, List[Any])](
                          ${ constrIn.asExprOf[Any] },
                          { constr =>
                              val (tag, args) = constr
                              args.foldLeft[Any]($caseFuncs(tag.toInt))((f, a) => {
                                  val result = f.asInstanceOf[Any => Any](a)
                                  result match
                                      case cc: CallCCTrampoline => cc
                                      case _                    => result.asInstanceOf[Any => Any]
                              })
                          }
                        )
                    }
            wrapWithDebug(result, termDesc)
        }

        val retval = '{ (logger: Logger, budget: BudgetSpender, params: MachineParams) =>
            val runtimeJitContext = new RuntimeJitContext()
            budget.spendBudget(Startup, params.machineCosts.startupCost, Nil)
            val result = ${ genCode(term, Nil, 'logger, 'budget, 'params, 'runtimeJitContext) }
            result match
                case cc: CallCCTrampoline =>
                    runtimeJitContext.stackDepth = 0
                    CallCCTrampoline.eval(cc.cont, runtimeJitContext)
                case v => v
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
