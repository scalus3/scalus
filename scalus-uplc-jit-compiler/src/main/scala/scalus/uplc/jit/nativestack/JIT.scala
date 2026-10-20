package scalus.uplc.jit.nativestack

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

    /** Whether a constant is built once per compiled script instead of in place. Unit, booleans and
      * strings are JVM literals and cost nothing to build in place.
      */
    private def isHoisted(const: Constant): Boolean = const match
        case Constant.Unit | Constant.Bool(_) | Constant.String(_) => false
        case _                                                     => true

    /** Let-bindings are inlined into a JVM method while it has fewer generated terms than this.
      * Small methods stay cheap for C2 to inline into their callers: with 150, guardrail-small
      * evaluated 20% slower than with 20, and stablecoin_1-3 was not faster.
      */
    private val MaxInlinedLetTerms = 20

    enum FunType:
        case Lam(f: Any => Any)
        case Builtin(f: Any => Any)

    private[jit] def genCodeFromTerm(
        term: Term
    )(using Quotes): Expr[(Logger, BudgetSpender, MachineParams) => Any] = {

        // Constants are built once, when the compiled function is created, and shared by all
        // evaluations, as the CEK machine shares the values of the term. Building them in the
        // function would allocate them on every evaluation, and a large constant (the applied
        // script context) makes the function body too big for HotSpot to compile.
        val constIndex = collection.mutable.LinkedHashMap.empty[Constant, Int]
        def collectConstants(t: Term): Unit = t match
            case Term.Const(const, _) =>
                if isHoisted(const) && !constIndex.contains(const) then
                    constIndex(const) = constIndex.size
            case Term.LamAbs(_, body, _) => collectConstants(body)
            case Term.Apply(f, arg, _)   => collectConstants(f); collectConstants(arg)
            case Term.Force(body, _)     => collectConstants(body)
            case Term.Delay(body, _)     => collectConstants(body)
            case Term.Constr(_, args, _) => args.foreach(collectConstants)
            case Term.Case(arg, cases, _) =>
                collectConstants(arg); cases.foreach(collectConstants)
            case _ => ()
        collectConstants(term)

        // Number of terms generated so far into the JVM method being generated. Each LamAbs and
        // Delay body is a method of its own; inlined let-bindings extend the current one.
        var methodTerms = 0
        def inNewMethod[A](body: => A): A = {
            val saved = methodTerms
            methodTerms = 0
            try body
            finally methodTerms = saved
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
            params: Expr[MachineParams],
            nativeStackContext: Expr[NativeStackContext],
            consts: Expr[Array[AnyRef]]
        )(using Quotes): Expr[Any] = {
            def gen(t: Term)(using Quotes): Expr[Any] =
                genCode(t, env, logger, budget, params, nativeStackContext, consts)

            methodTerms += 1
            term match
                case Term.Var(name, _) =>
                    val vr = env.find(_._1 == name.name).get._2
                    '{
                        $budget.spendBudget(Step(StepKind.Var), $params.machineCosts.varCost, Nil)
                        $vr
                    }
                case Term.LamAbs(name, body, _) =>
                    '{
                        $budget.spendBudget(
                          Step(StepKind.LamAbs),
                          $params.machineCosts.lamCost,
                          Nil
                        )
                        (arg: Any) =>
                            ${
                                inNewMethod(
                                  genCode(
                                    body,
                                    (name -> 'arg) :: env,
                                    logger,
                                    budget,
                                    params,
                                    nativeStackContext,
                                    consts
                                  )
                                )
                            }
                    }
                case Term.Apply(Term.LamAbs(name, body, _), arg, _)
                    if methodTerms < MaxInlinedLetTerms =>
                    // A let-binding `[(lam x body) arg]`: bind `arg` to a local value instead of
                    // creating and calling a closure. Every closure is a JVM method and class for
                    // HotSpot to compile, and large scripts take long to warm up.
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Apply),
                          $params.machineCosts.applyCost,
                          Nil
                        )
                        $budget.spendBudget(
                          Step(StepKind.LamAbs),
                          $params.machineCosts.lamCost,
                          Nil
                        )
                        val x = ${ gen(arg) }
                        ${
                            genCode(
                              body,
                              (name -> 'x) :: env,
                              logger,
                              budget,
                              params,
                              nativeStackContext,
                              consts
                            )
                        }
                    }
                case Term.Apply(f, arg, _) =>
                    // Optimize: detect fully-applied 1-argument builtins with simple arg
                    if UplcTermHelper.isApplyBuiltin1WithSimpleArg(term) then {
                        f match
                            case Term.Builtin(bn, _) if BuiltinAppliedGenerator.isSupported1(bn) =>
                                // Generate inline code for one-argument builtins
                                BuiltinEmitter.emitAppliedBuiltin1(bn, gen(arg), budget, params)
                            case _ =>
                                throw IllegalStateException(
                                  s"isApplyBuiltin1WithSimpleArg returned true but term has wrong shape"
                                )
                    }
                    // Optimize: detect fully-applied 2-argument builtins with simple args
                    else if UplcTermHelper.isApplyBuiltin2WithSimpleArgs(term) then {
                        f match
                            case Term.Apply(Term.Builtin(bn, _), arg1, _) =>
                                BuiltinEmitter.emitAppliedBuiltin2(
                                  bn,
                                  gen(arg1),
                                  gen(arg),
                                  budget,
                                  params
                                )
                            case _ =>
                                throw IllegalStateException(
                                  s"isApplyBuiltin2WithSimpleArgs returned true but term has wrong shape"
                                )
                    } else if UplcTermHelper.isSimpleTerm(f) && UplcTermHelper.isSimpleTerm(arg)
                    then {
                        // can not check stack-overflow
                        '{
                            $budget.spendBudget(
                              Step(StepKind.Apply),
                              $params.machineCosts.applyCost,
                              Nil
                            )
                            ${ gen(f) }.asInstanceOf[Any => Any].apply(${ gen(arg) })
                        }
                    } else
                        '{
                            $budget.spendBudget(
                              Step(StepKind.Apply),
                              $params.machineCosts.applyCost,
                              Nil
                            )
                            ${ nativeStackContext }.incr()
                            val r = ${ gen(f) }.asInstanceOf[Any => Any].apply(${ gen(arg) })
                            ${ nativeStackContext }.decr()
                            r
                        }

                case Term.Force(term, _) =>
                    '{
                        val forceTerm = ${ gen(term) }.asInstanceOf[() => Any]
                        $budget.spendBudget(
                          Step(StepKind.Force),
                          $params.machineCosts.forceCost,
                          Nil
                        )
                        forceTerm.apply()
                    }
                case Term.Delay(term, _) =>
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Delay),
                          $params.machineCosts.delayCost,
                          Nil
                        )
                        () => ${ inNewMethod(gen(term)) }
                    }
                case Term.Const(const, _) =>
                    val expr = constIndex.get(const) match
                        case Some(i) => '{ $consts(${ Expr(i) }) }
                        case None    => constantToExpr(const)
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Const),
                          $params.machineCosts.constCost,
                          Nil
                        )
                        $expr
                    }
                case Term.Builtin(bn, _) =>
                    BuiltinEmitter.emitBuiltin(bn, logger, budget, params)
                case _: Term.Error =>
                    '{ throw new JitEvaluationFailure("UPLC Error term evaluated") }
                case Term.Constr(tag, args, _) =>
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Constr),
                          $params.machineCosts.constrCost,
                          Nil
                        )
                        ${ Expr.ofTuple(Expr(tag.value) -> Expr.ofList(args.map(gen(_)))) }
                    }
                case Term.Case(arg, cases, _) =>
                    '{
                        $budget.spendBudget(Step(StepKind.Case), $params.machineCosts.caseCost, Nil)
                        ${
                            CaseHelper.genCaseDispatch(
                              gen(arg),
                              cases.map(c => (q: Quotes) ?=> gen(c)(using q))
                            )
                        }
                    }
        }

        val retval = '{
            val consts: Array[AnyRef] = Array[AnyRef](${
                Varargs(
                  constIndex.keys.toList.map(c => '{ ${ constantToExpr(c) }.asInstanceOf[AnyRef] })
                )
            }*)
            (logger: Logger, budget: BudgetSpender, params: MachineParams) =>
                val nativeStackContext = new NativeStackContext()
                budget.spendBudget(Startup, params.machineCosts.startupCost, Nil)
                ${ genCode(term, Nil, 'logger, 'budget, 'params, 'nativeStackContext, 'consts) }
        }

        // println(retval.show)

        retval
    }

    /** Compiles a UPLC term into an optimized JVM function using JIT compilation.
      */
    override def jitUplc(term: Term): (Logger, BudgetSpender, MachineParams) => Any =
        StagingCompilerThread.run(term)(genCodeFromTerm(term))

    override def isStackSafe: Boolean = false

}
