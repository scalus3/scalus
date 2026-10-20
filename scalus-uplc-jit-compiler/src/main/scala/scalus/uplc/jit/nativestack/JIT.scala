package scalus.uplc.jit.nativestack

import scalus.uplc.eval.*
import scalus.uplc.jit.*
import scalus.uplc.Term

import scala.quoted.*

/** The JIT compiler: compiles a UPLC term into JVM bytecode at run time with Scala 3 staging.
  *
  * The generated code evaluates the term as the CEK machine does, with the same result, the same
  * failures and the same spent budget, but with the JVM call stack instead of the machine's
  * continuation stack: a UPLC lambda is a JVM lambda ([[LamValue]]), an application a JVM call.
  * Generated code counts the depth of these calls and throws [[StackThresholdException]] at its
  * `maxDepth`; `UplcCompiler.Compiled.withFallback` then evaluates the term with the CEK machine.
  *
  * Scope: builtin semantics variant E and the machine of protocol version 11; see [[UplcCompiler]].
  */
object JIT {

    /** Let-bindings are inlined into a JVM method while it has fewer generated terms than this.
      * Small methods stay cheap for C2 to inline into their callers: with 150, guardrail-small
      * evaluated 20% slower than with 20, and stablecoin_1-3 was not faster.
      */
    private val MaxInlinedLetTerms = 20

    /** References the generator needs besides [[GenCtx]]: the stack-depth counter of the
      * evaluation, a local of the generated code; see [[genCodeFromTerm]].
      */
    private final case class NativeCtx(stack: Expr[NativeStackContext])

    /** Generates the code of a whole script: a function of the constants that returns the compiled
      * term.
      *
      * For `[(lam x [(builtin addInteger) x (con integer 1)]) (con integer 2)]` it generates, with
      * `pool.values == Array(Constant.Integer(1), Constant.Integer(2))`:
      * {{{
      * (consts: Array[UplcValue]) =>                              // applied once, to pool.values
      * ((logger: Logger, budget: BudgetSpender, params: MachineParams) =>   // a CompiledTerm
      *     val nativeStackContext = new NativeStackContext()
      *     JitBudget.spendStartup(budget, params)
      *     JitBudget.spendLet(budget, params)                  // the inlined let: Apply, LamAbs
      *     val x = { JitBudget.spendConst(budget, params); consts(1) }
      *     ... // the applied addInteger on x and consts(0), see BuiltinEmitter.emitApplied
      * ): CompiledTerm
      * }}}
      * The startup cost is spent first, as `Cek.scala` `evaluateTerm` does.
      */
    private[jit] def genCodeFromTerm(term: Term, pool: ConstantPool, maxDepth: Int)(using
        Quotes
    ): Expr[Array[UplcValue] => CompiledTerm] = {

        // Number of terms generated so far into the JVM method being generated. Each LamAbs and
        // Delay body is a method of its own; inlined let-bindings extend the current one.
        var methodTerms = 0
        def inNewMethod[A](body: => A): A = {
            val saved = methodTerms
            methodTerms = 0
            try body
            finally methodTerms = saved
        }

        /** Generates the code that evaluates `term` and returns its [[UplcValue]].
          *
          * `env` maps each variable in scope to the generated reference to its value: a lambda
          * parameter `arg` or an inlined let-binding `val x`. Code for each term kind, with
          * `budget` and `params` the parameters of the compiled term, and `spend(K)` one call of
          * `JitBudget`:
          * {{{
          * x                  ~> { spend(Var); x }
          * (lam x b)          ~> { spend(LamAbs); ((arg: UplcValue) => <b with x -> arg>): LamValue }
          * [(lam x b) a]      ~> { spend(Apply); spend(LamAbs); val x = <a>; <b> }  // a let, see below
          * [f a]              ~> { spend(Apply); val fun = <f>; val a = <a>
          *                         fun match { case l: LamValue => stack.incr(); val r = l.apply(a)
          *                                     stack.decr(); r
          *                                     case other => JitRuntime.cannotApply(other) } }
          * (force t)          ~> { spend(Force); <t> match { case d: DelayValue => d.apply()
          *                                                   case o => JitRuntime.cannotForce(o) } }
          * (delay t)          ~> { spend(Delay); (() => <t>): DelayValue }
          * (con c)            ~> { spend(Const); consts(i) }   // see ConstantPool
          * (builtin f)        ~> see BuiltinEmitter.emitBuiltin
          * [(force^k (builtin f)) a1 .. an]  ~> see BuiltinEmitter.emitApplied, if saturated
          * (constr i a*)      ~> { spend(Constr); ConstrValue(i, List(<a>*)) }
          * (case s b*)        ~> { spend(Case); <dispatch on <s>, see genCaseDispatch> }
          * (error)            ~> throw EvaluationFailure(Nil)                 // as the CEK machine
          * }}}
          * Each term spends the steps `Cek.scala` `computeCek` spends for it, in the same order
          * relative to the code of its subterms, so a `RestrictingBudgetSpender` stops at the same
          * step as on the CEK machine. An application or a force computes all its operands before
          * it checks them (in [[JitRuntime]]), as the CEK machine does.
          *
          * Every application that is not an inlined let counts its call depth: a UPLC loop is a
          * recursion through applications, so every cycle of JVM calls passes a counted one.
          *
          * Takes the `Quotes` of the splice it is called from, and every recursive call sits inside
          * the splice of its result. So each piece of code is created with its final owner. A
          * finished `Expr` spliced under a `val` or a lambda is copied to change its owner, which
          * makes the generation quadratic in the nesting depth of the term (spec [RU-1]).
          */
        def genCode(term: Term, env: Map[String, Expr[UplcValue]])(using
            ctx: GenCtx,
            nctx: NativeCtx,
            q: Quotes
        ): Expr[UplcValue] = {
            def gen(t: Term)(using Quotes): Expr[UplcValue] = genCode(t, env)

            methodTerms += 1
            term match
                case Term.Var(name, _) =>
                    // compile time: look the variable up. A free variable fails when it is
                    // evaluated, as in the CEK machine, not when the script is compiled.
                    val vr = env.getOrElse(name.name, GenCtx.freeVariable(name.name))
                    '{
                        ${ GenCtx.spendStep(StepKind.Var) }
                        $vr
                    }
                case Term.LamAbs(name, body, _) =>
                    '{
                        ${ GenCtx.spendStep(StepKind.LamAbs) }
                        (
                            (arg: UplcValue) =>
                                ${ inNewMethod(genCode(body, env.updated(name, 'arg))) }
                        ): LamValue
                    }
                case BuiltinEmitter.AppliedBuiltin(bn, forces, args) =>
                    // A saturated builtin: one static call, no closures. The arguments are
                    // generators, run inside the splices of emitApplied, in CEK order.
                    BuiltinEmitter.emitApplied(
                      bn,
                      forces,
                      args.map(a => (q: Quotes) ?=> gen(a)(using q))
                    )
                case Term.Apply(Term.LamAbs(name, body, _), arg, _)
                    if methodTerms < MaxInlinedLetTerms =>
                    // A let-binding `[(lam x body) arg]`: bind `arg` to a local value instead of
                    // creating and calling a closure. Every closure is a JVM method and class for
                    // HotSpot to compile, and large scripts take long to warm up. The CEK spends
                    // Apply, then LamAbs when it computes the function, then the steps of `arg`.
                    '{
                        ${ GenCtx.spendLet }
                        val x = ${ gen(arg) }
                        ${ genCode(body, env.updated(name, 'x)) }
                    }
                case Term.Apply(f, arg, _) =>
                    // The CEK machine spends Apply, computes the function, then the argument, and
                    // only then checks that the function is one. The call `l.apply(a)` is generated
                    // here, at every application, not in a shared helper: HotSpot keeps the
                    // receiver types of a call per bytecode, so each site sees the one or two
                    // lambda classes it calls and C2 inlines them. Behind one shared call every
                    // site saw thousands of classes, and no call was inlined (uniswap-1 1.9x
                    // slower). Only the failure is shared (JitRuntime.cannotApply).
                    '{
                        ${ GenCtx.spendStep(StepKind.Apply) }
                        val fun = ${ gen(f) }
                        val a = ${ gen(arg) }
                        fun match
                            case l: LamValue =>
                                ${ nctx.stack }.incr()
                                val r = l.apply(a)
                                ${ nctx.stack }.decr()
                                r
                            case other => JitRuntime.cannotApply(other)
                    }
                case Term.Force(term, _) =>
                    // The CEK machine spends Force before it computes the term. As for Apply,
                    // the call is generated at every force, so its receiver profile is its own.
                    '{
                        ${ GenCtx.spendStep(StepKind.Force) }
                        ${ gen(term) } match
                            case d: DelayValue => d.apply()
                            case other         => JitRuntime.cannotForce(other)
                    }
                case Term.Delay(term, _) =>
                    '{
                        ${ GenCtx.spendStep(StepKind.Delay) }
                        (() => ${ inNewMethod(gen(term)) }): DelayValue
                    }
                case Term.Const(const, _) =>
                    '{
                        ${ GenCtx.spendStep(StepKind.Const) }
                        ${ ctx.constant(const) }
                    }
                case Term.Builtin(bn, _) =>
                    BuiltinEmitter.emitBuiltin(bn)
                case _: Term.Error =>
                    // The CEK machine spends nothing for `error`.
                    '{ throw new EvaluationFailure(Nil) }
                case Term.Constr(tag, args, _) =>
                    // The CEK machine spends Constr, then computes the arguments in order.
                    '{
                        ${ GenCtx.spendStep(StepKind.Constr) }
                        ConstrValue(${ Expr(tag.value) }, ${ Expr.ofList(args.map(gen(_))) })
                    }
                case Term.Case(arg, cases, _) =>
                    '{
                        ${ GenCtx.spendStep(StepKind.Case) }
                        ${
                            genCaseDispatch(
                              gen(arg),
                              cases.map(c => (q: Quotes) ?=> gen(c)(using q))
                            )
                        }
                    }
        }

        // generated: a function of the constant array, applied once by ConstantPool.compile
        '{ (consts: Array[UplcValue]) =>
            // generated, runs once per evaluation
            val compiled: CompiledTerm =
                (logger: Logger, budget: BudgetSpender, params: MachineParams) =>
                    val nativeStackContext = new NativeStackContext(${ Expr(maxDepth) })
                    JitBudget.spendStartup(budget, params)
                    ${
                        given GenCtx = GenCtx('logger, 'budget, 'params, 'consts, pool)
                        given NativeCtx = NativeCtx('nativeStackContext)
                        genCode(term, Map.empty)
                    }
            compiled
        }
    }

    /** Generates the branch selection of `(case s b0 .. b(n-1))`: compute the scrutinee, choose the
      * branch, compute it, and apply it to the fields of the scrutinee.
      *
      * For `(case (constr 1 (con integer 2)) (lam a a) (lam b b))` it generates (the scrutinee and
      * branch code abbreviated, see [[genCodeFromTerm]]):
      * {{{
      * val s = <(constr 1 (con integer 2))>             // ConstrValue(1, List(Constant.Integer(2)))
      * val i = JitRuntime.caseIndex(s, 2)               // 1; fails if s selects no branch
      * if i == 0 then JitRuntime.applyFields(stack, <(lam a a)>, s)
      * else JitRuntime.applyFields(stack, <(lam b b)>, s) // the last branch needs no test
      * }}}
      * Only the selected branch runs, as in the CEK machine. No budget is spent here: the caller
      * spends the Case step (`Cek.scala` `computeCek`) before it, the scrutinee and the branch
      * spend their own steps, and applying the branch to the fields spends nothing, as the CEK
      * machine pushes them as values (`FrameAwaitFunValue`). [[JitRuntime.caseIndex]] has the
      * checks of the CEK machine for each kind of scrutinee; with no branches it always fails.
      *
      * The `if` chain compares `i` once per branch before the selected one; the scripts of the
      * corpus have few branches per `case`.
      *
      * Scrutinee and branches are generators, not finished expressions: each runs inside its own
      * splice, so its code is created with the owner of that splice (owner rule, spec [RU-1]). `s`
      * and `i` are references, which cost nothing to splice anywhere. [[JitRuntime.applyFields]]
      * counts the depth of every field call.
      */
    private def genCaseDispatch(
        scrutinee: Quotes ?=> Expr[UplcValue],
        branches: List[Quotes ?=> Expr[UplcValue]]
    )(using nctx: NativeCtx, q: Quotes): Expr[UplcValue] = {
        val n = branches.size
        def chain(
            k: Int,
            rest: List[Quotes ?=> Expr[UplcValue]],
            i: Expr[Int],
            s: Expr[UplcValue]
        )(using Quotes): Expr[UplcValue] = rest match
            case Nil         => '{ throw new IllegalStateException("case: no branch") } // n == 0
            case last :: Nil => '{ JitRuntime.applyFields(${ nctx.stack }, ${ last }, $s) }
            case b :: more =>
                '{
                    if $i == ${ Expr(k) } then JitRuntime.applyFields(${ nctx.stack }, ${ b }, $s)
                    else ${ chain(k + 1, more, i, s) }
                }
        '{
            val s = ${ scrutinee }
            val i = JitRuntime.caseIndex(s, ${ Expr(n) })
            ${ chain(0, branches, 'i, 's) }
        }
    }

    /** Compiles a UPLC term (with named variables, not De Bruijn indices) into a [[CompiledTerm]].
      *
      * `maxDepth` is the call-depth limit of the compiled code, a constant in it: at that depth it
      * throws [[StackThresholdException]]. Choose it for the stack of the threads that run it.
      *
      * Safe to call from any thread: the staging compiles run one at a time on a dedicated thread
      * ([[StagingCompilerThread]]).
      */
    def jitUplc(term: Term, maxDepth: Int): CompiledTerm =
        // the pool walks the term recursively: on the compiler thread, with its large stack
        StagingCompilerThread.onThread {
            val pool = ConstantPool(term)
            ConstantPool.compile(term, pool)(genCodeFromTerm(term, pool, maxDepth))
        }

    /** [[jitUplc]] with [[NativeStackContext.DefaultMaxDepth]], for a thread with a 64 MB stack. */
    def jitUplc(term: Term): CompiledTerm = jitUplc(term, NativeStackContext.DefaultMaxDepth)
}
