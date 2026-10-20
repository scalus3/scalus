package scalus.uplc.jit

import scalus.uplc.{DefaultFun, Meaning, Term}
import scalus.uplc.eval.*

import scala.annotation.tailrec
import scala.quoted.*

/** Emits the code of builtins.
  *
  * Every builtin compiles. Each has one of two implementations, chosen at JIT-compile time:
  *   - its method in [[BuiltinImpls]]: every polymorphic builtin, and the monomorphic builtins in
  *     [[Overrides]];
  *   - [[CekBridge]]: every other (monomorphic) builtin, run by the CEK machine's own runtime.
  *
  * Invariant used throughout: in every builtin `TypeScheme` the forces (type arguments) come before
  * the term arguments, so `argKinds` is some `false`s followed by some `true`s.
  */
object BuiltinEmitter {

    /** The overrides: the monomorphic builtins that run their [[BuiltinImpls]] method, not the
      * bridge. Of the seven builtins called most on the corpus, the two whose override made the
      * bench measurably faster (spec, "Step 6 measurements").
      */
    val Overrides: Set[DefaultFun] = Set(DefaultFun.UnConstrData, DefaultFun.UnBData)

    /** The name of the [[BuiltinImpls]] method of `bn`: the `DefaultFun` name with a lower-case
      * first letter, as in `SIRBuiltins` (`AddInteger` -> `addInteger`, `Sha2_256` -> `sha2_256`).
      */
    def implName(bn: DefaultFun): String = {
        val s = bn.toString
        s.head.toLower.toString + s.tail
    }

    /** What `bn` takes, in order: `true` for a term argument, `false` for a force. The CEK machine
      * reads the same array (`BuiltinRuntime.argKinds`). It does not depend on the cost model or
      * the semantics variant, so the runtime of any `CardanoBuiltins` will do.
      */
    def argKinds(bn: DefaultFun): List[Boolean] =
        Meaning.allBuiltins.getBuiltinRuntime(bn).argKinds.toList

    /** Whether `bn` is polymorphic: its `TypeScheme` has a type variable. */
    def isPolymorphic(bn: DefaultFun): Boolean =
        Meaning.allBuiltins.getBuiltinRuntime(bn).typeScheme.numTypeVars > 0

    /** Whether the code of `bn` calls its [[BuiltinImpls]] method rather than the bridge: every
      * polymorphic builtin, and the [[Overrides]].
      */
    def usesImpl(bn: DefaultFun): Boolean = isPolymorphic(bn) || Overrides.contains(bn)

    /** Generates the code of an unapplied builtin term `(builtin f)`: the Builtin step, then its
      * curried closure ([[curried]]).
      *
      * For `(builtin addInteger)` (bridged) it generates:
      * {{{
      * JitBudget.spendBuiltin(budget, params)
      * ((a: UplcValue) => ((a: UplcValue) =>                                 // each `a` its own
      *     CekBridge.call(CekBridge.builtinsFor(params), 0, logger, budget, Array[AnyRef](a, a))
      * ): BuiltinLamValue): BuiltinLamValue
      * }}}
      * The Builtin step is the one `Cek.scala` `computeCek` spends for a `Builtin` term. The
      * closures spend nothing until the last argument; then the builtin spends its cost, as in the
      * CEK machine. An argument is checked only when the builtin runs, so a partly applied builtin
      * with an argument of the wrong type does not fail, as in the CEK machine (conformance case
      * `term/unlifting-unsat`).
      *
      * Generated lambdas, not closures built by precompiled code: the bench measured the generated
      * ones 16 % faster (they are monomorphic call sites, which HotSpot inlines).
      */
    def emitBuiltin(bn: DefaultFun)(using ctx: GenCtx, q: Quotes): Expr[UplcValue] =
        '{
            ${ GenCtx.spendStep(StepKind.Builtin) }
            ${ curried(bn, argKinds(bn), Nil) }
        }

    /** Generates the curried closure of `bn` as lambdas, one per remaining argument in `kinds`;
      * `bound` holds the term arguments taken so far.
      *
      * For `(builtin headList)`, which takes a force and then a term argument, it generates:
      * {{{
      * (() => ((a: UplcValue) => BuiltinImpls.headList(logger, budget, params, a)): BuiltinLamValue): BuiltinDelayValue
      * }}}
      * A force is a [[BuiltinDelayValue]] and a term argument a [[BuiltinLamValue]], so a closure
      * applied to the wrong kind fails with the error of the CEK machine's `VBuiltin`. The
      * innermost lambda calls the builtin ([[callBuiltin]]) and spends its cost; the others spend
      * nothing. Each lambda is a generator called inside the splice of its body, so its code gets
      * its final owner.
      */
    private def curried(bn: DefaultFun, kinds: List[Boolean], bound: List[Expr[UplcValue]])(using
        ctx: GenCtx,
        q: Quotes
    ): Expr[UplcValue] = kinds match
        case Nil => callBuiltin(bn, bound)
        case true :: rest =>
            '{ ((a: UplcValue) => ${ curried(bn, rest, bound :+ 'a) }): BuiltinLamValue }
        case false :: rest =>
            '{ (() => ${ curried(bn, rest, bound) }): BuiltinDelayValue }

    /** A saturated builtin application `[(force^k (builtin f)) a1 .. an]`, where `f` takes exactly
      * `k` forces and then `n` term arguments. `unapply` gives `(f, k, List(a1, .., an))`.
      *
      * This covers every builtin, forced ones included (`[(force (builtin headList)) xs]`). It
      * relies on the invariant that every `TypeScheme` lists its forces before its term arguments,
      * so a saturated application is `k` forces inside `n` applications. Any other shape (too few
      * or too many arguments, a force in the wrong place) is not matched and compiles as plain
      * applications of the curried closure, which fail where the CEK machine fails.
      */
    object AppliedBuiltin {
        def unapply(t: Term): Option[(DefaultFun, Int, List[Term])] = {
            @tailrec def apps(t: Term, args: List[Term]): (Term, List[Term]) = t match
                case Term.Apply(f, a, _) => apps(f, a :: args)
                case other               => (other, args)
            @tailrec def forces(t: Term, n: Int): (Term, Int) = t match
                case Term.Force(b, _) => forces(b, n + 1)
                case other            => (other, n)
            val (head, args) = apps(t, Nil)
            if args.isEmpty then None
            else
                forces(head, 0) match
                    case (Term.Builtin(bn, _), k)
                        if argKinds(bn) == List.fill(k)(false) ++ List.fill(args.size)(true) =>
                        Some((bn, k, args))
                    case _ => None
        }
    }

    /** Generates a saturated builtin application `[(force^k (builtin f)) a1 .. an]` (see
      * [[AppliedBuiltin]]): one static call, no closures.
      *
      * For `[(builtin addInteger) x (con integer 1)]`, with `x` a lambda parameter and the constant
      * at index 3 of the pool, it generates:
      * {{{
      * JitBudget.spendSaturated(budget, params, 2, 0)                  // Apply, Apply, Builtin
      * val v = { JitBudget.spendVar(budget, params); x }               // a1, runs once
      * val v = { JitBudget.spendConst(budget, params); consts(3) }     // a2, runs once
      * BuiltinImpls.addInteger(logger, budget, params, v, v)           // each v its own val
      * }}}
      * For `[(force (builtin headList)) xs]` it spends `spendSaturated(budget, params, 1, 1)`
      * (Apply, Force, Builtin) and calls `BuiltinImpls.headList`. For a bridged builtin,
      * `[(builtin appendString) s1 s2]` (ordinal 24), the last line is
      * {{{
      * CekBridge.call(CekBridge.builtinsFor(params), 24, logger, budget, Array[AnyRef](v, v))
      * }}}
      *
      * The steps are spent in CEK order (`Cek.scala` `computeCek`): the CEK machine spends the
      * Apply steps, then the Force steps, then the Builtin step while it walks down to the builtin,
      * and only then computes the arguments, in order; it spends the cost of the builtin when the
      * last one arrives, which the implementation does. So a `RestrictingBudgetSpender` stops at
      * the same step as the CEK machine. One static call spends the walk-down steps, to keep the
      * generated method small.
      *
      * Each argument is bound to a `val` before the call: its code spends its own step, and the
      * call needs it as a reference; spliced twice, an argument would run and spend twice. The
      * arguments are generators, each run inside the splice of its `val`, so its code is created
      * with the owner of that `val` (owner rule, spec [RU-1]).
      */
    def emitApplied(bn: DefaultFun, forces: Int, args: List[Quotes ?=> Expr[UplcValue]])(using
        ctx: GenCtx,
        q: Quotes
    ): Expr[UplcValue] = {
        def bind(rest: List[Quotes ?=> Expr[UplcValue]], bound: List[Expr[UplcValue]])(using
            Quotes
        ): Expr[UplcValue] = rest match
            case a :: tail => '{ val v = ${ a }; ${ bind(tail, bound :+ 'v) } }
            case Nil       => callBuiltin(bn, bound)
        '{
            JitBudget.spendSaturated(
              ${ ctx.budget },
              ${ ctx.params },
              ${ Expr(args.size) },
              ${ Expr(forces) }
            )
            ${ bind(args, Nil) }
        }
    }

    /** Generates the call of the implementation of `bn` on `args`, one expression per term
      * argument. Each expression is spliced once, so it must be a reference (a lambda parameter or
      * a `val`), not code with effects.
      *
      * compile time: a hand-written builtin ([[BuiltinImpls]]) or the bridge. For `HeadList`
      * (hand-written) and `AppendString` (bridged) on references `v1`, `v2` it generates:
      * {{{
      * BuiltinImpls.headList(logger, budget, params, v1)                                 // callImpl
      * CekBridge.call(CekBridge.builtinsFor(params), 24, logger, budget, Array(v1, v2)) // callBridge
      * }}}
      */
    def callBuiltin(bn: DefaultFun, args: List[Expr[UplcValue]])(using
        ctx: GenCtx,
        q: Quotes
    ): Expr[UplcValue] =
        if usesImpl(bn) then callImpl(bn, args, ctx.logger, ctx.budget, ctx.params)
        else callBridge(bn, args)

    /** Generates a static call of the [[BuiltinImpls]] method of `bn`.
      *
      * For `AddInteger` and arguments `a1`, `a2` it generates:
      * {{{
      * BuiltinImpls.addInteger(logger, budget, params, a1, a2)
      * }}}
      * compile time: the method is found by name ([[implName]]) on the `BuiltinImpls` module with
      * the reflection API of the staging `Quotes`. The tree is
      * `Apply(Select(Ref(BuiltinImpls), addInteger), args)`; every argument and the result are
      * [[UplcValue]]s, so it needs no casts. A name that matches no method or an overloaded name
      * fails here; `BuiltinImplsTest` rules both out.
      */
    private def callImpl(
        bn: DefaultFun,
        args: List[Expr[UplcValue]],
        logger: Expr[Logger],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using q: Quotes): Expr[UplcValue] = {
        import q.reflect.*
        val module = Symbol.requiredModule("scalus.uplc.jit.BuiltinImpls")
        val method = module.methodMember(implName(bn)) match
            case List(m) => m
            case other   => throw IllegalStateException(s"BuiltinImpls.${implName(bn)}: $other")
        val all = List(logger.asTerm, budget.asTerm, params.asTerm) ++ args.map(_.asTerm)
        Apply(Select(Ref(module), method), all).asExprOf[UplcValue]
    }

    /** Generates the call of a monomorphic builtin through the CEK runtime.
      *
      * For `AppendString` (ordinal 24) and arguments `a1`, `a2` it generates:
      * {{{
      * CekBridge.call(CekBridge.builtinsFor(params), 24, logger, budget, Array[AnyRef](a1, a2))
      * }}}
      * `CekBridge.builtinsFor(params)` is a cached lookup (usually a volatile read and a reference
      * compare); it is not a prologue `val`, which every lambda of the script would capture and so
      * add to the constant pool of the generated class. The bridge spends the CEK cost.
      */
    private def callBridge(bn: DefaultFun, args: List[Expr[UplcValue]])(using
        ctx: GenCtx,
        q: Quotes
    ): Expr[UplcValue] =
        '{
            CekBridge.call(
              CekBridge.builtinsFor(${ ctx.params }),
              ${ Expr(bn.ordinal) },
              ${ ctx.logger },
              ${ ctx.budget },
              Array[AnyRef](${ Varargs(args) }*)
            )
        }
}
