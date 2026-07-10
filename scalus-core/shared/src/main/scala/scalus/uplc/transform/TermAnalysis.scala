package scalus.uplc.transform

import scalus.uplc.{DefaultFun, DefaultUni, Meaning, NamedDeBruijn, Term, TypeScheme}
import scalus.uplc.Term.*

/** Static analysis utilities for UPLC terms.
  *
  * Provides analysis methods for determining properties of UPLC terms that are useful for
  * optimization and transformation passes.
  */
object TermAnalysis:

    /** Extracts builtin information from a term consisting of Force and Apply nodes.
      *
      * Traverses the term structure to find the underlying builtin (if any) and counts how many
      * Force and Apply nodes wrap it, collecting the applied arguments.
      *
      * Only well-ordered chains of the shape `Apply*(Force*(Builtin))` are recognized: all builtin
      * type schemes are prenex (type arguments come first), so every Force must be applied before
      * (i.e. sit inside) every Apply. An interleaved chain like `Force(Apply(…))` errors at runtime
      * — the machine receives a term argument while expecting a type argument or vice versa — and
      * is reported as None so callers treat it conservatively.
      *
      * @param term
      *   the term to analyze
      * @return
      *   Some((builtin, numForces, numApplies, appliedArgs)) if term is a well-ordered builtin
      *   chain, where:
      *   - builtin: the DefaultFun found
      *   - numForces: number of Force nodes wrapping the builtin
      *   - numApplies: number of Apply nodes wrapping the builtin
      *   - appliedArgs: list of arguments applied to the builtin (in application order) Returns
      *     None if no builtin is found or the chain is interleaved.
      */
    private[transform] def extractBuiltinInfo(
        term: Term
    ): Option[(DefaultFun, Int, Int, List[Term])] =
        def go(
            t: Term,
            forces: Int,
            applies: Int,
            args: List[Term]
        ): Option[(DefaultFun, Int, Int, List[Term])] =
            t match
                // args are consed outermost-first while descending, so the accumulator is
                // already in application order (innermost Apply holds the first argument)
                case Builtin(bn, _)  => Some((bn, forces, applies, args))
                case Force(inner, _) => go(inner, forces + 1, applies, args)
                // an Apply below a Force means the chain is interleaved (invalid)
                case Apply(f, arg, _) if forces == 0 => go(f, forces, applies + 1, arg :: args)
                case _                               => None
        go(term, 0, 0, List.empty)

    /** Whether the applied arguments could conform to the builtin's declared parameter types.
      *
      * `isTotal` only guarantees a builtin cannot fail on well-typed input: a saturated total
      * builtin applied to an ill-typed argument (e.g. `addInteger 1 True`) still fails at
      * unlifting. This check rejects ''provably'' ill-typed applications: a constant argument in a
      * concretely-typed (`Type`/`App` shaped, i.e. unlifted) parameter position must have a
      * unifiable type, with type variables bound consistently across parameters.
      *
      * Non-constant arguments (variables, applications, …) have statically unknown types and are
      * accepted: the analysis assumes the surrounding program is well-typed, which holds for the
      * optimizer's input — UPLC lowered from typed SIR. A bare type-variable parameter is opaque —
      * never unlifted — so any argument conforms there (e.g. `ifThenElse` branches, delayed or
      * not).
      */
    private def argsConformToScheme(ts: TypeScheme, args: List[Term]): Boolean =
        def unify(
            p: TypeScheme,
            u: DefaultUni,
            env: Map[String, DefaultUni]
        ): Option[Map[String, DefaultUni]] = p match
            case TypeScheme.Type(pu) => if pu == u then Some(env) else None
            case TypeScheme.TVar(n) =>
                env.get(n) match
                    case Some(bound) => if bound == u then Some(env) else None
                    case None        => Some(env.updated(n, u))
            case TypeScheme.App(f, a) =>
                u match
                    case DefaultUni.Apply(uf, ua) => unify(f, uf, env).flatMap(unify(a, ua, _))
                    case _                        => None
            case _ => None
        def loop(t: TypeScheme, args: List[Term], env: Map[String, DefaultUni]): Boolean =
            args match
                case Nil => true
                case arg :: rest =>
                    t match
                        case TypeScheme.All(_, body) => loop(body, args, env)
                        case TypeScheme.Arrow(p, next) =>
                            p match
                                case TypeScheme.TVar(_) => loop(next, rest, env)
                                case _ =>
                                    arg match
                                        case Const(c, _) =>
                                            unify(p, c.tpe, env) match
                                                case Some(env2) => loop(next, rest, env2)
                                                case None       => false
                                        // unknown static type: assume well-typed program
                                        case _ => loop(next, rest, env)
                        case _ => false
        loop(ts, args, Map.empty)

    /** Whether evaluating `[branch a1 … an]` is guaranteed not to fail, assuming the `n` arguments
      * are pure values (as case branch applications are per the CEK machine). Peels one lambda per
      * argument; any other shape is conservatively considered impure because applying a non-lambda
      * value fails at runtime.
      */
    private def isPureWhenAppliedTo(branch: Term, argCount: Int): Boolean =
        if argCount == 0 then branch.isPure
        else
            branch match
                case LamAbs(_, body, _) => isPureWhenAppliedTo(body, argCount - 1)
                case _                  => false

    extension (term: Term)

        /** Whether the term is in value form (weak head normal form).
          *
          * A value requires no further computation to evaluate. This is used by:
          *   - [[PartialEvaluator]] to skip terms that are already fully reduced
          *   - [[Inliner]] to decide if a term is safe to inline in guarded positions
          *
          * Value forms include:
          *   - Variables, constants, lambda abstractions, delays, and unapplied builtins
          *   - Constructors with all-value arguments (`Constr(tag, args)` where each arg is a
          *     value)
          *   - Unsaturated builtin applications: Force/Apply chains over a Builtin that have not
          *     yet received all required arguments (e.g., `Force(Builtin(HeadList))`,
          *     `Apply(Builtin(AddInteger), Const(1))`, `Apply(Force(Builtin(MkCons)), Const(d))`)
          */
        def isValueForm: Boolean = term match
            case _: Var | _: Const | _: LamAbs | _: Delay | _: Builtin => true
            case Constr(_, args, _)                                    => args.forall(_.isValueForm)
            case _: Force | _: Apply =>
                extractBuiltinInfo(term) match
                    case Some((bn, numForces, numApplies, appliedArgs)) =>
                        val ts = Meaning.allBuiltins.getBuiltinRuntime(bn).typeScheme
                        // Not over-forced (would crash at runtime)
                        numForces <= ts.numTypeVars
                        // Value args only after all type args are supplied
                        && (numApplies == 0 || numForces == ts.numTypeVars)
                        // Not yet saturated (saturated builtins compute, not values)
                        && numApplies < ts.arity
                        // All applied arguments must themselves be values
                        && appliedArgs.forall(_.isValueForm)
                    case None => false
            case _ => false

        /** Returns the set of free variable names in a term.
          *
          * A variable is free if it is not bound by an enclosing lambda abstraction. This is used
          * by the partial evaluator to determine if a term is closed (has no free variables) and
          * can therefore be evaluated at compile time.
          *
          * @return
          *   the set of free variable names
          */
        def freeVars: Set[String] = term match
            case Var(NamedDeBruijn(n, _), _)      => Set(n)
            case LamAbs(n, body, _)               => body.freeVars - n
            case Apply(f, a, _)                   => f.freeVars ++ a.freeVars
            case Force(t, _)                      => t.freeVars
            case Delay(t, _)                      => t.freeVars
            case Constr(_, args, _)               => args.flatMap(_.freeVars).toSet
            case Case(scrut, cases, _)            => scrut.freeVars ++ cases.flatMap(_.freeVars)
            case _: Const | _: Builtin | _: Error => Set.empty

        /** Checks if this term is pure (guaranteed not to fail during evaluation).
          *
          * A term is pure if evaluating it cannot produce an error or halt evaluation. Pure terms
          * have no observable side effects and can be safely:
          *   - Eliminated if their result is unused (dead code elimination)
          *   - Duplicated (e.g., inlining)
          *   - Reordered relative to other pure terms
          *
          * ==Pure Terms==
          *
          * The following terms are guaranteed to be pure:
          *
          *   - '''Variables''' (`Var`): References to bound variables
          *   - '''Constants''' (`Const`): Literal values (integers, strings, etc.)
          *   - '''Builtins''' (`Builtin`): Builtin functions (unapplied)
          *   - '''Lambda abstractions''' (`LamAbs`): Function definitions
          *   - '''Delays''' (`Delay`): Suspended computations
          *   - '''Force of Delay''': `Force(Delay(t))` is pure iff `t` is pure, because
          *     `(force (delay t))` reduces to `t` — the delayed body is evaluated
          *   - '''Forced polymorphic builtins''': `Force(Builtin(bn))` where the builtin expects
          *     type arguments (e.g., `Force(HeadList)` is pure because HeadList needs a type
          *     argument before it can be applied)
          *   - '''Partially applied builtins''': Builtin applications where not all required
          *     arguments are provided yet (e.g., `AddInteger $ 1` is pure because it needs one more
          *     argument). These cannot fail until fully saturated.
          *   - '''Beta-redexes with pure parts''': `Apply(LamAbs(_, body), arg)` where both `body`
          *     and `arg` are pure
          *   - '''Constructors with pure arguments''': `Constr(tag, args)` where all args are pure
          *   - '''Case on a literal in-range constructor''': `Case(Constr(i, args), cases)` where
          *     `i < cases.length`, all args are pure, and the selected branch is a lambda per
          *     constructor argument with a pure body (`case` applies the branch to the args)
          *
          * ==Impure Terms (Can Fail)==
          *
          * The following terms are considered impure because they can halt evaluation:
          *
          *   - '''Error''' (`Error`): Always fails with an error
          *   - '''Force of non-delayed, non-builtin terms''': `Force(t)` where t is not `Delay(_)`
          *     and not a builtin awaiting type arguments. Forcing a non-delayed term will error at
          *     runtime. For example, `Force(Const(1))` will fail because you cannot force a
          *     constant. `Force(Delay(t))` is pure only when `t` is pure, since forcing evaluates
          *     the delayed body.
          *   - '''Saturated partial builtin applications''': Builtin applications where all
          *     required type and value arguments are provided and the builtin is not total. These
          *     may fail depending on the arguments (e.g., `DivideInteger $ 1 $ 0` will error due to
          *     division by zero). Note: saturated ''total'' builtins like `AddInteger $ 1 $ 2` are
          *     pure.
          *   - '''Ill-formed builtin applications''': under-forced (`Apply(Builtin(HeadList), xs)`
          *     — a type argument is expected first), over-forced, or over-applied (`AddInteger $ 1
          *     $ 2 $ 3`) builtin chains error at runtime.
          *   - '''Apply/Case with impure subterms''': If any subterm is impure, the whole term is
          *     considered impure
          *   - '''Case with unknown scrutinee''': `Case(scrut, cases)` where the scrutinee is not a
          *     literal `Constr` — a non-constructor or out-of-range tag halts evaluation, and the
          *     selected branch is applied to the constructor arguments (which can also fail)
          *
          * ==Usage in Optimization Passes==
          *
          * '''Dead Code Elimination''' (Inliner):
          * {{{
          * // Safe to eliminate unused pure arguments
          * if occurrences == 0 && inlinedArg.isPure then
          *   go(body, env)  // eliminate the argument
          * }}}
          *
          * '''Eta Reduction''' (EtaReduce):
          * {{{
          * // Safe to perform eta-reduction if the function is pure
          * case LamAbs(x, Apply(f, Var(x))) if !freeNames(f).contains(x) && f.isPure =>
          *   f  // reduce λx. f x to f
          * }}}
          *
          * ==Examples==
          *
          * {{{
          * Const(Integer(42)).isPure              // true - constant
          * Var(NamedDeBruijn("x", 0)).isPure      // true - variable
          * LamAbs("x", Var("x")).isPure           // true - lambda
          * Delay(Const(1)).isPure                 // true - delay
          * Force(Delay(Const(1))).isPure          // true - reduces to Const(1)
          *
          * Force(Delay(Error)).isPure             // false - reduces to Error
          * Force(Const(1)).isPure                 // false - forcing non-delayed term
          * Error.isPure                           // false - always fails
          *
          * // Builtins
          * Builtin(AddInteger).isPure             // true - unapplied builtin
          * Apply(Builtin(AddInteger), Const(1)).isPure  // true - partial application
          * Apply(Apply(Builtin(AddInteger), Const(1)), Const(2)).isPure  // true - saturated total
          * Apply(Apply(Builtin(DivideInteger), Const(1)), Const(0)).isPure  // false - saturated partial
          *
          * Force(Builtin(HeadList)).isPure        // true - builtin needs type arg
          * Apply(Force(Builtin(HeadList)), list).isPure  // depends on list.isPure
          *
          * // Constructors and Cases
          * Constr(0, List(Const(1), Const(2))).isPure  // true - pure args
          * Constr(0, List(Error, Const(2))).isPure     // false - impure arg
          * }}}
          *
          * @return
          *   true if the term is guaranteed to be pure (won't fail), false if the term might fail
          *   during evaluation
          *
          * @see
          *   [[https://github.com/IntersectMBO/plutus/blob/master/plutus-core/untyped-plutus-core/src/UntypedPlutusCore/Purity.hs UntypedPlutusCore.Purity]]
          *   — the UPLC-level counterpart of this analysis (note: it treats all saturated builtin
          *   applications and all Case terms as impure; this analysis is deliberately more precise)
          * @see
          *   [[https://github.com/IntersectMBO/plutus/blob/441b76d9e9745dfedb2afc29920498bdf632f162/plutus-core/plutus-ir/src/PlutusIR/Purity.hs#L272 Plutus IR Purity]]
          */
        def isPure: Boolean = term match
            case Apply(LamAbs(_, body, _), a, _) if a.isPure && body.isPure => true
            // Check if this is a partially applied builtin with pure arguments
            case app @ Apply(_, _, _) =>
                extractBuiltinInfo(app) match
                    case Some((bn, numForces, numApplies, appliedArgs)) =>
                        val ts = Meaning.allBuiltins.getBuiltinRuntime(bn).typeScheme
                        // All type args must be forced before value args are applied
                        // (numApplies >= 1 here): under- or over-forcing errors at runtime
                        numForces == ts.numTypeVars
                        // Applying the result of a saturated builtin to more args errors
                        && numApplies <= ts.arity
                        // Saturated builtins compute: pure only if the builtin is total AND
                        // the arguments are known well-typed (totality says nothing about
                        // ill-typed input, which fails at unlifting)
                        && (numApplies < ts.arity ||
                            (bn.isTotal && argsConformToScheme(ts, appliedArgs)))
                        // All applied arguments must themselves be pure
                        && appliedArgs.forall(_.isPure)
                    case None => false
            // (lam x [(lam ...) x]) can be eta-reduced to (lam ...)
            case LamAbs(_, _, _) => true
            // we had (lam x [(delay t) x]), it can be eta-reduced to (delay t)
            case Delay(_, _) => true
            // (lam x [(const ..) x]) can be eta-reduced to (const ..)
            case Const(_, _) => true
            // (lam x [(var f) x]) can be eta-reduced to (var f)
            case Var(_, _) => true // variables are pure
            // (lam x [(error) x]) can't be eta-reduced to (error)
            case Error(_) => false
            case Force(Force(Builtin(bn, _), _), _)
                if Meaning.allBuiltins.getBuiltinRuntime(bn).typeScheme.numTypeVars >= 2 =>
                true // this is pure
            case Force(Builtin(bn, _), _)
                if Meaning.allBuiltins.getBuiltinRuntime(bn).typeScheme.numTypeVars >= 1 =>
                true // this is pure
            // (force (delay x)) → x per the spec, so it is exactly as pure as x
            case Force(Delay(x, _), _) => x.isPure
            // force can halt the evaluation if the argument is not delayed
            // (lam x [(force t) x]) can't be eta-reduced in general
            // e.g. (lam x [(force (error)) x]) can't be eta-reduced to (force (error))
            // because (force (error)) will halt the evaluation and (lam x [(force (error)) x]) will not
            case Force(_, _) => false
            // (lam x [(builtin ..) x]) can be eta-reduced to (builtin ..)
            case Builtin(_, _)      => true
            case Constr(_, args, _) => args.forall(_.isPure)
            // (case (constr i V…) U1…Un) → [U_{i+1} V…] when 0 ≤ i ≤ n−1, errors otherwise.
            // Pure only when the scrutinee is a literal in-range Constr with pure args and the
            // selected branch tolerates being applied to them; any other scrutinee may fail.
            case Case(scrut, cases, _) =>
                scrut match
                    case Constr(tag, args, _) =>
                        val i = tag.toLong
                        0 <= i && i < cases.length
                        && args.forall(_.isPure)
                        && isPureWhenAppliedTo(cases(i.toInt), args.length)
                    case _ => false
