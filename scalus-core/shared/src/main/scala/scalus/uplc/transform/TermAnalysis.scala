package scalus.uplc.transform

import scalus.uplc.{DefaultFun, Meaning, Term}
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
      * @param term
      *   the term to analyze
      * @return
      *   Some((builtin, numForces, numApplies, appliedArgs)) if term contains a builtin, where:
      *   - builtin: the DefaultFun found
      *   - numForces: number of Force nodes wrapping the builtin
      *   - numApplies: number of Apply nodes wrapping the builtin
      *   - appliedArgs: list of arguments applied to the builtin (in application order) Returns
      *     None if no builtin is found.
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
                case Builtin(bn, _)   => Some((bn, forces, applies, args.reverse))
                case Force(inner, _)  => go(inner, forces + 1, applies, args)
                case Apply(f, arg, _) => go(f, forces, applies + 1, arg :: args)
                case _                => None
        go(term, 0, 0, List.empty)

    extension (term: Term)
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
          *   - '''Force of Delay''': `Force(Delay(t))` is pure for any t because it's essentially a
          *     no-op that evaluates to t
          *   - '''Forced polymorphic builtins''': `Force(Builtin(bn))` where the builtin expects
          *     type arguments (e.g., `Force(HeadList)` is pure because HeadList needs a type
          *     argument before it can be applied)
          *   - '''Partially applied builtins''': Builtin applications where not all required
          *     arguments are provided yet (e.g., `AddInteger $ 1` is pure because it needs one more
          *     argument). These cannot fail until fully saturated.
          *   - '''Beta-redexes with pure parts''': `Apply(LamAbs(_, body), arg)` where both `body`
          *     and `arg` are pure
          *   - '''Constructors with pure arguments''': `Constr(tag, args)` where all args are pure
          *   - '''Case expressions with pure parts''': `Case(scrut, cases)` where the scrutinee and
          *     all case branches are pure
          *
          * ==Impure Terms (Can Fail)==
          *
          * The following terms are considered impure because they can halt evaluation:
          *
          *   - '''Error''' (`Error`): Always fails with an error
          *   - '''Force of non-delayed, non-builtin terms''': `Force(t)` where t is not `Delay(_)`
          *     and not a builtin awaiting type arguments. Forcing a non-delayed term will error at
          *     runtime. For example, `Force(Const(1))` will fail because you cannot force a
          *     constant. However, `Force(Delay(t))` is pure because it's a no-op.
          *   - '''Saturated builtin applications''': Builtin applications where all required type
          *     and value arguments are provided. These may fail depending on the arguments (e.g.,
          *     `DivideInteger $ 1 $ 0` will error due to division by zero).
          *   - '''Apply/Case with impure subterms''': If any subterm is impure, the whole term is
          *     considered impure
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
          * Force(Delay(Const(1))).isPure          // true - Force(Delay) is a no-op
          *
          * Force(Const(1)).isPure                 // false - forcing non-delayed term
          * Error.isPure                           // false - always fails
          *
          * // Builtins
          * Builtin(AddInteger).isPure             // true - unapplied builtin
          * Apply(Builtin(AddInteger), Const(1)).isPure  // true - partial application
          * Apply(Apply(Builtin(AddInteger), Const(1)), Const(2)).isPure  // false - saturated
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
          *   [[https://github.com/IntersectMBO/plutus/blob/441b76d9e9745dfedb2afc29920498bdf632f162/plutus-core/plutus-ir/src/PlutusIR/Purity.hs#L272 Plutus IR Purity]]
          */
        def isPure: Boolean = term match
            case Apply(LamAbs(_, body, _), a, _) if a.isPure && body.isPure => true
            // Check if this is a partially applied builtin with pure arguments
            case app @ Apply(_, _, _) =>
                extractBuiltinInfo(app) match
                    case Some((bn, numForces, numApplies, appliedArgs)) =>
                        val meaning = Meaning.allBuiltins.getBuiltinRuntime(bn)
                        val requiredTypeArgs = meaning.typeScheme.numTypeVars
                        val requiredValueArgs = meaning.typeScheme.arity
                        // Saturated if all type and value arguments are applied
                        val isSaturated =
                            numForces >= requiredTypeArgs && numApplies >= requiredValueArgs
                        // Pure if not saturated and all applied arguments are pure
                        !isSaturated && appliedArgs.forall(_.isPure)
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
            // Force(Delay(x)) is pure - it's essentially a no-op that evaluates to x
            case Force(Delay(_, _), _) => true
            // force can halt the evaluation if the argument is not delayed
            // (lam x [(force t) x]) can't be eta-reduced in general
            // e.g. (lam x [(force (error)) x]) can't be eta-reduced to (force (error))
            // because (force (error)) will halt the evaluation and (lam x [(force (error)) x]) will not
            case Force(_, _) => false
            // (lam x [(builtin ..) x]) can be eta-reduced to (builtin ..)
            case Builtin(_, _)         => true
            case Constr(_, args, _)    => args.forall(_.isPure)
            case Case(scrut, cases, _) => scrut.isPure && cases.forall(_.isPure)
