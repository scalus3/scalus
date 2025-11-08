package scalus.uplc.transform

import scalus.*
import scalus.uplc.{DefaultFun, Meaning, NamedDeBruijn, Term}
import scalus.uplc.Term.*

/** Performs eta-reduction on a term.
  *
  * @see
  *   [[etaReduce]]
  */
object EtaReduce:
    /** Eta-reduces a term
      * @see
      *   [[etaReduce]]
      */
    def apply(term: Term): Term = etaReduce(term)

    /** Performs eta-reduction on a term.
      *
      * Eta-reduction is the process of removing redundant lambda abstractions from a term. For
      * example, the term `λx. f x` can be eta-reduced to `f` but only if
      *   - `x` is not free in `f`
      *   - `f` is pure expression
      *
      * A term is pure if it does not contain any side effects, such as `Error`, `Force` or
      * saturated builtin applications.
      *
      *   - `Error` is not pure because it halts the computation.
      *   - `Force` is not pure because it can halt the evaluation if the argument is not a
      *     `Delay`ed term or a builtin that must be forced.
      *   - a saturated builtin application is not pure because it can halt the evaluation in some
      *     cases. For example, `DivideInteger(1, 0)` will halt the evaluation.
      *
      * A builtin application is saturated if it has all its arguments applied. For example,
      * `AddInteger(1, 2)` is saturated but `AddInteger(1)` is not.
      *
      * @see
      *   [[https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B7-reduction Eta reduction]]
      */
    def etaReduce(term: Term): Term = term match
        case LamAbs(name1, Term.Apply(f, Term.Var(name2)))
            if name1 == name2.name && !freeNames(f).contains(name1) && isPure(f) =>
            etaReduce(f)
        case LamAbs(name, body) =>
            val body1 = etaReduce(body)
            if body != body1 then etaReduce(LamAbs(name, body1)) else term
        case Apply(f, arg) => Apply(etaReduce(f), etaReduce(arg))
        case Force(term)   => Force(etaReduce(term))
        case Delay(term)   => Delay(etaReduce(term))
        case _             => term

    /** Returns the set of free names in a term */
    def freeNames(term: Term): Set[String] = term match
        case Var(NamedDeBruijn(name, _)) => Set(name)
        case LamAbs(name, body)          => freeNames(body) - name
        case Apply(f, arg)               => freeNames(f) ++ freeNames(arg)
        case Force(term)                 => freeNames(term)
        case Delay(term)                 => freeNames(term)
        case _                           => Set.empty

    /** Extracts builtin information from a term with Force and Apply nodes
      *
      * @return
      *   Some((builtin, numForces, numApplies, appliedArgs)) or None if not a builtin application
      */
    private def extractBuiltinInfo(
        term: Term
    ): Option[(DefaultFun, Int, Int, List[Term])] =
        def go(
            t: Term,
            forces: Int,
            applies: Int,
            args: List[Term]
        ): Option[(DefaultFun, Int, Int, List[Term])] =
            t match
                case Builtin(bn)   => Some((bn, forces, applies, args.reverse))
                case Force(inner)  => go(inner, forces + 1, applies, args)
                case Apply(f, arg) => go(f, forces, applies + 1, arg :: args)
                case _             => None
        go(term, 0, 0, List.empty)

    /** Checks if a term is pure
      *
      * TODO: this is a very conservative definition of purity. We can improve it by considering the
      * semantics of the builtins.
      * @see
      *   [isPure](https://github.com/IntersectMBO/plutus/blob/441b76d9e9745dfedb2afc29920498bdf632f162/plutus-core/plutus-ir/src/PlutusIR/Purity.hs#L272)
      */
    private def isPure(term: Term): Boolean = term match
        case Apply(LamAbs(_, body), a) if isPure(a) && isPure(body) => true
        // Check if this is a partially applied builtin with pure arguments
        case app @ Apply(_, _) =>
            extractBuiltinInfo(app) match
                case Some((bn, numForces, numApplies, appliedArgs)) =>
                    val meaning = Meaning.allBuiltins.getBuiltinRuntime(bn)
                    val requiredTypeArgs = meaning.typeScheme.numTypeVars
                    val requiredValueArgs = meaning.typeScheme.arity
                    // Saturated if all type and value arguments are applied
                    val isSaturated =
                        numForces >= requiredTypeArgs && numApplies >= requiredValueArgs
                    // Pure if not saturated and all applied arguments are pure
                    !isSaturated && appliedArgs.forall(isPure)
                case None => false
        // (lam x [(lam ...) x]) can be eta-reduced to (lam ...)
        case LamAbs(_, _) => true
        // we had (lam x [(delay t) x]), it can be eta-reduced to (delay t)
        case Delay(_) => true
        // (lam x [(const ..) x]) can be eta-reduced to (const ..)
        case Const(_) => true
        // (lam x [(var f) x]) can be eta-reduced to (var f)
        case Var(_) => true // variables are pure
        // (lam x [(error) x]) can't be eta-reduced to (error)
        case Error => false
        case Force(Force(Builtin(bn)))
            if Meaning.allBuiltins.getBuiltinRuntime(bn).typeScheme.numTypeVars >= 2 =>
            true // this is pure
        case Force(Builtin(bn))
            if Meaning.allBuiltins.getBuiltinRuntime(bn).typeScheme.numTypeVars >= 1 =>
            true // this is pure
        // force can halt the evaluation if the argument is not delayed
        // (lam x [(force t) x]) can't be eta-reduced in general
        // e.g. (lam x [(force (error)) x]) can't be eta-reduced to (force (error))
        // because (force (error)) will halt the evaluation and (lam x [(force (error)) x]) will not
        case Force(_) => false
        // (lam x [(builtin ..) x]) can be eta-reduced to (builtin ..)
        case Builtin(_)         => true
        case Constr(_, args)    => args.forall(isPure)
        case Case(scrut, cases) => isPure(scrut) && cases.forall(isPure)
