package scalus.uplc.transform

import scalus.*
import scalus.uplc.Constant.flatConstant
import scalus.uplc.Term.*
import scalus.uplc.eval.{Log, Logger}
import scalus.uplc.transform.TermAnalysis.isPure
import scalus.uplc.{NamedDeBruijn, Term}

/** Optimizer that performs function inlining, beta-reduction, and dead code elimination.
  *
  * The Inliner performs several transformations:
  *   - '''Beta-reduction''': Replaces function application with direct substitution when safe
  *   - '''Identity function inlining''': Eliminates identity functions like `λx.x`
  *   - '''Dead code elimination''': Removes unused lambda parameters when the argument is pure
  *   - '''Small value inlining''': Inlines variables, small constants, and builtins
  *   - '''Force/Delay elimination''': Simplifies `Force(Delay(t))` to `t`
  *
  * ==Inlining Strategy==
  *
  * The inliner uses occurrence counting and purity analysis to decide what is safe to inline:
  *   - Variables, builtins, and small constants (≤64 bits) can be duplicated safely
  *   - Larger values are only inlined if used once
  *   - Pure unused arguments are eliminated entirely
  *
  * ==Example==
  *
  * {{{
  * // Input: (λx. λy. x) 42 100
  * // After inlining identity and dead code elimination:
  * // Output: 42
  *
  * val inliner = new Inliner()
  * val optimized = inliner(term)
  * // Check what was optimized
  * println(inliner.logs.mkString("\n"))
  * }}}
  *
  * ==Implementation Details==
  *
  * The inliner performs capture-avoiding substitution to prevent variable capture during
  * beta-reduction. It maintains an environment of inlined bindings and recursively processes the
  * term tree.
  *
  * @param logger
  *   Logger for tracking inlining operations (defaults to new Log())
  *
  * @see
  *   [[TermAnalysis.isPure]] for purity analysis used in dead code elimination
  * @see
  *   [[Optimizer]] for the base optimizer trait
  */
class Inliner(logger: Logger = new Log()) extends Optimizer:
    def apply(term: Term): Term =
        val inlined = inlinePass(inlineConstVarBuiltin)(term)
        inlined

    def logs: Seq[String] = logger.getLogs.toSeq

    /** Counts the number of free occurrences of a variable in a term.
      *
      * This is used to determine if inlining a value is safe:
      *   - 0 occurrences: dead code, can be eliminated if argument is pure
      *   - 1 occurrence: safe to inline any value
      *   - Multiple occurrences: only inline if the value is small/cheap
      *
      * The count stops when the variable is shadowed by a lambda binding, as those occurrences
      * refer to a different binding.
      *
      * @param term
      *   The term to search
      * @param name
      *   The variable name to count
      * @return
      *   Number of free occurrences of the variable
      */
    private def countOccurrences(term: Term, name: String): Int = term match
        case Var(NamedDeBruijn(n, _), _) => if n == name then 1 else 0
        case LamAbs(n, body, _) =>
            if n == name then 0 // Stop counting if shadowed
            else countOccurrences(body, name)
        case Apply(f, arg, _)   => countOccurrences(f, name) + countOccurrences(arg, name)
        case Force(t, _)        => countOccurrences(t, name)
        case Delay(t, _)        => countOccurrences(t, name)
        case Constr(_, args, _) => args.map(countOccurrences(_, name)).sum
        case Case(scrutinee, cases, _) =>
            countOccurrences(scrutinee, name) + cases.map(countOccurrences(_, name)).sum
        case _: Const | _: Builtin | _: Error => 0

    /** Determines if a term is safe to inline based on its type and occurrence count.
      *
      * This is the default inlining policy used by the Inliner. It follows these rules:
      *   - '''Variables''': Always safe to inline (no duplication cost)
      *   - '''Small constants''': Safe if ≤64 bits or used once
      *   - '''Builtins''': Always safe (just references)
      *   - '''Everything else''': Not safe to inline
      *
      * @param name
      *   The variable name being inlined
      * @param body
      *   The lambda body where the variable appears
      * @param inlining
      *   The term being inlined in place of the variable
      * @param occurrences
      *   Number of times the variable appears in the body
      * @return
      *   `true` if the term should be inlined, `false` otherwise
      */
    def inlineConstVarBuiltin(name: String, body: Term, inlining: Term, occurrences: Int): Boolean =
        inlining match
            case Var(_, _) => true // Variables are safe to duplicate
            case Const(c, _) =>
                if occurrences == 1 then true
                else flatConstant.bitSize(c) <= 64 // Small constants are safe
            case Builtin(_, _) => true
            case _             => false

    /** Performs capture-avoiding substitution `[x → s]t`.
      *
      * Substitutes all free occurrences of variable `x` with term `s` in term `t`, while avoiding
      * variable capture. This is the fundamental operation for beta-reduction.
      *
      * ==Capture Avoidance==
      *
      * When substituting under a lambda that binds a variable that's free in the replacement term,
      * alpha-conversion (renaming) is performed to avoid capture:
      *
      * {{{
      * // [x → y](λy. x)  would incorrectly become  λy. y  without alpha-conversion
      * // With alpha-conversion:
      * // [x → y](λy. x)  →  [x → y](λy'. x)  →  λy'. y
      * }}}
      *
      * ==Bound Variable Handling==
      *
      * Substitution stops at lambda bindings that shadow the variable:
      * {{{
      * [x → 42](λx. x)  →  λx. x  // inner x refers to lambda parameter, not substituted
      * }}}
      *
      * @param term
      *   The term in which to perform substitution
      * @param name
      *   The variable name to replace
      * @param replacement
      *   The term to substitute in place of the variable
      * @return
      *   The term with all free occurrences of `name` replaced by `replacement`
      */
    def substitute(term: Term, name: String, replacement: Term): Term =
        // Get all free variables in the replacement term
        def freeVars(t: Term): Set[String] = t match
            case Var(NamedDeBruijn(n, _), _) => Set(n)
            case LamAbs(n, body, _)          => freeVars(body) - n
            case Apply(f, a, _)              => freeVars(f) ++ freeVars(a)
            case Force(t, _)                 => freeVars(t)
            case Delay(t, _)                 => freeVars(t)
            case Constr(_, args, _)          => args.flatMap(freeVars).toSet
            case Case(scrutinee, cases, _) =>
                freeVars(scrutinee) ++ cases.flatMap(freeVars)
            case _: Const | _: Builtin | _: Error => Set.empty

        // Generate a fresh name that doesn't clash with any names in the set
        def freshName(base: String, avoid: Set[String]): String =
            if !avoid.contains(base) then base
            else
                var i = 0
                var fresh = s"${base}_$i"
                while avoid.contains(fresh) do
                    i += 1
                    fresh = s"${base}_$i"
                fresh

        // Compute free variables of replacement term once
        lazy val replacementFreeVars = freeVars(replacement)

        def go(t: Term, boundVars: Set[String]): Term = t match
            case Var(NamedDeBruijn(n, _), _) =>
                if n == name && !boundVars.contains(n) then replacement
                else t

            case LamAbs(n, body, ann) =>
                if n == name then t
                else if replacementFreeVars.contains(n) then
                    val freshN = freshName(n, boundVars ++ replacementFreeVars)
                    LamAbs(
                      freshN,
                      go(substitute(body, n, Var(NamedDeBruijn(freshN))), boundVars + freshN),
                      ann
                    )
                else LamAbs(n, go(body, boundVars + n), ann)

            case Apply(f, arg, ann) => Apply(go(f, boundVars), go(arg, boundVars), ann)

            case Force(t, ann) => Force(go(t, boundVars), ann)
            case Delay(t, ann) => Delay(go(t, boundVars), ann)

            case Constr(tag, args, ann) =>
                Constr(tag, args.map(arg => go(arg, boundVars)), ann)

            case Case(scrutinee, cases, ann) =>
                Case(
                  go(scrutinee, boundVars),
                  cases.map(c => go(c, boundVars)),
                  ann
                )

            case _: Const | _: Builtin | _: Error => t

        go(term, Set.empty)

    /** Main inlining pass that recursively optimizes the term tree.
      *
      * This method performs a bottom-up traversal of the term, applying various optimizations:
      *
      * '''Beta-reduction optimizations:'''
      *   - Identity functions `λx.x` are eliminated
      *   - Lambda applications are reduced when the inlining policy allows it
      *   - Dead code elimination removes unused pure arguments
      *
      * '''Other optimizations:'''
      *   - `Force(Delay(t))` is simplified to `t`
      *   - Variable substitution based on the environment
      *
      * The method uses the provided `shouldInline` predicate to decide which terms can be safely
      * inlined. It maintains an environment of inlined bindings and performs capture-avoiding
      * substitution when beta-reducing.
      *
      * @param shouldInline
      *   Predicate that determines if a term should be inlined. Takes (variable name, lambda body,
      *   term to inline, occurrence count) and returns true if inlining is safe.
      * @param term
      *   The term to optimize
      * @return
      *   The optimized term
      *
      * @see
      *   [[TermAnalysis.isPure]] for purity analysis used in dead code elimination
      */
    private def inlinePass(shouldInline: (String, Term, Term, Int) => Boolean)(
        term: Term
    ): Term =
        def go(term: Term, env: Map[String, Term]): Term = term match
            case Var(NamedDeBruijn(name, _), _) =>
                env.get(name) match
                    case Some(value) => value
                    case _           => term

            case Apply(f, arg, ann) =>
                val inlinedF = go(f, env)
                val inlinedArg = go(arg, env)
                // Try beta reduction if possible
                inlinedF match
                    // Inline identity functions
                    case LamAbs(name, Var(NamedDeBruijn(vname, _), _), _) if name == vname =>
                        logger.log(s"Inlining identity function: $name")
                        inlinedArg
                    case LamAbs(name, body, _) =>
                        // Count occurrences to decide if we should inline
                        val occurrences = countOccurrences(body, name)
                        if occurrences == 0 && inlinedArg.isPure then
                            // Dead code elimination - variable is never used
                            logger.log(s"Eliminating dead code: $name")
                            go(body, env)
                        else if shouldInline(name, body, inlinedArg, occurrences) then
                            logger.log(s"Inlining $name with ${inlinedArg.show}")
                            go(substitute(body, name, inlinedArg), env)
                        else
                            // non-safe term - keep the lambda
                            Apply(inlinedF, inlinedArg, ann)
                    case _ =>
                        Apply(inlinedF, inlinedArg, ann)

            case LamAbs(name, body, ann) => LamAbs(name, go(body, env - name), ann)
            case Force(Delay(t, _), _) =>
                logger.log(s"Eliminating Force(Delay(t)), t: ${t.showHighlighted}")
                go(t, env)
            case Force(t, ann)          => Force(go(t, env), ann)
            case Delay(t, ann)          => Delay(go(t, env), ann)
            case Constr(tag, args, ann) => Constr(tag, args.map(arg => go(arg, env)), ann)

            case Case(scrutinee, cases, ann) =>
                Case(
                  go(scrutinee, env),
                  cases.map(c => go(c, env)),
                  ann
                )

            case _: Const | _: Builtin | _: Error => term

        go(term, Map.empty)

/** Companion object providing convenient factory methods for the Inliner. */
object Inliner:
    /** Applies inlining optimization to a term using default settings.
      *
      * This is a convenience method equivalent to `new Inliner().apply(term)`.
      *
      * @param term
      *   The term to optimize
      * @return
      *   The optimized term with inlining applied
      */
    def apply(term: Term): Term = new Inliner().apply(term)
