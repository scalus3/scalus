package scalus.uplc.transform

import scalus.*
import scalus.uplc.Constant.flatConstant
import scalus.uplc.Term.*
import scalus.uplc.eval.{Log, Logger}
import scalus.uplc.transform.TermAnalysis.{freeVars, isPure}
import scalus.uplc.{NamedDeBruijn, Term}

/** Optimizer that performs function inlining, beta-reduction, and dead code elimination.
  *
  * The Inliner performs several transformations:
  *   - '''Beta-reduction''': Replaces function application with direct substitution when safe
  *   - '''Identity function inlining''': Eliminates identity functions like `λx.x`
  *   - '''Dead code elimination''': Removes unused lambda parameters when the argument is pure
  *   - '''Small value inlining''': Inlines variables, small constants, and builtins
  *   - '''Force/Delay elimination''': Simplifies `Force(Delay(t))` to `t`
  *   - '''Partial evaluation''': Evaluates closed subexpressions at compile time via the CEK
  *     machine (e.g., `addInteger 2 3` → `5`, `(λx. addInteger x 1) 2` → `3`)
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
  * beta-reduction.
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
    def apply(term: Term): Term = go(term)

    def logs: Seq[String] = logger.getLogs.toSeq

    /** Classifies how a variable occurs in a term body, distinguishing direct (always-evaluated)
      * positions from guarded (deferred/conditional) positions.
      *
      *   - '''Zero''': Variable does not occur — dead code, can be eliminated if argument is pure
      *   - '''OnceDirect''': Occurs exactly once in a direct position (always evaluated, same
      *     timing as the Apply site) — safe to inline any term
      *   - '''OnceGuarded''': Occurs exactly once under a Delay, Case branch, or LamAbs body — safe
      *     to inline values only (no side effects to defer/suppress)
      *   - '''Many''': Multiple occurrences — only inline if the value is small/cheap
      */
    private enum OccurrenceInfo:
        case Zero
        case OnceDirect
        case OnceGuarded
        case Many

    private object OccurrenceInfo:
        def combine(a: OccurrenceInfo, b: OccurrenceInfo): OccurrenceInfo =
            (a, b) match
                case (Zero, x) => x
                case (x, Zero) => x
                case _         => Many

        def guard(info: OccurrenceInfo): OccurrenceInfo =
            info match
                case Zero => Zero
                case _    => OnceGuarded

    /** Analyzes how a variable occurs in a term, tracking whether occurrences are in direct
      * (always-evaluated) or guarded (deferred/conditional) positions.
      *
      * @param term
      *   The term to analyze
      * @param name
      *   The variable name to look for
      * @param guarded
      *   Whether the current position is guarded (under Delay, Case branch, or LamAbs body)
      * @return
      *   The occurrence classification for the variable
      */
    private def analyzeOccurrence(
        term: Term,
        name: String,
        guarded: Boolean = false
    ): OccurrenceInfo =
        import OccurrenceInfo.*
        term match
            case Var(NamedDeBruijn(n, _), _) =>
                if n == name then if guarded then OnceGuarded else OnceDirect else Zero
            case LamAbs(n, body, _) =>
                if n == name then Zero // shadowed
                else analyzeOccurrence(body, name, guarded = true)
            case Apply(f, arg, _) =>
                val fInfo = analyzeOccurrence(f, name, guarded)
                if fInfo == Many then Many
                else combine(fInfo, analyzeOccurrence(arg, name, guarded))
            case Force(t, _) => analyzeOccurrence(t, name, guarded)
            case Delay(t, _) => analyzeOccurrence(t, name, guarded = true)
            case Constr(_, args, _) =>
                args.foldLeft(Zero: OccurrenceInfo) { (acc, a) =>
                    if acc == Many then Many
                    else combine(acc, analyzeOccurrence(a, name, guarded))
                }
            case Case(scrutinee, cases, _) =>
                val sInfo = analyzeOccurrence(scrutinee, name, guarded)
                if sInfo == Many then Many
                else
                    combine(
                      sInfo,
                      cases.foldLeft(Zero: OccurrenceInfo) { (acc, c) =>
                          if acc == Many then Many
                          else combine(acc, analyzeOccurrence(c, name, guarded = true))
                      }
                    )
            case _: Const | _: Builtin | _: Error => Zero

    /** Checks whether a term is a value (no computation needed to evaluate it). */
    private def isValue(term: Term): Boolean = term match
        case _: Var | _: Const | _: LamAbs | _: Delay | _: Builtin => true
        case Constr(_, Nil, _)                                     => true
        case _                                                     => false

    /** Determines if a term is safe to inline based on its type and occurrence info.
      *
      *   - '''OnceDirect''': Always safe — evaluation timing is unchanged
      *   - '''OnceGuarded''': Safe only for values (no side effects to defer/suppress)
      *   - '''Many''': Only safe for variables, small constants (≤64 bits), and builtins
      *   - '''Zero''': Not inlined (dead code handled separately)
      */
    private def shouldInline(inlining: Term, occInfo: OccurrenceInfo): Boolean =
        import OccurrenceInfo.*
        occInfo match
            case OnceDirect  => true
            case OnceGuarded => isValue(inlining)
            case Many =>
                inlining match
                    case Var(_, _)     => true
                    case Const(c, _)   => flatConstant.bitSize(c) <= 64
                    case Builtin(_, _) => true
                    case _             => false
            case Zero => false

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
        lazy val replacementFreeVars = replacement.freeVars

        def go(t: Term, boundVars: Set[String]): Term = t match
            case Var(NamedDeBruijn(n, _), _) =>
                if n == name && !boundVars.contains(n) then replacement
                else t

            case LamAbs(n, body, ann) =>
                if n == name then t
                else if replacementFreeVars.contains(n) then
                    val freshN = freshName(n, boundVars ++ replacementFreeVars ++ body.freeVars)
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

    /** Attempts to partially evaluate a term using the CEK machine.
      *
      * If the term is a closed, reducible expression that evaluates to a constant, returns that
      * constant. Otherwise returns the term unchanged.
      */
    private def tryPartialEval(term: Term): Term =
        PartialEvaluator.tryEval(term) match
            case Some(result) =>
                logger.log(s"Partial evaluation: ${term.showShort} => ${result.showShort}")
                result
            case None => term

    /** Main optimization pass that recursively optimizes the term tree.
      *
      * Performs a bottom-up traversal applying:
      *   - Identity function elimination: `(λx.x) t` → `t`
      *   - Dead code elimination: `(λx. body) arg` → `body` when `x` unused and `arg` is pure
      *   - Beta-reduction: `(λx. body) arg` → `body[x := arg]` for safe-to-inline terms
      *   - Force/Delay cancellation: `Force(Delay(t))` → `t`
      *   - Partial evaluation: closed reducible subexpressions are evaluated via the CEK machine
      *
      * @see
      *   [[TermAnalysis.isPure]] for purity analysis used in dead code elimination
      */
    private def go(term: Term): Term = term match
        case _: Var => term

        case Apply(f, arg, ann) =>
            val inlinedF = go(f)
            val inlinedArg = go(arg)
            inlinedF match
                // Inline identity functions
                case LamAbs(name, Var(NamedDeBruijn(vname, _), _), _) if name == vname =>
                    logger.log(s"Inlining identity function: $name")
                    inlinedArg
                case LamAbs(name, body, _) =>
                    val occInfo = analyzeOccurrence(body, name)
                    if occInfo == OccurrenceInfo.Zero && inlinedArg.isPure then
                        logger.log(s"Eliminating dead code: $name")
                        go(body)
                    else if shouldInline(inlinedArg, occInfo) then
                        logger.log(s"Inlining $name with ${inlinedArg.show}")
                        go(substitute(body, name, inlinedArg))
                    else tryPartialEval(Apply(inlinedF, inlinedArg, ann))
                case _ =>
                    tryPartialEval(Apply(inlinedF, inlinedArg, ann))

        case LamAbs(name, body, ann) => LamAbs(name, go(body), ann)
        case Force(Delay(t, _), _) =>
            logger.log(s"Eliminating Force(Delay(t)), t: ${t.showHighlighted}")
            go(t)
        case Force(t, ann) =>
            go(t) match
                case Delay(inner, _) =>
                    logger.log(s"Eliminating Force(Delay(t)) after optimization")
                    inner
                case optimized =>
                    tryPartialEval(Force(optimized, ann))
        case Delay(t, ann)          => Delay(go(t), ann)
        case Constr(tag, args, ann) => Constr(tag, args.map(go), ann)

        case Case(scrutinee, cases, ann) =>
            tryPartialEval(
              Case(
                go(scrutinee),
                cases.map(go),
                ann
              )
            )

        case _: Const | _: Builtin | _: Error => term

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
