package scalus.uplc.transform

import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.{DefaultFun, Meaning, NamedDeBruijn}
import scalus.uplc.eval.{Log, Logger}
import scalus.uplc.transform.TermAnalysis.{freeVars, isValueForm}

import scala.collection.mutable

/** Common Subexpression Elimination (CSE) for UPLC terms.
  *
  * Identifies duplicated subexpressions and hoists them into shared `let`-bindings (encoded as
  * `Apply(LamAbs(name, body), expr)`). This reduces on-chain execution budget by avoiding redundant
  * computation.
  *
  * ==Algorithm==
  *
  * Adapted from the Plutus CSE (3-pass path-based algorithm):
  *
  *   1. '''Count pass''': Traverse the term, assigning unique path IDs at evaluation boundaries
  *      (LamAbs, Delay, Case branches). Count `(TermKey, Path) -> occurrences` pairs. Track which
  *      variables are introduced at each path segment.
  *   1. '''Merge & filter''': Merge counts to common ancestor paths and filter to candidates with
  *      total count >= 2 that are not work-free. Verify that all free variables of the candidate
  *      are in scope at the bind path.
  *   1. '''Substitute pass''': For each candidate (largest first), replace occurrences with a fresh
  *      variable and insert `Apply(LamAbs(freshName, body), candidate)` at the bind point.
  *
  * ==Safety==
  *
  *   - Work-free terms (Var, Const, LamAbs, Delay, Builtin, unsaturated builtins) are never
  *     extracted
  *   - Error terms are never extracted
  *   - Bare `Force(Builtin(...))` patterns are skipped (handled by [[ForcedBuiltinsExtractor]])
  *   - CSE does not hoist expressions past lambdas that bind their free variables
  *   - CSE does not hoist across evaluation boundaries, preserving evaluation order
  *
  * @param logger
  *   Logger for tracking CSE operations
  *
  * @see
  *   [[https://github.com/IntersectMBO/plutus/blob/master/plutus-core/untyped-plutus-core/src/UntypedPlutusCore/Transform/Cse.hs Plutus CSE]]
  */
class CommonSubexpressionElimination(logger: Logger = new Log()) extends Optimizer {
    import CommonSubexpressionElimination.*

    def apply(term: Term): Term = cse(term)

    def logs: Seq[String] = logger.getLogs.toSeq

    private def cse(term: Term): Term = {
        // Pass 1: Count occurrences of each subexpression at each path.
        // A "path" is a Vector[Int] of unique IDs assigned at evaluation boundaries
        // (LamAbs body, Delay body, Case branches). Subexpressions under the same
        // path prefix share the same evaluation context.
        var nextId = 0
        def freshPathId(): Int = { val id = nextId; nextId += 1; id }

        // Track which variable name each path segment introduces (for scope checking)
        val pathIdToVar = mutable.HashMap.empty[Int, String]

        val counts = mutable.HashMap.empty[TermKey, mutable.ArrayBuffer[(Path, Int)]]
        // Track which path IDs correspond to conditional evaluation boundaries
        // (Case branches and Delay nodes). These are positions where the expression
        // may not be evaluated depending on which branch is taken. LamAbs are excluded
        // because lambda bodies are unconditionally evaluated when the lambda is applied.
        val conditionalPathIds = mutable.HashSet.empty[Int]

        def addCount(key: TermKey, path: Path): Unit = {
            val pathCounts = counts.getOrElseUpdate(key, mutable.ArrayBuffer.empty)
            val idx = pathCounts.indexWhere(_._1 == path)
            if idx >= 0 then pathCounts(idx) = (path, pathCounts(idx)._2 + 1)
            else pathCounts += ((path, 1))
        }

        def countPass(t: Term, path: Path): Unit = t match
            case Var(_, _) | Const(_, _) | Builtin(_, _) | Error(_) => ()
            case LamAbs(name, body, _) =>
                val id = freshPathId()
                pathIdToVar(id) = name
                countPass(body, path :+ id)
            case Apply(f, arg, _) =>
                if !isSkippable(t) then addCount(new TermKey(t), path)
                countPass(f, path)
                countPass(arg, path)
            case Force(inner, _) =>
                if !isSkippable(t) then addCount(new TermKey(t), path)
                countPass(inner, path)
            case Delay(inner, _) =>
                val id = freshPathId()
                conditionalPathIds += id
                countPass(inner, path :+ id)
            case Constr(_, args, _) =>
                if !isSkippable(t) then addCount(new TermKey(t), path)
                args.foreach(countPass(_, path))
            case Case(scrutinee, cases, _) =>
                if !isSkippable(t) then addCount(new TermKey(t), path)
                countPass(scrutinee, path)
                cases.foreach { c =>
                    val id = freshPathId()
                    conditionalPathIds += id
                    countPass(c, path :+ id)
                }

        countPass(term, Vector.empty)

        // Compute which variables are in scope at a given path.
        // Variables are introduced by LamAbs nodes; the path segment ID maps to the var name.
        val topLevelFreeVars = term.freeVars
        def varsInScope(path: Path): Set[String] =
            topLevelFreeVars ++ path.flatMap(id => pathIdToVar.get(id))

        // Check if hoisting from occurrencePath to bindPath is safe for the given free vars.
        // A variable must be in scope at bindPath AND must NOT be re-bound (shadowed) by any
        // lambda between bindPath and occurrencePath. If a variable is shadowed, the expression
        // at the occurrence refers to the inner binding, but at the bind path it would refer
        // to a different (outer) binding.
        def isSafeToHoist(
            freeVars: Set[String],
            bindPath: Path,
            occurrencePath: Path
        ): Boolean = {
            val scopeAtBind = varsInScope(bindPath)
            if !freeVars.subsetOf(scopeAtBind) then return false
            // Check for shadowing: path segments between bindPath and occurrencePath
            val extensionSegments = occurrencePath.drop(bindPath.length)
            val shadowedVars = extensionSegments.flatMap(id => pathIdToVar.get(id)).toSet
            // If any free variable is re-bound in the extension, the binding is unsafe
            freeVars.intersect(shadowedVars).isEmpty
        }

        // Pass 2: For each expression, compute the bind path (longest common ancestor
        // of all occurrence paths) and total count.
        case class CseCandidate(key: TermKey, bindPath: Path, totalCount: Int, size: Int)

        val candidates = mutable.ArrayBuffer.empty[CseCandidate]

        for (key, pathCounts) <- counts do
            val allPaths = pathCounts.flatMap { case (path, count) =>
                Iterator.fill(count)(path)
            }.toVector

            val totalCount = allPaths.size
            if totalCount >= 2 then
                val bindPath = longestCommonPrefix(allPaths)
                val candidateFreeVars = key.term.freeVars
                // Safety check: free variables must be in scope at the bind path AND
                // must not be shadowed between bind path and ANY occurrence path.
                val safeForAll = allPaths.forall(p => isSafeToHoist(candidateFreeVars, bindPath, p))
                // Check if hoisting crosses a conditional boundary with partial builtins.
                // Conditional boundaries are Case branches and Delay nodes (used in
                // IfThenElse patterns). If occurrences are inside conditional branches and
                // the expression contains partial builtins (headList, unConstrData, etc.),
                // hoisting above the boundary removes the data-shape guarantee provided by
                // branch selection.
                val crossesConditional = allPaths.exists { path =>
                    path.length > bindPath.length &&
                    conditionalPathIds.contains(path(bindPath.length))
                }
                val unsafeCaseCrossing =
                    crossesConditional && referencesPartialBuiltin(key.term)
                if safeForAll && !unsafeCaseCrossing then
                    candidates += CseCandidate(key, bindPath, totalCount, termSize(key.term))

        if candidates.isEmpty then return term

        // Sort by size descending, then by name for determinism
        val sortedCandidates = candidates.sortBy(c => (-c.size, c.key.toString))

        // Collect all existing names to avoid collisions
        val existingNames = collectNames(term)
        var nameCounter = 0
        def freshCseName(): String = {
            var name = s"__cse_$nameCounter"
            nameCounter += 1
            while existingNames.contains(name) do
                name = s"__cse_$nameCounter"
                nameCounter += 1
            existingNames += name
            name
        }

        // Pass 3: Apply substitutions one at a time (largest candidates first).
        // After each substitution, the replaced nodes are gone, so smaller candidates
        // that were subterms of the larger one will naturally see fewer occurrences.
        var currentTerm = term

        for cand <- sortedCandidates do
            // Re-count occurrences of this candidate in the (possibly modified) current term.
            // This is necessary because prior substitutions may have removed some occurrences.
            val occurrences = mutable.ArrayBuffer.empty[Path]
            var reNextId = 0
            def reFreshId(): Int = { val id = reNextId; reNextId += 1; id }

            // Re-build pathIdToVar for the re-count pass on the modified term
            val rePathIdToVar = mutable.HashMap.empty[Int, String]
            val reConditionalPathIds = mutable.HashSet.empty[Int]

            def reCount(t: Term, path: Path): Unit = t match
                case _ if cand.key.term ~=~ t =>
                    occurrences += path
                    // Don't recurse into matched subterms -- they'll all be replaced
                case Var(_, _) | Const(_, _) | Builtin(_, _) | Error(_) => ()
                case LamAbs(name, body, _) =>
                    val id = reFreshId()
                    rePathIdToVar(id) = name
                    reCount(body, path :+ id)
                case Apply(f, arg, _) =>
                    reCount(f, path)
                    reCount(arg, path)
                case Force(inner, _) =>
                    reCount(inner, path)
                case Delay(inner, _) =>
                    val id = reFreshId()
                    reConditionalPathIds += id
                    reCount(inner, path :+ id)
                case Constr(_, args, _) =>
                    args.foreach(reCount(_, path))
                case Case(scrutinee, cases, _) =>
                    reCount(scrutinee, path)
                    cases.foreach { c =>
                        val id = reFreshId()
                        reConditionalPathIds += id
                        reCount(c, path :+ id)
                    }

            reCount(currentTerm, Vector.empty)

            if occurrences.size >= 2 then
                val bindPath = longestCommonPrefix(occurrences.toVector)

                // Re-check scope safety with the modified term's path-to-var mapping.
                // Must verify no shadowing between bind path and any occurrence path.
                val reFreeVars = currentTerm.freeVars
                val candFreeVars = cand.key.term.freeVars
                def reIsSafeToHoist(bindP: Path, occP: Path): Boolean = {
                    val scope = reFreeVars ++ bindP.flatMap(id => rePathIdToVar.get(id))
                    if !candFreeVars.subsetOf(scope) then return false
                    val ext = occP.drop(bindP.length)
                    val shadowed = ext.flatMap(id => rePathIdToVar.get(id)).toSet
                    candFreeVars.intersect(shadowed).isEmpty
                }
                val reSafe = occurrences.forall(p => reIsSafeToHoist(bindPath, p))
                val reCrossesConditional = occurrences.exists { path =>
                    path.length > bindPath.length &&
                    reConditionalPathIds.contains(path(bindPath.length))
                }
                val reUnsafeCaseCrossing =
                    reCrossesConditional && referencesPartialBuiltin(cand.key.term)
                if reSafe && !reUnsafeCaseCrossing then
                    val freshName = freshCseName()
                    val varTerm = Var(NamedDeBruijn(freshName))

                    // Substitute all occurrences with the fresh variable
                    var subNextId = 0
                    def subFreshId(): Int = { val id = subNextId; subNextId += 1; id }

                    def doSubstitute(t: Term, path: Path): Term = t match
                        case _ if (cand.key.term ~=~ t) && isAncestorOrSelf(bindPath, path) =>
                            varTerm
                        case Var(_, _) | Const(_, _) | Builtin(_, _) | Error(_) => t
                        case LamAbs(name, body, ann) =>
                            val id = subFreshId()
                            LamAbs(name, doSubstitute(body, path :+ id), ann)
                        case Apply(f, arg, ann) =>
                            Apply(doSubstitute(f, path), doSubstitute(arg, path), ann)
                        case Force(inner, ann) =>
                            Force(doSubstitute(inner, path), ann)
                        case Delay(inner, ann) =>
                            val id = subFreshId()
                            Delay(doSubstitute(inner, path :+ id), ann)
                        case Constr(tag, args, ann) =>
                            Constr(tag, args.map(doSubstitute(_, path)), ann)
                        case Case(scrutinee, cases, ann) =>
                            val newScrutinee = doSubstitute(scrutinee, path)
                            val newCases = cases.map { c =>
                                val id = subFreshId()
                                doSubstitute(c, path :+ id)
                            }
                            Case(newScrutinee, newCases, ann)

                    val substituted = doSubstitute(currentTerm, Vector.empty)

                    // Insert the let-binding at the bind path
                    currentTerm = insertLetAtPath(substituted, bindPath, freshName, cand.key.term)
                    logger.log(
                      s"CSE: extracted ${cand.key.term.showShort} (${occurrences.size} occurrences) as $freshName"
                    )

        currentTerm
    }

    /** Inserts `Apply(LamAbs(name, <hole>), expr)` at the position corresponding to the bind path.
      *
      * The bind path identifies a point in the tree where the computation is guaranteed to be
      * evaluated if any of the occurrences are evaluated. We walk the tree, tracking path IDs at
      * evaluation boundaries, and wrap the subtree at the target point.
      */
    private def insertLetAtPath(term: Term, bindPath: Path, name: String, expr: Term): Term = {
        if bindPath.isEmpty then return Apply(LamAbs(name, term), expr)

        var nextId = 0
        def freshId(): Int = { val id = nextId; nextId += 1; id }

        def go(t: Term, remaining: Path): Term = t match
            case LamAbs(n, body, ann) =>
                val id = freshId()
                if remaining.nonEmpty && remaining.head == id then
                    if remaining.size == 1 then
                        // Wrap the lambda body with the let-binding
                        LamAbs(n, Apply(LamAbs(name, body), expr), ann)
                    else LamAbs(n, go(body, remaining.tail), ann)
                else LamAbs(n, go(body, remaining), ann)
            case Apply(f, arg, ann) =>
                Apply(go(f, remaining), go(arg, remaining), ann)
            case Force(inner, ann) =>
                Force(go(inner, remaining), ann)
            case Delay(inner, ann) =>
                val id = freshId()
                if remaining.nonEmpty && remaining.head == id then
                    if remaining.size == 1 then Delay(Apply(LamAbs(name, inner), expr), ann)
                    else Delay(go(inner, remaining.tail), ann)
                else Delay(go(inner, remaining), ann)
            case Constr(tag, args, ann) =>
                Constr(tag, args.map(go(_, remaining)), ann)
            case Case(scrutinee, cases, ann) =>
                val newScrutinee = go(scrutinee, remaining)
                val newCases = cases.map { c =>
                    val id = freshId()
                    if remaining.nonEmpty && remaining.head == id then
                        if remaining.size == 1 then Apply(LamAbs(name, c), expr)
                        else go(c, remaining.tail)
                    else go(c, remaining)
                }
                Case(newScrutinee, newCases, ann)
            case _ => t

        go(term, bindPath)
    }
}

object CommonSubexpressionElimination {

    /** Annotation-ignoring structural equality wrapper for use as HashMap key. */
    private[transform] final class TermKey(val term: Term) {
        override def equals(that: Any): Boolean = that match
            case other: TermKey => term ~=~ other.term
            case _              => false

        override def hashCode(): Int = structuralHash(term)

        private def structuralHash(t: Term): Int = t match
            case Var(name, _)          => name.hashCode * 31 + 1
            case LamAbs(name, body, _) => (name.hashCode * 31 + structuralHash(body)) * 31 + 2
            case Apply(f, arg, _)      => (structuralHash(f) * 31 + structuralHash(arg)) * 31 + 3
            case Force(inner, _)       => structuralHash(inner) * 31 + 4
            case Delay(inner, _)       => structuralHash(inner) * 31 + 5
            case Const(c, _)           => c.hashCode * 31 + 6
            case Builtin(bn, _)        => bn.hashCode * 31 + 7
            case Error(_)              => 8
            case Constr(tag, args, _) =>
                args.foldLeft(tag.hashCode * 31 + 9)((h, a) => h * 31 + structuralHash(a))
            case Case(arg, cases, _) =>
                cases.foldLeft(structuralHash(arg) * 31 + 10)((h, c) => h * 31 + structuralHash(c))

        override def toString: String = s"TermKey(${term.showShort})"
    }

    private type Path = Vector[Int]

    private def isAncestorOrSelf(ancestor: Path, descendant: Path): Boolean =
        descendant.startsWith(ancestor)

    /** Computes the longest common prefix of a collection of paths. */
    private def longestCommonPrefix(paths: Vector[Path]): Path = {
        if paths.isEmpty then Vector.empty
        else if paths.size == 1 then paths.head
        else
            val minLen = paths.map(_.length).min
            var prefixLen = 0
            var done = false
            while prefixLen < minLen && !done do
                val elem = paths.head(prefixLen)
                if paths.forall(_(prefixLen) == elem) then prefixLen += 1
                else done = true
            paths.head.take(prefixLen)
    }

    /** Builtins whose failure depends on the **shape/type** of their input data. These are unsafe
      * to hoist across conditional (Case/Delay) boundaries because branch selection provides
      * data-shape guarantees that the builtin relies on.
      *
      * This is a subset of non-total builtins (`!DefaultFun.isTotal`). We intentionally exclude:
      *   - Value-dependent failures (division by zero, index OOB, overflow) — safe to hoist since
      *     Case branches don't discriminate on these conditions
      *   - `MkCons` / `Trace` — while non-total, blocking them is too conservative in practice
      *     since they appear pervasively in smart contract code
      *
      * @see
      *   [[DefaultFun.isTotal]] for the full list of non-total builtins
      */
    private val shapePartialBuiltins: Set[DefaultFun] = Set(
      // Data destructors: fail on wrong Data variant (shape-dependent)
      DefaultFun.UnConstrData,
      DefaultFun.UnMapData,
      DefaultFun.UnListData,
      DefaultFun.UnIData,
      DefaultFun.UnBData,
      // List operations: fail on empty vs non-empty (shape-dependent)
      DefaultFun.HeadList,
      DefaultFun.TailList
    )

    /** Variable name prefixes for shape-partial builtins extracted by [[ForcedBuiltinsExtractor]].
      * ForcedBuiltinsExtractor replaces `Force(Builtin(HeadList))` with `Var("__HeadList")`, so we
      * must also detect these variable references.
      */
    private val partialBuiltinVarPrefixes: Set[String] =
        shapePartialBuiltins.map(fn => s"__$fn")

    /** Whether a term references any shape-partial builtin in an evaluated position. Detects both
      * direct `Builtin(fn)` references and variable references created by
      * [[ForcedBuiltinsExtractor]] (e.g., `__HeadList`). Does not recurse into LamAbs or Delay
      * bodies (those are deferred).
      */
    private[transform] def referencesPartialBuiltin(t: Term): Boolean = t match
        case Builtin(fn, _) => shapePartialBuiltins.contains(fn)
        case Var(NamedDeBruijn(name, _), _) =>
            partialBuiltinVarPrefixes.exists(name.startsWith)
        case _: Const | _: Error  => false
        case _: LamAbs | _: Delay => false
        case Apply(f, arg, _)     => referencesPartialBuiltin(f) || referencesPartialBuiltin(arg)
        case Force(inner, _)      => referencesPartialBuiltin(inner)
        case Constr(_, args, _)   => args.exists(referencesPartialBuiltin)
        case Case(arg, cases, _) =>
            referencesPartialBuiltin(arg) || cases.exists(referencesPartialBuiltin)

    /** Whether a term should be skipped for CSE (work-free or otherwise not worth extracting). */
    private[transform] def isSkippable(t: Term): Boolean = t match
        case _: Var | _: Const | _: LamAbs | _: Delay | _: Builtin => true
        case _: Error                                              => true
        // Bare Force(Builtin) / Force(Force(Builtin)) -- handled by ForcedBuiltinsExtractor
        case Force(Builtin(_, _), _)           => true
        case Force(Force(Builtin(_, _), _), _) => true
        // Unsaturated builtin applications are value forms -- not worth extracting
        case _ if t.isValueForm => true
        // Terms containing Error will definitely fail when evaluated --
        // extracting them to an eagerly-evaluated binding would change semantics
        case _ if containsError(t) => true
        case _                     => false

    /** Whether a term contains an Error node anywhere in its tree (including inside Delay/LamAbs).
      *
      * Terms containing Error are typically error-handling paths (e.g., `force [trace "msg" (delay
      * error)]`). Extracting these as common subexpressions would move the error to an
      * eagerly-evaluated binding position, causing the error to fire unconditionally. Even when
      * Error is inside a Delay, a surrounding Force can unwrap it.
      */
    private[transform] def containsError(t: Term): Boolean = t match
        case _: Error                       => true
        case _: Var | _: Const | _: Builtin => false
        case LamAbs(_, body, _)             => containsError(body)
        case Delay(inner, _)                => containsError(inner)
        case Apply(f, arg, _)               => containsError(f) || containsError(arg)
        case Force(inner, _)                => containsError(inner)
        case Constr(_, args, _)             => args.exists(containsError)
        case Case(arg, cases, _)            => containsError(arg) || cases.exists(containsError)

    /** Count the number of nodes in a term (used for sorting candidates). */
    private[transform] def termSize(t: Term): Int = t match
        case Var(_, _) | Const(_, _) | Builtin(_, _) | Error(_) => 1
        case LamAbs(_, body, _)                                 => 1 + termSize(body)
        case Apply(f, arg, _)                                   => 1 + termSize(f) + termSize(arg)
        case Force(inner, _)                                    => 1 + termSize(inner)
        case Delay(inner, _)                                    => 1 + termSize(inner)
        case Constr(_, args, _)                                 => 1 + args.map(termSize).sum
        case Case(arg, cases, _) => 1 + termSize(arg) + cases.map(termSize).sum

    /** Collect all variable/lambda names used in a term. */
    private def collectNames(t: Term): mutable.HashSet[String] = {
        val names = mutable.HashSet.empty[String]
        def go(t: Term): Unit = t match
            case Var(NamedDeBruijn(n, _), _)      => names += n
            case LamAbs(n, body, _)               => names += n; go(body)
            case Apply(f, arg, _)                 => go(f); go(arg)
            case Force(inner, _)                  => go(inner)
            case Delay(inner, _)                  => go(inner)
            case Constr(_, args, _)               => args.foreach(go)
            case Case(arg, cases, _)              => go(arg); cases.foreach(go)
            case _: Const | _: Builtin | _: Error => ()
        go(t)
        names
    }

    /** Applies CSE to a term using default settings. */
    def apply(term: Term): Term = {
        val cse = new CommonSubexpressionElimination()
        cse(term)
    }
}
