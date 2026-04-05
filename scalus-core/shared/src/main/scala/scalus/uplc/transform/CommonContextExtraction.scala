package scalus.uplc.transform

import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.NamedDeBruijn
import scalus.uplc.eval.{Log, Logger}
import scalus.uplc.transform.CommonSubexpressionElimination.{containsError, isSkippable, referencesPartialBuiltin, termSize, TermKey}
import scalus.uplc.transform.TermAnalysis.freeVars

import scala.collection.mutable

/** Common Context Extraction (CCE) for UPLC terms.
  *
  * Generalizes CSE by extracting common *function contexts* applied to different arguments. While
  * CSE handles identical subexpressions (same arguments), CCE handles cases like:
  *
  * {{{
  * headList(tailList(sndPair(unConstrData(x)))) + headList(tailList(sndPair(unConstrData(y))))
  * → let f = \a -> headList(tailList(sndPair(unConstrData(a)))) in f(x) + f(y)
  * }}}
  *
  * This reduces on-chain script **code size** (CBOR bytes), which directly affects transaction
  * fees.
  *
  * ==Algorithm==
  *
  *   1. '''Collect pass''': Traverse the term. For each Apply chain, decompose into `(template,
  *      leaf)` via right-spine decomposition. Record templates with `termSize >= 5` (the smallest
  *      size that is realistically profitable — see [[MinTemplateSize]]) along with their paths.
  *      Track path IDs at evaluation boundaries (LamAbs, Delay, Case) — same as CSE.
  *   1. '''Group & filter pass''': Group by TemplateKey. For each group:
  *      - Require >= 2 **distinct** leaves (identical leaves are handled by CSE)
  *      - Profitability check: `(N-1) * templateSize > N + 3`
  *      - Compute bind path as longest common prefix of occurrence paths
  *      - Scope safety: free vars of template (excluding HOLE) in scope at bind path
  *      - Shadowing safety: no re-binding between bind and occurrence paths
  *      - Conditional boundary safety: no hoisting shape-partial builtins across Case/Delay
  *   1. '''Substitute pass''': For each candidate (largest template first):
  *      - Re-count in (possibly modified) term
  *      - Create fresh name `__cce_N`
  *      - Replace each matching Apply chain with `Apply(Var(lambdaName), leaf)`
  *      - Insert at bind path: `Apply(LamAbs(lambdaName, body), LamAbs(param, templateBody))`
  *
  * @param logger
  *   Logger for tracking CCE operations
  *
  * @see
  *   [[CommonSubexpressionElimination]] for the CSE pass that handles identical subexpressions
  */
class CommonContextExtraction(logger: Logger = new Log()) extends Optimizer {
    import CommonContextExtraction.*

    def apply(term: Term): Term = cce(term)

    def logs: Seq[String] = logger.getLogs.toSeq

    private def cce(term: Term): Term = {
        // Pass 1: Collect (template, leaf) pairs from Apply chains via right-spine decomposition.
        var nextId = 0
        def freshPathId(): Int = { val id = nextId; nextId += 1; id }

        val pathIdToVar = mutable.HashMap.empty[Int, String]
        val conditionalPathIds = mutable.HashSet.empty[Int]

        // Map from TemplateKey -> list of (path, leaf) occurrences
        val templateOccurrences =
            mutable.HashMap.empty[TermKey, mutable.ArrayBuffer[(Path, Term)]]

        def addOccurrence(templateKey: TermKey, path: Path, leaf: Term): Unit = {
            val occs = templateOccurrences.getOrElseUpdate(templateKey, mutable.ArrayBuffer.empty)
            occs += ((path, leaf))
        }

        def collectPass(t: Term, path: Path): Unit = t match
            case Var(_, _) | Const(_, _) | Builtin(_, _) | Error(_) => ()
            case LamAbs(name, body, _) =>
                val id = freshPathId()
                pathIdToVar(id) = name
                collectPass(body, path :+ id)
            case app @ Apply(_, _, _) =>
                // Try right-spine decomposition
                decomposeRightSpine(app) match
                    case Some((template, leaf)) =>
                        val tSize = termSize(template)
                        if tSize >= MinTemplateSize && !isSkippable(app) && !containsError(template)
                        then addOccurrence(new TermKey(template), path, leaf)
                    case None => ()
                // Always recurse into subterms
                val Apply(f, arg, _) = app: @unchecked
                collectPass(f, path)
                collectPass(arg, path)
            case Force(inner, _) =>
                collectPass(inner, path)
            case Delay(inner, _) =>
                val id = freshPathId()
                conditionalPathIds += id
                collectPass(inner, path :+ id)
            case Constr(_, args, _) =>
                args.foreach(collectPass(_, path))
            case Case(scrutinee, cases, _) =>
                collectPass(scrutinee, path)
                cases.foreach { c =>
                    val id = freshPathId()
                    conditionalPathIds += id
                    collectPass(c, path :+ id)
                }

        collectPass(term, Vector.empty)

        // Pass 2: Group & filter candidates.
        case class CceCandidate(
            key: TermKey,
            bindPath: Path,
            occurrences: Vector[(Path, Term)],
            templateSize: Int
        )

        val topLevelFreeVars = term.freeVars

        def varsInScope(path: Path): Set[String] =
            topLevelFreeVars ++ path.flatMap(id => pathIdToVar.get(id))

        def isSafeToHoist(
            freeVarSet: Set[String],
            bindPath: Path,
            occurrencePath: Path
        ): Boolean = {
            val scopeAtBind = varsInScope(bindPath)
            if !freeVarSet.subsetOf(scopeAtBind) then return false
            val extensionSegments = occurrencePath.drop(bindPath.length)
            val shadowedVars = extensionSegments.flatMap(id => pathIdToVar.get(id)).toSet
            freeVarSet.intersect(shadowedVars).isEmpty
        }

        val candidates = mutable.ArrayBuffer.empty[CceCandidate]

        for (key, occs) <- templateOccurrences do
            // Require at least 2 occurrences with distinct leaves
            val distinctLeafKeys = occs.map { case (_, leaf) => new TermKey(leaf) }.toSet
            if distinctLeafKeys.size >= 2 then
                val n = occs.size
                val tSize = termSize(key.term)

                // Profitability: extracting saves (N-1)*templateSize nodes (template
                // duplicated N times minus the one copy kept in the lambda body).
                // Cost is N+3: 1 LamAbs + 1 Apply for let-binding, 1 LamAbs for param, N Applys.
                if (n - 1) * tSize > n + 3 then
                    val allPaths = occs.map(_._1).toVector
                    val bindPath = longestCommonPrefix(allPaths)

                    // Template free vars exclude the HOLE sentinel — only real vars matter
                    val templateFreeVars = key.term.freeVars - holeSentinelName

                    val safeForAll =
                        allPaths.forall(p => isSafeToHoist(templateFreeVars, bindPath, p))

                    val crossesConditional = allPaths.exists { path =>
                        path.length > bindPath.length &&
                        conditionalPathIds.contains(path(bindPath.length))
                    }
                    val unsafeCaseCrossing =
                        crossesConditional && referencesPartialBuiltin(key.term)

                    if safeForAll && !unsafeCaseCrossing then
                        candidates += CceCandidate(key, bindPath, occs.toVector, tSize)

        if candidates.isEmpty then return term

        // Sort by template size descending, then by name for determinism
        val sortedCandidates = candidates.sortBy(c => (-c.templateSize, c.key.toString))

        // Collect all existing names to avoid collisions
        val existingNames = collectNames(term)
        var nameCounter = 0
        def freshCceName(): String = {
            var name = s"__cce_$nameCounter"
            nameCounter += 1
            while existingNames.contains(name) do
                name = s"__cce_$nameCounter"
                nameCounter += 1
            existingNames += name
            name
        }

        // Pass 3: Substitute — for each candidate (largest template first),
        // re-count in the modified term and apply substitution.
        var currentTerm = term

        for cand <- sortedCandidates do
            // Re-count occurrences in the (possibly modified) term
            val reOccurrences = mutable.ArrayBuffer.empty[(Path, Term)]
            var reNextId = 0
            def reFreshId(): Int = { val id = reNextId; reNextId += 1; id }

            val rePathIdToVar = mutable.HashMap.empty[Int, String]
            val reConditionalPathIds = mutable.HashSet.empty[Int]

            def reCollect(t: Term, path: Path): Unit = t match
                case app @ Apply(_, _, _) =>
                    decomposeRightSpine(app) match
                        case Some((template, leaf)) if cand.key.term ~=~ template =>
                            reOccurrences += ((path, leaf))
                            return // Don't recurse into matched subterms
                        case _ => ()
                    val Apply(f, arg, _) = app: @unchecked
                    reCollect(f, path)
                    reCollect(arg, path)
                case Var(_, _) | Const(_, _) | Builtin(_, _) | Error(_) => ()
                case LamAbs(name, body, _) =>
                    val id = reFreshId()
                    rePathIdToVar(id) = name
                    reCollect(body, path :+ id)
                case Force(inner, _) =>
                    reCollect(inner, path)
                case Delay(inner, _) =>
                    val id = reFreshId()
                    reConditionalPathIds += id
                    reCollect(inner, path :+ id)
                case Constr(_, args, _) =>
                    args.foreach(reCollect(_, path))
                case Case(scrutinee, cases, _) =>
                    reCollect(scrutinee, path)
                    cases.foreach { c =>
                        val id = reFreshId()
                        reConditionalPathIds += id
                        reCollect(c, path :+ id)
                    }

            reCollect(currentTerm, Vector.empty)

            // Need distinct leaves AND at least 2 occurrences
            val reDistinctLeaves = reOccurrences.map { case (_, leaf) => new TermKey(leaf) }.toSet
            if reOccurrences.size >= 2 && reDistinctLeaves.size >= 2 then
                val allPaths = reOccurrences.map(_._1).toVector
                val bindPath = longestCommonPrefix(allPaths)

                val templateFreeVars = cand.key.term.freeVars - holeSentinelName
                val reTopFreeVars = currentTerm.freeVars

                def reVarsInScope(path: Path): Set[String] =
                    reTopFreeVars ++ path.flatMap(id => rePathIdToVar.get(id))

                def reIsSafeToHoist(bindP: Path, occP: Path): Boolean = {
                    val scope = reVarsInScope(bindP)
                    if !templateFreeVars.subsetOf(scope) then return false
                    val ext = occP.drop(bindP.length)
                    val shadowed = ext.flatMap(id => rePathIdToVar.get(id)).toSet
                    templateFreeVars.intersect(shadowed).isEmpty
                }

                val reSafe = allPaths.forall(p => reIsSafeToHoist(bindPath, p))
                val reCrossesConditional = allPaths.exists { path =>
                    path.length > bindPath.length &&
                    reConditionalPathIds.contains(path(bindPath.length))
                }
                val reUnsafeCaseCrossing =
                    reCrossesConditional && referencesPartialBuiltin(cand.key.term)

                if reSafe && !reUnsafeCaseCrossing then
                    val lambdaName = freshCceName()
                    var paramName = s"${lambdaName}_p"
                    while existingNames.contains(paramName) do paramName = s"${paramName}_"
                    existingNames += paramName
                    val varTerm = Var(NamedDeBruijn(lambdaName))

                    // Substitute all matching Apply chains with Apply(Var(lambdaName), leaf)
                    var subNextId = 0
                    def subFreshId(): Int = { val id = subNextId; subNextId += 1; id }

                    def doSubstitute(t: Term, path: Path): Term = t match
                        case app @ Apply(_, _, _) =>
                            decomposeRightSpine(app) match
                                case Some((template, leaf))
                                    if (cand.key.term ~=~ template) && isAncestorOrSelf(
                                      bindPath,
                                      path
                                    ) =>
                                    Apply(varTerm, leaf)
                                case _ =>
                                    val Apply(f, arg, ann) = app: @unchecked
                                    Apply(
                                      doSubstitute(f, path),
                                      doSubstitute(arg, path),
                                      ann
                                    )
                        case Var(_, _) | Const(_, _) | Builtin(_, _) | Error(_) => t
                        case LamAbs(name, body, ann) =>
                            val id = subFreshId()
                            LamAbs(name, doSubstitute(body, path :+ id), ann)
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

                    // Build the lambda: \param -> template[param/HOLE]
                    val templateBody = replaceHole(cand.key.term, Var(NamedDeBruijn(paramName)))
                    val lambda = LamAbs(paramName, templateBody)

                    // Insert let-binding at bind path:
                    // Apply(LamAbs(lambdaName, <body>), lambda)
                    currentTerm = insertLetAtPath(substituted, bindPath, lambdaName, lambda)
                    logger.log(
                      s"CCE: extracted template ${cand.key.term.showShort} (${reOccurrences.size} occurrences, ${reDistinctLeaves.size} distinct leaves) as $lambdaName"
                    )

        currentTerm
    }

    /** Inserts `Apply(LamAbs(name, <hole>), expr)` at the position corresponding to the bind path.
      *
      * '''Path ID synchronization invariant:''' This method traverses the post-substitution term,
      * while bindPath was computed from `reCollect` on the pre-substitution term. The paths stay in
      * sync because `doSubstitute` only replaces nodes at/below occurrence positions, and
      * `bindPath` is their LCA — so all LamAbs/Delay/Case nodes from root to bindPath are
      * structurally identical in both the pre- and post-substitution terms.
      */
    private def insertLetAtPath(term: Term, bindPath: Path, name: String, expr: Term): Term = {
        if bindPath.isEmpty then return Apply(LamAbs(name, term), expr)

        var nextId = 0
        def freshId(): Int = { val id = nextId; nextId += 1; id }

        def go(t: Term, remaining: Path): Term = t match
            case LamAbs(n, body, ann) =>
                val id = freshId()
                if remaining.nonEmpty && remaining.head == id then
                    if remaining.size == 1 then LamAbs(n, Apply(LamAbs(name, body), expr), ann)
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

object CommonContextExtraction {

    private type Path = Vector[Int]

    /** Minimum template size (in nodes) for extraction to be considered.
      *
      * Extraction cost is N+3 nodes (1 LamAbs + 1 Apply for let-binding, 1 LamAbs for the param, N
      * Apply calls). Savings are (N-1)*tSize. Profitability requires `(N-1)*tSize > N+3`.
      *
      *   - tSize=3 (`f(HOLE)`): needs N>=7 — too trivial to extract.
      *   - tSize=4: needs N>=4 — rare with distinct leaves.
      *   - tSize=5 (`f(g(HOLE))`): needs N>=3 — a two-deep chain, worth tracking.
      *   - tSize=6+: profitable even at N=2 — always collected.
      *
      * So 5 is the smallest template that is realistically profitable.
      */
    private val MinTemplateSize = 5

    /** Sentinel variable name used as the HOLE placeholder in templates. */
    private[transform] val holeSentinelName = "__CCE_HOLE__"

    /** A sentinel term representing the HOLE in a template. */
    private[transform] val holeSentinel: Term = Var(NamedDeBruijn(holeSentinelName))

    /** Decomposes an Apply chain by right-spine traversal.
      *
      * Given `f(g(h(x)))`, produces `(f(g(h(HOLE))), x)`. Given `f(g(x))`, produces
      * `(f(g(HOLE)), x)`. Given `f(x)`, produces `(f(HOLE), x)`.
      *
      * @return
      *   Some((template, leaf)) where template has HOLE in place of the innermost argument, or None
      *   if the term is not a suitable Apply chain.
      */
    private[transform] def decomposeRightSpine(t: Term): Option[(Term, Term)] = t match
        case Apply(f, arg @ Apply(g, x, argAnn), ann) =>
            decomposeRightSpine(arg) match
                case Some((innerTemplate, leaf)) =>
                    Some((Apply(f, innerTemplate, ann), leaf))
                case None =>
                    // arg is Apply(g, x) where x is not itself an Apply
                    Some((Apply(f, Apply(g, holeSentinel, argAnn), ann), x))
        case Apply(f, arg, ann) =>
            Some((Apply(f, holeSentinel, ann), arg))
        case _ => None

    /** Replace the HOLE sentinel in a template with the given replacement term. */
    private[transform] def replaceHole(template: Term, replacement: Term): Term = template match
        case Var(NamedDeBruijn(name, _), _) if name == holeSentinelName => replacement
        case _: Var | _: Const | _: Builtin | _: Error                  => template
        case LamAbs(name, body, ann) => LamAbs(name, replaceHole(body, replacement), ann)
        case Apply(f, arg, ann) =>
            Apply(replaceHole(f, replacement), replaceHole(arg, replacement), ann)
        case Force(inner, ann)      => Force(replaceHole(inner, replacement), ann)
        case Delay(inner, ann)      => Delay(replaceHole(inner, replacement), ann)
        case Constr(tag, args, ann) => Constr(tag, args.map(replaceHole(_, replacement)), ann)
        case Case(scrutinee, cases, ann) =>
            Case(replaceHole(scrutinee, replacement), cases.map(replaceHole(_, replacement)), ann)

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

    /** Applies CCE to a term using default settings. */
    def apply(term: Term): Term = {
        val cce = new CommonContextExtraction()
        cce(term)
    }
}
