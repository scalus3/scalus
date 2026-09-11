package scalus.uplc.transform

import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.{DefaultFun, NamedDeBruijn}
import scalus.uplc.eval.{CekMachineCosts, Log, Logger}
import scalus.uplc.transform.CommonSubexpressionElimination.{cachedTermBits, TermKey, TermTagBits, VarBits}
import scalus.uplc.transform.TermAnalysis.{freeVars, isValueForm}

import scala.collection.mutable

/** Common Context Extraction (CCE) for UPLC terms.
  *
  * Generalizes CSE by extracting common *contexts* (subtrees differing in exactly one position)
  * into shared lambda-abstractions. While CSE handles identical subexpressions, CCE handles cases
  * where repeated structures differ in a single "hole" at '''any''' child position:
  *
  * {{{
  * // Right-spine (original):
  * headList(tailList(sndPair(unConstrData(x)))) + headList(tailList(sndPair(unConstrData(y))))
  * → let f = \a -> headList(tailList(sndPair(unConstrData(a)))) in f(x) + f(y)
  *
  * // Left-spine (new): f(x, z) + f(y, z)
  * → let g = \a -> f(a, z) in g(x) + g(y)
  *
  * // Case scrutinee (new): case x of [...] and case y of [...]
  * → let h = \a -> case a of [...] in h(x) + h(y)
  * }}}
  *
  * This reduces on-chain script **code size** (CBOR bytes), which directly affects transaction
  * fees.
  *
  * ==Algorithm==
  *
  *   1. '''Collect pass''': Traverse the term. For each non-leaf node, generate all single-hole
  *      decompositions via [[CommonContextExtraction.decompose]]. Record templates with
  *      `termBits >= [[CommonContextExtraction.MinTemplateBits]]` along with their paths. Track
  *      path IDs at evaluation boundaries (LamAbs, Delay, Case), same as CSE.
  *   1. '''Group & filter pass''': Group by TemplateKey. For each group:
  *      - Require >= 2 **distinct** leaves (identical leaves are handled by CSE)
  *      - Profitability check: [[CommonContextExtraction.extractionSavingBits]] > 0
  *      - Compute bind path as longest common prefix of occurrence paths
  *      - Scope safety: free vars of template (excluding HOLE) in scope at bind path
  *      - Shadowing safety: no re-binding between bind and occurrence paths
  *      - Conditional boundary safety: no hoisting shape-partial builtins across Case/Delay
  *   1. '''Substitute pass''': For each candidate (largest template first):
  *      - Re-count in (possibly modified) term via [[CommonContextExtraction.matchTemplate]]
  *      - Create fresh name `__cce_<HeadBuiltin>[_N]` (e.g. `__cce_HeadList`, `__cce_SndPair_1`)
  *      - Replace each matching occurrence with `Apply(Var(lambdaName), leaf)`
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
        val sizeOf = cachedTermBits()
        // Pass 1: Collect (template, leaf) pairs via generalized single-hole decomposition.
        var nextId = 0
        def freshPathId(): Int = { val id = nextId; nextId += 1; id }

        val pathIdToVar = mutable.HashMap.empty[Int, String]
        val conditionalPathIds = mutable.HashSet.empty[Int]

        // Map from TemplateKey -> list of (path, leaf) occurrences
        val templateOccurrences =
            mutable.LinkedHashMap.empty[TermKey, mutable.ArrayBuffer[(Path, Term)]]

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
            case nonLeaf =>
                // Generate all single-hole decompositions at any child position
                if !isSkippable(nonLeaf) then
                    decompose(nonLeaf).foreach { case (template, leaf) =>
                        if sizeOf(template).exists(_ >= MinTemplateBits) && !containsError(template)
                        then addOccurrence(new TermKey(template), path, leaf)
                    }
                // Always recurse into subterms
                nonLeaf match
                    case Apply(f, arg, _) =>
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
                    case _ => () // LamAbs already handled above

        collectPass(term, Vector.empty)

        // Pass 2: Group & filter candidates.
        case class CceCandidate(
            key: TermKey,
            bindPath: Path,
            occurrences: Vector[(Path, Term)],
            templateBits: Int
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

        for
            (key, occs) <- templateOccurrences
            tBits <- sizeOf(key.term)
        do
            // Require at least 2 occurrences with distinct leaves
            val distinctLeafKeys = occs.map { case (_, leaf) => new TermKey(leaf) }.toSet
            if distinctLeafKeys.size >= 2 then
                val n = occs.size

                // Profitability is measured in flat-encoded bits plus the CEK steps the
                // extraction adds, because that is what the chain charges. See
                // [[extractionSavingBits]].
                if extractionSavingBits(n, tBits) > 0 then
                    val allPaths = occs.map(_._1).toVector
                    val bindPath = longestCommonPrefix(allPaths)

                    // Template free vars exclude the HOLE sentinel, only real vars matter
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
                        candidates += CceCandidate(key, bindPath, occs.toVector, tBits)

        if candidates.isEmpty then return term

        // Sort by template size descending, then by name. Ties keep first-occurrence order (the map
        // above is insertion-ordered). The key is computed once per candidate: `sortBy` would
        // re-evaluate it on every comparison, and `key.toString` renders the whole term.
        val sortedCandidates =
            candidates.map(c => ((-c.templateBits, c.key.toString), c)).sortBy(_._1).map(_._2)

        // Collect all existing names to avoid collisions
        val existingNames = collectNames(term)
        // Track how many times each head-name base has been used, for disambiguation
        val headNameCounts = mutable.HashMap.empty[String, Int]
        def freshCceName(template: Term): String = {
            val base = templateHeadName(template)
            val count = headNameCounts.getOrElse(base, 0)
            headNameCounts(base) = count + 1
            var name = if count == 0 then s"__cce_$base" else s"__cce_${base}_$count"
            while existingNames.contains(name) do
                val c = headNameCounts(base)
                headNameCounts(base) = c + 1
                name = s"__cce_${base}_$c"
            existingNames += name
            name
        }

        // Pass 3: Substitute, for each candidate (largest template first),
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
                case Var(_, _) | Const(_, _) | Builtin(_, _) | Error(_) => ()
                case LamAbs(name, body, _) =>
                    val id = reFreshId()
                    rePathIdToVar(id) = name
                    reCollect(body, path :+ id)
                case nonLeaf =>
                    matchTemplate(cand.key.term, nonLeaf) match
                        case Some(leaf) =>
                            reOccurrences += ((path, leaf))
                            return // Don't recurse into matched subterms
                        case None => ()
                    nonLeaf match
                        case Apply(f, arg, _) =>
                            reCollect(f, path)
                            reCollect(arg, path)
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
                        case _ => ()

            reCollect(currentTerm, Vector.empty)

            // Need distinct leaves AND at least 2 occurrences. Re-check profitability too:
            // candidates were scored before any extraction ran, and an earlier extraction can
            // have swallowed some of this template's occurrences, leaving too few to pay for
            // the framing and the CEK steps the abstraction adds.
            val reDistinctLeaves = reOccurrences.map { case (_, leaf) => new TermKey(leaf) }.toSet
            if reOccurrences.size >= 2 && reDistinctLeaves.size >= 2
                && extractionSavingBits(reOccurrences.size, cand.templateBits) > 0
            then
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
                    val lambdaName = freshCceName(cand.key.term)
                    var paramName = s"${lambdaName}_a"
                    while existingNames.contains(paramName) do paramName = s"${paramName}_"
                    existingNames += paramName
                    val varTerm = Var(NamedDeBruijn(lambdaName))

                    // Substitute all matching Apply chains with Apply(Var(lambdaName), leaf)
                    var subNextId = 0
                    def subFreshId(): Int = { val id = subNextId; subNextId += 1; id }

                    def doSubstitute(t: Term, path: Path): Term = t match
                        case Var(_, _) | Const(_, _) | Builtin(_, _) | Error(_) => t
                        case LamAbs(name, body, ann) =>
                            val id = subFreshId()
                            LamAbs(name, doSubstitute(body, path :+ id), ann)
                        case nonLeaf =>
                            if isAncestorOrSelf(bindPath, path) then
                                matchTemplate(cand.key.term, nonLeaf) match
                                    case Some(leaf) => Apply(varTerm, leaf)
                                    case None       => substituteChildren(nonLeaf, path)
                            else substituteChildren(nonLeaf, path)

                    def substituteChildren(t: Term, path: Path): Term = t match
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
                        case other => other

                    val substituted = doSubstitute(currentTerm, Vector.empty)

                    // Build the lambda: \param -> template[param/HOLE]
                    val templateBody = replaceHole(cand.key.term, Var(NamedDeBruijn(paramName)))
                    val lambda = LamAbs(paramName, templateBody)

                    // Insert let-binding at bind path:
                    // Apply(LamAbs(lambdaName, <body>), lambda)
                    currentTerm = insertLetAtPath(substituted, bindPath, lambdaName, lambda)
                    val nOcc = reOccurrences.size
                    val savedBits = extractionSavingBits(nOcc, cand.templateBits)
                    logger.log(
                      s"CCE: extracted template (bits=${cand.templateBits}, $nOcc occ, ${reDistinctLeaves.size} leaves, saved=$savedBits bits) as $lambdaName: ${cand.key.term.showShort}"
                    )

        currentTerm
    }

    /** Inserts `Apply(LamAbs(name, <hole>), expr)` at the position corresponding to the bind path.
      *
      * '''Path ID synchronization invariant:''' This method traverses the post-substitution term,
      * while bindPath was computed from `reCollect` on the pre-substitution term. The paths stay in
      * sync because `doSubstitute` only replaces nodes at/below occurrence positions, and
      * `bindPath` is their LCA, so all LamAbs/Delay/Case nodes from root to bindPath are
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

    /** Legacy shape filter used by CCE. CSE does not consult it: admissibility depends only on
      * occurrence regions and lexical scope. This is not a totality or purity analysis.
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

    /** Whether a term references any shape-partial builtin in an evaluated position.
      *
      * Detects three cases:
      *   - Direct `Builtin(fn)` references for shape-partial builtins.
      *   - Variable references created by [[ForcedBuiltinsExtractor]] (e.g., `__HeadList`).
      *   - Applications `Apply(Var(name), arg)`, regardless of the helper's name. Such helpers may
      *     internally reference partial builtins; a `List.head` helper transitively invokes
      *     `__HeadList`. Without tracing through the helper's body we cannot prove it total, so we
      *     conservatively assume it is partial when crossing a conditional boundary. This is only
      *     consulted by CCE's `unsafeCaseCrossing` checks.
      *
      * Does not recurse into LamAbs or Delay bodies (those are deferred, so the builtin there
      * doesn't fire at the extraction point).
      */
    private[transform] def referencesPartialBuiltin(t: Term): Boolean = t match
        case Builtin(fn, _) => shapePartialBuiltins.contains(fn)
        case Var(NamedDeBruijn(name, _), _) =>
            partialBuiltinVarPrefixes.exists(name.startsWith)
        case _: Const | _: Error  => false
        case _: LamAbs | _: Delay => false
        case Apply(_: Var, _, _)  =>
            // User-defined helper, may transitively call a partial builtin in its body.
            // Be conservative: treat the application as potentially partial, irrespective
            // of what the argument contains.
            true
        case Apply(f, arg, _)   => referencesPartialBuiltin(f) || referencesPartialBuiltin(arg)
        case Force(inner, _)    => referencesPartialBuiltin(inner)
        case Constr(_, args, _) => args.exists(referencesPartialBuiltin)
        case Case(arg, cases, _) =>
            referencesPartialBuiltin(arg) || cases.exists(referencesPartialBuiltin)

    /** Legacy CCE candidate filter. */
    private[transform] def isSkippable(t: Term): Boolean = t match
        case _: Var | _: Const | _: LamAbs | _: Delay | _: Builtin => true
        case _: Error                                              => true
        // Forced builtins are value forms too; their 16/20-bit hole templates cannot reach
        // MinTemplateBits, so exempting them from this filter never enabled extraction.
        // Other unsaturated builtin applications are value forms -- not worth extracting
        case _ if t.isValueForm => true
        // Keep the existing conservative filter for terms containing explicit error paths.
        case _ if containsError(t) => true
        case _                     => false

    /** Whether a term contains an Error node anywhere, including deferred bodies. This syntactic
      * filter is not a proof that evaluating the term fails.
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

    private type Path = Vector[Int]

    private val costs = CekMachineCosts.defaultMachineCosts

    // Each call adds an Apply, the function lookup and the parameter lookup. For example,
    // f(x) adds these around the original leaf x; the leaf itself still evaluates once.
    private val CallBits = SharingCost.lovelace(costs.applyCost + costs.varCost + costs.varCost) /
        SharingCost.lovelacePerBit

    // The outer let evaluates one Apply and two lambdas: (λf -> body)(λx -> template(x)).
    private val BindingBits =
        SharingCost.lovelace(costs.applyCost + costs.lamCost + costs.lamCost) /
            SharingCost.lovelacePerBit

    /** Smallest template, in flat-encoded bits, that any occurrence count could make profitable.
      *
      * A collection-time filter: `occurrences` is not yet known, so this is the loosest possible
      * bound. Reading [[extractionSavingBits]] as a function of `n`, the coefficient of `n` is
      * `skeletonBits - (TermTagBits + VarBits) - CallBits`. Unless that is positive, no occurrence
      * count can pay however much the framing is amortized. Adding the hole variable back gives the
      * template size. Must be declared after [[CallBits]], which it reads.
      */
    private[transform] val MinTemplateBits: Int = {
        val minSkeletonBits = math.floor((TermTagBits + VarBits) + CallBits).toInt + 1
        minSkeletonBits + VarBits
    }

    /** Net saving, in bits, of extracting a one-hole template that occurs `occurrences` times.
      *
      * `templateBits` is [[CommonSubexpressionElimination.termBits]] of the template, including its
      * hole sentinel.
      *
      * Counting in bits rather than AST nodes matters because the framing an extraction pays for is
      * `Var`-heavy (12 bits each) while the template it saves is usually `Apply`-heavy (4 bits
      * each), so a node count overstates the saving and understates the cost. The step term is
      * needed because extraction never saves work: it adds `3n+3` CEK steps.
      *
      * No execution profile is available in the optimizer, so every site is assumed to run once.
      * That is right for the majority of a validator that runs exactly once per transaction, and
      * optimistic on hot paths, where the real step cost is higher.
      */
    private[transform] def extractionSavingBits(occurrences: Int, templateBits: Int): Double = {
        val n = occurrences
        // Each site keeps its own leaf in place of the hole, so only the skeleton is duplicated.
        val skeletonBits = templateBits - VarBits
        // Per site an Apply and a Var reference; once, the let (Apply + LamAbs), the lambda
        // parameter and the hole variable.
        val framingBits = n * (TermTagBits + VarBits) + 3 * TermTagBits + VarBits
        (n - 1) * skeletonBits - framingBits - n * CallBits - BindingBits
    }

    /** Sentinel variable name used as the HOLE placeholder in templates. */
    private[transform] val holeSentinelName = "__CCE_HOLE__"

    /** A sentinel term representing the HOLE in a template. */
    private[transform] val holeSentinel: Term = Var(NamedDeBruijn(holeSentinelName))

    /** Maximum recursion depth for decomposition. Prevents O(S^2) pathological chains. */
    private val MaxDecomposeDepth = 30

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

    /** Generates all single-hole decompositions of a term.
      *
      * For each non-leaf child position, yields `(template, leaf)` where `template` is the original
      * term with HOLE at that position, and `leaf` is the replaced subtree. Recurses into children
      * to find deeper hole positions. Skips LamAbs bodies (bound variable scope) and Delay bodies
      * (deferred evaluation). A whole delayed value can still be a leaf: `Force(Delay(x))` yields
      * `(Force(HOLE), Delay(x))`, never `(Force(Delay(HOLE)), x)`.
      *
      * Examples:
      *   - `Apply(f, x)` yields `(Apply(HOLE, x), f)` and `(Apply(f, HOLE), x)`; then recurses into
      *     `f` and `x` for deeper decompositions.
      *   - `Case(s, [b1, b2])` yields `(Case(HOLE, [b1, b2]), s)`, plus deeper decompositions of
      *     `s`.
      *   - `Constr(0, [a, b, c])` yields `(Constr(0, [HOLE, b, c]), a)`,
      *     `(Constr(0, [a, HOLE, c]), b)`, `(Constr(0, [a, b, HOLE]), c)`, plus deeper
      *     decompositions.
      *
      * @return
      *   Iterator of (template, leaf) pairs. Empty for leaf nodes.
      */
    private[transform] def decompose(t: Term): Iterator[(Term, Term)] =
        decomposeImpl(t, 0)

    private def decomposeImpl(t: Term, depth: Int): Iterator[(Term, Term)] = {
        if depth >= MaxDecomposeDepth then return Iterator.empty
        t match
            case _: Var | _: Const | _: Builtin | _: Error | _: LamAbs => Iterator.empty

            case Apply(f, arg, ann) =>
                // Hole at function position
                val holeAtF = Iterator.single((Apply(holeSentinel, arg, ann), f))
                // Hole at arg position
                val holeAtArg = Iterator.single((Apply(f, holeSentinel, ann), arg))
                // Deeper decompositions: hole in substructure of f
                val deeperInF = decomposeImpl(f, depth + 1).map { case (fTemplate, leaf) =>
                    (Apply(fTemplate, arg, ann), leaf)
                }
                // Deeper decompositions: hole in substructure of arg
                val deeperInArg = decomposeImpl(arg, depth + 1).map { case (argTemplate, leaf) =>
                    (Apply(f, argTemplate, ann), leaf)
                }
                holeAtF ++ holeAtArg ++ deeperInF ++ deeperInArg

            case Force(inner, ann) =>
                // Hole at inner position
                val holeAtInner = Iterator.single((Force(holeSentinel, ann), inner))
                // Deeper decompositions in inner
                val deeperInInner = decomposeImpl(inner, depth + 1).map {
                    case (innerTemplate, leaf) =>
                        (Force(innerTemplate, ann), leaf)
                }
                holeAtInner ++ deeperInInner

            case _: Delay =>
                // Extracted leaves become strict function arguments. For example, replacing
                // delay(headList(xs)) with (λx -> delay(x))(headList(xs)) evaluates the
                // head before the delay is forced, so an unselected branch can now fail.
                // The collect pass still visits the body to find contexts within this delay.
                Iterator.empty

            case Constr(tag, args, ann) =>
                // Hole at each arg position
                val holesAtArgs = args.iterator.zipWithIndex.flatMap { case (arg, i) =>
                    val replaced = args.updated(i, holeSentinel)
                    val holeHere = Iterator.single((Constr(tag, replaced, ann), arg))
                    // Deeper decompositions in this arg
                    val deeper = decomposeImpl(arg, depth + 1).map { case (argTemplate, leaf) =>
                        val updated = args.updated(i, argTemplate)
                        (Constr(tag, updated, ann), leaf)
                    }
                    holeHere ++ deeper
                }
                holesAtArgs

            case Case(scrutinee, cases, ann) =>
                // Hole at scrutinee position
                val holeAtScrutinee =
                    Iterator.single((Case(holeSentinel, cases, ann), scrutinee))
                // Deeper decompositions in scrutinee
                val deeperInScrutinee = decomposeImpl(scrutinee, depth + 1).map {
                    case (scrTemplate, leaf) =>
                        (Case(scrTemplate, cases, ann), leaf)
                }
                // NOTE: We skip hole positions inside Case branches because they are
                // evaluation boundaries (like LamAbs bodies), extracting from them
                // requires scope tracking that the template approach doesn't capture.
                holeAtScrutinee ++ deeperInScrutinee
    }

    /** Matches a template against a concrete term, returning the leaf at the HOLE position.
      *
      * Walks template and term in lockstep. When the template has `holeSentinel`, the corresponding
      * subtree from `term` is the leaf. There must be exactly one HOLE in the template.
      *
      * @return
      *   Some(leaf) if the term matches the template structure, None otherwise.
      */
    private[transform] def matchTemplate(template: Term, term: Term): Option[Term] = {
        sealed trait MatchResult
        case object NoHole extends MatchResult
        case class Found(leaf: Term) extends MatchResult

        def go(tmpl: Term, t: Term): Option[MatchResult] = (tmpl, t) match
            case (Var(NamedDeBruijn(name, _), _), _) if name == holeSentinelName =>
                Some(Found(t))
            case (Var(a, _), Var(b, _)) if a.name == b.name => Some(NoHole)
            case (Const(a, _), Const(b, _)) if a == b       => Some(NoHole)
            case (Builtin(a, _), Builtin(b, _)) if a == b   => Some(NoHole)
            case (Error(_), Error(_))                       => Some(NoHole)
            case (LamAbs(n1, b1, _), LamAbs(n2, b2, _)) if n1 == n2 =>
                go(b1, b2)
            case (Apply(f1, a1, _), Apply(f2, a2, _)) =>
                for
                    r1 <- go(f1, f2)
                    r2 <- go(a1, a2)
                    merged <- mergeResults(r1, r2)
                yield merged
            case (Force(i1, _), Force(i2, _)) => go(i1, i2)
            case (Delay(i1, _), Delay(i2, _)) => go(i1, i2)
            case (Constr(t1, a1, _), Constr(t2, a2, _)) if t1 == t2 && a1.size == a2.size =>
                a1.zip(a2)
                    .foldLeft(Option[MatchResult](NoHole)) { case (acc, (ta, tb)) =>
                        acc.flatMap { prev =>
                            go(ta, tb).flatMap(mergeResults(prev, _))
                        }
                    }
            case (Case(s1, c1, _), Case(s2, c2, _)) if c1.size == c2.size =>
                val scrResult = go(s1, s2)
                scrResult.flatMap { sr =>
                    c1.zip(c2)
                        .foldLeft(Option[MatchResult](sr)) { case (acc, (ca, cb)) =>
                            acc.flatMap { prev =>
                                go(ca, cb).flatMap(mergeResults(prev, _))
                            }
                        }
                }
            case _ => None

        def mergeResults(a: MatchResult, b: MatchResult): Option[MatchResult] = (a, b) match
            case (NoHole, other) => Some(other)
            case (other, NoHole) => Some(other)
            case _               => None // Two holes, not a single-hole template

        go(template, term).flatMap {
            case Found(leaf) => Some(leaf)
            case NoHole      => None // Template matched but no HOLE found, not useful
        }
    }

    /** Checks whether a term contains the HOLE sentinel anywhere. */
    private[transform] def containsHole(t: Term): Boolean = t match
        case Var(NamedDeBruijn(name, _), _) if name == holeSentinelName => true
        case _: Var | _: Const | _: Builtin | _: Error                  => false
        case LamAbs(_, body, _)                                         => containsHole(body)
        case Apply(f, arg, _)   => containsHole(f) || containsHole(arg)
        case Force(inner, _)    => containsHole(inner)
        case Delay(inner, _)    => containsHole(inner)
        case Constr(_, args, _) => args.exists(containsHole)
        case Case(s, cases, _)  => containsHole(s) || cases.exists(containsHole)

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

    /** Builds a short, human-readable name from a template.
      *
      * For Apply chains, walks collecting each function position (right-spine style). For other
      * node types (Case, Constr, Force, Delay), uses the outermost constructor name. Falls back to
      * `"app"` for unrecognized shapes.
      */
    private[transform] def templateHeadName(template: Term): String = {
        val fns = collectTemplateFunctions(template)
        fns match
            case Nil           => "app"
            case single :: Nil => single
            case multiple      => multiple.map(TermNaming.abbreviateBuiltin).mkString("_")
    }

    /** Collects readable names from a template for naming purposes.
      *
      * Walks Apply chains collecting function names. For other node types, returns a descriptive
      * name based on the outermost constructor.
      */
    private def collectTemplateFunctions(t: Term): List[String] = t match
        case Apply(f, arg, _) =>
            TermNaming.extractFunctionName(f, holeSentinelName) :: collectTemplateFunctions(arg)
        case Case(_, _, _)   => List("Case")
        case Constr(_, _, _) => List("Constr")
        case Force(_, _)     => List("Force")
        case Delay(_, _)     => List("Delay")
        case _               => Nil

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
