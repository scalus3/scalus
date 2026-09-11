package scalus.uplc.transform

import scalus.uplc.{Constant, Term}
import scalus.uplc.Term.*
import scalus.uplc.{DeBruijn, DefaultFun, NamedDeBruijn}
import scalus.uplc.eval.{Log, Logger}
import scalus.uplc.transform.TermAnalysis.{freeVars, isValueForm}
import scalus.cardano.ledger.Word64
import scalus.serialization.flat.Flat
import scalus.uplc.Constant.flatConstant

import scala.collection.mutable
import scala.util.Try

/** Share repeated expressions by introducing a strict UPLC binding, using Plutus 1.63.0.0's
  * ancestor-or-self rule. In the examples below, `let x = e in body` means `[(lam x body) e]`; `e`
  * is evaluated before `body`, even when the uses of `x` are delayed. For example,
  * `let x = Error in Delay(x)` fails immediately, whereas `Delay(Error)` is a value.
  *
  * {{{
  * // When sharing pays, keep one copy of e and replace its uses with variables:
  * Constr(0, [e, e])  =>  let x = e in Constr(0, [x, x])
  *
  * // An existing strict occurrence can also supply a deferred use:
  * Constr(0, [e, Delay(e)])  =>  let x = e in Constr(0, [x, Delay(x)])
  *
  * // No strict occurrence outside the delays: do not move e out of them.
  * Constr(0, [Delay(e), Delay(e)])
  * }}}
  *
  * ==Safety: where may the binding go?==
  * Lambda bodies, delayed bodies and individual Case branches are separate evaluation regions.
  * Other children inherit their parent's region, including the body of an immediately applied
  * lambda (a strict let). A group must contain an occurrence in its outermost region; occurrences
  * in descendant regions can join it. Sibling regions alone cannot justify an outer binding.
  * Otherwise extracting, for example, a failing `e` from an unused delay would make a successful
  * program fail. The same placement rule applies to every candidate, including constants:
  *
  * {{{
  * LamAbs(a, Constr(0, [e(a), e(a)]))
  *   => LamAbs(a, let x = e(a) in Constr(0, [x, x]))  // stay inside the lambda
  *
  * let a = e in Constr(0, [a, e])
  *   => let x = e in (let a = x in Constr(0, [a, x])) // applied lambda is strict
  *
  * Case(tag, [e, e, 0])  // no outer occurrence of e; selecting branch 2 must not run e
  * Constr(0, [Delay(largeConstant), Delay(largeConstant)])
  *   // No special constant-hoisting exception: the two constant uses stay in separate regions.
  *   // Sharing the whole Delay values is allowed; that does not evaluate their bodies.
  * }}}
  *
  * Like Plutus CSE, this preserves results and success/failure, not trace multiplicity, trace
  * order, failure messages or exact budgets. A region is not an evaluation-order barrier: sharing
  * may move work earlier within it. For example, with `e = trace("B", 1)` and `a = trace("A", 0)`,
  * sharing e in `Constr(0, [a, e, e])` can change logs from `["A", "B", "B"]` to `["B", "A"]` while
  * returning the same constructor. If a and e both fail, moving e first can change which failure is
  * reported, but still fails. Even sharing a constant adds Apply/LamAbs/Var work, so exact budgets
  * are not preserved.
  *
  * No builtin-totality or name-prefix assumptions are needed: `divideInteger n d` can be a
  * candidate even when d might be zero, provided placement is safe and sharing pays. Renaming n to
  * `__partial_n` does not change that decision.
  *
  * ==Profitability: should we introduce the binding?==
  * Safe placement alone is insufficient. [[SharingCost]] prices estimated reference-script bits
  * saved minus the extra binding's execution fee for values. Repeated computations such as `f(xs)`
  * are also shared when that estimate is negative: the callee's avoided work is unknown, so this
  * is a runtime-sharing preference, not a prediction of lower total fees. Each round takes the
  * greatest eligible estimate, then recollects on the changed tree. For example, three uses of a 54-bit constant save
  * `(3 - 1) * 54 - 3 * 12 - 8 = 64` estimated bits: remove two copies, insert three variables, and
  * pay the Apply/LamAbs tags. The default fee estimate subtracts about 20.77 lovelace of binding
  * work from 120 lovelace of reference-size savings. Two uses of an 18-bit integer constant instead
  * save `18 - 24 - 8 = -14` bits and are rejected. See `docs/design/cse-placement.md` for the
  * contract and pricing assumptions.
  */
class CommonSubexpressionElimination(logger: Logger = new Log()) extends Optimizer {
    import CommonSubexpressionElimination.*

    def apply(term: Term): Term = {
        val names = collectNames(term)
        // If __cse_e and __cse_e_1 already occur anywhere in the input, choose __cse_e_2.
        def freshName(base: String): String = {
            var name = base
            var suffix = 0
            while names.contains(name) do
                suffix += 1
                name = s"${base}_$suffix"
            names += name
            name
        }

        // Preserve existing unique names (including debug labels). Rename only binders which
        // collide with another binder or a free variable; all generated names avoid input names.
        // For example, e(x) under two different `lam x` binders must not be grouped as the
        // same expression. Globally unique names make structural equality distinguish them.
        // For example, Constr(0, [lam x. e(x), lam x. e(x)]) becomes
        // Constr(0, [lam x. e(x), lam x_cse. e(x_cse)]) during collection.
        // Free names are reserved too: Constr(0, [x, lam x. x]) must keep its free x separate
        // from the lambda parameter. An already unique debug name such as amount is retained.
        val boundNames = mutable.HashSet.from(term.freeVars)
        def uniqueBinders(t: Term, env: Map[String, String]): Term = t match
            case Var(n, ann) => Var(n.copy(name = env.getOrElse(n.name, n.name)), ann)
            case LamAbs(n, body, ann) =>
                val unique = if boundNames.add(n) then n else freshName(s"${n}_cse")
                boundNames += unique
                LamAbs(unique, uniqueBinders(body, env.updated(n, unique)), ann)
            case _ => mapChildren(t)((child, _) => uniqueBinders(child, env))

        var current = uniqueBinders(term, Map.empty)
        var changed = false
        val sizeOf = cachedTermBits()
        val hashes = new java.util.IdentityHashMap[Term, java.lang.Integer]()
        val keys = new java.util.IdentityHashMap[Term, AlphaTermKey]()
        def hashOf(t: Term): Int = {
            val cached = hashes.get(t)
            if cached != null then cached.intValue
            else
                val hash = structuralHash(t, hashOf, ignoreNames = true)
                hashes.put(t, hash)
                hash
        }
        def keyOf(t: Term): AlphaTermKey = {
            val cached = keys.get(t)
            if cached != null then cached
            else
                val key = new AlphaTermKey(t, hashOf(t))
                keys.put(t, key)
                key
        }
        var candidate = collect(current, sizeOf, keyOf)
        while candidate.nonEmpty do
            val c = candidate.get
            val name = freshName(s"__cse_${TermNaming.termDescription(c.expr)}")
            current = extract(current, c, name)
            changed = true
            logger.log(
              s"CSE: extracted ${c.expr.showShort} (${c.occurrences.size} occurrences, size/binding estimate=${c.savedLovelace} lovelace) as $name"
            )
            // Extraction changes both occurrence counts and paths. For example, sharing f(e)
            // removes copies of the e inside it. A second candidate from the old tree would
            // have stale uses and stale savings, so select again from the rewritten tree:
            // Constr(0, [f(e), f(e)]) => let x = f(e) in Constr(0, [x, x]).
            // There is now just one e, and the original child paths point into a new let.
            candidate = collect(current, sizeOf, keyOf)

        // Alpha-renaming alone is not an optimization, and should not change a no-op result.
        // For example, lam x. lam x. x needs internal renaming but has nothing to share;
        // return the original names/annotations instead of exposing that preparation step.
        if changed then current else term
    }

    def logs: Seq[String] = logger.getLogs.toSeq

    // The region of an actual occurrence, not merely the common ancestor of all uses.
    // This occurrence justifies sharing into descendant regions that may not execute.
    // In Constr(0, [e, Delay(e)]), the first e supplies the group's region. In
    // Constr(0, [Delay(e), Delay(e)]), the common ancestor contains no occurrence of e itself.
    private case class Group(region: Path, occurrences: mutable.ArrayBuffer[Path])
    private case class Candidate(
        expr: Term,
        occurrences: Set[Path],
        bindAt: Path,
        savedLovelace: Double = 0
    )

    /** Collect equal expressions into groups with safe placement, then choose one profitable group.
      *
      * `path` identifies a node; `region` identifies the root of its evaluation region. These are
      * different: many nodes share a region, but the binding belongs at their lowest common
      * ancestor, not necessarily at the region root. For Constr(0, [a, Constr(0, [e, e])]), the e
      * paths are [1, 0] and [1, 1], both in region []; insert the binding at [1].
      */
    private def collect(
        term: Term,
        sizeOf: Term => Option[Int],
        keyOf: Term => AlphaTermKey
    ): Option[Candidate] = {
        val groups = mutable.LinkedHashMap.empty[AlphaTermKey, mutable.ArrayBuffer[Group]]

        def add(t: Term, path: Path, region: Path): Unit = {
            val entries = groups.getOrElseUpdate(keyOf(t), mutable.ArrayBuffer.empty)
            entries.find(g => isAncestorOrSelf(g.region, region)) match
                case Some(ancestor) =>
                    // A strict occurrence was already found in this region or an enclosing one.
                    // Its binding can supply this use even if this use is inside a delay/branch.
                    // In Constr(0, [e, Delay(e)]), visiting the delayed e finds the first e's group.
                    ancestor.occurrences += path
                case None =>
                    // Traversal order need not discover the outer occurrence first. In
                    // Constr(0, [Delay(e), Delay(e), e]), the final e justifies merging both
                    // previously separate groups. Without that e, the siblings stay separate.
                    val occurrences = mutable.ArrayBuffer(path)
                    entries.filterInPlace { g =>
                        if isAncestorOrSelf(region, g.region) then
                            occurrences ++= g.occurrences
                            false
                        else true
                    }
                    entries += Group(region, occurrences)
        }

        def go(t: Term, path: Path, region: Path): Unit = {
            // Record a Delay/LamAbs itself in the current region: constructing the value is
            // strict, although evaluating its body is not. Only the body starts a new region.
            // Constr(0, [Delay(e), Delay(e)]) may become let d = Delay(e) in Constr(0, [d, d]);
            // e remains unevaluated. Likewise, evaluating a lambda value does not call it.
            add(t, path, region)
            t match
                // Let bodies are strict, but still have lexical scope. Unique binders and the
                // structural LCA below keep any expression using the parameter inside its body.
                // (lam a. Constr(0, [f(a), f(a)])) arg shares f(a) inside lam a, never around arg.
                // Body path [0, 0] still records the intervening lambda despite inheriting region [].
                case Apply(LamAbs(_, body, _), arg, _) =>
                    go(body, path :+ 0 :+ 0, region)
                    go(arg, path :+ 1, region)
                case LamAbs(_, body, _) =>
                    val p = path :+ 0
                    go(body, p, p)
                case Delay(body, _) =>
                    val p = path :+ 0
                    go(body, p, p)
                case Case(scrutinee, branches, _) =>
                    // The scrutinee is strict; only the selected branch executes. A repeated
                    // expression confined to different branches cannot be hoisted above Case.
                    // Case(tag, [e, e, 0]) must still return 0 for tag 2 even when e would fail.
                    go(scrutinee, path :+ 0, region)
                    branches.zipWithIndex.foreach { (branch, i) =>
                        val p = path :+ (i + 1)
                        go(branch, p, p)
                    }
                case _ =>
                    children(t).zipWithIndex.foreach((child, i) => go(child, path :+ i, region))
        }

        go(term, Vector.empty, Vector.empty)
        // Safety is established by grouping, independently of expression size or builtin kind.
        // Greatest eligible fee estimate first, retaining traversal order for ties. For example,
        // a value saving 100 lovelace precedes f(xs), whose size/binding estimate may be negative.
        // Runtime sharing terminates even when bits grow: count structural nodes excluding leaves
        // and administrative Apply(LamAbs(...), arg) pairs. Sharing f(xs) removes one Apply per
        // eliminated copy; the new let adds none. Fee-only leaf sharing instead reduces termBits.
        // Thus (non-let structural nodes, termBits) decreases lexicographically. Collection skips
        // immediate lambda function positions, so replacing a lambda cannot turn an ignored let
        // into a counted Apply(Var(...), arg). Alpha-equivalent copies have the same node structure.
        var best: Option[Candidate] = None
        for (key, entries) <- groups do
            for group <- entries if group.occurrences.size >= 2 do
                val paths = group.occurrences.toVector
                // Unique binders ensure all uses of an expression with a bound variable lie
                // within that binder's body. Their LCA therefore keeps its dependencies in scope.
                // Placing here also avoids wrapping unrelated parts of the enclosing region.
                // In lam a. Constr(0, [other, Constr(0, [f(a), f(a)])]), bind f(a) only around
                // the inner Constr: a stays in scope and other stays outside the new binding.
                val c = Candidate(key.term, paths.toSet, longestCommonPrefix(paths))
                sizeOf(c.expr).foreach { bits =>
                    val saving = SharingCost.savingLovelace(bits, c.occurrences.size)
                    val eligible = saving > 0 || preferComputationSharing(c.expr)
                    if eligible && best.forall(saving > _.savedLovelace) then
                        best = Some(c.copy(savedLovelace = saving))
                }
        best
    }

    /** Prefer evaluating repeated computations once, independently of their unknown runtime cost.
      * For example, two `f(xs)` calls can cost more than their few encoded bits suggest. Values
      * such as `Delay(f(xs))` still use fee pricing. Administrative lets use fee pricing too:
      * `(λx. x)(y)` adds no non-let structural node to the termination measure above.
      */
    private def preferComputationSharing(t: Term): Boolean = t match
        case Apply(_: LamAbs, _, _) => false
        case _: Apply | _: Force | _: Case | _: Constr => !t.isValueForm
        case _ => false

    /** Replace uses and insert their binding in one traversal of the original paths.
      *
      * For a subtree `Constr(0, [e, e])`, first build `Constr(0, [x, x])`, then wrap it in
      * `[(lam x ...) e]`. The binding's right-hand side is the original e: visiting it with the
      * replacement rule would incorrectly replace it with x and create a self-reference.
      */
    private def extract(term: Term, candidate: Candidate, name: String): Term = {
        def go(t: Term, path: Path): Term = {
            if candidate.occurrences.contains(path) then Var(NamedDeBruijn(name), t.annotation)
            else {
                val body = mapChildren(t)((child, i) => go(child, path :+ i))
                if path == candidate.bindAt then
                    Apply(LamAbs(name, body, t.annotation), candidate.expr, t.annotation)
                else body
            }
        }
        go(term, Vector.empty)
    }

    // Child numbering is shared by collection and rewriting: Apply uses 0=function, 1=argument;
    // Case uses 0=scrutinee, 1..n=branches. These indices must agree with mapChildren below.
    // For Apply(f, Case(s, [b0, b1])), path [1, 2] must identify b1 in both walks.
    private def children(t: Term): List[Term] = t match
        case LamAbs(_, body, _)           => List(body)
        case Apply(f, arg, _)             => List(f, arg)
        case Force(body, _)               => List(body)
        case Delay(body, _)               => List(body)
        case Constr(_, args, _)           => args
        case Case(scrutinee, branches, _) => scrutinee :: branches
        case _                            => Nil

    // Preserve unchanged subtree identities so per-pass size/key caches survive extraction rounds.
    private def mapChildren(t: Term)(f: (Term, Int) => Term): Term = t match
        case LamAbs(n, body, ann) =>
            val next = f(body, 0)
            if next eq body then t else LamAbs(n, next, ann)
        case Apply(fn, arg, ann) =>
            val nextFn = f(fn, 0)
            val nextArg = f(arg, 1)
            if (nextFn eq fn) && (nextArg eq arg) then t else Apply(nextFn, nextArg, ann)
        case Force(body, ann) =>
            val next = f(body, 0)
            if next eq body then t else Force(next, ann)
        case Delay(body, ann) =>
            val next = f(body, 0)
            if next eq body then t else Delay(next, ann)
        case Constr(tag, args, ann) =>
            val next = args.zipWithIndex.map(f.tupled)
            if next.zip(args).forall((a, b) => a eq b) then t else Constr(tag, next, ann)
        case Case(scrutinee, branches, ann) =>
            val nextScrutinee = f(scrutinee, 0)
            val nextBranches = branches.zipWithIndex.map((b, i) => f(b, i + 1))
            if (nextScrutinee eq scrutinee) && nextBranches.zip(branches).forall((a, b) => a eq b)
            then t
            else Case(nextScrutinee, nextBranches, ann)
        case _ => t
}

object CommonSubexpressionElimination {

    /** Canonicalize only a candidate's internal binders. Free names retain the identities assigned
      * by uniqueBinders, so lambdas with different captures remain distinct. The round trip gives
      * bound variables canonical names and positive indices, while free occurrences keep their
      * names and negative indices; even a free `i0` cannot equal the canonical bound `i0`. Keep the
      * original term for extraction, including its names, indices and annotations.
      */
    private[transform] final class AlphaTermKey(val term: Term, hash: Int) {
        def this(term: Term) = this(term, alphaHash(term))

        // Most large candidates are unique. Hash their shape first; pay for canonicalization only
        // when another key has the same hash. The canonical key resolves all weak-hash collisions.
        private lazy val canonical = new TermKey(
          DeBruijn.fromDeBruijnTerm(DeBruijn.deBruijnTerm(term))
        )

        override def equals(that: Any): Boolean = that match
            case other: AlphaTermKey => canonical == other.canonical
            case _                   => false

        override def hashCode(): Int = hash
    }

    // Preserve the emitted 1.1.0 methods for binary compatibility; these filters belong to CCE.
    // For example, an already compiled call to CSE.isSkippable(t) still resolves, but forwards
    // to CCE.isSkippable(t). The CSE collection algorithm above never calls these filters.
    @deprecated("use CommonContextExtraction.referencesPartialBuiltin", "1.1.1")
    private[transform] def referencesPartialBuiltin(t: Term): Boolean =
        CommonContextExtraction.referencesPartialBuiltin(t)

    @deprecated("use CommonContextExtraction.isSkippable", "1.1.1")
    private[transform] def isSkippable(t: Term): Boolean = CommonContextExtraction.isSkippable(t)

    @deprecated("use CommonContextExtraction.containsError", "1.1.1")
    private[transform] def containsError(t: Term): Boolean =
        CommonContextExtraction.containsError(t)

    /** Structural equality and hashing ignore annotations, so different source locations do not
      * prevent sharing. This is not alpha-equivalence: binder names still matter. The initial
      * unique-binder traversal prevents equal-looking variables from referring to different
      * binders. For example, `addInteger x 1` at source line 10 equals the same syntax at line 20,
      * but `lam x. x` and `lam y. y` are distinct keys despite being alpha-equivalent.
      */
    private[transform] final class TermKey(val term: Term) {
        override def equals(that: Any): Boolean = that match
            case other: TermKey => term ~=~ other.term
            case _              => false

        private lazy val hash = {
            def go(t: Term): Int = structuralHash(t, go, ignoreNames = false)
            go(term)
        }
        override def hashCode(): Int = hash

        override def toString: String = s"TermKey(${term.showShort})"
    }

    private def alphaHash(t: Term): Int = structuralHash(t, alphaHash, ignoreNames = true)

    /** Alpha keys use a weaker name-independent hash; structural keys preserve their existing hash.
      * Equal alpha terms necessarily share this shape, but only canonical equality decides sharing.
      */
    private def structuralHash(t: Term, recur: Term => Int, ignoreNames: Boolean): Int = t match
        case Var(name, _) => (if ignoreNames then 0 else name.hashCode) * 31 + 1
        case LamAbs(name, body, _) =>
            ((if ignoreNames then 0 else name.hashCode) * 31 + recur(body)) * 31 + 2
        case Apply(f, arg, _) => (recur(f) * 31 + recur(arg)) * 31 + 3
        case Force(inner, _)  => recur(inner) * 31 + 4
        case Delay(inner, _)  => recur(inner) * 31 + 5
        case Const(c, _)      => c.hashCode * 31 + 6
        case Builtin(bn, _)   => bn.ordinal * 31 + 7
        case Error(_)         => 8
        case Constr(tag, args, _) =>
            args.foldLeft(tag.hashCode * 31 + 9)((h, a) => h * 31 + recur(a))
        case Case(arg, cases, _) =>
            cases.foldLeft(recur(arg) * 31 + 10)((h, c) => h * 31 + recur(c))

    // A path is a sequence of child indices from the root, not a preorder traversal number.
    // For example, [0, 1] and [0, 2] have LCA [0]; wrapping that subtree leaves its siblings alone.
    private type Path = Vector[Int]

    // [] encloses every path; [1] encloses [1, 0] and itself, but not sibling [2].
    private def isAncestorOrSelf(ancestor: Path, descendant: Path): Boolean =
        descendant.startsWith(ancestor)

    /** Computes the LCA: paths [1, 0] and [1, 2, 0] give [1]; [0] and [1] give the root []. */
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

    /** Width of a UPLC term tag: Error costs 4 bits; Delay(t) costs 4 plus the size of t. */
    private[transform] val TermTagBits = 4

    /** Flat-encoded width of a `Var`: a 4-bit tag plus an 8-bit index group (7 payload bits and a
      * continuation bit).
      *
      * Exact while every de Bruijn index stays below 128. The largest index measured across the
      * example validators is 66. For example, index 1 costs 4 + 8 = 12 bits, whereas index 128
      * needs two groups (20 bits); this estimate would still charge it 12.
      */
    private[transform] val VarBits = TermTagBits + 8

    /** Approximate Flat-encoded bit size used for CSE and CCE profitability.
      *
      * Mirrors `Flat[Term].bitSize` in Term.scala, except that every `Var` is priced at the minimum
      * [[VarBits]] rather than from its de Bruijn index. Flat accepts unassigned index 0, so named
      * terms and CCE hole templates can be sized directly. The approximation deliberately ignores
      * index growth: index 127 takes one group, while 128 takes two. Negative free-variable indices
      * produced by de Bruijn conversion are also priced at the same fixed width.
      *
      * This estimate ignores variable-index growth and uses worst-case byte-array padding.
      * Profitability is heuristic; semantic safety comes from the placement rules. For example,
      * `(con (list data) [I 1])` is estimated at 54 bits: 4 for the term tag, 16 for the type, 2
      * for list framing, and 32 for the byte-wrapped CBOR datum, including worst-case alignment.
      * Counting this as one AST node would hide its serialization cost. The CBOR integer 1 occupies
      * one byte, but its Flat byte wrapper is estimated as four bytes: alignment, chunk length,
      * payload, and terminator. Actual alignment can use fewer than eight bits. A marginal positive
      * estimate can therefore lose its advantage after serialization, for example when extraction
      * pushes an existing variable index from 127 to 128.
      */
    private[transform] def termBits(t: Term): Int =
        cachedTermBits()(t).getOrElse(
          throw new IllegalArgumentException("Flat size is unavailable for this term")
        )

    /** One estimator per optimizer pass, reused across extraction rounds. An unavailable constant
      * encoding rejects its containing candidate; it must never become a large profitable sentinel.
      * Ask the encoder itself so serializable containers such as an empty BLS list remain
      * supported. Identity caches avoid reserializing constants when their surrounding term
      * wrappers change.
      */
    private[transform] def cachedTermBits(): Term => Option[Int] = {
        val terms = new java.util.IdentityHashMap[Term, Option[Int]]()
        val constants = new java.util.IdentityHashMap[Constant, Option[Int]]()

        def constantBits(c: Constant): Option[Int] = {
            val cached = constants.get(c)
            if cached != null then cached
            else
                val bits = Try(flatConstant.bitSize(c)).toOption
                constants.put(c, bits)
                bits
        }

        def listBits(ts: List[Term]): Option[Int] =
            ts.foldLeft(Option(ts.size + 1)) { (sum, t) =>
                for a <- sum; b <- size(t) yield a + b
            }

        def size(t: Term): Option[Int] = {
            val cached = terms.get(t)
            if cached != null then cached
            else {
                val bits = t match
                    case Var(_, _)   => Some(VarBits)
                    case Const(c, _) => constantBits(c).map(TermTagBits + _)
                    case Apply(f, arg, _) =>
                        for a <- size(f); b <- size(arg) yield TermTagBits + a + b
                    case LamAbs(_, body, _) => size(body).map(TermTagBits + _)
                    case Force(inner, _)    => size(inner).map(TermTagBits + _)
                    case Delay(inner, _)    => size(inner).map(TermTagBits + _)
                    case Builtin(bn, _) =>
                        Some(TermTagBits + summon[Flat[DefaultFun]].bitSize(bn))
                    case Error(_) => Some(TermTagBits)
                    case Constr(tag, args, _) =>
                        listBits(args).map(TermTagBits + summon[Flat[Word64]].bitSize(tag) + _)
                    case Case(arg, cases, _) =>
                        for a <- size(arg); bs <- listBits(cases) yield TermTagBits + a + bs
                terms.put(t, bits)
                bits
            }
        }
        size
    }

    /** AST node count: Apply(Var(f), Const(bytes)) has 3 nodes even for a kilobyte of bytes. Unlike
      * termBits, this does not measure script-size savings.
      */
    private[transform] def termSize(t: Term): Int = t match
        case Var(_, _) | Const(_, _) | Builtin(_, _) | Error(_) => 1
        case LamAbs(_, body, _)                                 => 1 + termSize(body)
        case Apply(f, arg, _)                                   => 1 + termSize(f) + termSize(arg)
        case Force(inner, _)                                    => 1 + termSize(inner)
        case Delay(inner, _)                                    => 1 + termSize(inner)
        case Constr(_, args, _)                                 => 1 + args.map(termSize).sum
        case Case(arg, cases, _) => 1 + termSize(arg) + cases.map(termSize).sum

    /** Reserve bound and free names: lam x. Apply(x, y) contributes both x and y. */
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

    /** Convenience entry point: CSE(term) runs a fresh CSE instance with its default logger. */
    def apply(term: Term): Term = {
        val cse = new CommonSubexpressionElimination()
        cse(term)
    }
}
