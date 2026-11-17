package scalus.uplc.transform

import scalus.uplc.{Constant, Term}
import scalus.uplc.Term.*
import scalus.uplc.{DeBruijn, DefaultFun, NamedDeBruijn}
import scalus.uplc.eval.{Log, Logger}
import scalus.uplc.internal.TermTraversal.{foreachChildIndexed, mapChildren, nodeCount}
import scalus.uplc.transform.TermAnalysis.{collectNames, freeVars, isValueForm}
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
  * are also shared when that estimate is negative: the callee's avoided work is unknown, so this is
  * a runtime-sharing preference, not a prediction of lower total fees. Each round takes the
  * greatest eligible estimate, then recollects on the changed tree. For example, three uses of a
  * 54-bit constant save `(3 - 1) * 54 - 3 * 12 - 8 = 64` estimated bits: remove two copies, insert
  * three variables, and pay the Apply/LamAbs tags. The default fee estimate subtracts about 20.77
  * lovelace of binding work from 120 lovelace of reference-size savings. Two uses of an 18-bit
  * integer constant instead save `18 - 24 - 8 = -14` bits and are rejected. See
  * `docs/design/cse-placement.md` for the contract and pricing assumptions.
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
            case _ => t.mapChildren(uniqueBinders(_, env))

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
            val (tree, c) = candidate.get
            val name = freshName(s"__cse_${TermNaming.termDescription(c.expr)}")
            current = extract(current, tree, c, name)
            changed = true
            logger.log(
              s"CSE: extracted ${c.expr.showShort} (${c.occurrences.size} occurrences, size/binding estimate=${c.savedLovelace} lovelace) as $name"
            )
            // Extraction changes both occurrence counts and node numbers. For example, sharing
            // f(e) removes copies of the e inside it. A second candidate from the old tree would
            // have stale uses and stale savings, so select again from the rewritten tree:
            // Constr(0, [f(e), f(e)]) => let x = f(e) in Constr(0, [x, x]).
            // There is now just one e, and the new let shifts the numbers of the nodes after it.
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
    private final class Group(val region: Int, val occurrences: mutable.ArrayBuffer[Int])
    private final class Candidate(
        val expr: Term,
        val occurrences: mutable.ArrayBuffer[Int],
        val bindAt: Int,
        val savedLovelace: Double
    )

    /** Collect equal expressions into groups with safe placement, then choose one profitable group.
      *
      * A node is identified by its preorder number in `tree`; `region` is the number of the root of
      * its evaluation region. These are different: many nodes share a region, but the binding
      * belongs at their lowest common ancestor, not necessarily at the region root. In Constr(0,
      * [a, Constr(0, [e, e])]), the e nodes are 3 and 4, both in region 0 (the root); insert the
      * binding at node 2, the inner Constr.
      */
    private def collect(
        term: Term,
        sizeOf: Term => Option[Int],
        keyOf: Term => AlphaTermKey
    ): Option[(TreeIndex, Candidate)] = {
        val tree = new TreeIndex(term.nodeCount)
        val groups = mutable.LinkedHashMap.empty[AlphaTermKey, mutable.ArrayBuffer[Group]]

        def add(t: Term, node: Int, region: Int): Unit = {
            val entries = groups.getOrElseUpdate(keyOf(t), mutable.ArrayBuffer.empty)
            entries.find(g => tree.isAncestorOrSelf(g.region, region)) match
                case Some(ancestor) =>
                    // A strict occurrence was already found in this region or an enclosing one.
                    // Its binding can supply this use even if this use is inside a delay/branch.
                    // In Constr(0, [e, Delay(e)]), visiting the delayed e finds the first e's group.
                    ancestor.occurrences += node
                case None =>
                    // Traversal order need not discover the outer occurrence first. In
                    // Constr(0, [Delay(e), Delay(e), e]), the final e justifies merging both
                    // previously separate groups. Without that e, the siblings stay separate.
                    val occurrences = mutable.ArrayBuffer(node)
                    entries.filterInPlace { g =>
                        if tree.isAncestorOrSelf(region, g.region) then
                            occurrences ++= g.occurrences
                            false
                        else true
                    }
                    entries += new Group(region, occurrences)
        }

        // `region` is NewRegion when t is a body that starts its own region: t is its root.
        def go(t: Term, parent: Int, inherited: Int): Unit = {
            val node = tree.enter(t, parent)
            val region = if inherited == NewRegion then node else inherited
            // Record a Delay/LamAbs itself in the current region: constructing the value is
            // strict, although evaluating its body is not. Only the body starts a new region.
            // Constr(0, [Delay(e), Delay(e)]) may become let d = Delay(e) in Constr(0, [d, d]);
            // e remains unevaluated. Likewise, evaluating a lambda value does not call it.
            add(t, node, region)
            t match
                // Let bodies are strict, but still have lexical scope. Unique binders and the
                // structural LCA below keep any expression using the parameter inside its body.
                // (lam a. Constr(0, [f(a), f(a)])) arg shares f(a) inside lam a, never around arg.
                // The lambda gets a number but no record: the body inherits the let's region.
                case Apply(lam @ LamAbs(_, body, _), arg, _) =>
                    val lamNode = tree.enter(lam, node)
                    go(body, lamNode, region)
                    tree.exit(lamNode)
                    go(arg, node, region)
                case LamAbs(_, body, _)           => go(body, node, NewRegion)
                case Delay(body, _)               => go(body, node, NewRegion)
                case Case(scrutinee, branches, _) =>
                    // The scrutinee is strict; only the selected branch executes. A repeated
                    // expression confined to different branches cannot be hoisted above Case.
                    // Case(tag, [e, e, 0]) must still return 0 for tag 2 even when e would fail.
                    go(scrutinee, node, region)
                    branches.foreach(go(_, node, NewRegion))
                // Child order [TT-2] must agree with `extract`, which uses mapChildren. For
                // Apply(f, Case(s, [b0, b1])), b1 gets the same number in both walks.
                case _ =>
                    t.foreachChildIndexed((child, _) => go(child, node, region))
            tree.exit(node)
        }

        go(term, NoNode, NewRegion)
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
                sizeOf(key.term).foreach { bits =>
                    val saving = SharingCost.savingLovelace(bits, group.occurrences.size)
                    val eligible = saving > 0 || preferComputationSharing(key.term)
                    if eligible && best.forall(saving > _.savedLovelace) then
                        // Unique binders ensure all uses of an expression with a bound variable
                        // lie within that binder's body. Their LCA therefore keeps its
                        // dependencies in scope. Placing here also avoids wrapping unrelated
                        // parts of the enclosing region. In
                        // lam a. Constr(0, [other, Constr(0, [f(a), f(a)])]), bind f(a) only
                        // around the inner Constr: a stays in scope and other stays outside.
                        val bindAt = tree.lca(group.occurrences.min, group.occurrences.max)
                        best = Some(new Candidate(key.term, group.occurrences, bindAt, saving))
                }
        best.map((tree, _))
    }

    /** Prefer evaluating repeated computations once, independently of their unknown runtime cost.
      * For example, two `f(xs)` calls can cost more than their few encoded bits suggest. Values
      * such as `Delay(f(xs))` still use fee pricing. Administrative lets use fee pricing too:
      * `(λx. x)(y)` adds no non-let structural node to the termination measure above.
      */
    private def preferComputationSharing(t: Term): Boolean = t match
        case Apply(_: LamAbs, _, _)                    => false
        case _: Apply | _: Force | _: Case | _: Constr => !t.isValueForm
        case _                                         => false

    /** Replace uses and insert their binding in one traversal, numbering nodes as `collect` did.
      *
      * For a subtree `Constr(0, [e, e])`, first build `Constr(0, [x, x])`, then wrap it in
      * `[(lam x ...) e]`. The binding's right-hand side is the original e: visiting it with the
      * replacement rule would incorrectly replace it with x and create a self-reference.
      */
    private def extract(term: Term, tree: TreeIndex, candidate: Candidate, name: String): Term = {
        val replaced = new Array[Boolean](tree.size)
        candidate.occurrences.foreach(replaced(_) = true)
        var next = 0
        def go(t: Term): Term = {
            val node = next
            tree.checkNode(node, t)
            if replaced(node) then
                next = tree.end(node) // the whole subtree goes
                Var(NamedDeBruijn(name), t.annotation)
            else {
                next += 1
                // Unchanged subtrees keep their identity, so the per-pass size and key caches
                // survive extraction rounds.
                val body = t.mapChildren(go)
                if node == candidate.bindAt then
                    Apply(LamAbs(name, body, t.annotation), candidate.expr, t.annotation)
                else body
            }
        }
        go(term)
    }
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

    /** The parent of the root. */
    private val NoNode = -1

    /** Passed as the region of a body that starts its own region, such as a lambda body. */
    private val NewRegion = -1

    /** Preorder numbers of the nodes of one term, children in [TT-2] order; the root is node 0.
      *
      * A subtree is a range of numbers: node a is an ancestor-or-self of node d iff
      * `a <= d < end(a)`. For Constr(0, [x, Delay(y)]), the nodes are 0 = Constr, 1 = x, 2 = Delay
      * and 3 = y, and end(2) = 4. While the walk is still inside a node, its end is Int.MaxValue:
      * every node numbered after it so far is in its subtree, and the test says so.
      */
    private final class TreeIndex(val size: Int) {
        private val terms = new Array[Term](size)
        private val parents = new Array[Int](size)
        private val depths = new Array[Int](size)
        private val ends = new Array[Int](size)
        private var next = 0

        /** Numbers `t`, the next node in preorder. */
        def enter(t: Term, parent: Int): Int = {
            val node = next
            next += 1
            terms(node) = t
            parents(node) = parent
            depths(node) = if parent == NoNode then 0 else depths(parent) + 1
            ends(node) = Int.MaxValue
            node
        }

        /** Closes the subtree of `node` after the walk has numbered all of it. */
        def exit(node: Int): Unit = ends(node) = next

        def end(node: Int): Int = ends(node)

        def isAncestorOrSelf(ancestor: Int, node: Int): Boolean =
            ancestor <= node && node < ends(ancestor)

        /** Lowest common ancestor: in Constr(0, [x, Delay(y)]), lca(1, 3) = 0 and lca(2, 3) = 2. */
        def lca(a: Int, b: Int): Int = {
            var x = a
            var y = b
            while depths(x) > depths(y) do x = parents(x)
            while depths(y) > depths(x) do y = parents(y)
            while x != y do
                x = parents(x)
                y = parents(y)
            x
        }

        /** A later walk must visit the same node under the same number. */
        def checkNode(node: Int, t: Term): Unit =
            if !(terms(node) eq t) then
                throw new IllegalStateException(s"CSE node $node is numbered differently")
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

    /** Kept because released bytecode has it; use `t.nodeCount` from TermTraversal. */
    private[transform] def termSize(t: Term): Int = t.nodeCount

    /** Convenience entry point: CSE(term) runs a fresh CSE instance with its default logger. */
    def apply(term: Term): Term = {
        val cse = new CommonSubexpressionElimination()
        cse(term)
    }
}
