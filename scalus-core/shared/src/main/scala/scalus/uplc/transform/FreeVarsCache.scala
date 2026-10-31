package scalus.uplc.transform

import scalus.uplc.{NamedDeBruijn, Term}
import scalus.uplc.Term.{LamAbs, Var}
import scalus.uplc.internal.TermTraversal.{foreachChild, visit}

import java.util.IdentityHashMap

/** Free variables per term node, memoized for one optimizer pass. Gives the same result as
  * [[TermAnalysis.freeVars]].
  *
  * [[Inliner]] asks for the free variables of each `Apply`, `Force` and `Case` that it rebuilds.
  * Without the cache each query walks the whole subterm, which is quadratic for deep terms. The
  * rebuilt node is new, but its children are mostly subterms already seen, so they hit the cache.
  * Keyed by identity: two equal subterms at different places are different nodes.
  */
private[transform] final class FreeVarsCache {
    private val cache = new IdentityHashMap[Term, Set[String]]()

    def apply(term: Term): Set[String] = {
        // Skip subtrees already cached [MG-4]. On exit every child of a node is cached, so the
        // node's set is the union of its children's sets, minus the binder of a LamAbs.
        term.visit(n => !cache.containsKey(n)) { n =>
            val result = n match
                case Var(NamedDeBruijn(name, _), _) => Set(name)
                case LamAbs(name, body, _)          => cache.get(body) - name
                case _                              =>
                    // Start from the first child's set, not Set.empty: a one-child node then
                    // shares its child's set instead of copying it.
                    var union: Set[String] | Null = null
                    n.foreachChild { c =>
                        val childVars = cache.get(c)
                        union = if union == null then childVars else union.nn ++ childVars
                    }
                    if union == null then Set.empty[String] else union.nn
            cache.put(n, result)
        }
        cache.get(term)
    }
}
