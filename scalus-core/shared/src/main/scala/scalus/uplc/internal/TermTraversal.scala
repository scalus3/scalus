package scalus.uplc.internal

import scalus.uplc.{NamedDeBruijn, Term}
import scalus.uplc.Term.*

import scala.collection.mutable.ListBuffer

/** One shared walk over UPLC [[Term]].
  *
  * Spec: docs/superpowers/specs/2026-10-27-term-traversal-design.md. Rule IDs such as `[TT-1]`
  * refer to it.
  *
  * Child order `[TT-1]`: `LamAbs` body; `Apply` function, then argument; `Force` body; `Delay`
  * body; `Constr` arguments in order; `Case` scrutinee, then branches in order.
  *
  * Whole-tree combinators (`visit`, `foreach`, `exists`, `foldLeft`, `transformUp`,
  * `transformDown`, `freeVars`) recurse on the JVM stack, one frame per tree level, like the
  * hand-written walks they replace. An explicit stack measured 1.5 to 2.2 times slower, so stack
  * safety is deferred (spec, `[TT-14]`). They allocate nothing per visited node, except for the
  * nodes a rewrite rebuilds `[TT-16]`.
  *
  * Rewrites keep identity: when nothing below a node changed, the node itself is returned (`eq`)
  * `[TT-12]`. Identity-keyed caches in the optimizer passes depend on that.
  *
  * The combinators are `inline` with `inline` callbacks `[TT-19]`. Each call site gets its own
  * local recursive function with the callback body in it, so there is no megamorphic `Function1`
  * call per node. For example, `t.exists(p)` expands to:
  * {{{
  * def go(n: Term): Boolean = <body of p with n> || <go on each child of n, stopping at true>
  * go(t)
  * }}}
  *
  * The combinators are extension methods on `Term`. Import the ones a file uses by name, for
  * example `import scalus.uplc.internal.TermTraversal.{exists, mapChildren}`. They are not members
  * of the public `Term` type. `freeVars` is a plain method, `TermTraversal.freeVars(t)`, because
  * `TermAnalysis` already provides a `freeVars` extension.
  */
object TermTraversal {

    // ---------------------------------------------------------------------------------------
    // One level
    // ---------------------------------------------------------------------------------------

    extension (t: Term) {

        /** Calls `f` on each child of `t`, in child order `[TT-1]`. Allocates nothing `[TT-18]`. */
        inline def foreachChild(inline f: Term => Unit): Unit =
            t.foreachChildIndexed((child, _) => f(child))

        /** Like [[foreachChild]], and also passes the child index, numbered as in
          * [[mapChildrenIndexed]] `[TT-2]`.
          */
        inline def foreachChildIndexed(inline f: (Term, Int) => Unit): Unit = t match
            case LamAbs(_, body, _) => f(body, 0)
            case Apply(fn, arg, _) =>
                f(fn, 0)
                f(arg, 1)
            case Force(body, _)     => f(body, 0)
            case Delay(body, _)     => f(body, 0)
            case Constr(_, args, _) => foreachInList(args, 0)(f)
            case Case(scrutinee, branches, _) =>
                f(scrutinee, 0)
                foreachInList(branches, 1)(f)
            case _ => ()

        /** Replaces each child of `t` by `f(child)`, in child order `[TT-1]`.
          *
          * Returns `t` itself when every result is `eq` to its child `[TT-12]`. Allocates a new
          * argument or branch list only when an element changed `[TT-13]`.
          */
        inline def mapChildren(inline f: Term => Term): Term =
            t.mapChildrenIndexed((child, _) => f(child))

        /** Like [[mapChildren]], and also passes the child index `[TT-2]`: `Apply` 0 = function, 1 =
          * argument; `Case` 0 = scrutinee, 1..n = branches; `Constr` 0..n-1 = arguments; one-child
          * nodes 0.
          */
        inline def mapChildrenIndexed(inline f: (Term, Int) => Term): Term = t match
            case LamAbs(name, body, ann) =>
                val next = f(body, 0)
                if next eq body then t else LamAbs(name, next, ann)
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
                val next = mapList(args, 0)(f)
                if next eq args then t else Constr(tag, next, ann)
            case Case(scrutinee, branches, ann) =>
                val nextScrutinee = f(scrutinee, 0)
                val nextBranches = mapList(branches, 1)(f)
                if (nextScrutinee eq scrutinee) && (nextBranches eq branches) then t
                else Case(nextScrutinee, nextBranches, ann)
            case _ => t

        // ---------------------------------------------------------------------------------------
        // Whole tree: queries
        // ---------------------------------------------------------------------------------------

        /** Walks `t` in pre-order.
          *
          * Calls `enter` on a node before its children `[TT-3]`. When `enter` returns `false`, the
          * children of that node are skipped `[TT-4]`. Calls `exit` once on every entered node,
          * after all its children `[TT-5]`. A pass can keep scope state in `enter`/`exit`, for
          * example push a binder on `LamAbs` enter and pop it on exit.
          */
        inline def visit(inline enter: Term => Boolean)(inline exit: Term => Unit): Unit = {
            def go(n: Term): Unit = {
                if enter(n) then
                    n.foreachChild(go)
                    exit(n)
            }
            go(t)
        }

        /** Calls `f` on every node of `t` in pre-order `[TT-6]`. */
        inline def foreach(inline f: Term => Unit): Unit = {
            def go(n: Term): Unit = {
                f(n)
                n.foreachChild(go)
            }
            go(t)
        }

        /** True if `p` holds for some node of `t`. Tests nodes in pre-order and stops at the first
          * match `[TT-7]` `[TT-21]`.
          */
        inline def exists(inline p: Term => Boolean): Boolean = {
            def go(n: Term): Boolean = p(n) || existsChild(n)(go)
            go(t)
        }

        /** Folds `f` over the nodes of `t` in pre-order `[TT-8]`. */
        inline def foldLeft[A](z: A)(inline f: (A, Term) => A): A = {
            def go(acc: A, n: Term): A = {
                var next = f(acc, n)
                n.foreachChild(c => next = go(next, c))
                next
            }
            go(z, t)
        }

        /** The number of nodes in `t`. Every node counts 1, a leaf too, whatever its size: `[f x]`
          * has 3 nodes, and so does `[f (con bytestring #...1 KB...)]`.
          */
        def nodeCount: Int = t.foldLeft(0)((n, _) => n + 1)

        // ---------------------------------------------------------------------------------------
        // Whole tree: rewrites
        // ---------------------------------------------------------------------------------------

        /** Bottom-up rewrite: rewrites all children of a node, then calls `f` on the rebuilt node
          * `[TT-9]`. Calls `f` once per node `[TT-11]`. Returns `t` itself when nothing changed
          * `[TT-12]`.
          */
        inline def transformUp(inline f: Term => Term): Term = {
            def go(n: Term): Term = f(n.mapChildren(go))
            go(t)
        }

        /** Top-down rewrite: calls `f` on a node, then rewrites the children of the result
          * `[TT-10]`. Calls `f` once per node and does not call it again on its own result
          * `[TT-11]`. Returns `t` itself when nothing changed `[TT-12]`.
          */
        inline def transformDown(inline f: Term => Term): Term = {
            def go(n: Term): Term = f(n).mapChildren(go)
            go(t)
        }
    }

    /** The names of the variables that occur free in `t`. */
    def freeVars(t: Term): Set[String] = {
        // Binder depth per name, in a one-element array mutated in place. Entries are never
        // removed, so each distinct name allocates once, however deep the shadowing [TT-16].
        // The map is created at the first binder: callers often pass small terms without one.
        var bound: java.util.HashMap[String, Array[Int]] | Null = null
        // An immutable set grown in place: for up to 4 names it is a small Set1..Set4, as the
        // old recursive version returned, and adding a name already present allocates nothing.
        var free = Set.empty[String]
        t.visit {
            case LamAbs(name, _, _) =>
                if bound == null then bound = new java.util.HashMap[String, Array[Int]]()
                var depth = bound.nn.get(name)
                if depth == null then
                    depth = new Array[Int](1)
                    bound.nn.put(name, depth)
                depth(0) += 1
                true
            case Var(NamedDeBruijn(name, _), _) =>
                val depth = if bound == null then null else bound.nn.get(name)
                if depth == null || depth(0) == 0 then free += name
                true
            case _ => true
        } {
            case LamAbs(name, _, _) => bound.nn.get(name)(0) -= 1
            case _                  => ()
        }
        free
    }

    /** A `while` loop, so no closure is allocated for `List.foreach`. Indices from `firstIndex`. */
    private inline def foreachInList(xs: List[Term], firstIndex: Int)(
        inline f: (Term, Int) => Unit
    ): Unit = {
        var rest = xs
        var i = firstIndex
        while rest.nonEmpty do
            f(rest.head, i)
            rest = rest.tail
            i += 1
    }

    /** True if `p` holds for some element. Stops at the first match. */
    private inline def existsInList(xs: List[Term])(inline p: Term => Boolean): Boolean = {
        var rest = xs
        var found = false
        while !found && rest.nonEmpty do
            found = p(rest.head)
            rest = rest.tail
        found
    }

    /** True if `p` holds for some child of `t`, tested in child order. Stops at the first match. */
    private inline def existsChild(t: Term)(inline p: Term => Boolean): Boolean = t match
        case LamAbs(_, body, _)           => p(body)
        case Apply(fn, arg, _)            => p(fn) || p(arg)
        case Force(body, _)               => p(body)
        case Delay(body, _)               => p(body)
        case Constr(_, args, _)           => existsInList(args)(p)
        case Case(scrutinee, branches, _) => p(scrutinee) || existsInList(branches)(p)
        case _                            => false

    /** Maps `xs` with indices from `firstIndex`. Calls `f` once per element, in order. Returns `xs`
      * itself when no element changed. Otherwise copies the unchanged prefix once, at the first
      * change.
      */
    private inline def mapList(xs: List[Term], firstIndex: Int)(
        inline f: (Term, Int) => Term
    ): List[Term] = {
        var rest = xs
        var i = 0
        var buf: ListBuffer[Term] | Null = null
        while rest.nonEmpty do
            val x = rest.head
            val y = f(x, firstIndex + i)
            if buf != null then buf.nn += y
            else if y ne x then
                val b = ListBuffer.empty[Term]
                var prefix = xs
                while prefix ne rest do
                    b += prefix.head
                    prefix = prefix.tail
                b += y
                buf = b
            rest = rest.tail
            i += 1
        if buf == null then xs else buf.nn.toList
    }
}
