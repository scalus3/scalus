package scalus.uplc.eval

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}

/** Candidate on-chain sorting algorithms, measured against each other by [[SortBudgetTest]].
  *
  * Every candidate is written the way the prelude writes `sort`: generic in `A` with an `Ord[A]`
  * context bound, so the comparison closure is passed at runtime exactly as it is for the shipped
  * `List.sort`. That keeps the comparison apples-to-apples – what differs between rows of the
  * benchmark is the algorithm shape, not the calling convention.
  *
  * The candidates deliberately do not call other prelude list helpers (`++`, `filter`, `foldRight`,
  * `reverse`), so each one's measured cost is its own rather than partly someone else's.
  */
@Compile
object SortCandidates {

    /** Result carrier for the alternating deal in [[mergeSortDeal]].
      *
      * A case class rather than a `Tuple2`: `List.sort` carries an `INVESTIGATION` note that a
      * `Tuple2`-returning partition trips a lowering bug under `optimizeUplc = false`, and
      * `UplcConstrListOperations.sort` uses a local `Partition` case class for the same reason.
      */
    case class Split[A](left: List[A], right: List[A])

    /** Result carrier for the three-way partition in [[quicksortMedian3]]. */
    case class Parts3[A](lt: List[A], eq: List[A], gt: List[A])

    // ----------------------------------------------------------------------------------
    // 1. Baseline – the currently shipped prelude sort (head-pivot quicksort).
    // ----------------------------------------------------------------------------------

    def preludeSort[A: Ord](self: List[A]): List[A] = self.sort

    /** The head-pivot quicksort the prelude shipped BEFORE this work, preserved verbatim so the
      * report can state a before/after under identical conditions. Once `List.sort` changed,
      * [[preludeSort]] stopped being a baseline and became the new implementation.
      */
    def headPivotQuicksort[A: Ord](self: List[A]): List[A] = {
        def partition(
            lst: List[A],
            pivot: A,
            before: List[A],
            after: List[A]
        ): (List[A], List[A]) = lst match
            case Nil => (before, after)
            case Cons(h, t) =>
                if (h <=> pivot).isLess then partition(t, pivot, Cons(h, before), after)
                else partition(t, pivot, before, Cons(h, after))

        def sortAcc(lst: List[A], acc: List[A]): List[A] = lst match
            case Nil => acc
            case Cons(pivot, rest) =>
                val parts = partition(rest, pivot, Nil, Nil)
                sortAcc(parts._1, Cons(pivot, sortAcc(parts._2, acc)))

        sortAcc(self, Nil)
    }

    // ----------------------------------------------------------------------------------
    // 2. Insertion sort, right-to-left – the exact shape of Aiken stdlib `list.sort`
    //    (aiken-lang/stdlib v3.0.0, lib/aiken/collection/list.ak:1109-1126).
    //    Sorts the tail first, then inserts the head. O(n) on ASCENDING input, because the
    //    head is then already the smallest and `insert` stops at the first comparison.
    // ----------------------------------------------------------------------------------

    def insertionSortRight[A: Ord](self: List[A]): List[A] = {
        def insert(lst: List[A], e: A): List[A] = lst match
            case Nil => Cons(e, Nil)
            case Cons(x, xs) =>
                if (e <=> x).isLess then Cons(e, lst)
                else Cons(x, insert(xs, e))

        def go(lst: List[A]): List[A] = lst match
            case Nil         => Nil
            case Cons(x, xs) => insert(go(xs), x)

        go(self)
    }

    // ----------------------------------------------------------------------------------
    // 3. Insertion sort, left-to-right – tail-recursive outer loop over a growing sorted
    //    accumulator. Mirror image of #2: O(n) on DESCENDING input, O(n^2) on ascending.
    //    Included to show that the two insertion variants have opposite best cases.
    // ----------------------------------------------------------------------------------

    def insertionSortLeft[A: Ord](self: List[A]): List[A] = {
        def insert(lst: List[A], e: A): List[A] = lst match
            case Nil => Cons(e, Nil)
            case Cons(x, xs) =>
                if (e <=> x).isLess then Cons(e, lst)
                else Cons(x, insert(xs, e))

        def go(lst: List[A], acc: List[A]): List[A] = lst match
            case Nil         => acc
            case Cons(x, xs) => go(xs, insert(acc, x))

        go(self, Nil)
    }

    // ----------------------------------------------------------------------------------
    // 4. Top-down merge sort with an alternating deal.
    //    `deal` swaps its two accumulators on every step, so elements land alternately in
    //    the two halves in a single tail-recursive pass – no length count, no tuple.
    //    Theta(n log n) on every input; no pivot for an attacker to aim at.
    // ----------------------------------------------------------------------------------

    def mergeSortDeal[A: Ord](self: List[A]): List[A] = {
        def deal(lst: List[A], l: List[A], r: List[A]): Split[A] = lst match
            case Nil         => Split(l, r)
            case Cons(x, xs) => deal(xs, r, Cons(x, l))

        def merge(a: List[A], b: List[A]): List[A] = a match
            case Nil => b
            case Cons(x, xs) =>
                b match
                    case Nil => a
                    case Cons(y, ys) =>
                        if (y <=> x).isLess then Cons(y, merge(a, ys))
                        else Cons(x, merge(xs, b))

        def go(lst: List[A]): List[A] = lst match
            case Nil => Nil
            case Cons(_, tail) =>
                tail match
                    case Nil => lst
                    case Cons(_, _) =>
                        val s = deal(lst, Nil, Nil)
                        merge(go(s.left), go(s.right))

        go(self)
    }

    // ----------------------------------------------------------------------------------
    // 5. Natural merge sort, ASCENDING RUNS ONLY: split into maximal ascending runs, then
    //    merge the runs pairwise. O(n) on ascending input, Theta(n log n) worst case.
    //    Costs a nested `List[List[A]]`, whose per-element Data wrapping this measures.
    //
    //    This is deliberately WEAKER than GHC's / Plutus-tx's `sortBy`, which also detects
    //    DESCENDING runs and reverses them in place. On descending input this candidate
    //    produces n singleton runs where the full version produces one, so its descending
    //    numbers are an upper bound on what the recommended implementation would cost.
    //    Kept in this weaker form so that the measured advantage is not overstated.
    // ----------------------------------------------------------------------------------

    def mergeSortNatural[A: Ord](self: List[A]): List[A] = {
        def merge(a: List[A], b: List[A]): List[A] = a match
            case Nil => b
            case Cons(x, xs) =>
                b match
                    case Nil => a
                    case Cons(y, ys) =>
                        if (y <=> x).isLess then Cons(y, merge(a, ys))
                        else Cons(x, merge(xs, b))

        // Accumulate an ascending run in `acc` (reversed), flush it when the run breaks.
        def runs(lst: List[A], cur: A, acc: List[A]): List[List[A]] = lst match
            case Nil => Cons(reverseOnto(Cons(cur, acc), Nil), Nil)
            case Cons(x, xs) =>
                if (x <=> cur).isLess then Cons(reverseOnto(Cons(cur, acc), Nil), runs(xs, x, Nil))
                else runs(xs, x, Cons(cur, acc))

        def mergePairs(ls: List[List[A]]): List[List[A]] = ls match
            case Nil => Nil
            case Cons(a, rest) =>
                rest match
                    case Nil         => ls
                    case Cons(b, tl) => Cons(merge(a, b), mergePairs(tl))

        def mergeAll(ls: List[List[A]]): List[A] = ls match
            case Nil => Nil
            case Cons(a, rest) =>
                rest match
                    case Nil        => a
                    case Cons(_, _) => mergeAll(mergePairs(ls))

        self match
            case Nil         => Nil
            case Cons(x, xs) => mergeAll(runs(xs, x, Nil))
    }

    /** Tail-recursive `reverse ++ acc`, used by [[mergeSortNatural]] to flush a run. */
    def reverseOnto[A](lst: List[A], acc: List[A]): List[A] = lst match
        case Nil         => acc
        case Cons(x, xs) => reverseOnto(xs, Cons(x, acc))

    // ----------------------------------------------------------------------------------
    // 6. Quicksort with a median-of-three pivot and a three-way partition.
    //    The "can the incumbent be saved cheaply" candidate. Median of head/middle/last
    //    fixes the sorted and reverse-sorted cases; the three-way split makes all-equal
    //    input linear. It does NOT remove the adversarial O(n^2) – a crafted input can
    //    still defeat any fixed deterministic pivot rule.
    // ----------------------------------------------------------------------------------

    def quicksortMedian3[A: Ord](self: List[A]): List[A] = {
        def lastOf(lst: List[A], cur: A): A = lst match
            case Nil         => cur
            case Cons(x, xs) => lastOf(xs, x)

        // Slow/fast walk: `slow` advances one step per two steps of `fast`.
        def midOf(slow: List[A], fast: List[A], cur: A): A = fast match
            case Nil => cur
            case Cons(_, ft) =>
                ft match
                    case Nil => cur
                    case Cons(_, ftt) =>
                        slow match
                            case Nil         => cur
                            case Cons(s, st) => midOf(st, ftt, s)

        def median3(a: A, b: A, c: A): A =
            if (a <=> b).isLess then
                if (b <=> c).isLess then b
                else if (a <=> c).isLess then c
                else a
            else if (a <=> c).isLess then a
            else if (b <=> c).isLess then c
            else b

        def partition3(
            lst: List[A],
            pivot: A,
            lt: List[A],
            eq: List[A],
            gt: List[A]
        ): Parts3[A] = lst match
            case Nil => Parts3(lt, eq, gt)
            case Cons(x, xs) =>
                x <=> pivot match
                    case Order.Less    => partition3(xs, pivot, Cons(x, lt), eq, gt)
                    case Order.Equal   => partition3(xs, pivot, lt, Cons(x, eq), gt)
                    case Order.Greater => partition3(xs, pivot, lt, eq, Cons(x, gt))

        // `sortAcc(lst, acc)` computes `sorted(lst) ++ acc` without ever calling `++`.
        def sortAcc(lst: List[A], acc: List[A]): List[A] = lst match
            case Nil => acc
            case Cons(h, tail) =>
                tail match
                    case Nil => Cons(h, acc)
                    case Cons(_, _) =>
                        val pivot = median3(h, midOf(lst, lst, h), lastOf(tail, h))
                        val parts = partition3(lst, pivot, Nil, Nil, Nil)
                        // `eq` elements all compare equal, so their order among themselves
                        // is irrelevant and a tail-recursive prepend is enough.
                        sortAcc(parts.lt, reverseOnto(parts.eq, sortAcc(parts.gt, acc)))

        sortAcc(self, Nil)
    }

    // ----------------------------------------------------------------------------------
    // 7. Verify instead of sort – the floor, and a candidate stdlib API in its own right.
    //    If a caller can supply the sorted list off-chain, checking the order is one pass
    //    and n-1 comparisons, with zero allocation.
    // ----------------------------------------------------------------------------------

    /** True when every element is less than or equal to its successor. */
    def isSorted[A: Ord](self: List[A]): Boolean = self match
        case Nil => true
        case Cons(x, xs) =>
            def go(prev: A, rest: List[A]): Boolean = rest match
                case Nil => true
                case Cons(y, ys) =>
                    if (y <=> prev).isLess then false else go(y, ys)
            go(x, xs)

    /** True when every element is strictly less than its successor (sorted and duplicate-free). */
    def isStrictlyAscending[A: Ord](self: List[A]): Boolean = self match
        case Nil => true
        case Cons(x, xs) =>
            def go(prev: A, rest: List[A]): Boolean = rest match
                case Nil => true
                case Cons(y, ys) =>
                    if (prev <=> y).isLess then go(y, ys) else false
            go(x, xs)

    // ----------------------------------------------------------------------------------
    // 8. Guarded merge sort – pay one O(n) `isSorted` pass to make the already-sorted
    //    case linear, then fall back to the worst-case-safe merge sort.
    // ----------------------------------------------------------------------------------

    def mergeSortGuarded[A: Ord](self: List[A]): List[A] =
        if isSorted(self) then self else mergeSortDeal(self)

    // ----------------------------------------------------------------------------------
    // 9. Counted bottom-up merge sort. Counts the list once, then splits by count rather
    //    than by dealing or by run detection, so it never builds a `List[List[A]]` – the
    //    nested list is what makes [[mergeSortNatural]] expensive at small n. Stable, and
    //    Theta(n log n) on every input with no best case to speak of.
    // ----------------------------------------------------------------------------------

    /** `sorted` holds the first `n` elements in order; `rest` is what was left over. */
    case class Taken[A](sorted: List[A], rest: List[A])

    def mergeSortCounted[A: Ord](self: List[A]): List[A] = {
        def merge(a: List[A], b: List[A]): List[A] = a match
            case Nil => b
            case Cons(x, xs) =>
                b match
                    case Nil => a
                    case Cons(y, ys) =>
                        if (y <=> x).isLess then Cons(y, merge(a, ys))
                        else Cons(x, merge(xs, b))

        def count(lst: List[A], acc: BigInt): BigInt = lst match
            case Nil        => acc
            case Cons(_, t) => count(t, acc + 1)

        def sortN(n: BigInt, xs: List[A]): Taken[A] =
            if n <= BigInt(1) then
                xs match
                    case Nil        => Taken(Nil, Nil)
                    case Cons(h, t) => Taken(Cons(h, Nil), t)
            else
                val half = n / 2
                val a = sortN(half, xs)
                val b = sortN(n - half, a.rest)
                Taken(merge(a.sorted, b.sorted), b.rest)

        val n = count(self, 0)
        if n <= BigInt(1) then self else sortN(n, self).sorted
    }

    // ----------------------------------------------------------------------------------
    // 10. Hybrid: insertion sort for short lists, natural merge sort above the threshold.
    //     The length test walks at most `threshold + 1` cons cells, so it costs O(1) on a
    //     long list rather than a full length pass.
    // ----------------------------------------------------------------------------------

    /** True when `lst` holds at most `k` elements. Walks at most `k + 1` cells. */
    def atMost[A](lst: List[A], k: BigInt): Boolean =
        if k < BigInt(0) then false
        else
            lst match
                case Nil        => true
                case Cons(_, t) => atMost(t, k - 1)

    def sortHybrid[A: Ord](self: List[A]): List[A] =
        if atMost(self, 8) then insertionSortLeft(self) else mergeSortNatural(self)

    // ----------------------------------------------------------------------------------
    // 11. The same natural merge sort, but driven by a BOOLEAN less-than instead of the
    //     three-way `Ord`. `Ord[A]` is `(A, A) => Order`, so every single comparison
    //     allocates an `Order` constructor and then pattern-matches it back down to a
    //     boolean. A `(A, A) => Boolean` comparator skips both steps, and at `BigInt` it
    //     lets the monomorphic `<` fast path lower straight to `lessThanInteger`.
    //
    //     liqwid-plutarch-extra makes the same choice: its merge uses a boolean `pleqBy`
    //     rather than the 3-way `pcompareBy`. This candidate measures what that is worth.
    // ----------------------------------------------------------------------------------

    def mergeSortNaturalLt[A](self: List[A], lt: (A, A) => Boolean): List[A] = {
        def merge(a: List[A], b: List[A]): List[A] = a match
            case Nil => b
            case Cons(x, xs) =>
                b match
                    case Nil => a
                    case Cons(y, ys) =>
                        if lt(y, x) then Cons(y, merge(a, ys))
                        else Cons(x, merge(xs, b))

        def revOnto(l: List[A], acc: List[A]): List[A] = l match
            case Nil         => acc
            case Cons(x, xs) => revOnto(xs, Cons(x, acc))

        def runs(rest: List[A], cur: A, acc: List[A]): List[List[A]] = rest match
            case Nil => Cons(revOnto(Cons(cur, acc), Nil), Nil)
            case Cons(x, xs) =>
                if lt(x, cur) then Cons(revOnto(Cons(cur, acc), Nil), runs(xs, x, Nil))
                else runs(xs, x, Cons(cur, acc))

        def mergePairs(ls: List[List[A]]): List[List[A]] = ls match
            case Nil => Nil
            case Cons(a, rest) =>
                rest match
                    case Nil         => ls
                    case Cons(b, tl) => Cons(merge(a, b), mergePairs(tl))

        def mergeAll(ls: List[List[A]]): List[A] = ls match
            case Nil => Nil
            case Cons(a, rest) =>
                rest match
                    case Nil        => a
                    case Cons(_, _) => mergeAll(mergePairs(ls))

        self match
            case Nil         => Nil
            case Cons(x, xs) => mergeAll(runs(xs, x, Nil))
    }

    /** Insertion sort driven by a boolean less-than, for the same comparison at small n. */
    def insertionSortLeftLt[A](self: List[A], lt: (A, A) => Boolean): List[A] = {
        def insert(lst: List[A], e: A): List[A] = lst match
            case Nil => Cons(e, Nil)
            case Cons(x, xs) =>
                if lt(e, x) then Cons(e, lst)
                else Cons(x, insert(xs, e))

        def go(lst: List[A], acc: List[A]): List[A] = lst match
            case Nil         => acc
            case Cons(x, xs) => go(xs, insert(acc, x))

        go(self, Nil)
    }

    // ----------------------------------------------------------------------------------
    // 12. Identical to [[mergeSortNaturalLt]] except that `merge` tests the two-cons case
    //     FIRST and the Nil cases last. On the PlutusTx code generator this clause ordering
    //     was worth 25% CPU (IntersectMBO/plutus PR #4063 review discussion). This candidate
    //     exists to find out whether the Scalus lowering has the same sensitivity.
    // ----------------------------------------------------------------------------------

    def mergeSortNaturalLtConsFirst[A](self: List[A], lt: (A, A) => Boolean): List[A] = {
        def merge(a: List[A], b: List[A]): List[A] = a match
            case Cons(x, xs) =>
                b match
                    case Cons(y, ys) =>
                        if lt(y, x) then Cons(y, merge(a, ys))
                        else Cons(x, merge(xs, b))
                    case Nil => a
            case Nil => b

        def revOnto(l: List[A], acc: List[A]): List[A] = l match
            case Nil         => acc
            case Cons(x, xs) => revOnto(xs, Cons(x, acc))

        def runs(rest: List[A], cur: A, acc: List[A]): List[List[A]] = rest match
            case Cons(x, xs) =>
                if lt(x, cur) then Cons(revOnto(Cons(cur, acc), Nil), runs(xs, x, Nil))
                else runs(xs, x, Cons(cur, acc))
            case Nil => Cons(revOnto(Cons(cur, acc), Nil), Nil)

        def mergePairs(ls: List[List[A]]): List[List[A]] = ls match
            case Cons(a, rest) =>
                rest match
                    case Cons(b, tl) => Cons(merge(a, b), mergePairs(tl))
                    case Nil         => ls
            case Nil => Nil

        def mergeAll(ls: List[List[A]]): List[A] = ls match
            case Cons(a, rest) =>
                rest match
                    case Cons(_, _) => mergeAll(mergePairs(ls))
                    case Nil        => a
            case Nil => Nil

        self match
            case Cons(x, xs) => mergeAll(runs(xs, x, Nil))
            case Nil         => Nil
    }

    // ----------------------------------------------------------------------------------
    // 13. [[mergeSortNatural]] with every comparison written as a direct `match` on `Order`
    //     instead of `(y <=> x).isLess`. `isLess` is itself a match that produces a
    //     `Boolean` which is then branched on, so matching the `Order` once should skip a
    //     materialized value and a branch. Everything else is identical to #5, so the
    //     difference between the two rows is exactly what the idiom is worth.
    // ----------------------------------------------------------------------------------

    def mergeSortNaturalOrdMatch[A: Ord](self: List[A]): List[A] = {
        def merge(a: List[A], b: List[A]): List[A] = a match
            case Nil => b
            case Cons(x, xs) =>
                b match
                    case Nil => a
                    case Cons(y, ys) =>
                        y <=> x match
                            case Order.Less => Cons(y, merge(a, ys))
                            case _          => Cons(x, merge(xs, b))

        def runs(lst: List[A], cur: A, acc: List[A]): List[List[A]] = lst match
            case Nil => Cons(reverseOnto(Cons(cur, acc), Nil), Nil)
            case Cons(x, xs) =>
                x <=> cur match
                    case Order.Less => Cons(reverseOnto(Cons(cur, acc), Nil), runs(xs, x, Nil))
                    case _          => runs(xs, x, Cons(cur, acc))

        def mergePairs(ls: List[List[A]]): List[List[A]] = ls match
            case Nil => Nil
            case Cons(a, rest) =>
                rest match
                    case Nil         => ls
                    case Cons(b, tl) => Cons(merge(a, b), mergePairs(tl))

        def mergeAll(ls: List[List[A]]): List[A] = ls match
            case Nil => Nil
            case Cons(a, rest) =>
                rest match
                    case Nil        => a
                    case Cons(_, _) => mergeAll(mergePairs(ls))

        self match
            case Nil         => Nil
            case Cons(x, xs) => mergeAll(runs(xs, x, Nil))
    }

    /** `isSorted` driven by a boolean less-than. */
    def isSortedLt[A](self: List[A], lt: (A, A) => Boolean): Boolean = self match
        case Nil => true
        case Cons(x, xs) =>
            def go(prev: A, rest: List[A]): Boolean = rest match
                case Nil => true
                case Cons(y, ys) =>
                    if lt(y, prev) then false else go(y, ys)
            go(x, xs)
}
