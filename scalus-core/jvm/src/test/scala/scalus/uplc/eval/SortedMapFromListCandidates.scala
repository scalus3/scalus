package scalus.uplc.eval

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}

/** Candidate implementations of `SortedMap.fromList`, measured by [[SortedMapFromListBudgetTest]].
  *
  * `SortedMap.fromList` is an insertion sort in disguise: it folds LEFT over the input, inserting
  * each pair into a growing sorted accumulator. That makes it O(n^2), and specifically it makes
  * ALREADY-ASCENDING input its worst case – every insert walks the whole accumulator to reach the
  * end. Ascending is exactly how ledger data arrives.
  *
  * All three candidates keep the documented contract: keys end up in strictly ascending order, and
  * where a key occurs more than once the FIRST occurrence in the input prevails.
  */
@Compile
object SortedMapFromListCandidates {

    /** The shipped implementation, copied verbatim from `SortedMap.fromList` so that the baseline
      * measures the real code rather than a paraphrase of it.
      */
    def baseline[A: Ord, B](lst: List[(A, B)]): List[(A, B)] = {
        def insertIfDoesNotExist(lst: List[(A, B)], key: A, value: B): List[(A, B)] = lst match
            case Nil => List.singleton((key, value))
            case Cons(pair, tail) =>
                pair match
                    case (k, v) =>
                        key <=> k match
                            case Order.Less    => Cons((key, value), lst)
                            case Order.Greater => Cons(pair, insertIfDoesNotExist(tail, key, value))
                            case Order.Equal   => lst

        lst.foldLeft(List.empty) { (acc, pair) => insertIfDoesNotExist(acc, pair._1, pair._2) }
    }

    /** Same insertion sort, but folded from the RIGHT. Ascending input becomes the BEST case
      * instead of the worst: the tail is sorted first, so each new head is already the smallest and
      * `insert` stops at its first comparison.
      *
      * Duplicate handling stays correct by mirroring the direction: because elements are inserted
      * later-first, an existing equal key always came from a LATER input position, so the incoming
      * pair replaces it and the first occurrence still wins.
      */
    def insertRight[A: Ord, B](lst: List[(A, B)]): List[(A, B)] = {
        def insert(sorted: List[(A, B)], p: (A, B)): List[(A, B)] = sorted match
            case Nil => Cons(p, Nil)
            case Cons(q, tail) =>
                p._1 <=> q._1 match
                    case Order.Less    => Cons(p, sorted)
                    case Order.Greater => Cons(q, insert(tail, p))
                    case Order.Equal   => Cons(p, tail)

        def go(rest: List[(A, B)]): List[(A, B)] = rest match
            case Nil           => Nil
            case Cons(p, tail) => insert(go(tail), p)

        go(lst)
    }

    /** Stable natural merge sort on the key, then a single pass that drops adjacent duplicates.
      *
      * Stability is what makes the dedup pass correct: the sort preserves input order among equal
      * keys, so the head of each run of equal keys is the first occurrence, and keeping heads
      * satisfies the contract. Theta(n log n) worst case, O(n) on already-ascending input because
      * the whole list is then a single run.
      */
    def sortDedup[A: Ord, B](lst: List[(A, B)]): List[(A, B)] = {
        def merge(a: List[(A, B)], b: List[(A, B)]): List[(A, B)] = a match
            case Nil => b
            case Cons(x, xs) =>
                b match
                    case Nil         => a
                    case Cons(y, ys) =>
                        // strictly-less keeps `a` ahead of `b` on ties, which is what makes the
                        // merge stable
                        if (y._1 <=> x._1).isLess then Cons(y, merge(a, ys))
                        else Cons(x, merge(xs, b))

        def reverseOnto(l: List[(A, B)], acc: List[(A, B)]): List[(A, B)] = l match
            case Nil         => acc
            case Cons(x, xs) => reverseOnto(xs, Cons(x, acc))

        def runs(rest: List[(A, B)], cur: (A, B), acc: List[(A, B)]): List[List[(A, B)]] =
            rest match
                case Nil => Cons(reverseOnto(Cons(cur, acc), Nil), Nil)
                case Cons(x, xs) =>
                    if (x._1 <=> cur._1).isLess then
                        Cons(reverseOnto(Cons(cur, acc), Nil), runs(xs, x, Nil))
                    else runs(xs, x, Cons(cur, acc))

        def mergePairs(ls: List[List[(A, B)]]): List[List[(A, B)]] = ls match
            case Nil => Nil
            case Cons(a, rest) =>
                rest match
                    case Nil         => ls
                    case Cons(b, tl) => Cons(merge(a, b), mergePairs(tl))

        def mergeAll(ls: List[List[(A, B)]]): List[(A, B)] = ls match
            case Nil => Nil
            case Cons(a, rest) =>
                rest match
                    case Nil        => a
                    case Cons(_, _) => mergeAll(mergePairs(ls))

        def skipEq(k: A, rest: List[(A, B)]): List[(A, B)] = rest match
            case Nil => Nil
            case Cons(q, tail) =>
                k <=> q._1 match
                    case Order.Equal => skipEq(k, tail)
                    case _           => Cons(q, skipEq(q._1, tail))

        val sorted = lst match
            case Nil         => Nil
            case Cons(x, xs) => mergeAll(runs(xs, x, Nil))

        sorted match
            case Nil           => Nil
            case Cons(p, tail) => Cons(p, skipEq(p._1, tail))
    }
}
