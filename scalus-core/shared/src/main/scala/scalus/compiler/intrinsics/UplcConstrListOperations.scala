package scalus.compiler.intrinsics

import scalus.Compile
import scalus.cardano.onchain.plutus.prelude.{fail, List, Option}
import scalus.compiler.intrinsics.IntrinsicHelpers.*

/** UplcConstr list operations — recursive implementations with local go functions.
  *
  * TypeVars are post-processed to Transparent after module loading, so pattern matching on the List
  * sum type uses passthrough representations (no Data wrapping).
  *
  * Uses local `go` functions (compiled as letrec) instead of module-level recursion to avoid
  * infinite support module binding resolution.
  */
@Compile
object UplcConstrListOperations {

    def map[A, B](self: List[A], mapper: A => B): List[B] = {
        def go(lst: List[A]): List[B] = lst match
            case List.Cons(h, t) => List.Cons(mapper(h), go(t))
            case List.Nil        => List.Nil
        go(self)
    }

    def filter[A](self: List[A], predicate: A => Boolean): List[A] = {
        def go(lst: List[A]): List[A] = lst match
            case List.Cons(h, t) =>
                if predicate(h) then List.Cons(h, go(t))
                else go(t)
            case List.Nil => List.Nil
        go(self)
    }

    def foldLeft[A, B](self: List[A], init: B, combiner: (B, A) => B): B = {
        def go(lst: List[A], acc: B): B = lst match
            case List.Cons(h, t) => go(t, combiner(acc, h))
            case List.Nil        => acc
        go(self, init)
    }

    def foldRight[A, B](self: List[A], init: B, combiner: (A, B) => B): B = {
        def go(lst: List[A]): B = lst match
            case List.Cons(h, t) => combiner(h, go(t))
            case List.Nil        => init
        go(self)
    }

    def find[A](self: List[A], predicate: A => Boolean): Option[A] = {
        def go(lst: List[A]): Option[A] = lst match
            case List.Cons(h, t) =>
                if predicate(h) then Option.Some(h) else go(t)
            case List.Nil => Option.None
        go(self)
    }

    def filterMap[A, B](self: List[A], predicate: A => Option[B]): List[B] = {
        def go(lst: List[A]): List[B] = lst match
            case List.Cons(h, t) =>
                predicate(h) match
                    case Option.None        => go(t)
                    case Option.Some(value) => List.Cons(value, go(t))
            case List.Nil => List.Nil
        go(self)
    }

    def quicksort[A](
        self: List[A],
        ord: (A, A) => scalus.cardano.onchain.plutus.prelude.Order
    ): List[A] = {
        def go(lst: List[A]): List[A] = lst match
            case List.Nil => List.Nil
            case List.Cons(head, tail) =>
                val before = filter(tail, (elem: A) => ord(elem, head).isLess)
                val after = filter(tail, (elem: A) => !ord(elem, head).isLess)
                append(go(before), prepended(go(after), head))
        go(self)
    }

    def contains[A](self: List[A], elem: A, eq: (A, A) => Boolean): Boolean = {
        def go(lst: List[A]): Boolean = lst match
            case List.Cons(h, t) =>
                if eq(h, elem) then true
                else go(t)
            case List.Nil => false
        go(self)
    }

    def length[A](self: List[A]): BigInt = {
        def go(lst: List[A], acc: BigInt): BigInt = lst match
            case List.Cons(_, t) => go(t, acc + BigInt(1))
            case List.Nil        => acc
        go(self, BigInt(0))
    }

    def reverse[A](self: List[A]): List[A] = {
        def go(lst: List[A], acc: List[A]): List[A] = lst match
            case List.Cons(h, t) => go(t, List.Cons(h, acc))
            case List.Nil        => acc
        go(self, List.Nil)
    }

    def append[A](self: List[A], other: List[A]): List[A] = {
        def go(lst: List[A]): List[A] = lst match
            case List.Cons(h, t) => List.Cons(h, go(t))
            case List.Nil        => other
        go(self)
    }

    def drop[A](self: List[A], n: BigInt): List[A] = {
        def go(lst: List[A], remaining: BigInt): List[A] =
            if remaining <= BigInt(0) then lst
            else
                lst match
                    case List.Cons(_, t) => go(t, remaining - BigInt(1))
                    case List.Nil        => List.Nil
        go(self, n)
    }

    def prepended[A](self: List[A], elem: A): List[A] = List.Cons(elem, self)

    def dropRight[A](self: List[A], n: BigInt): List[A] = {
        if n <= BigInt(0) then self
        else
            val len = length(self)
            val take = len - n
            def go(lst: List[A], remaining: BigInt): List[A] =
                if remaining <= BigInt(0) then List.Nil
                else
                    lst match
                        case List.Cons(h, t) => List.Cons(h, go(t, remaining - BigInt(1)))
                        case List.Nil        => List.Nil
            go(self, take)
    }

    def init[A](self: List[A]): List[A] = dropRight(self, BigInt(1))

}
