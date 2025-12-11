package scalus.prelude

import scala.collection.mutable

/** Stub version of scalus.prelude.List for the compiler plugin.
  *
  * The real implementation is in scalus-core. This minimal stub provides only what's needed for
  * Data.scala and FlatInstances serialization.
  */
enum List[+A]:
    case Nil extends List[Nothing]
    case Cons(head: A, tail: List[A]) extends List[A]

object List {
    def empty[A]: List[A] = List.Nil

    def apply[A](args: A*): List[A] =
        args.foldRight(empty[A])((a, acc) => List.Cons(a, acc))

    def from[A](i: IterableOnce[A]): List[A] =
        i.iterator.foldRight(empty[A]) { case (a, b) => Cons(a, b) }

    extension [A](self: List[A])
        def toScalaList: scala.collection.immutable.List[A] =
            val buf = mutable.ListBuffer.empty[A]
            var current = self
            while current != List.Nil do
                current match
                    case List.Cons(head, tail) =>
                        buf.addOne(head)
                        current = tail
                    case List.Nil => ()
            buf.toList
}
