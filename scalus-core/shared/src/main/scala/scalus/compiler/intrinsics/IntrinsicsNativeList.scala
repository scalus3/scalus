package scalus.compiler.intrinsics

import scalus.Compile
import scalus.cardano.onchain.plutus.prelude.{List, Option}
import scalus.compiler.intrinsics.IntrinsicHelpers.*
import scalus.uplc.builtin.BuiltinList
import scalus.uplc.builtin.Builtins.*

/** Native list intrinsics — list operations with builtin (Transparent) TypeVars.
  *
  * Elements are stored in native UPLC representation via defaultRepresentation(elementType).
  * No extra iData/unIData wrapping needed — headList/mkCons work on native values.
  *
  * The IntrinsicResolver dispatches to these when the list has native element
  * representation (SumBuiltinList with !isPackedData element repr).
  */
@Compile
object IntrinsicsNativeList {

    def unboxedNil[A]: List[A] = List.Nil

    def isEmpty[A](self: List[A]): Boolean =
        nullList(typeProxy[BuiltinList[A]](self))

    def head[A](self: List[A]): A =
        headList(typeProxy[BuiltinList[A]](self))

    def tail[A](self: List[A]): List[A] =
        typeProxy[List[A]](
          tailList(typeProxy[BuiltinList[A]](self))
        )

    def map[A, B](self: List[A], mapper: A => B): List[B] = {
        val blist = typeProxy[BuiltinList[A]](self)
        def go(lst: BuiltinList[A]): List[B] =
            if nullList(lst) then List.Nil
            else {
                val h = headList(lst)
                val t = tailList(lst)
                List.Cons(mapper(h), go(t))
            }
        go(blist)
    }

    def filter[A](self: List[A], predicate: A => Boolean): List[A] = {
        val blist = typeProxy[BuiltinList[A]](self)
        def go(lst: BuiltinList[A]): List[A] =
            if nullList(lst) then List.Nil
            else {
                val h = headList(lst)
                val t = tailList(lst)
                if predicate(h) then List.Cons(h, go(t))
                else go(t)
            }
        go(blist)
    }

    def foldLeft[A, B](self: List[A], init: B, combiner: (B, A) => B): B = {
        val blist = typeProxy[BuiltinList[A]](self)
        def go(lst: BuiltinList[A], acc: B): B =
            if nullList(lst) then acc
            else {
                val h = headList(lst)
                val t = tailList(lst)
                go(t, combiner(acc, h))
            }
        go(blist, init)
    }

    def foldRight[A, B](self: List[A], init: B, combiner: (A, B) => B): B = {
        val blist = typeProxy[BuiltinList[A]](self)
        def go(lst: BuiltinList[A]): B =
            if nullList(lst) then init
            else {
                val h = headList(lst)
                val t = tailList(lst)
                combiner(h, go(t))
            }
        go(blist)
    }

    def find[A](self: List[A], predicate: A => Boolean): Option[A] = {
        val blist = typeProxy[BuiltinList[A]](self)
        def go(lst: BuiltinList[A]): Option[A] =
            if nullList(lst) then Option.None
            else {
                val h = headList(lst)
                val t = tailList(lst)
                if predicate(h) then Option.Some(h)
                else go(t)
            }
        go(blist)
    }

}
