package scalus.compiler.intrinsics

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{List, Option}
import scalus.compiler.intrinsics.IntrinsicHelpers.*
import scalus.uplc.builtin.BuiltinList
import scalus.uplc.builtin.Builtins.*
import scalus.compiler.intrinsics.IntrinsicHelpers.toDefaultTypeVarRepr

/** Native list intrinsics — thin delegation to NativeListOperations.
  *
  * The IntrinsicResolver dispatches to these when the list has native element representation
  * (SumBuiltinList with !isPackedData element repr). Each method delegates to NativeListOperations
  * which has Transparent TypeVars and the actual implementations.
  *
  * Simple methods (isEmpty, head, tail) are implemented inline since they're single builtins.
  */
@Compile
object IntrinsicsNativeList {

    def isEmpty[A](self: List[A]): Boolean =
        nullList(typeProxy[BuiltinList[A]](self))

    def head[A](self: List[A]): A =
        headList(typeProxy[BuiltinList[A]](self))

    def tail[A](self: List[A]): List[A] =
        typeProxy[List[A]](
          tailList(typeProxy[BuiltinList[A]](self))
        )

    def map[A, B](self: List[A], mapper: A => B): List[B] =
        NativeListOperations.map(self, mapper)

    def filter[A](self: List[A], predicate: A => Boolean): List[A] =
        NativeListOperations.filter(self, predicate)

    def foldLeft[A, B](self: List[A], init: B, combiner: (B, A) => B): B =
        NativeListOperations.foldLeft(self, init, combiner)

    def foldRight[A, B](self: List[A], init: B, combiner: (A, B) => B): B =
        NativeListOperations.foldRight(self, init, combiner)

    def find[A](self: List[A], predicate: A => Boolean): Option[A] =
        NativeListOperations.find(self, predicate)

    def contains[A](self: List[A], elem: A, eq: (A, A) => Boolean): Boolean =
        NativeListOperations.contains(
          self,
          elem,
          (a: A, b: A) => eq(toDefaultTypeVarRepr(a), toDefaultTypeVarRepr(b))
        )

}
