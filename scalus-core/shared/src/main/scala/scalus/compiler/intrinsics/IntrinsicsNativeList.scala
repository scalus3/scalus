package scalus.compiler.intrinsics

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{List, Option}
import scalus.compiler.UplcRepr
import scalus.compiler.UplcRepresentation.TypeVar
import scalus.compiler.UplcRepresentation.TypeVarKind.Transparent
import scalus.compiler.intrinsics.IntrinsicHelpers.*
import scalus.uplc.builtin.BuiltinList
import scalus.uplc.builtin.Builtins.*

/** Native list intrinsics — thin delegation to NativeListOperations.
  *
  * The IntrinsicResolver dispatches to these when the list has native element representation
  * (SumBuiltinList with !isPackedData element repr). Each method delegates to NativeListOperations
  * which has the actual implementations.
  *
  * Type parameters carry `@UplcRepr(TypeVar(Transparent))` so element bytes flow through without
  * Data wrapping (matching the historical `stampTransparent` post-load semantics this annotation
  * replaces). `find` is unannotated — see NativeListOperations.find for the rationale.
  *
  * Simple methods (isEmpty, head, tail) are implemented inline since they're single builtins.
  */
@Compile
object IntrinsicsNativeList {

    def isEmpty[@UplcRepr(TypeVar(Transparent)) A](self: List[A]): Boolean =
        nullList(typeProxy[BuiltinList[A]](self))

    def head[@UplcRepr(TypeVar(Transparent)) A](self: List[A]): A =
        headList(typeProxy[BuiltinList[A]](self))

    def tail[@UplcRepr(TypeVar(Transparent)) A](self: List[A]): List[A] =
        typeProxy[List[A]](
          tailList(typeProxy[BuiltinList[A]](self))
        )

    def map[
        @UplcRepr(TypeVar(Transparent)) A,
        @UplcRepr(TypeVar(Transparent)) B
    ](self: List[A], mapper: A => B): List[B] =
        NativeListOperations.map(self, mapper)

    def filter[@UplcRepr(TypeVar(Transparent)) A](
        self: List[A],
        predicate: A => Boolean
    ): List[A] =
        NativeListOperations.filter(self, predicate)

    def foldLeft[
        @UplcRepr(TypeVar(Transparent)) A,
        @UplcRepr(TypeVar(Transparent)) B
    ](self: List[A], init: B, combiner: (B, A) => B): B =
        NativeListOperations.foldLeft(self, init, combiner)

    def foldRight[
        @UplcRepr(TypeVar(Transparent)) A,
        @UplcRepr(TypeVar(Transparent)) B
    ](self: List[A], init: B, combiner: (A, B) => B): B =
        NativeListOperations.foldRight(self, init, combiner)

    def find[A](self: List[A], predicate: A => Boolean): Option[A] =
        NativeListOperations.find(self, predicate)

    // `eq` parameter dropped (the resolver strips the caller's `Eq` argument). The dispatcher is
    // re-lowered per concrete element type, so `equalsRepr` resolves here; structural equality is
    // supplied to the (abstract, support-module) leaf.
    def contains[@UplcRepr(TypeVar(Transparent)) A](
        self: List[A],
        elem: A
    ): Boolean =
        NativeListOperations.contains(self, elem, (a: A, b: A) => equalsRepr(a, b))

    // `eq` dropped (resolver strips it); structural `equalsRepr` supplied to the support leaf.
    def indexOf[@UplcRepr(TypeVar(Transparent)) A](
        self: List[A],
        elem: A
    ): BigInt =
        NativeListOperations.indexOf(self, elem, (a: A, b: A) => equalsRepr(a, b))

    def deleteFirst[@UplcRepr(TypeVar(Transparent)) A](
        self: List[A],
        elem: A
    ): List[A] =
        NativeListOperations.deleteFirst(self, elem, (a: A, b: A) => equalsRepr(a, b))

    def distinct[@UplcRepr(TypeVar(Transparent)) A](self: List[A]): List[A] =
        NativeListOperations.distinct(self, (a: A, b: A) => equalsRepr(a, b))

    def diff[@UplcRepr(TypeVar(Transparent)) A](
        self: List[A],
        other: List[A]
    ): List[A] =
        NativeListOperations.diff(self, other, (a: A, b: A) => equalsRepr(a, b))

}
