package scalus.compiler.intrinsics

import scalus.Compile
import scalus.cardano.onchain.plutus.prelude.{fail, List, Option}
import scalus.compiler.intrinsics.IntrinsicHelpers.{equalsRepr, fromDefaultTypeVarRepr, toDefaultTypeVarRepr}
import scalus.compiler.UplcRepr
import scalus.compiler.UplcRepresentation.TypeVar
import scalus.compiler.UplcRepresentation.TypeVarKind.Transparent

/** UplcConstr list intrinsics — thin delegation to UplcConstrListOperations.
  *
  * The IntrinsicResolver dispatches to these when the list has SumUplcConstr representation. Type
  * parameters are annotated `@UplcRepr(TypeVar(Transparent))`: the inliner substitutes the caller's
  * concrete representation at the inlining site. Simple methods (isEmpty, head, tail) are
  * implemented inline since they're single pattern matches. Complex recursive methods delegate to
  * `UplcConstrListOperations` (a non-inlined support module whose type params are `Unwrapped`).
  *
  * For contains: Eq has known semantics (structural equality), so we use equalsData directly
  * instead of calling the Eq function. Elements are converted to Data via toDefaultTypeVarRepr,
  * then compared with a single equalsData builtin call.
  */
@Compile
object IntrinsicsUplcConstrList {

    def isEmpty[@UplcRepr(TypeVar(Transparent)) A](self: List[A]): Boolean = self match
        case List.Cons(_, _) => false
        case List.Nil        => true

    def head[@UplcRepr(TypeVar(Transparent)) A](self: List[A]): A = self match
        case List.Cons(h, _) => h
        case List.Nil        => fail()

    def tail[@UplcRepr(TypeVar(Transparent)) A](self: List[A]): List[A] = self match
        case List.Cons(_, t) => t
        case List.Nil        => fail()

    def map[
        @UplcRepr(TypeVar(Transparent)) A,
        @UplcRepr(TypeVar(Transparent)) B
    ](self: List[A], mapper: A => B): List[B] =
        UplcConstrListOperations.map(self, (item: A) => mapper(fromDefaultTypeVarRepr(item)))

    def filter[@UplcRepr(TypeVar(Transparent)) A](
        self: List[A],
        predicate: A => Boolean
    ): List[A] =
        UplcConstrListOperations.filter(
          self,
          (item: A) => predicate(fromDefaultTypeVarRepr(item))
        )

    def foldLeft[
        @UplcRepr(TypeVar(Transparent)) A,
        @UplcRepr(TypeVar(Transparent)) B
    ](self: List[A], init: B, combiner: (B, A) => B): B =
        UplcConstrListOperations.foldLeft(
          self,
          init,
          (acc: B, item: A) => combiner(acc, fromDefaultTypeVarRepr(item))
        )

    def foldRight[
        @UplcRepr(TypeVar(Transparent)) A,
        @UplcRepr(TypeVar(Transparent)) B
    ](self: List[A], init: B, combiner: (A, B) => B): B =
        UplcConstrListOperations.foldRight(
          self,
          init,
          (item: A, acc: B) => combiner(fromDefaultTypeVarRepr(item), acc)
        )

    def find[@UplcRepr(TypeVar(Transparent)) A](
        self: List[A],
        predicate: A => Boolean
    ): Option[A] =
        UplcConstrListOperations.find(
          self,
          (item: A) => predicate(fromDefaultTypeVarRepr(item))
        )

    def filterMap[
        @UplcRepr(TypeVar(Transparent)) A,
        @UplcRepr(TypeVar(Transparent)) B
    ](self: List[A], predicate: A => Option[B]): List[B] =
        UplcConstrListOperations.filterMap(
          self,
          (item: A) => predicate(fromDefaultTypeVarRepr(item))
        )

    def quicksort[@UplcRepr(TypeVar(Transparent)) A](
        self: List[A],
        ord: (A, A) => scalus.cardano.onchain.plutus.prelude.Order
    ): List[A] =
        // The user-provided `ord` compiles with `Fixed`-kind TypeVar args (Data-encoded),
        // but the list elements here are native Constr. Convert to the default TypeVar repr
        // (Data) before calling ord — matches the pattern in `contains` for `eq`. Without this,
        // the `Fixed` in ord's signature leaks into downstream representation inference and
        // triggers a Data/native mismatch at runtime.
        UplcConstrListOperations.quicksort(
          self,
          (a: A, b: A) => ord(toDefaultTypeVarRepr(a), toDefaultTypeVarRepr(b))
        )

    def contains[@UplcRepr(TypeVar(Transparent)) A](
        self: List[A],
        elem: A,
        eq: (A, A) => Boolean
    ): Boolean =
        UplcConstrListOperations.contains(
          self,
          elem,
          (a: A, b: A) => equalsRepr(a, b)
        )

    def length[@UplcRepr(TypeVar(Transparent)) A](self: List[A]): BigInt =
        UplcConstrListOperations.length(self)

    def reverse[@UplcRepr(TypeVar(Transparent)) A](self: List[A]): List[A] =
        UplcConstrListOperations.reverse(self)

    def append[@UplcRepr(TypeVar(Transparent)) A](
        self: List[A],
        other: List[A]
    ): List[A] =
        UplcConstrListOperations.append(self, other)

    def appendedAll[@UplcRepr(TypeVar(Transparent)) A](
        self: List[A],
        other: List[A]
    ): List[A] =
        UplcConstrListOperations.append(self, other)

    def drop[@UplcRepr(TypeVar(Transparent)) A](self: List[A], n: BigInt): List[A] =
        UplcConstrListOperations.drop(self, n)

    def prepended[@UplcRepr(TypeVar(Transparent)) A](
        self: List[A],
        elem: A
    ): List[A] =
        UplcConstrListOperations.prepended(self, elem)

    def dropRight[@UplcRepr(TypeVar(Transparent)) A](
        self: List[A],
        n: BigInt
    ): List[A] =
        UplcConstrListOperations.dropRight(self, n)

    def init[@UplcRepr(TypeVar(Transparent)) A](self: List[A]): List[A] =
        UplcConstrListOperations.init(self)

}
