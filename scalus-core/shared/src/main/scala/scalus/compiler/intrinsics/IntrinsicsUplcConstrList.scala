package scalus.compiler.intrinsics

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{fail, List, Option}
import scalus.compiler.intrinsics.IntrinsicHelpers.{equalsRepr, fromDefaultTypeVarRepr, toDefaultTypeVarRepr}
import scalus.compiler.UplcRepr
import scalus.compiler.UplcRepresentation
import scalus.compiler.UplcRepresentation.TypeVar
import scalus.compiler.UplcRepresentation.TypeVarKind.Unwrapped

/** UplcConstr list intrinsics — thin delegation to UplcConstrListOperations.
  *
  * The IntrinsicResolver dispatches to these when the list has SumUplcConstr representation. Type
  * parameters are annotated `@UplcRepr(TypeVar(Unwrapped))`: the inliner substitutes the caller's
  * concrete representation at the inlining site. Every `List[_]` / `Option[_]` in the signatures is
  * annotated `@UplcRepr(UplcConstr)` — under the per-signature annotation regime (no
  * `uplcGeneratorPolicy` swap), this is how the dispatcher's inline `self match { ... }` match
  * lands on the UplcConstr code path and how its result stays in UplcConstr form. Simple methods
  * (isEmpty, head, tail) are implemented inline since they're single pattern matches. Complex
  * recursive methods delegate to `UplcConstrListOperations` (support module whose type params are
  * `Unwrapped`).
  *
  * For contains: Eq has known semantics (structural equality), so we use equalsData directly
  * instead of calling the Eq function. Elements are converted to Data via toDefaultTypeVarRepr,
  * then compared with a single equalsData builtin call.
  */
@Compile
object IntrinsicsUplcConstrList {

    def isEmpty[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A]
    ): Boolean = self match
        case List.Cons(_, _) => false
        case List.Nil        => true

    def head[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A]
    ): A = self match
        case List.Cons(h, _) => h
        case List.Nil        => fail()

    @UplcRepr(UplcRepresentation.UplcConstr)
    def tail[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A]
    ): List[A] = self match
        case List.Cons(_, t) => t
        case List.Nil        => fail()

    @UplcRepr(UplcRepresentation.UplcConstr)
    def map[
        @UplcRepr(TypeVar(Unwrapped)) A,
        @UplcRepr(TypeVar(Unwrapped)) B
    ](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A],
        mapper: A => B
    ): List[B] =
        UplcConstrListOperations.map(self, (item: A) => mapper(fromDefaultTypeVarRepr(item)))

    @UplcRepr(UplcRepresentation.UplcConstr)
    def filter[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A],
        predicate: A => Boolean
    ): List[A] =
        UplcConstrListOperations.filter(
          self,
          (item: A) => predicate(fromDefaultTypeVarRepr(item))
        )

    def foldLeft[
        @UplcRepr(TypeVar(Unwrapped)) A,
        @UplcRepr(TypeVar(Unwrapped)) B
    ](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A],
        init: B,
        combiner: (B, A) => B
    ): B =
        UplcConstrListOperations.foldLeft(
          self,
          init,
          (acc: B, item: A) => combiner(acc, fromDefaultTypeVarRepr(item))
        )

    def foldRight[
        @UplcRepr(TypeVar(Unwrapped)) A,
        @UplcRepr(TypeVar(Unwrapped)) B
    ](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A],
        init: B,
        combiner: (A, B) => B
    ): B =
        UplcConstrListOperations.foldRight(
          self,
          init,
          (item: A, acc: B) => combiner(fromDefaultTypeVarRepr(item), acc)
        )

    @UplcRepr(UplcRepresentation.UplcConstr)
    def find[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A],
        predicate: A => Boolean
    ): Option[A] =
        UplcConstrListOperations.find(
          self,
          (item: A) => predicate(fromDefaultTypeVarRepr(item))
        )

    @UplcRepr(UplcRepresentation.UplcConstr)
    def filterMap[
        @UplcRepr(TypeVar(Unwrapped)) A,
        @UplcRepr(TypeVar(Unwrapped)) B
    ](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A],
        predicate: A => Option[B]
    ): List[B] =
        UplcConstrListOperations.filterMap(
          self,
          (item: A) => predicate(fromDefaultTypeVarRepr(item))
        )

    @UplcRepr(UplcRepresentation.UplcConstr)
    def sort[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A],
        ord: (A, A) => scalus.cardano.onchain.plutus.prelude.Order
    ): List[A] =
        // The user-provided `ord` compiles with `Fixed`-kind TypeVar args (Data-encoded),
        // but the list elements here are native Constr. Convert to the default TypeVar repr
        // (Data) before calling ord — matches the pattern in `contains` for `eq`. Without this,
        // the `Fixed` in ord's signature leaks into downstream representation inference and
        // triggers a Data/native mismatch at runtime.
        UplcConstrListOperations.sort(
          self,
          (a: A, b: A) => ord(toDefaultTypeVarRepr(a), toDefaultTypeVarRepr(b))
        )

    def contains[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A],
        elem: A,
        eq: (A, A) => Boolean
    ): Boolean =
        UplcConstrListOperations.contains(
          self,
          elem,
          (a: A, b: A) => equalsRepr(a, b)
        )

    def length[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A]
    ): BigInt =
        UplcConstrListOperations.length(self)

    @UplcRepr(UplcRepresentation.UplcConstr)
    def reverse[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A]
    ): List[A] =
        UplcConstrListOperations.reverse(self)

    @UplcRepr(UplcRepresentation.UplcConstr)
    def append[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A],
        @UplcRepr(UplcRepresentation.UplcConstr) other: List[A]
    ): List[A] =
        UplcConstrListOperations.append(self, other)

    @UplcRepr(UplcRepresentation.UplcConstr)
    def appendedAll[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A],
        @UplcRepr(UplcRepresentation.UplcConstr) other: List[A]
    ): List[A] =
        UplcConstrListOperations.append(self, other)

    @UplcRepr(UplcRepresentation.UplcConstr)
    def drop[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A],
        n: BigInt
    ): List[A] =
        UplcConstrListOperations.drop(self, n)

    @UplcRepr(UplcRepresentation.UplcConstr)
    def prepended[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A],
        elem: A
    ): List[A] =
        UplcConstrListOperations.prepended(self, elem)

    @UplcRepr(UplcRepresentation.UplcConstr)
    def dropRight[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A],
        n: BigInt
    ): List[A] =
        UplcConstrListOperations.dropRight(self, n)

    @UplcRepr(UplcRepresentation.UplcConstr)
    def init[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: List[A]
    ): List[A] =
        UplcConstrListOperations.init(self)

}
