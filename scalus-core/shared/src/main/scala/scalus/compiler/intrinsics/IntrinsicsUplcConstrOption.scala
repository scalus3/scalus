package scalus.compiler.intrinsics

import scalus.Compile
import scalus.cardano.onchain.plutus.prelude.{fail, Option}
import scalus.compiler.intrinsics.IntrinsicHelpers.fromDefaultTypeVarRepr
import scalus.compiler.UplcRepr
import scalus.compiler.UplcRepresentation
import scalus.compiler.UplcRepresentation.TypeVar
import scalus.compiler.UplcRepresentation.TypeVarKind.Transparent

/** Native-Constr Option intrinsics — thin delegation to `UplcConstrOptionOperations`.
  *
  * The `IntrinsicResolver` dispatches to these when `Option.method(...)` is called on an `Option`
  * whose lowered representation is `SumUplcConstr`. Type parameters are annotated
  * `@UplcRepr(TypeVar(Transparent))`: the caller's concrete representation substitutes at the
  * inline site. Every `Option[_]` container in the signatures is annotated `@UplcRepr(UplcConstr)`
  * — under the per-signature annotation regime (no `uplcGeneratorPolicy` swap), this is how the
  * dispatcher's inline `self match { ... }` lands on the UplcConstr code path and how its result
  * stays in UplcConstr form.
  *
  * Simple operations (isDefined, isEmpty, get) are implemented inline since they're single pattern
  * matches. Higher-order operations (map, flatMap, filter, exists, forall) delegate to
  * `UplcConstrOptionOperations`.
  */
@Compile
object IntrinsicsUplcConstrOption {

    def isDefined[@UplcRepr(TypeVar(Transparent)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A]
    ): Boolean = self match
        case Option.Some(_) => true
        case Option.None    => false

    def isEmpty[@UplcRepr(TypeVar(Transparent)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A]
    ): Boolean = self match
        case Option.Some(_) => false
        case Option.None    => true

    def get[@UplcRepr(TypeVar(Transparent)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A]
    ): A = self match
        case Option.Some(value) => value
        case Option.None        => fail("None.get")

    def getOrElse[@UplcRepr(TypeVar(Transparent)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A],
        default: A
    ): A = self match
        case Option.Some(value) => value
        case Option.None        => default

    @UplcRepr(UplcRepresentation.UplcConstr)
    def map[
        @UplcRepr(TypeVar(Transparent)) A,
        @UplcRepr(TypeVar(Transparent)) B
    ](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A],
        mapper: A => B
    ): Option[B] =
        UplcConstrOptionOperations.map(self, (item: A) => mapper(fromDefaultTypeVarRepr(item)))

    @UplcRepr(UplcRepresentation.UplcConstr)
    def flatMap[
        @UplcRepr(TypeVar(Transparent)) A,
        @UplcRepr(TypeVar(Transparent)) B
    ](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A],
        mapper: A => Option[B]
    ): Option[B] =
        UplcConstrOptionOperations.flatMap(
          self,
          (item: A) => mapper(fromDefaultTypeVarRepr(item))
        )

    @UplcRepr(UplcRepresentation.UplcConstr)
    def filter[@UplcRepr(TypeVar(Transparent)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A],
        predicate: A => Boolean
    ): Option[A] =
        UplcConstrOptionOperations.filter(
          self,
          (item: A) => predicate(fromDefaultTypeVarRepr(item))
        )

    def exists[@UplcRepr(TypeVar(Transparent)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A],
        predicate: A => Boolean
    ): Boolean =
        UplcConstrOptionOperations.exists(
          self,
          (item: A) => predicate(fromDefaultTypeVarRepr(item))
        )

    def forall[@UplcRepr(TypeVar(Transparent)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A],
        predicate: A => Boolean
    ): Boolean =
        UplcConstrOptionOperations.forall(
          self,
          (item: A) => predicate(fromDefaultTypeVarRepr(item))
        )
}
