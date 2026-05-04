package scalus.compiler.intrinsics

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{fail, Option}
import scalus.compiler.UplcRepr
import scalus.compiler.UplcRepresentation
import scalus.compiler.UplcRepresentation.TypeVar
import scalus.compiler.UplcRepresentation.TypeVarKind.Unwrapped

/** Native-Constr Option operations — support-module bindings.
  *
  * Every `Option[_]` in the signatures is explicitly annotated `@UplcRepr(UplcConstr)` so the
  * support-op body operates on native-Constr Options regardless of the caller's policy. Type
  * parameters are `@UplcRepr(TypeVar(Unwrapped))` — element bytes are in `A`'s stable default
  * representation, unaffected by any policy switch (there isn't one anymore).
  *
  * Callers (the dispatcher in `IntrinsicsUplcConstrOption`) receive the user's Option in whatever
  * shape and rely on the standard representation-conversion machinery to pass a SumUplcConstr value
  * here. The match in each body is consequently always a `genMatchUplcConstr` destructure.
  */
@Compile
object UplcConstrOptionOperations {

    def isDefined[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A]
    ): Boolean = self match
        case Option.Some(_) => true
        case Option.None    => false

    def isEmpty[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A]
    ): Boolean = self match
        case Option.Some(_) => false
        case Option.None    => true

    def get[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A]
    ): A = self match
        case Option.Some(value) => value
        case Option.None        => fail("None.get")

    def getOrElse[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A],
        default: A
    ): A = self match
        case Option.Some(value) => value
        case Option.None        => default

    @UplcRepr(UplcRepresentation.UplcConstr)
    def map[
        @UplcRepr(TypeVar(Unwrapped)) A,
        @UplcRepr(TypeVar(Unwrapped)) B
    ](
        self: Option[A] @UplcRepr(UplcRepresentation.UplcConstr),
        mapper: A => B
    ): Option[B] = self match
        case Option.Some(value) => Option.Some(mapper(value))
        case Option.None        => Option.None

    @UplcRepr(UplcRepresentation.UplcConstr)
    def flatMap[
        @UplcRepr(TypeVar(Unwrapped)) A,
        @UplcRepr(TypeVar(Unwrapped)) B
    ](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A],
        mapper: A => Option[B] @UplcRepr(UplcRepresentation.UplcConstr)
    ): Option[B] = self match
        case Option.Some(value) => mapper(value)
        case Option.None        => Option.None

    @UplcRepr(UplcRepresentation.UplcConstr)
    def filter[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A],
        predicate: A => Boolean
    ): Option[A] = self match
        case Option.Some(value) => if predicate(value) then self else Option.None
        case Option.None        => Option.None

    def exists[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A],
        predicate: A => Boolean
    ): Boolean = self match
        case Option.Some(value) => predicate(value)
        case Option.None        => false

    def forall[@UplcRepr(TypeVar(Unwrapped)) A](
        @UplcRepr(UplcRepresentation.UplcConstr) self: Option[A],
        predicate: A => Boolean
    ): Boolean = self match
        case Option.Some(value) => predicate(value)
        case Option.None        => true
}
