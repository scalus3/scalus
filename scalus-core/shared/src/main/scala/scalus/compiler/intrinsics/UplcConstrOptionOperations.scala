package scalus.compiler.intrinsics

import scalus.Compile
import scalus.cardano.onchain.plutus.prelude.{fail, Option}
import scalus.compiler.UplcRepr
import scalus.compiler.UplcRepresentation.TypeVar
import scalus.compiler.UplcRepresentation.TypeVarKind.Unwrapped

/** Native-Constr Option operations — support-module bindings whose type parameters are annotated
  * `@UplcRepr(TypeVar(Unwrapped))`.
  *
  * These are the Option counterpart of `UplcConstrListOperations`. The methods pattern-match on
  * `Option[A]` but are resolved by the `IntrinsicResolver` only when the scrutinee is a
  * `SumUplcConstr` Option (registered under `UplcConstrOptionRepr`) — that's where the pattern
  * match goes through `genMatchUplcConstr` and native-Constr field extraction, preserving `A`'s
  * Unwrapped repr across the match boundary.
  *
  * IMPORTANT: these methods assume their `Option` argument is already in `SumUplcConstr` form at
  * the call site. A `DataConstr` Option matched here would Fixed-label the extracted field — that
  * is the classic `Unwrapped→Fixed` leak. Callers (the dispatcher in `IntrinsicsUplcConstrOption`)
  * must ensure the Option they pass is in the native-Constr shape before delegating.
  */
@Compile
object UplcConstrOptionOperations {

    def isDefined[@UplcRepr(TypeVar(Unwrapped)) A](self: Option[A]): Boolean = self match
        case Option.Some(_) => true
        case Option.None    => false

    def isEmpty[@UplcRepr(TypeVar(Unwrapped)) A](self: Option[A]): Boolean = self match
        case Option.Some(_) => false
        case Option.None    => true

    def get[@UplcRepr(TypeVar(Unwrapped)) A](self: Option[A]): A = self match
        case Option.Some(value) => value
        case Option.None        => fail("None.get")

    def getOrElse[@UplcRepr(TypeVar(Unwrapped)) A](self: Option[A], default: A): A = self match
        case Option.Some(value) => value
        case Option.None        => default

    def map[
        @UplcRepr(TypeVar(Unwrapped)) A,
        @UplcRepr(TypeVar(Unwrapped)) B
    ](self: Option[A], mapper: A => B): Option[B] = self match
        case Option.Some(value) => Option.Some(mapper(value))
        case Option.None        => Option.None

    def flatMap[
        @UplcRepr(TypeVar(Unwrapped)) A,
        @UplcRepr(TypeVar(Unwrapped)) B
    ](self: Option[A], mapper: A => Option[B]): Option[B] = self match
        case Option.Some(value) => mapper(value)
        case Option.None        => Option.None

    def filter[@UplcRepr(TypeVar(Unwrapped)) A](
        self: Option[A],
        predicate: A => Boolean
    ): Option[A] = self match
        case Option.Some(value) => if predicate(value) then self else Option.None
        case Option.None        => Option.None

    def exists[@UplcRepr(TypeVar(Unwrapped)) A](
        self: Option[A],
        predicate: A => Boolean
    ): Boolean = self match
        case Option.Some(value) => predicate(value)
        case Option.None        => false

    def forall[@UplcRepr(TypeVar(Unwrapped)) A](
        self: Option[A],
        predicate: A => Boolean
    ): Boolean = self match
        case Option.Some(value) => predicate(value)
        case Option.None        => true
}
