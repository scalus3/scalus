package scalus.compiler.intrinsics

import scalus.Compile
import scalus.cardano.onchain.plutus.prelude.{List, Option}
import scalus.compiler.UplcRepr
import scalus.compiler.UplcRepresentation
import scalus.compiler.UplcRepresentation.TypeVar
import scalus.compiler.UplcRepresentation.TypeVarKind.{Transparent, Unwrapped}

/** UplcConstr list operations — recursive implementations with local go functions.
  *
  * Every `List[_]` / `Option[_]` in the signatures carries a type-level
  * `@UplcRepr(UplcRepresentation.UplcConstr)` annotation (`List[A] @UplcRepr(UplcConstr)`). The
  * plugin's `SIRTyper.sirTypeInEnv` handles this `AnnotatedType` and wraps the SIR type with
  * `SIRType.Annotated(..., uplcRepr=UplcConstr)`. Unlike symbol-level annotations (which only wrap
  * the DefDef's declared return), type-level annotations land in the SIR type itself, so a local
  * `def go(lst: List[A] @UplcRepr(UplcConstr)): List[A] @UplcRepr(UplcConstr)` has a `rhs.tp` that
  * is fully annotated on both in/out — matching what `lowerLet:560` reads when computing `rhsRepr`.
  *
  * Type parameters remain `@UplcRepr(TypeVar(Unwrapped))`: element bytes flow through in `A`'s
  * stable default representation.
  */
@Compile
object UplcConstrListOperations {

    def map[
        @UplcRepr(TypeVar(Unwrapped)) A,
        @UplcRepr(TypeVar(Unwrapped)) B
    ](
        self: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
        mapper: A => B
    ): List[B] @UplcRepr(UplcRepresentation.UplcConstr) = {
        def go(
            lst: List[A] @UplcRepr(UplcRepresentation.UplcConstr)
        ): List[B] @UplcRepr(UplcRepresentation.UplcConstr) = lst match
            case List.Cons(h, t) => List.Cons(mapper(h), go(t))
            case List.Nil        => List.Nil
        go(self)
    }

    def filter[@UplcRepr(TypeVar(Unwrapped)) A](
        self: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
        predicate: A => Boolean
    ): List[A] @UplcRepr(UplcRepresentation.UplcConstr) = {
        def go(
            lst: List[A] @UplcRepr(UplcRepresentation.UplcConstr)
        ): List[A] @UplcRepr(UplcRepresentation.UplcConstr) = lst match
            case List.Cons(h, t) =>
                if predicate(h) then List.Cons(h, go(t))
                else go(t)
            case List.Nil => List.Nil
        go(self)
    }

    def foldLeft[
        @UplcRepr(TypeVar(Unwrapped)) A,
        @UplcRepr(TypeVar(Unwrapped)) B
    ](
        self: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
        init: B,
        combiner: (B, A) => B
    ): B = {
        def go(
            lst: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
            acc: B
        ): B = lst match
            case List.Cons(h, t) => go(t, combiner(acc, h))
            case List.Nil        => acc
        go(self, init)
    }

    def foldRight[
        @UplcRepr(TypeVar(Unwrapped)) A,
        @UplcRepr(TypeVar(Unwrapped)) B
    ](
        self: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
        init: B,
        combiner: (A, B) => B
    ): B = {
        def go(lst: List[A] @UplcRepr(UplcRepresentation.UplcConstr)): B = lst match
            case List.Cons(h, t) => combiner(h, go(t))
            case List.Nil        => init
        go(self)
    }

    def find[@UplcRepr(TypeVar(Unwrapped)) A](
        self: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
        predicate: A => Boolean
    ): Option[A] @UplcRepr(UplcRepresentation.UplcConstr) = {
        def go(
            lst: List[A] @UplcRepr(UplcRepresentation.UplcConstr)
        ): Option[A] @UplcRepr(UplcRepresentation.UplcConstr) = lst match
            case List.Cons(h, t) =>
                if predicate(h) then Option.Some(h) else go(t)
            case List.Nil => Option.None
        go(self)
    }

    def filterMap[
        @UplcRepr(TypeVar(Unwrapped)) A,
        @UplcRepr(TypeVar(Unwrapped)) B
    ](
        self: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
        predicate: A => Option[B] @UplcRepr(UplcRepresentation.UplcConstr)
    ): List[B] @UplcRepr(UplcRepresentation.UplcConstr) = {
        def go(
            lst: List[A] @UplcRepr(UplcRepresentation.UplcConstr)
        ): List[B] @UplcRepr(UplcRepresentation.UplcConstr) = lst match
            case List.Cons(h, t) =>
                predicate(h) match
                    case Option.None        => go(t)
                    case Option.Some(value) => List.Cons(value, go(t))
            case List.Nil => List.Nil
        go(self)
    }

    def quicksort[@UplcRepr(TypeVar(Unwrapped)) A](
        self: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
        ord: (A, A) => scalus.cardano.onchain.plutus.prelude.Order
    ): List[A] @UplcRepr(UplcRepresentation.UplcConstr) = {
        // Self-contained quicksort — no calls to other support bindings (append/prepended/
        // filter) or external prelude methods (`Order.isLess`). All external references
        // avoided so this can be lowered in isolation during eager support-binding init.
        //
        // Shape:
        //   - `partition` walks once, returns (before, after) via local `Partition` type.
        //   - `qsAux lst acc` computes `sorted(lst) ++ acc` — accumulator style eliminates
        //     the need for `append` at the combine step.
        //   - Pattern-match `ord(h, p)` on `Order.Less` directly (no `.isLess` call).
        def partition(
            lst: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
            pivot: A
        ): Partition[A] = lst match
            case List.Nil => Partition(List.Nil, List.Nil)
            case List.Cons(h, t) =>
                val rest = partition(t, pivot)
                ord(h, pivot) match
                    case scalus.cardano.onchain.plutus.prelude.Order.Less =>
                        Partition(List.Cons(h, rest.before), rest.after)
                    case _ =>
                        Partition(rest.before, List.Cons(h, rest.after))
        def qsAux(
            lst: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
            acc: List[A] @UplcRepr(UplcRepresentation.UplcConstr)
        ): List[A] @UplcRepr(UplcRepresentation.UplcConstr) = lst match
            case List.Nil => acc
            case List.Cons(pivot, rest) =>
                val parts = partition(rest, pivot)
                qsAux(parts.before, List.Cons(pivot, qsAux(parts.after, acc)))
        qsAux(self, List.Nil)
    }

    /** Local pair type for `quicksort`'s one-pass partition result. Annotated
      * `@UplcRepr(UplcConstr)` so construction uses native-Constr emission — avoids Data-encoding
      * the `List[A]` fields for abstract element type `A`.
      */
    @UplcRepr(UplcRepresentation.UplcConstr)
    case class Partition[@UplcRepr(TypeVar(Unwrapped)) A_Partition](
        before: List[A_Partition] @UplcRepr(UplcRepresentation.UplcConstr),
        after: List[A_Partition] @UplcRepr(UplcRepresentation.UplcConstr)
    )

    def contains[@UplcRepr(TypeVar(Unwrapped)) A](
        self: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
        elem: A,
        eq: (A, A) => Boolean
    ): Boolean = {
        def go(lst: List[A] @UplcRepr(UplcRepresentation.UplcConstr)): Boolean = lst match
            case List.Cons(h, t) =>
                if eq(h, elem) then true
                else go(t)
            case List.Nil => false
        go(self)
    }

    def length[@UplcRepr(TypeVar(Unwrapped)) A](
        self: List[A] @UplcRepr(UplcRepresentation.UplcConstr)
    ): BigInt = {
        def go(
            lst: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
            acc: BigInt
        ): BigInt = lst match
            case List.Cons(_, t) => go(t, acc + BigInt(1))
            case List.Nil        => acc
        go(self, BigInt(0))
    }

    def reverse[@UplcRepr(TypeVar(Transparent)) A](
        self: List[A] @UplcRepr(UplcRepresentation.UplcConstr)
    ): List[A] @UplcRepr(UplcRepresentation.UplcConstr) = {
        def go(
            lst: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
            acc: List[A] @UplcRepr(UplcRepresentation.UplcConstr)
        ): List[A] @UplcRepr(UplcRepresentation.UplcConstr) = lst match
            case List.Cons(h, t) => go(t, List.Cons(h, acc))
            case List.Nil        => acc
        go(self, List.Nil)
    }

    def append[@UplcRepr(TypeVar(Unwrapped)) A](
        self: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
        other: List[A] @UplcRepr(UplcRepresentation.UplcConstr)
    ): List[A] @UplcRepr(UplcRepresentation.UplcConstr) = {
        def go(
            lst: List[A] @UplcRepr(UplcRepresentation.UplcConstr)
        ): List[A] @UplcRepr(UplcRepresentation.UplcConstr) = lst match
            case List.Cons(h, t) => List.Cons(h, go(t))
            case List.Nil        => other
        go(self)
    }

    def drop[@UplcRepr(TypeVar(Unwrapped)) A](
        self: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
        n: BigInt
    ): List[A] @UplcRepr(UplcRepresentation.UplcConstr) = {
        def go(
            lst: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
            remaining: BigInt
        ): List[A] @UplcRepr(UplcRepresentation.UplcConstr) =
            if remaining <= BigInt(0) then lst
            else
                lst match
                    case List.Cons(_, t) => go(t, remaining - BigInt(1))
                    case List.Nil        => List.Nil
        go(self, n)
    }

    def prepended[@UplcRepr(TypeVar(Unwrapped)) A](
        self: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
        elem: A
    ): List[A] @UplcRepr(UplcRepresentation.UplcConstr) = List.Cons(elem, self)

    def dropRight[@UplcRepr(TypeVar(Unwrapped)) A](
        self: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
        n: BigInt
    ): List[A] @UplcRepr(UplcRepresentation.UplcConstr) = {
        if n <= BigInt(0) then self
        else
            val len = length(self)
            val take = len - n
            def go(
                lst: List[A] @UplcRepr(UplcRepresentation.UplcConstr),
                remaining: BigInt
            ): List[A] @UplcRepr(UplcRepresentation.UplcConstr) =
                if remaining <= BigInt(0) then List.Nil
                else
                    lst match
                        case List.Cons(h, t) => List.Cons(h, go(t, remaining - BigInt(1)))
                        case List.Nil        => List.Nil
            go(self, take)
    }

    def init[@UplcRepr(TypeVar(Unwrapped)) A](
        self: List[A] @UplcRepr(UplcRepresentation.UplcConstr)
    ): List[A] @UplcRepr(UplcRepresentation.UplcConstr) =
        dropRight(self, BigInt(1))

}
