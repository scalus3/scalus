package scalus.compiler.intrinsics

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{List, Option}
import scalus.compiler.UplcRepr
import scalus.compiler.UplcRepresentation.TypeVar
import scalus.compiler.UplcRepresentation.TypeVarKind.Unwrapped
import scalus.compiler.intrinsics.IntrinsicHelpers.*
import scalus.uplc.builtin.BuiltinList
import scalus.uplc.builtin.Builtins.*

/** Native list operations — implementations with Unwrapped TypeVars.
  *
  * Each generic type parameter carries `@UplcRepr(TypeVar(Unwrapped))` so element bytes flow
  * through in their stable default representation. `find` is intentionally NOT annotated — its
  * `Option.Some` / `Option.None` if-then-else body exposes a lowering issue (Option.None doesn't
  * propagate the target's annotated type-args when constructed under a `@UplcRepr(UplcConstr)`
  * parent). Without the annotation, the Fixed-default lowering still works (this matched the
  * historical behaviour, since the legacy `IntrinsicResolver.stampTransparent` call for this
  * support module was a no-op due to a string-mismatch typo).
  *
  * This is a support module — bindings are resolved on demand when referenced from intrinsic
  * bodies. IntrinsicsNativeList delegates to these methods.
  */
@Compile
object NativeListOperations {

    def map[
        @UplcRepr(TypeVar(Unwrapped)) A,
        @UplcRepr(TypeVar(Unwrapped)) B
    ](self: List[A], mapper: A => B): List[B] = {
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

    def filter[@UplcRepr(TypeVar(Unwrapped)) A](
        self: List[A],
        predicate: A => Boolean
    ): List[A] = {
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

    def foldLeft[
        @UplcRepr(TypeVar(Unwrapped)) A,
        @UplcRepr(TypeVar(Unwrapped)) B
    ](self: List[A], init: B, combiner: (B, A) => B): B = {
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

    def foldRight[
        @UplcRepr(TypeVar(Unwrapped)) A,
        @UplcRepr(TypeVar(Unwrapped)) B
    ](self: List[A], init: B, combiner: (A, B) => B): B = {
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

    def contains[@UplcRepr(TypeVar(Unwrapped)) A](
        self: List[A],
        elem: A,
        eq: (A, A) => Boolean
    ): Boolean = {
        val blist = typeProxy[BuiltinList[A]](self)
        def go(lst: BuiltinList[A]): Boolean =
            if nullList(lst) then false
            else {
                val h = headList(lst)
                if eq(h, elem) then true
                else go(tailList(lst))
            }
        go(blist)
    }

}
