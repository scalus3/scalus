package scalus.compiler.intrinsics

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.List
import scalus.compiler.intrinsics.IntrinsicHelpers.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Builtins.*

/** Support module (lowered once) for the BuiltinList / Data representation of `List`.
  *
  * Every element here is in `Data` representation, so equality is just `equalsData` — no per-type
  * code and no `eq` function to thread through. (Contrast `NativeListOperations`, whose elements
  * are in Constant/native repr: there the dispatcher must pass `equalsRepr`, which produces
  * different code per concrete type and so can't be baked into a once-lowered support.)
  *
  * The intrinsic dispatchers (`BuiltinListOperations` / `...V11`) just forward here. See
  * `docs/local/claude/compiler/v3-eq-eliminating.md`.
  *
  * List-returning ops build the result as `List[Data]` (each element a `Data` value) and return it
  * `typeProxy`'d to `List[A]`. This support is lowered ONCE with `A` abstract, so it cannot relabel
  * the result to `A`'s concrete element repr (the element type is an unresolved TypeVar here). The
  * **dispatcher** (`BuiltinListOperations`, re-lowered per concrete element type) does that relabel
  * via `typeProxyRepr` — see there.
  */
@Compile
object BuiltinListSupport {

    def contains[A](self: List[A], elem: A): Boolean = {
        val elemData = typeProxy[Data](elem)
        def go(lst: List[Data]): Boolean = lst match
            case List.Cons(h, t) =>
                if equalsData(h, elemData) then true
                else go(t)
            case List.Nil => false
        go(typeProxy[List[Data]](self))
    }

    def indexOf[A](self: List[A], elem: A): BigInt = {
        val elemData = typeProxy[Data](elem)
        def go(lst: List[Data], idx: BigInt): BigInt = lst match
            case List.Cons(h, t) =>
                if equalsData(h, elemData) then idx
                else go(t, idx + BigInt(1))
            case List.Nil => BigInt(-1)
        go(typeProxy[List[Data]](self), BigInt(0))
    }

    def deleteFirst[A](self: List[A], elem: A): List[A] = {
        val elemData = typeProxy[Data](elem)
        def go(lst: List[Data]): List[Data] = lst match
            case List.Cons(h, t) =>
                if equalsData(h, elemData) then t
                else List.Cons(h, go(t))
            case List.Nil => List.Nil
        typeProxy[List[A]](go(typeProxy[List[Data]](self)))
    }

    // Mirrors the prelude `distinct` algorithm: fold building the result (prepend-then-reverse,
    // like `foldLeft(...).reverse`), with membership tested by the sibling `contains` (the prelude
    // uses `acc.exists(_ === e)`, i.e. `contains`) instead of a reimplemented scan. `loop`/`rev` are
    // pure spine ops (no equality).
    def distinct[A](self: List[A]): List[A] = {
        def rev(lst: List[A], acc: List[A]): List[A] = lst match
            case List.Cons(h, t) => rev(t, List.Cons(h, acc))
            case List.Nil        => acc
        def loop(lst: List[A], acc: List[A]): List[A] = lst match
            case List.Cons(h, t) =>
                if contains(acc, h) then loop(t, acc)
                else loop(t, List.Cons(h, acc))
            case List.Nil => acc
        rev(loop(self, List.Nil), List.Nil)
    }

    // Mirrors the prelude `diff` algorithm: compose the (already intrinsified) sibling `deleteFirst`,
    // short-circuit on empty `acc` (prelude's `if isEmpty then Nil`), and tail-recurse over `other`,
    // instead of reimplementing element removal. The recursion is a LOCAL `go` — a top-level support
    // def cannot reference itself during eager support-init.
    def diff[A](self: List[A], other: List[A]): List[A] = {
        def go(acc: List[A], o: List[A]): List[A] = acc match
            case List.Nil => List.Nil
            case List.Cons(_, _) =>
                o match
                    case List.Cons(head, tail) => go(deleteFirst(acc, head), tail)
                    case List.Nil              => acc
        go(self, other)
    }

}
