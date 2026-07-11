package scalus.cardano.onchain.plutus.prelude

import scalus.compiler.Compile
import scalus.compiler.CompileDerivations
import scalus.compiler.Ignore
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data

/** Structural equality typeclass.
  *
  * On-chain, `Eq` is a *marker*: the compiler always lowers `===` and `Eq`-based operations
  * (`List.contains`, `List.distinct`, `Option.contains`, ...) to structural equality of the
  * arguments' runtime representation — the instance body is never called. For that to be sound,
  * every `Eq` instance must implement structural equality (`eq(a, b)` iff `a.toData == b.toData`).
  * The compiler plugin therefore rejects hand-written instances in on-chain code: use
  * [[Eq.derived]], or [[Eq.structural]] to assert that a function is structural equality.
  */
@FunctionalInterface
trait Eq[-A] extends ((A, A) => Boolean) with CompileDerivations {
    override def apply(lhs: A, rhs: A): Boolean
}

extension [A](x: A)
    inline infix def ===(inline y: A)(using inline eq: Eq[A]): Boolean = eq(x, y)
    inline infix def !==(inline y: A)(using inline eq: Eq[A]): Boolean = !eq(x, y)

@Compile
object Eq:
    inline def apply[A: Eq]: Eq[A] = summon[Eq[A]]

    /** Derives an Eq instance for type A (case class, enum, or sealed trait) */
    inline def derived[A]: Eq[A] = ${ EqMacros.eqImpl[A] }

    /** Asserts that `f` implements *structural* equality for `A` and wraps it as an `Eq[A]`.
      *
      * This is the only way (besides [[derived]]) to introduce an `Eq` instance usable in on-chain
      * code: the compiler plugin rejects bare lambdas because on-chain `===` and `Eq`-based
      * operations are always lowered to structural comparison of the runtime representation — a
      * non-structural `f` would be silently ignored there. By wrapping `f` you assert that
      * `f(a, b)` iff `a` and `b` are structurally equal (`a.toData == b.toData`); the compiler is
      * free to replace it with an equivalent representation-optimal comparison.
      */
    @Ignore
    def structural[A](f: (A, A) => Boolean): Eq[A] = f(_, _)

    given Eq[Unit] = structural((_: Unit, _: Unit) => true)
    given Eq[BigInt] = structural((x: BigInt, y: BigInt) => equalsInteger(x, y))
    given Eq[String] = structural((x: String, y: String) => equalsString(x, y))
    given Eq[Boolean] = structural((x: Boolean, y: Boolean) => x == y)
    given Eq[Data] = structural((x: Data, y: Data) => equalsData(x, y))

    given [A: Eq, B: Eq]: Eq[(A, B)] =
        structural((lhs: (A, B), rhs: (A, B)) => lhs._1 === rhs._1 && lhs._2 === rhs._2)

    given [A: Eq, B: Eq, C: Eq]: Eq[(A, B, C)] = structural((lhs: (A, B, C), rhs: (A, B, C)) =>
        lhs._1 === rhs._1 && lhs._2 === rhs._2 && lhs._3 === rhs._3
    )

    /** Compare-by-key equality. Non-structural, so not available in on-chain code: on-chain
      * equality is always structural (see [[Eq]]).
      */
    @Ignore
    def by[A, B: Eq](mapper: A => B): Eq[A] = (lhs: A, rhs: A) => mapper(lhs) === mapper(rhs)

    extension [A](self: Eq[A])
        inline def eqv(inline lhs: A, inline rhs: A): Boolean = self(lhs, rhs)
        inline def notEqv(inline lhs: A, inline rhs: A): Boolean = !self.eqv(lhs, rhs)

        /** Conjunction of two Eq instances. Produces a potentially non-structural instance, so not
          * available in on-chain code: on-chain equality is always structural (see [[Eq]]).
          */
        @Ignore
        def orElse(other: Eq[A]): Eq[A] = (lhs: A, rhs: A) =>
            if self.eqv(lhs, rhs) then other.eqv(lhs, rhs) else false

        /** Refine equality by a projection. Produces a potentially non-structural instance, so not
          * available in on-chain code: on-chain equality is always structural (see [[Eq]]).
          */
        @Ignore
        def orElseBy[B: Eq](mapper: A => B): Eq[A] = (lhs: A, rhs: A) =>
            if self.eqv(lhs, rhs) then Eq[B].eqv(mapper(lhs), mapper(rhs)) else false

    end extension

end Eq
