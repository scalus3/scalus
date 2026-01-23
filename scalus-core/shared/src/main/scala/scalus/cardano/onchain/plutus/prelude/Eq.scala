package scalus.cardano.onchain.plutus.prelude

import scalus.{Compile, CompileDerivations}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data

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

    given Eq[Unit] = (_: Unit, _: Unit) => true
    given Eq[BigInt] = (x: BigInt, y: BigInt) => equalsInteger(x, y)
    given Eq[String] = (x: String, y: String) => equalsString(x, y)
    given Eq[Boolean] = (x: Boolean, y: Boolean) => x == y
    given Eq[Data] = (x: Data, y: Data) => equalsData(x, y)

    given [A: Eq, B: Eq]: Eq[(A, B)] = (lhs: (A, B), rhs: (A, B)) =>
        lhs._1 === rhs._1 && lhs._2 === rhs._2

    given [A: Eq, B: Eq, C: Eq]: Eq[(A, B, C)] = (lhs: (A, B, C), rhs: (A, B, C)) =>
        lhs._1 === rhs._1 && lhs._2 === rhs._2 && lhs._3 === rhs._3

    def by[A, B: Eq](mapper: A => B): Eq[A] = (lhs: A, rhs: A) => mapper(lhs) === mapper(rhs)

    extension [A](self: Eq[A])
        inline def eqv(inline lhs: A, inline rhs: A): Boolean = self(lhs, rhs)
        inline def notEqv(inline lhs: A, inline rhs: A): Boolean = !self.eqv(lhs, rhs)

        def orElse(other: Eq[A]): Eq[A] = (lhs: A, rhs: A) =>
            if self.eqv(lhs, rhs) then other.eqv(lhs, rhs) else false

        def orElseBy[B: Eq](mapper: A => B): Eq[A] = (lhs: A, rhs: A) =>
            if self.eqv(lhs, rhs) then Eq[B].eqv(mapper(lhs), mapper(rhs)) else false

    end extension

    def keyPairEq[A: Eq, B]: Eq[(A, B)] = (lhs: (A, B), rhs: (A, B)) => lhs._1 === rhs._1

end Eq
