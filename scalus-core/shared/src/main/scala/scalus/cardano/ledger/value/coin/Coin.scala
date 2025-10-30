package scalus.cardano.ledger.value.coin

import spire.algebra.*
import spire.implicits.*
import spire.math.{Rational, SafeLong}

import scala.annotation.targetName

type Coin = Coin.Coin

object Coin {
    import ArithmeticError.*
    import CoinSubtypes.*

    /** Non-negative and bounded amount of coins. Can be used in tx outputs without any additional
      * checks.
      */
    opaque type Coin = Long

    enum ArithmeticError extends Throwable:
        case Underflow
        case Overflow

    def apply(x: Long): Either[Underflow.type, Coin] =
        if x.sign < 0 then Left(Underflow) else Right(x)

    def apply(self: SafeLong): Either[ArithmeticError, Coin] =
        try { Right(unsafeApply(self)) }
        catch { case e: ArithmeticError => Left(e) }

    def unsafeApply(x: Long): Coin =
        if x.sign < 0 then throw Underflow else x

    def unsafeApply(self: SafeLong): Coin =
        if self.isValidLong && self >= 0
        then self.longValue
        else if self.signum < 0 then throw Underflow
        else throw Coin.ArithmeticError.Overflow

    def zero: Coin = Coin.unsafeApply(0)

    extension (self: Coin)
        def underlying: Long = self

        def toCoinUnbounded: Unbounded = Unbounded(self)
        def toCoinFractional: Fractional = Fractional(self)

        def scaleIntegral[I](c: I)(using int: spire.math.Integral[I]): Unbounded =
            Unbounded(self) :* c.toSafeLong

        def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional =
            Fractional(self) :* c.toRational

        def signum: Int = LongAlgebra.signum(self)

        @targetName("addCoerce")
        infix def +~(other: Coin): Unbounded = Unbounded(self) + Unbounded(other)

        @targetName("addCoerce")
        infix def +~(other: Unbounded): Unbounded = Unbounded(self) + other

        @targetName("addCoerce")
        infix def +~(other: Fractional): Fractional = Fractional(self) + other

        @targetName("subtractCoerce")
        infix def -~(other: Coin): Unbounded = Unbounded(self) - Unbounded(other)

        @targetName("subtractCoerce")
        infix def -~(other: Unbounded): Unbounded = Unbounded(self) - other

        @targetName("subtractCoerce")
        infix def -~(other: Fractional): Fractional = Fractional(self) - other

        @targetName("negate")
        infix def unary_- : Unbounded = -Unbounded(self)

    given algebra: Algebra.type = Algebra

    object Algebra extends Order[Coin] {
        override def compare(self: Coin, other: Coin): Int = LongAlgebra.compare(self, other)
    }

    // This AdditiveMonoid is available for manual import, but it isn't implicitly given to users
    // because adding `Long` coins is unsafe (it can overflow/underflow without warning/error).
    object AdditiveMonoid extends AdditiveMonoid[Coin] {
        override def zero: Coin = Coin.zero
        override def plus(x: Coin, y: Coin): Coin = x + y
    } 

    extension (self: IterableOnce[Coin]) {
        def averageCoin: Option[Fractional] = toUnbounded.averageCoin

        def max: Coin = Aggregate.max(self)

        def min: Coin = Aggregate.min(self)

        def sumCoins: Unbounded = toUnbounded.sumCoins

        private def toUnbounded: IterableOnce[Unbounded] = self.iterator.map(_.toCoinUnbounded)
    }

    // ===================================
    // Re-exported nested objects and types
    // ===================================
    // We re-export these to avoid having them be directly defined in the root object,
    // so that the root's opaque type stays opaque to the nested objects.

    type Unbounded = CoinSubtypes.Unbounded

    object Unbounded { export CoinSubtypes.Unbounded.{*, given} }

    type Fractional = CoinSubtypes.Fractional

    object Fractional { export CoinSubtypes.Fractional.{*, given} }
}

private object CoinSubtypes {
    import Coin.ArithmeticError

    // ===================================
    // Coin.Unbounded
    // ===================================

    type Unbounded = Unbounded.Unbounded

    object Unbounded {

        import spire.compat.integral

        opaque type Unbounded = SafeLong

        def apply(x: SafeLong): Unbounded = x

        def zero: Unbounded = 0

        extension (self: Unbounded)
            def underlying: SafeLong = self

            def toCoin: Either[ArithmeticError, Coin] =
                try {
                    Right(Coin.unsafeApply(self))
                } catch {
                    case e: ArithmeticError => Left(e)
                }

            def unsafeToCoin: Coin = Coin.unsafeApply(self)

            def toCoinFractional: Fractional = Fractional(self.toRational)

            def scaleIntegral[I](c: I)(using int: spire.math.Integral[I]): Unbounded =
                self :* c.toSafeLong

            def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional =
                self.toCoinFractional :* c.toRational

            def signum: Int = self.signum

            @targetName("addCoerce")
            infix def +~(other: Coin): Unbounded = self + other.toCoinUnbounded

            @targetName("addCoerce")
            infix def +~(other: Unbounded): Unbounded = self + other

            @targetName("addCoerce")
            infix def +~(other: Fractional): Fractional = Fractional(self) + other

            @targetName("subtractCoerce")
            infix def -~(other: Coin): Unbounded = self - other.toCoinUnbounded

            @targetName("subtractCoerce")
            infix def -~(other: Unbounded): Unbounded = self - other

            @targetName("subtractCoerce")
            infix def -~(other: Fractional): Fractional = Fractional(self) - other

        extension (self: IterableOnce[Unbounded]) {
            def averageCoin: Option[Fractional] = Aggregate.average(self, _.toCoinFractional)

            def max: Unbounded = Aggregate.max(self)

            def min: Unbounded = Aggregate.min(self)

            def sumCoins: Unbounded = Aggregate.sum(self)
        }

        given algebra: Algebra.type = Algebra

        object Algebra extends Order[Unbounded], CModule[Unbounded, SafeLong] {
            override def compare(self: Unbounded, other: Unbounded): Int = self.compare(other)

            override def scalar: CRing[SafeLong] = CRing[SafeLong]

            override def zero: Unbounded = Unbounded.zero

            override def negate(self: Unbounded): Unbounded = -self

            override def plus(self: Unbounded, other: Unbounded): Unbounded = self + other

            override def minus(self: Unbounded, other: Unbounded): Unbounded = self - other

            override def timesl(s: SafeLong, self: Unbounded): Unbounded = s * self
        }
    }

    // ===================================
    // Coin.Fractional
    // ===================================

    type Fractional = Fractional.Fractional

    object Fractional {
        import RationalExtensions.*

        opaque type Fractional = Rational

        def apply(x: Rational): Fractional = x

        def zero: Fractional = 0

        extension (self: Fractional)
            def underlying: Rational = self

            def toUnbounded: Unbounded = Unbounded(self.roundHalfEven)

            def toCoin: Either[ArithmeticError, Coin] =
                try {
                    Right(unsafeToCoin)
                } catch {
                    case e: Coin.ArithmeticError => Left(e)
                }

            def unsafeToCoin: Coin = Coin.unsafeApply(self.roundHalfEven)

            def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional =
                self :* c.toRational

            def signum: Int = self.signum

            @targetName("addCoerce")
            infix def +~(other: Coin): Fractional = self + other.toCoinFractional

            @targetName("addCoerce")
            infix def +~(other: Unbounded): Fractional = self + other.toCoinFractional

            @targetName("addCoerce")
            infix def +~(other: Fractional): Fractional = self + other

            @targetName("subtractCoerce")
            infix def -~(other: Coin): Fractional = self - other.toCoinFractional

            @targetName("subtractCoerce")
            infix def -~(other: Unbounded): Fractional = self - other.toCoinFractional

            @targetName("subtractCoerce")
            infix def -~(other: Fractional): Fractional = self - other

        extension (self: IterableOnce[Fractional])
            def min: Fractional = self.iterator.min

            def max: Fractional = self.iterator.max

            def sumCoins: Fractional = Aggregate.sum(self)

            def averageCoin: Option[Fractional] = Aggregate.average(self)

        given algebra: Algebra.type = Algebra

        object Algebra extends Order[Fractional], VectorSpace[Fractional, Rational] {
            override def compare(self: Fractional, other: Fractional): Int = self.compare(other)

            override def scalar: Field[Rational] = Field[Rational]

            override def zero: Fractional = Fractional.zero

            override def negate(x: Fractional): Fractional = Fractional(-x)

            override def plus(x: Fractional, y: Fractional): Fractional = x + y

            override def minus(x: Fractional, y: Fractional): Fractional = x - y

            override def timesl(s: Rational, v: Fractional): Fractional = Fractional(s * v)
        }
    }
}
