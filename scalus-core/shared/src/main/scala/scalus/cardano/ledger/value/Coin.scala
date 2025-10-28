package scalus.cardano.ledger.value

import spire.compat.integral
import spire.algebra.*
import spire.implicits.*
import spire.math.{Rational, SafeLong}

import java.math.MathContext
import scala.math.BigDecimal.defaultMathContext
import scala.annotation.targetName

type Coin = Coin.Coin

object Coin {
    import ArithmeticError.*

    /** Non-negative and bounded amount of coins. Can be used in tx outputs without any additional
      * checks.
      */
    opaque type Coin = Long

    def apply(self: Long): Either[Underflow.type, Coin] = {
        if self.signum < 0 then Left(Underflow) else Right(self)
    }

    def apply(unbounded: Unbounded): Either[Coin.ArithmeticError, Coin] = unbounded.toCoin

    def unsafeApply(self: Long): Coin =
        if self.signum < 0 then throw Underflow else self

    def zero: Coin = Coin.unsafeApply(0)

    given Conversion[Coin, Unbounded] = Unbounded.apply
    given Conversion[Coin, Fractional] = Fractional.apply

    // Only `Coin` should have coercive operators defined.
    // Defining similar coercive operators for `Coin.Unbounded` and `Coin.Fractional`
    // would cause clashes with the spire implicit operators.
    extension (self: Coin)
        def toLong: Long = self

        def toCoinUnbounded: Unbounded = Unbounded(self)
        def toCoinFractional: Fractional = Fractional(self)

        def signum: Int = if self > 0 then 1 else 0

        @targetName("addCoerceCoins")
        infix def +(other: Coin): Unbounded = Unbounded(self) + Unbounded(other)

        @targetName("subtractCoerceCoins")
        infix def -(other: Coin): Unbounded = Unbounded(self) - Unbounded(other)

        @targetName("negate")
        infix def unary_- : Unbounded = -Unbounded(self)

        def scale(c: SafeLong): Unbounded = Unbounded(c * self)

        def scale(c: Rational): Fractional = Fractional(c * self)

    given algebra: Algebra.type = Algebra

    object Algebra extends Order[Coin] {
        override def compare(self: Coin, other: Coin): Int = self.compare(other)
    }

    extension (self: IterableOnce[Coin]) {
        def averageCoin: Option[Fractional] = toUnbounded.averageCoin

        def max: Coin = Aggregate.max(self)

        def min: Coin = Aggregate.min(self)

        def sumCoins: Unbounded = toUnbounded.sumCoins

        private def toUnbounded: IterableOnce[Unbounded] = self.iterator.map(_.toCoinUnbounded)
    }

    // ===================================
    // Coin.Unbounded
    // ===================================

    type Unbounded = Unbounded.Unbounded

    object Unbounded {
        opaque type Unbounded = SafeLong

        def apply(x: SafeLong): Unbounded = x

        def zero: Unbounded = 0

        given Conversion[Unbounded, Fractional] = Fractional.apply

        extension (self: Unbounded)
            def toSafeLong: SafeLong = self

            def toCoin: Either[ArithmeticError, Coin] =
                if self.isValidLong
                then Coin(self.longValue)
                else if self.signum < 0 then Left(Underflow)
                else Left(Coin.ArithmeticError.Overflow)

            def unsafeToCoin: Coin =
                if self.isValidLong
                then Coin.unsafeApply(self.longValue)
                else if self.signum < 0 then throw Underflow
                else throw Coin.ArithmeticError.Overflow

            def toCoinFractional: Fractional = Fractional(self.toRational)

            def scale(c: SafeLong): Unbounded = algebra.timesl(c, self)

            def scale(c: Rational): Fractional =
                Fractional.algebra.timesl(c, self.toCoinFractional)

            def signum: Int = self.signum

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
        opaque type Fractional = Rational

        def apply(x: Rational): Fractional = x

        def zero: Fractional = 0

        extension (self: Fractional)
            def toRational: Rational = self

            def toUnbounded(mc: MathContext = defaultMathContext): Unbounded = {
                Unbounded(SafeLong(asIntegralBigDecimal(mc).bigDecimal.toBigIntegerExact))
            }

            def toCoin(mc: MathContext = defaultMathContext): Either[ArithmeticError, Coin] = try {
                Right(self.unsafeToCoin(mc))
            } catch {
                case e: Coin.ArithmeticError => Left(e)
            }

            def unsafeToCoin(mc: MathContext = defaultMathContext): Coin = {
                val rounded = asIntegralBigDecimal(mc)
                try {
                    // Succeeds if `rounded` is an exact and positive Long.
                    Coin.unsafeApply(rounded.toLongExact)
                } catch {
                    // Thrown by `bigDecimal.toLongExact` if out of bounds
                    case _: java.lang.ArithmeticException =>
                        // Figure out whether we have over or underflow
                        val bigInteger = rounded.bigDecimal.toBigIntegerExact
                        if bigInteger.signum < 0 then throw Underflow else throw Overflow
                    // Re-throw Coin.ArithmeticError from `Coin.unsafeApply` in the try block
                    case e: Coin.ArithmeticError => throw e
                }
            }

            def scale(c: Rational): Fractional = algebra.timesl(c, self)

            def signum: Int = self.signum

            // Round the Rational via BigDecimal with the defaultMathContext
            // `defaultMathContext` uses banker's rounding and 34 decimal digits of precision.
            private def asIntegralBigDecimal(mc: MathContext = defaultMathContext): BigDecimal =
                self.toBigDecimal(mc).setScale(0)

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

    private object Aggregate {
        def average[T <: SafeLong | Rational, R](
            self: IterableOnce[T],
            convert: T => R = identity[R]
        )(using evT: AdditiveMonoid[T], evR: VectorSpace[R, Rational]): Option[R] = {
            val (sum, length) = Aggregate.sumLength(self)

            Option.when(length > 0)(convert(sum) :/ length)
        }

        def max[T](self: IterableOnce[T])(using ev: Ordering[T]): T = self.iterator.max

        def min[T](self: IterableOnce[T])(using ev: Ordering[T]): T = self.iterator.min

        def sum[T <: SafeLong | Rational](self: IterableOnce[T])(using ev: AdditiveMonoid[T]): T =
            self.iterator.foldRight(ev.zero)(ev.plus)

        private def sumLength[T <: SafeLong | Rational](
            self: IterableOnce[T]
        )(using ev: AdditiveMonoid[T]): (T, Int) = {
            type Acc = (T, Int)

            def f(x: T, acc: Acc): Acc = (acc._1 + x, acc._2 + 1)

            self.iterator.foldRight((ev.zero, 0))(f)
        }
    }

    enum ArithmeticError extends Throwable:
        case Underflow
        case Overflow
}
