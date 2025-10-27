package scalus.cardano.ledger.value

import spire.compat.integral
import spire.algebra.*
import spire.implicits.*
import spire.math.SafeLong

import scala.annotation.targetName
import scala.math.BigDecimal.RoundingMode.RoundingMode

type Coin = Coin.Coin

object Coin {

    /** Non-negative and bounded amount of coins. Can be used in tx outputs without any additional
      * checks.
      */
    opaque type Coin = Long

    def apply(self: Long): Either[Coin.ArithmeticError.Underflow.type, Coin] = {
        if self.signum < 0 then Left(Coin.ArithmeticError.Underflow) else Right(self)
    }

    def apply(unbounded: Unbounded): Either[Coin.ArithmeticError, Coin] = unbounded.toCoin

    def unsafeApply(self: Long): Coin =
        if self.signum < 0 then throw Coin.ArithmeticError.Underflow else self

    def zero: Coin = Coin.unsafeApply(0)

    // Only `Coin` should have coercive operators defined.
    // Defining similar coercive operators for `Coin.Unbounded` and `Coin.Fractional`
    // would cause clashes with the spire implicit operators.
    extension (self: Coin)
        def toLong: Long = self

        def toCoinUnbounded: Unbounded = Unbounded(self)
        def toCoinFractional: Fractional = Fractional(self)

        def signum: Int = if self > 0 then 1 else 0

        @targetName("add")
        infix def +(other: Coin): Unbounded = Unbounded(self) + Unbounded(other)

        @targetName("add")
        infix def +(other: Unbounded): Unbounded = Unbounded(self) + other

        @targetName("add")
        infix def +(other: Fractional): Fractional = Fractional(self) + other

        @targetName("subtract")
        infix def -(other: Coin): Unbounded = Unbounded(self) - Unbounded(other)

        @targetName("subtract")
        infix def -(other: Unbounded): Unbounded = Unbounded(self) - other

        @targetName("subtract")
        infix def -(unbounded: Fractional): Fractional = Fractional(self) - unbounded

        @targetName("negate")
        infix def unary_- : Unbounded = -Unbounded(self)

        def scale(c: SafeLong): Unbounded = Unbounded(c * self)

        def scale(c: BigDecimal): Fractional = Fractional(c * self)

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

        import ArithmeticError.*

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

            def toCoinFractional: Fractional = Fractional(self.toBigDecimal)

            def scale(c: SafeLong): Unbounded = algebra.timesl(c, self)

            def scale(c: BigDecimal): Fractional =
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
        opaque type Fractional = BigDecimal

        def apply(x: BigDecimal): Fractional = x

        def zero: Fractional = 0

        extension (self: Fractional)
            def toBigDecimal: BigDecimal = self

            def round(mode: RoundingMode): Unbounded = Unbounded(self.setScale(0, mode).toBigInt)

            def toCoin(mode: RoundingMode): Either[ArithmeticError, Coin] = round(mode).toCoin

            def unsafeToCoin(mode: RoundingMode): Coin = round(mode).unsafeToCoin

            def scale(c: BigDecimal): Fractional = algebra.timesl(c, self)

            def signum: Int = self.signum

        extension (self: IterableOnce[Fractional])
            def min: Fractional = self.iterator.min

            def max: Fractional = self.iterator.max

            def sumCoins: Fractional = Aggregate.sum(self)

            def averageCoin: Option[Fractional] = Aggregate.average(self)

        given algebra: Algebra.type = Algebra

        object Algebra extends Order[Fractional], VectorSpace[Fractional, BigDecimal] {
            override def compare(self: Fractional, other: Fractional): Int = self.compare(other)

            override def scalar: Field[BigDecimal] = Field[BigDecimal]

            override def zero: Fractional = Fractional.zero

            override def negate(x: Fractional): Fractional = Fractional(-x)

            override def plus(x: Fractional, y: Fractional): Fractional = x + y

            override def minus(x: Fractional, y: Fractional): Fractional = x - y

            override def timesl(s: BigDecimal, v: Fractional): Fractional = Fractional(s * v)
        }
    }

    private object Aggregate {
        def average[T <: SafeLong | BigDecimal, R](
            self: IterableOnce[T],
            convert: T => R = identity[R]
        )(using evT: AdditiveMonoid[T], evR: VectorSpace[R, BigDecimal]): Option[R] = {
            val (sum, length) = Aggregate.sumLength(self)

            Option.when(length > 0)(convert(sum) :/ length)
        }

        def max[T](self: IterableOnce[T])(using ev: Ordering[T]): T = self.iterator.max

        def min[T](self: IterableOnce[T])(using ev: Ordering[T]): T = self.iterator.min

        def sum[T <: SafeLong | BigDecimal](self: IterableOnce[T])(using ev: AdditiveMonoid[T]): T =
            self.iterator.foldRight(ev.zero)(ev.plus)

        private def sumLength[T <: SafeLong | BigDecimal](
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
