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

    extension (self: Coin)
        def toCoinUnbounded: Unbounded = Unbounded(self)
        def toCoinFractional: Fractional = Fractional(self)

        def signum: Int = if self > 0 then 1 else 0

        @targetName("add")
        infix def +(other: Coin): Unbounded = Unbounded(self) + Unbounded(other)

        @targetName("subtract")
        infix def -(other: Coin): Unbounded = Unbounded(self) - Unbounded(other)

        @targetName("add")
        infix def +(unbounded: Unbounded): Either[Coin.ArithmeticError, Coin] = Coin(
          Unbounded(self) + unbounded
        )

        @targetName("subtract")
        infix def -(unbounded: Unbounded): Either[Coin.ArithmeticError, Coin] = Coin(
          Unbounded(self) - unbounded
        )

        @targetName("negate")
        def unary_- : Unbounded = -Unbounded(self)

        def scale(c: SafeLong): Unbounded = Unbounded(c * self)

        def scale(c: BigDecimal): Fractional = Fractional(c * self)

    given Conversion[Coin, Long] = identity

    given order: Order[Coin] with
        override def compare(self: Coin, other: Coin): Int = self.compare(other)

    // Aggregation operations over Coin
    object Aggregate:

        def sum(coins: List[Coin]): Unbounded = Unbounded(coins.map(_.toCoinUnbounded.convert).sum)

        def average(coins: List[Coin]): Fractional =
            Fractional(coins.map(_.toCoinFractional.convert).sum / coins.length)

        def max(coins: List[Coin]): Coin = unsafeApply(coins.map(_.convert).max)

        def min(coins: List[Coin]): Coin = unsafeApply(coins.map(_.convert).min)

    // ===================================
    // Coin.Unbounded
    // ===================================

    type Unbounded = Unbounded.Unbounded

    object Unbounded {
        opaque type Unbounded = SafeLong

        def apply(x: SafeLong): Unbounded = x

        def zero: Unbounded = 0

        given Conversion[Unbounded, SafeLong] = identity

        import ArithmeticError.*

        extension (self: Unbounded)
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

            def scale(c: SafeLong): Unbounded = ops.timesl(c, self)

            def scale(c: BigDecimal): Fractional = Fractional.ops.timesl(c, self.toCoinFractional)

            def signum: Int = self.signum

        given order: Order[Unbounded] with
            override def compare(self: Unbounded, other: Unbounded): Int = self.compare(other)

        given ops: CModule[Unbounded, SafeLong] with {
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

        given Conversion[Fractional, BigDecimal] = identity

        extension (self: Fractional)
            def round(mode: RoundingMode): Unbounded = Unbounded(self.setScale(0, mode).toBigInt)

            def toCoin(mode: RoundingMode): Either[ArithmeticError, Coin] = round(mode).toCoin

            def unsafeToCoin(mode: RoundingMode): Coin = round(mode).unsafeToCoin

            def scale(c: BigDecimal): Fractional = ops.timesl(c, self)

            def signum: Int = self.signum

        given order: Order[Fractional] with
            override def compare(self: Fractional, other: Fractional): Int = self.compare(other)

        given ops: VectorSpace[Fractional, BigDecimal] with
            override def scalar: Field[BigDecimal] = Field[BigDecimal]

            override def zero: Fractional = Fractional.zero

            override def negate(x: Fractional): Fractional = Fractional(-x)

            override def plus(x: Fractional, y: Fractional): Fractional = x + y

            override def minus(x: Fractional, y: Fractional): Fractional = x - y

            override def timesl(s: BigDecimal, v: Fractional): Fractional = Fractional(s * v)
    }

    enum ArithmeticError extends Throwable:
        case Underflow
        case Overflow
}
