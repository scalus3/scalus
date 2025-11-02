package scalus.cardano.ledger.value.coin

import cats.data.NonEmptyList
import spire.algebra.*
import spire.math.{Rational, SafeLong}
import spire.implicits.{additiveGroupOps, additiveSemigroupOps, rms, seqOps, toRational, vectorSpaceOps, LongAlgebra}

import scala.annotation.targetName

import RationalExtensions.*

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

    given eqArithmeticError: Eq[ArithmeticError] = Eq.fromUniversalEquals

    def apply(long: Long): Either[Underflow.type, Coin] =
        if long.sign < 0 then Left(Underflow) else Right(long)

    def apply(safeLong: SafeLong): Either[ArithmeticError, Coin] =
        try { Right(unsafeApply(safeLong)) }
        catch { case e: ArithmeticError => Left(e) }

    def unsafeApply(long: Long): Coin =
        if long.sign < 0 then throw Underflow else long

    def unsafeApply(safeLong: SafeLong): Coin =
        if safeLong.isValidLong && safeLong >= 0
        then safeLong.longValue
        else if safeLong.signum < 0 then throw Underflow
        else throw Coin.ArithmeticError.Overflow

    def zero: Coin = Coin.unsafeApply(0)

    extension (self: Coin)
        def underlying: Long = self

        def toUnbounded: Unbounded = Unbounded(self)
        def toFractional: Fractional = Fractional(self)

        def signum: Int = LongAlgebra.signum(self)

        @targetName("negate")
        infix def unary_- : Unbounded = -Unbounded(self)

        @targetName("addCoerce_Coin")
        infix def +~(other: Coin): Unbounded = Unbounded(self) + Unbounded(other)

        @targetName("addCoerce_Unbounded")
        infix def +~(other: Unbounded): Unbounded = Unbounded(self) + other

        @targetName("addCoerce_Unbounded")
        infix def +~(other: Fractional): Fractional = Fractional(self) + other

        @targetName("subtractCoerce_Coin")
        infix def -~(other: Coin): Unbounded = Unbounded(self) - Unbounded(other)

        @targetName("subtractCoerce_Unbounded")
        infix def -~(other: Unbounded): Unbounded = Unbounded(self) - other

        @targetName("subtractCoerce_Fractional")
        infix def -~(other: Fractional): Fractional = Fractional(self) - other

        @targetName("scale")
        infix def *~(c: SafeLong): Unbounded = Unbounded(self) :* c

        @targetName("scale")
        infix def *~(c: Rational): Fractional = Fractional(self) :* c

        @targetName("div")
        infix def /~(c: SafeLong): Fractional = Fractional(self) :/ c.toRational

        @targetName("div")
        infix def /~(c: Rational): Fractional = Fractional(self) :/ c

        /** Distribute a [[Coin]] amount according to a list of normalized weights.
          *
          * @param weights
          *   the list of rational weights that always sums to one.
          *
          * @return
          *   a list of shares that always sums to the amount that was distributed.
          */
        def distribute(weights: Distribution.NormalizedWeights): NonEmptyList[Coin] =
            // `unsafeToCoin` is safe here because the weights sum to one
            toUnbounded.distribute(weights).map(_.unsafeToCoin)

    given algebra: Algebra.type = Algebra

    object Algebra extends CoinOrder

    object AlgebraFull extends CoinOrder, CoinAdditiveMonoid

    trait CoinOrder extends Order[Coin] {
        override def compare(self: Coin, other: Coin): Int = LongAlgebra.compare(self, other)
    }

    /** This AdditiveMonoid is available for manual import, but it isn't implicitly given to users
      * because adding `Long` Inners is unsafe (it can overflow/underflow without warning/error).
      */
    trait CoinAdditiveMonoid extends AdditiveMonoid[Coin] {
        override def zero: Coin = Coin.zero
        override def plus(x: Coin, y: Coin): Coin = x + y
    }

    extension (self: Iterable[Coin]) {
        def coinAverage: Option[Fractional] = unbounded.coinAverage

        def coinSum: Unbounded = unbounded.coinSum

        def coinMin: Coin = self.qmin

        def coinMax: Coin = self.qmax

        private def unbounded: Iterable[Coin.Unbounded] = self.view.map(_.toUnbounded)
    }

    // ===================================
    // Re-exported nested objects and types
    // ===================================
    // We re-export these to avoid having them be directly defined in the root object,
    // so that the root's opaque type stays opaque to the nested objects.

    type Unbounded = CoinSubtypes.Unbounded

    object Unbounded { export CoinSubtypes.Unbounded.* }

    type Fractional = CoinSubtypes.Fractional

    object Fractional { export CoinSubtypes.Fractional.* }
}

private object CoinSubtypes {
    import Coin.ArithmeticError

    // ===================================
    // Coin.Unbounded
    // ===================================

    type Unbounded = Unbounded.Unbounded

    object Unbounded {
        opaque type Unbounded = SafeLong

        def apply(safeLong: SafeLong): Unbounded = safeLong

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

            def toFractional: Fractional = Fractional(self.toRational)

            @targetName("signum_Unbounded")
            def signum: Int = self.signum

            @targetName("addCoerce_Coin")
            infix def +~(other: Coin): Unbounded = self + other.toUnbounded

            @targetName("addCoerce_Unbounded")
            infix def +~(other: Unbounded): Unbounded = self + other

            @targetName("addCoerce_Fractional")
            infix def +~(other: Fractional): Fractional = Fractional(self) + other

            @targetName("subtractCoerce_Coin")
            infix def -~(other: Coin): Unbounded = self - other.toUnbounded

            @targetName("subtractCoerce_Unbounded")
            infix def -~(other: Unbounded): Unbounded = self - other

            @targetName("subtractCoerce_Fractional")
            infix def -~(other: Fractional): Fractional = Fractional(self) - other

            @targetName("scale")
            infix def *~(c: SafeLong): Unbounded = self :* c

            @targetName("scale")
            infix def *~(c: Rational): Fractional = Fractional(self) :* c

            @targetName("div")
            infix def /~(c: SafeLong): Fractional = Fractional(self) :/ c.toRational

            @targetName("div")
            infix def /~(c: Rational): Fractional = Fractional(self) :/ c

            /** Distribute a [[Coin.Unbounded]] amount according to a list of normalized weights.
              *
              * @param weights
              *   the list of rational weights that always sums to one.
              * @return
              *   a list of shares that always sums to the amount that was distributed.
              */
            def distribute(weights: Distribution.NormalizedWeights): NonEmptyList[Unbounded] =
                weights.distribute(self)

        extension (self: Iterable[Unbounded]) {
            def coinAverage: Option[Fractional] = {
                val l = self.iterator.length
                Option.when(l > 0)(Fractional(Rational(self.qsum, l)))
            }

            def coinSum: Unbounded = self.qsum

            def coinMin: Unbounded = self.qmin

            def coinMax: Unbounded = self.qmax
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

        def apply(rational: Rational): Fractional = rational

        def zero: Fractional = 0

        def one: Fractional = 1

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

            def signum: Int = self.signum

            @targetName("addCoerce_Coin")
            infix def +~(other: Coin): Fractional = self + other.toFractional

            @targetName("addCoerce_Unbounded")
            infix def +~(other: Unbounded): Fractional = self + other.toFractional

            @targetName("addCoerce_Fractional")
            infix def +~(other: Fractional): Fractional = self + other

            @targetName("subtractCoerce_Coin")
            infix def -~(other: Coin): Fractional = self - other.toFractional

            @targetName("subtractCoerce_Unbounded")
            infix def -~(other: Unbounded): Fractional = self - other.toFractional

            @targetName("subtractCoerce_Fractional")
            infix def -~(other: Fractional): Fractional = self - other

            @targetName("scale")
            infix def *~(c: SafeLong): Fractional = self :* c.toRational

            @targetName("scale")
            infix def *~(c: Rational): Fractional = self :* c

            @targetName("div")
            infix def /~(c: SafeLong): Fractional = self :/ c.toRational

            @targetName("div")
            infix def /~(c: Rational): Fractional = self :/ c

        extension (self: Iterable[Fractional])
            def coinSum: Fractional = self.qsum

            def coinAverage: Option[Fractional] = Option.when(self.nonEmpty)(self.qmean)

        given algebra: Algebra.type = Algebra

        object Algebra
            extends Order[Fractional],
              VectorSpace[Fractional, Rational],
              Field[Fractional] {
            override def compare(self: Fractional, other: Fractional): Int = self.compare(other)

            override def scalar: Field[Rational] = Field[Rational]

            override def zero: Fractional = Fractional.zero

            override def negate(x: Fractional): Fractional = Fractional(-x)

            override def plus(x: Fractional, y: Fractional): Fractional = x + y

            override def minus(x: Fractional, y: Fractional): Fractional = x - y

            override def timesl(s: Rational, v: Fractional): Fractional = Fractional(s * v)

            override def one: Fractional = Fractional.one

            override def times(x: Fractional, y: Fractional): Fractional = x * y

            override def div(x: Fractional, y: Fractional): Fractional = x / y
        }
    }
}
