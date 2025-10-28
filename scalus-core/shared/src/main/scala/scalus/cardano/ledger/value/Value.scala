package scalus.cardano.ledger.value

import spire.algebra.*
import spire.implicits.*
import spire.math.{Rational, SafeLong}

import java.math.MathContext
import scala.math.BigDecimal.defaultMathContext

case class Value (lovelace: Coin, assets: MultiAsset = MultiAsset.zero) {
    def scaleIntegral[I](c: I)(using frac: spire.math.Integral[I]): Value.Unbounded =
        Value.Unbounded(lovelace.scaleIntegral(c), assets.scaleIntegral(c))

    def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Value.Fractional =
        Value.Fractional(lovelace.scaleFractional(c), assets.scaleFractional(c))
}

object Value {
    def zero: Value = Value(Coin.zero)

    given algebra: Algebra.type = Algebra

    object Algebra extends PartialOrder[Value] {
        override def partialCompare(self: Value, other: Value): Double =
            partialCompareImpl(self.lovelace, self.assets, other.lovelace, other.assets)
    }

    enum ArithmeticError extends Throwable:
        case Lovelace(e: Coin.ArithmeticError)
        case Assets(e: MultiAsset.ArithmeticError)

    case class Unbounded(
        lovelace: Coin.Unbounded,
        assets: MultiAsset.Unbounded = MultiAsset.Unbounded.zero
    ) {
        def toValue: Either[ArithmeticError, Value] = try {
            Right(this.unsafeToValue)
        } catch {
            case e: ArithmeticError => Left(e)
        }

        def unsafeToValue: Value = {
            val lovelace =
                try {
                    this.lovelace.unsafeToCoin
                } catch {
                    case e: Coin.ArithmeticError => throw ArithmeticError.Lovelace(e)
                }

            val assets =
                try {
                    this.assets.unsafeToMultiAsset
                } catch {
                    case e: MultiAsset.ArithmeticError => throw ArithmeticError.Assets(e)
                }

            Value(lovelace, assets)
        }

        def scaleIntegral[I](c: I)(using int: spire.math.Integral[I]): Unbounded =
            this :* c.toSafeLong

        def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional =
            Fractional(this.lovelace.scaleFractional(c), this.assets.scaleFractional(c))
    }

    object Unbounded {
        def zero: Unbounded = Unbounded(Coin.Unbounded.zero)

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Value.Unbounded], CModule[Unbounded, SafeLong] {
            override def partialCompare(self: Unbounded, other: Unbounded): Double =
                partialCompareImpl(self.lovelace, self.assets, other.lovelace, other.assets)

            override def scalar: CRing[SafeLong] = CRing[SafeLong]

            override def zero: Unbounded = Unbounded.zero

            override def negate(self: Unbounded): Unbounded =
                Unbounded(-self.lovelace, -self.assets)

            override def plus(self: Unbounded, other: Unbounded): Unbounded =
                Unbounded(self.lovelace + other.lovelace, self.assets + other.assets)

            override def minus(self: Unbounded, other: Unbounded): Unbounded =
                Unbounded(self.lovelace - other.lovelace, self.assets - other.assets)

            override def timesl(s: SafeLong, self: Unbounded): Unbounded =
                Unbounded(self.lovelace :* s, self.assets :* s)
        }
    }

    case class Fractional(
        lovelace: Coin.Fractional,
        assets: MultiAsset.Fractional = MultiAsset.Fractional.zero
    ) {
        def toUnbounded(mc: MathContext = defaultMathContext): Unbounded =
            Unbounded(this.lovelace.toUnbounded(mc), this.assets.toUnbounded(mc))

        def toValue(mc: MathContext = defaultMathContext): Either[ArithmeticError, Value] = try {
            Right(this.unsafeToValue(mc))
        } catch {
            case e: ArithmeticError => Left(e)
        }

        def unsafeToValue(mc: MathContext = defaultMathContext): Value = {
            val lovelace =
                try {
                    this.lovelace.unsafeToCoin(mc)
                } catch {
                    case e: Coin.ArithmeticError => throw ArithmeticError.Lovelace(e)
                }

            val assets =
                try {
                    this.assets.unsafeToMultiAsset(mc)
                } catch {
                    case e: MultiAsset.ArithmeticError => throw ArithmeticError.Assets(e)
                }

            Value(lovelace, assets)
        }

        def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional =
            this :* c.toRational
    }

    object Fractional {
        def zero: Fractional = Fractional(Coin.Fractional.zero)

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Fractional], VectorSpace[Fractional, Rational] {
            override def partialCompare(self: Fractional, other: Fractional): Double =
                partialCompareImpl(self.lovelace, self.assets, other.lovelace, other.assets)

            override def scalar: Field[Rational] = Field[Rational]

            override def zero: Fractional = Fractional.zero

            override def negate(self: Fractional): Fractional =
                Fractional(-self.lovelace, -self.assets)

            override def plus(self: Fractional, other: Fractional): Fractional =
                Fractional(self.lovelace + other.lovelace, self.assets + other.assets)

            override def minus(self: Fractional, other: Fractional): Fractional =
                Fractional(self.lovelace - other.lovelace, self.assets - other.assets)

            override def timesl(s: Rational, self: Fractional): Fractional =
                Fractional(self.lovelace :* s, self.assets :* s)
        }
    }

    private def partialCompareImpl[C, MA](
        selfLovelace: C,
        selfAssets: MA,
        otherLovelace: C,
        otherAssets: MA
    )(using
        Order[C],
        PartialOrder[MA]
    ): Double = {
        val coinComparison: Double = selfLovelace.compare(otherLovelace).toDouble
        val assetsComparison: Double = selfAssets.partialCompare(otherAssets)
        if coinComparison === assetsComparison then coinComparison else Double.NaN
    }
}
