package scalus.cardano.ledger.value

import spire.algebra.*
import spire.implicits.*
import spire.math.SafeLong

import scala.math.BigDecimal.RoundingMode.RoundingMode

case class Value private (lovelace: Coin, assets: MultiAsset = MultiAsset.zero)

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

        def scale(s: SafeLong): Unbounded = Unbounded.algebra.timesl(s, this)

        def scale(s: BigDecimal): Fractional =
            Fractional(this.lovelace.scale(s), this.assets.scale(s))
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
                Unbounded(self.lovelace.scale(s), self.assets.scale(s))
        }
    }

    case class Fractional(
        lovelace: Coin.Fractional,
        assets: MultiAsset.Fractional = MultiAsset.Fractional.zero
    ) {
        def round(mode: RoundingMode): Unbounded =
            Unbounded(this.lovelace.round(mode), this.assets.round(mode))

        def toValue(mode: RoundingMode): Either[ArithmeticError, Value] = try {
            Right(this.unsafeToValue(mode))
        } catch {
            case e: ArithmeticError => Left(e)
        }

        def unsafeToValue(mode: RoundingMode): Value = {
            val lovelace =
                try {
                    this.lovelace.unsafeToCoin(mode)
                } catch {
                    case e: Coin.ArithmeticError => throw ArithmeticError.Lovelace(e)
                }

            val assets =
                try {
                    this.assets.unsafeToMultiAsset(mode)
                } catch {
                    case e: MultiAsset.ArithmeticError => throw ArithmeticError.Assets(e)
                }

            Value(lovelace, assets)
        }

        def scale(s: BigDecimal): Fractional = Fractional.algebra.timesl(s, this)
    }

    object Fractional {
        def zero: Fractional = Fractional(Coin.Fractional.zero)

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Fractional], VectorSpace[Fractional, BigDecimal] {
            override def partialCompare(self: Fractional, other: Fractional): Double =
                partialCompareImpl(self.lovelace, self.assets, other.lovelace, other.assets)

            override def scalar: Field[BigDecimal] = Field[BigDecimal]

            override def zero: Fractional = Fractional.zero

            override def negate(self: Fractional): Fractional =
                Fractional(-self.lovelace, -self.assets)

            override def plus(self: Fractional, other: Fractional): Fractional =
                Fractional(self.lovelace + other.lovelace, self.assets + other.assets)

            override def minus(self: Fractional, other: Fractional): Fractional =
                Fractional(self.lovelace - other.lovelace, self.assets - other.assets)

            override def timesl(s: BigDecimal, self: Fractional): Fractional =
                Fractional(self.lovelace.scale(s), self.assets.scale(s))
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
