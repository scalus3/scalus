package scalus.cardano.ledger.value

import scalus.cardano.ledger.value.coin.Coin
import scalus.cardano.ledger.value.multiasset.MultiAsset
import spire.algebra.*
import spire.implicits.*
import spire.math.{Rational, SafeLong}

import scala.annotation.targetName

case class Value(lovelace: Coin, assets: MultiAsset = MultiAsset.zero) {
    def scaleIntegral[I](c: I)(using frac: spire.math.Integral[I]): Value.Unbounded =
        Value.Unbounded(lovelace.scaleIntegral(c), assets.scaleIntegral(c))

    def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Value.Fractional =
        Value.Fractional(lovelace.scaleFractional(c), assets.scaleFractional(c))

    @targetName("addCoerce_Value")
    infix def +~(other: Value): Value.Unbounded =
        Value.Unbounded(lovelace +~ other.lovelace, assets +~ other.assets)
        
    @targetName("addCoerce_Unbounded")
    infix def +~(other: Value.Unbounded): Value.Unbounded =
        Value.Unbounded(lovelace +~ other.lovelace, assets +~ other.assets)

    @targetName("addCoerce_Fractional")
    infix def +~(other: Value.Fractional): Value.Fractional =
        Value.Fractional(lovelace +~ other.lovelace, assets +~ other.assets)

    @targetName("subtractCoerce_Value")
    infix def -~(other: Value): Value.Unbounded =
        Value.Unbounded(lovelace -~ other.lovelace, assets -~ other.assets)

    @targetName("subtractCoerce_Unbounded")
    infix def -~(other: Value.Unbounded): Value.Unbounded =
        Value.Unbounded(lovelace -~ other.lovelace, assets -~ other.assets)

    @targetName("subtractCoerce_Fractional")
    infix def -~(other: Value.Fractional): Value.Fractional =
        Value.Fractional(lovelace -~ other.lovelace, assets -~ other.assets)

    @targetName("negate")
    infix def unary_- : Value.Unbounded =
        Value.Unbounded(-lovelace, -assets)
}

object Value {
    import ValueVariants.partialCompareImpl

    def zero: Value = Value(Coin.zero)

    given algebra: Algebra.type = Algebra

    object Algebra extends PartialOrder[Value] {
        override def partialCompare(self: Value, other: Value): Double =
            partialCompareImpl(self.lovelace, self.assets, other.lovelace, other.assets)
    }

    enum ArithmeticError extends Throwable:
        case Lovelace(e: Coin.ArithmeticError)
        case Assets(e: MultiAsset.ArithmeticError)

    export ValueVariants.Unbounded
    export ValueVariants.Fractional
}

private object ValueVariants {
    import Value.ArithmeticError

    // ===================================
    // Value.Unbounded
    // ===================================

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
            val newLovelace =
                try { lovelace.unsafeToCoin }
                catch { case e: Coin.ArithmeticError => throw ArithmeticError.Lovelace(e) }

            val newAssets =
                try { assets.unsafeToMultiAsset }
                catch { case e: MultiAsset.ArithmeticError => throw ArithmeticError.Assets(e) }

            Value(newLovelace, newAssets)
        }

        def scaleIntegral[I](c: I)(using int: spire.math.Integral[I]): Unbounded =
            this :* c.toSafeLong

        def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional =
            Fractional(lovelace.scaleFractional(c), assets.scaleFractional(c))

        @targetName("addCoerce_Value")
        infix def +~(other: Value): Value.Unbounded =
            Value.Unbounded(lovelace +~ other.lovelace, assets +~ other.assets)

        @targetName("addCoerce_Unbounded")
        infix def +~(other: Value.Unbounded): Value.Unbounded = this + other

        @targetName("addCoerce_Fractional")
        infix def +~(other: Value.Fractional): Value.Fractional =
            Value.Fractional(lovelace +~ other.lovelace, assets +~ other.assets)

        @targetName("subtractCoerce_Value")
        infix def -~(other: Value): Value.Unbounded =
            Value.Unbounded(lovelace -~ other.lovelace, assets -~ other.assets)

        @targetName("subtractCoerce_Unbounded")
        infix def -~(other: Value.Unbounded): Value.Unbounded = this - other

        @targetName("subtractCoerce_Fractional")
        infix def -~(other: Value.Fractional): Value.Fractional =
            Value.Fractional(lovelace -~ other.lovelace, assets -~ other.assets)
    }

    object Unbounded {
        def zero: Unbounded = Unbounded(Coin.Unbounded.zero)

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Unbounded], CModule[Unbounded, SafeLong] {
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

    // ===================================
    // Value.Fractional
    // ===================================

    case class Fractional(
        lovelace: Coin.Fractional,
        assets: MultiAsset.Fractional = MultiAsset.Fractional.zero
    ) {
        def toUnbounded: Unbounded =
            Unbounded(lovelace.toUnbounded, assets.toUnbounded)

        def toValue: Either[ArithmeticError, Value] =
            try { Right(unsafeToValue) }
            catch { case e: ArithmeticError => Left(e) }

        def unsafeToValue: Value = {
            val newLovelace =
                try { lovelace.unsafeToCoin }
                catch { case e: Coin.ArithmeticError => throw ArithmeticError.Lovelace(e) }

            val newAssets =
                try { assets.unsafeToMultiAsset }
                catch { case e: MultiAsset.ArithmeticError => throw ArithmeticError.Assets(e) }

            Value(newLovelace, newAssets)
        }

        def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional =
            this :* c.toRational

        @targetName("addCoerce_Value")
        infix def +~(other: Value): Value.Fractional =
            Value.Fractional(lovelace +~ other.lovelace, assets +~ other.assets)

        @targetName("addCoerce_Unbounded")
        infix def +~(other: Value.Unbounded): Value.Fractional =
            Value.Fractional(lovelace +~ other.lovelace, assets +~ other.assets)

        @targetName("addCoerce_Fractional")
        infix def +~(other: Value.Fractional): Value.Fractional = this + other

        @targetName("subtractCoerce_Value")
        infix def -~(other: Value): Value.Fractional =
            Value.Fractional(lovelace -~ other.lovelace, assets -~ other.assets)

        @targetName("subtractCoerce_Unbounded")
        infix def -~(other: Value.Unbounded): Value.Fractional =
            Value.Fractional(lovelace -~ other.lovelace, assets -~ other.assets)

        @targetName("subtractCoerce_Fractional")
        infix def -~(other: Value.Fractional): Value.Fractional = this - other
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

    def partialCompareImpl[C, MA](
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
        if coinComparison == assetsComparison then coinComparison else Double.NaN
    }
}
