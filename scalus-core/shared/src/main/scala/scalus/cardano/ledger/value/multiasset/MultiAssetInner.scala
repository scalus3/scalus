package scalus.cardano.ledger.value.multiasset

import scalus.cardano.ledger.AssetName
import scalus.cardano.ledger.value.coin.Coin
import scalus.cardano.ledger.value.multiasset.CanonicalSortedMap.{CanonicalSortedMapPartialOrder, *}
import scalus.cardano.ledger.value.multiasset.CanonicalSortedMap.CombineWith.*
import spire.algebra.*
import spire.implicits.*
import spire.math.{Rational, SafeLong}

private object MultiAssetInner {

    import AssetName.given
    // ===================================
    // MultiAsset.Inner
    // ===================================

    type Inner = Inner.Inner

    object Inner {
        case class Inner(underlying:  CanonicalSortedMap[AssetName, Coin, Coin.Algebra.type ])

        def zero: Inner = Inner(CanonicalSortedMap.empty(using Ordering[AssetName]))

        extension (self: Inner)
            def underlying: CanonicalSortedMap[AssetName, Coin, Coin.Algebra.type ] = self.underlying

            def scaleIntegral[I](c: I)(using int: spire.math.Integral[I]): Unbounded.Unbounded = {
              Unbounded.Unbounded(self.underlying.mapValues(_.scaleIntegral(c)))
            }

            def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional.Fractional = {
                Fractional.Fractional(self.underlying.mapValues(_.scaleFractional(c)))
            }

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Inner] {
            implicit val mi : Monoid[Int] = Monoid.instance(0, (_ + _))
            private val mapPartialOrder = CanonicalSortedMapPartialOrder[AssetName,
              Coin,
              Int,
              Coin.Algebra.type,
              mi.type ](
              // If both keys exist, compare the values.
              // If only the left key exists, compare the left value against zero.
              // If only the right key exists, compare the right value against zero.
              compareBoth = Coin.algebra.compare,
              compareLeft = _.signum,
              compareRight = -_.signum
            )

            override def partialCompare(self: Inner, other: Inner): Double =
                mapPartialOrder.partialCompare(self.underlying, other.underlying)

            override def eqv(x: Inner, y: Inner): Boolean = x == y
        }

        enum ArithmeticError extends Throwable:
            def assetName: AssetName
            case Underflow(assetName: AssetName) extends ArithmeticError
            case Overflow(assetName: AssetName) extends ArithmeticError

        object ArithmeticError {
            def withAssetName(
                e: Coin.ArithmeticError,
                assetName: AssetName
            ): ArithmeticError =
                e match {
                    case Coin.ArithmeticError.Underflow =>
                        ArithmeticError.Underflow(assetName)
                    case Coin.ArithmeticError.Overflow => ArithmeticError.Overflow(assetName)
                }

        }
    }

    // ===================================
    // MultiAsset.Inner.Unbounded
    // ===================================

    type Unbounded = Unbounded.Unbounded

    object Unbounded {
        case class Unbounded(underlying :  CanonicalSortedMap[AssetName, Coin.Unbounded, Coin.Unbounded.Algebra.type])

        def zero: Unbounded = Unbounded(CanonicalSortedMap.empty)

        import Coin.Unbounded.algebra as coinAlgebra

        extension (self: Unbounded)

            def toInner: Either[Inner.ArithmeticError, Inner] =
                try {
                    Right(self.unsafeToInner)
                } catch {
                    case e: Inner.ArithmeticError => Left(e)
                }

            def unsafeToInner: Inner = Inner.Inner(
              self.underlying.map((assetName: AssetName, coinUnbounded: Coin.Unbounded) =>
                      try {
                          (assetName, coinUnbounded.unsafeToCoin)
                      } catch {
                          case e: Coin.ArithmeticError =>
                              throw Inner.ArithmeticError.withAssetName(e, assetName)
                      }
                  )
            )

            def toFractional: Fractional.Fractional =
                Fractional.Fractional(self.underlying.mapValues(_.toCoinFractional))

            def scaleIntegral[I](c: I)(using int: spire.math.Integral[I]): Unbounded =
                self :* c.toSafeLong

            def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional.Fractional = {
                Fractional.Fractional(self.underlying.mapValues(_.scaleFractional(c)))
            }

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Unbounded], CModule[Unbounded, SafeLong] {
            private val mapPartialOrder =
                implicit val mi : Monoid[Int] = Monoid.instance(0, (_ + _))
                CanonicalSortedMapPartialOrder[AssetName, Coin.Unbounded, Int, Coin.Unbounded.Algebra.type, mi.type ](
                  // If both keys exist, compare the values.
                  // If only the left key exists, compare the left value against zero.
                  // If only the right key exists, compare the right value against zero.
                  compareBoth = coinAlgebra.compare,
                  compareLeft = _.signum,
                  compareRight = -_.signum
                )

            override def partialCompare(self: Unbounded, other: Unbounded): Double =
                mapPartialOrder.partialCompare(self.underlying, other.underlying)

            override def scalar: CRing[SafeLong] = CRing[SafeLong]

            override def zero: Unbounded = Unbounded(CanonicalSortedMap.empty)

            override def negate(self: Unbounded): Unbounded =
                Unbounded(self.underlying.mapValues(coinAlgebra.negate))

            override def plus(self: Unbounded, other: Unbounded): Unbounded = {
              Unbounded(combineWith(coinAlgebra.plus)(self.underlying, other.underlying))
            }

            override def minus(self: Unbounded, other: Unbounded): Unbounded =
                Unbounded(combineWith(coinAlgebra.minus, identity, coinAlgebra.negate)
                  (self.underlying, other.underlying))

            override def timesl(s: SafeLong, self: Unbounded): Unbounded =
                Unbounded(self.underlying.mapValues(_ :* s))

            override def eqv(x: Unbounded, y: Unbounded): Boolean = x == y
        }
    }

    // ===================================
    // MultiAsset.Inner.Fractional
    // ===================================

    type Fractional = Fractional.Fractional

    object Fractional {
        case class Fractional(underlying : CanonicalSortedMap[AssetName, Coin.Fractional, Coin.Fractional.Algebra.type ])

        def zero: Fractional = Fractional(CanonicalSortedMap.empty)

        extension (self: Fractional)

            def toUnbounded: Unbounded =
                Unbounded.Unbounded(self.underlying.mapValues(_.toUnbounded))

            def toInner: Either[Inner.ArithmeticError, Inner] =
                try {
                    Right(self.unsafeToInner)
                } catch {
                    case e: Inner.ArithmeticError => Left(e)
                }

            def unsafeToInner: Inner = Inner.Inner(
              self.underlying
                  .map((assetName: AssetName, coinFractional: Coin.Fractional) =>
                      try {
                          (assetName, coinFractional.unsafeToCoin)
                      } catch {
                          case e: Coin.ArithmeticError =>
                              throw Inner.ArithmeticError.withAssetName(e, assetName)
                      }
                  )
            )

            def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional =
                self :* c.toRational

        import Coin.Fractional.algebra as coinAlgebra

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Fractional], VectorSpace[Fractional, Rational] {
            implicit val mi: Monoid[Int] = Monoid.instance(0, (_ + _))
            private val mapPartialOrder =
                CanonicalSortedMapPartialOrder[AssetName, Coin.Fractional, Int, Coin.Fractional.Algebra.type, mi.type](
                  compareBoth = coinAlgebra.compare,
                  compareLeft = _.signum,
                  compareRight = -_.signum
                )

            override def partialCompare(self: Fractional, other: Fractional): Double =
                mapPartialOrder.partialCompare(self.underlying, other.underlying)

            override def scalar: Field[Rational] = Field[Rational]

            override def zero: Fractional = Fractional(CanonicalSortedMap.empty)

            override def negate(self: Fractional): Fractional =
                Fractional(self.underlying.mapValues(coinAlgebra.negate))

            override def plus(self: Fractional, other: Fractional): Fractional =
                Fractional(combineWith(coinAlgebra.plus)(self.underlying, other.underlying))

            override def minus(self: Fractional, other: Fractional): Fractional =
                Fractional(combineWith(coinAlgebra.minus, identity, coinAlgebra.negate)
                  (self.underlying, other.underlying))

            override def timesl(s: Rational, self: Fractional): Fractional =
                Fractional(self.underlying.mapValues(_ :* s))

            override def eqv(x: Fractional, y: Fractional): Boolean = x == y
        }
    }
}
