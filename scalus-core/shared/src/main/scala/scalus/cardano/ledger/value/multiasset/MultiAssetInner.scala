package scalus.cardano.ledger.value.multiasset

import scalus.cardano.ledger.AssetName
import scalus.cardano.ledger.value.coin.Coin
import scalus.cardano.ledger.value.multiasset.SortedMapUtils.Canonical
import scalus.cardano.ledger.value.multiasset.SortedMapUtils.CombineWith.*
import scalus.cardano.ledger.value.multiasset.SortedMapUtils.SortedMapPartialOrder
import spire.algebra.*
import spire.math.{Rational, SafeLong}

import spire.implicits.*
import spire.implicits.MapEq as _
import spire.implicits.{MapMonoid as _, MapCSemiring as _}
import spire.implicits.{MapCRng as _, MapGroup as _}
import spire.implicits.{ MapVectorSpace as _, MapInnerProductSpace as _}

import scala.collection.immutable.SortedMap

private object MultiAssetInner {
    // ===================================
    // MultiAsset.Inner
    // ===================================

    type Inner = Inner.Inner

    object Inner {
        opaque type Inner = SortedMap[AssetName, Coin]

        def apply(x: SortedMap[AssetName, Coin]): Inner =
            Canonical.sortedMap(x)(using vMonoid = Coin.AdditiveMonoid)

        def zero: Inner = SortedMap.empty

        extension (self: Inner)
            def underlying: SortedMap[AssetName, Coin] = self

            def scaleIntegral[I](c: I)(using int: spire.math.Integral[I]): Unbounded =
                Unbounded(self.view.mapValues(_.scaleIntegral(c)).to(SortedMap))

            def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional = {
                Fractional(self.view.mapValues(_.scaleFractional(c)).to(SortedMap))
            }

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Inner] {
            override def eqv(self: Inner, other: Inner): Boolean = self == other

            override def partialCompare(self: Inner, other: Inner): Double =
                mapPartialOrder.partialCompare(self, other)

            private val mapPartialOrder = SortedMapPartialOrder[AssetName, Coin, Int](
                // If both keys exist, compare the values.
                // If only the left key exists, compare the left value against zero.
                // If only the right key exists, compare the right value against zero.
                compareBoth = Coin.algebra.compare,
                compareLeft = _.signum,
                compareRight = -_.signum
            )
        }

        // This AdditiveMonoid is available for manual import, but it isn't implicitly given to users
        // because adding `Long` coins is unsafe (it can overflow/underflow without warning/error).
        object AdditiveMonoid extends AdditiveMonoid[Inner] {
            override def zero: Inner = Inner.zero
            override def plus(self: Inner, other: Inner): Inner =
                combineWith(Coin.AdditiveMonoid.plus)(self, other)(using
                  vResultMonoid = Coin.AdditiveMonoid
                )
        }

        enum ArithmeticError extends Throwable:
            def assetName: AssetName
            case Underflow(assetName: AssetName) extends ArithmeticError
            case Overflow(assetName: AssetName) extends ArithmeticError

        object ArithmeticError {
            def withAssetName(
                e: Coin.ArithmeticError,
                assetName: AssetName
            ): Inner.ArithmeticError =
                e match {
                    case Coin.ArithmeticError.Underflow =>
                        Inner.ArithmeticError.Underflow(assetName)
                    case Coin.ArithmeticError.Overflow => Inner.ArithmeticError.Overflow(assetName)
                }

        }
    }

    // ===================================
    // MultiAsset.Inner.Unbounded
    // ===================================

    type Unbounded = Unbounded.Unbounded

    object Unbounded {
        opaque type Unbounded = SortedMap[AssetName, Coin.Unbounded]

        def apply(x: SortedMap[AssetName, Coin.Unbounded]): Unbounded = Canonical.sortedMap(x)

        def zero: Unbounded = SortedMap.empty

        import Coin.Unbounded.algebra as coinAlgebra

        extension (self: Unbounded)
            def underlying: SortedMap[AssetName, Coin.Unbounded] = self

            def toInner: Either[Inner.ArithmeticError, Inner] =
                try {
                    Right(self.unsafeToInner)
                } catch {
                    case e: Inner.ArithmeticError => Left(e)
                }

            def unsafeToInner: Inner = Inner(
              self.view
                  .map((assetName: AssetName, coinUnbounded: Coin.Unbounded) =>
                      try {
                          (assetName, coinUnbounded.unsafeToCoin)
                      } catch {
                          case e: Coin.ArithmeticError =>
                              throw Inner.ArithmeticError.withAssetName(e, assetName)
                      }
                  )
                  .to(SortedMap)
            )

            def toFractional: Fractional =
                Fractional(self.view.mapValues(_.toCoinFractional).to(SortedMap))

            def scaleIntegral[I](c: I)(using int: spire.math.Integral[I]): Unbounded =
                self :* c.toSafeLong

            def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional = {
                Fractional(self.view.mapValues(_.scaleFractional(c)).to(SortedMap))
            }

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Unbounded], CModule[Unbounded, SafeLong] {
            override def eqv(self: Unbounded, other: Unbounded): Boolean = self == other

            override def partialCompare(self: Unbounded, other: Unbounded): Double =
                mapPartialOrder.partialCompare(self, other)

            override def scalar: CRing[SafeLong] = CRing[SafeLong]

            override def zero: Unbounded = Unbounded.zero

            override def negate(self: Unbounded): Unbounded =
                self.view.mapValues(coinAlgebra.negate).to(SortedMap)

            override def plus(self: Unbounded, other: Unbounded): Unbounded =
                combineWith(coinAlgebra.plus)(self, other)

            override def minus(self: Unbounded, other: Unbounded): Unbounded =
                combineWith(coinAlgebra.minus, identity, coinAlgebra.negate)(self, other)

            override def timesl(s: SafeLong, self: Unbounded): Unbounded =
                self.view.mapValues(_ :* s).to(SortedMap)

            private val mapPartialOrder =
                SortedMapPartialOrder[AssetName, Coin.Unbounded, Int](
                    // If both keys exist, compare the values.
                    // If only the left key exists, compare the left value against zero.
                    // If only the right key exists, compare the right value against zero.
                    compareBoth = coinAlgebra.compare,
                    compareLeft = _.signum,
                    compareRight = -_.signum
                )
        }
    }

    // ===================================
    // MultiAsset.Inner.Fractional
    // ===================================

    type Fractional = Fractional.Fractional

    object Fractional {
        opaque type Fractional = SortedMap[AssetName, Coin.Fractional]

        def apply(x: SortedMap[AssetName, Coin.Fractional]): Fractional = Canonical.sortedMap(x)

        def zero: Fractional = SortedMap.empty

        extension (self: Fractional)
            def underlying: SortedMap[AssetName, Coin.Fractional] = self

            def toUnbounded: Unbounded =
                Unbounded(self.view.mapValues(_.toUnbounded).to(SortedMap))

            def toInner: Either[Inner.ArithmeticError, Inner] =
                try {
                    Right(self.unsafeToInner)
                } catch {
                    case e: Inner.ArithmeticError => Left(e)
                }

            def unsafeToInner: Inner = Inner(
              self.view
                  .map((assetName: AssetName, coinFractional: Coin.Fractional) =>
                      try {
                          (assetName, coinFractional.unsafeToCoin)
                      } catch {
                          case e: Coin.ArithmeticError =>
                              throw Inner.ArithmeticError.withAssetName(e, assetName)
                      }
                  )
                  .to(SortedMap)
            )

            def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional =
                self :* c.toRational

        import Coin.Fractional.algebra as coinAlgebra

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Fractional], VectorSpace[Fractional, Rational] {
            override def eqv(self: Fractional, other: Fractional): Boolean = self == other

            override def partialCompare(self: Fractional, other: Fractional): Double =
                mapPartialOrder.partialCompare(self, other)

            override def scalar: Field[Rational] = Field[Rational]

            override def zero: Fractional = Fractional.zero

            override def negate(self: Fractional): Fractional =
                self.view.mapValues(coinAlgebra.negate).to(SortedMap)

            override def plus(self: Fractional, other: Fractional): Fractional =
                combineWith(coinAlgebra.plus)(self, other)

            override def minus(self: Fractional, other: Fractional): Fractional =
                combineWith(coinAlgebra.minus, identity, coinAlgebra.negate)(self, other)

            override def timesl(s: Rational, self: Fractional): Fractional =
                self.view.mapValues(_ :* s).to(SortedMap)

            private val mapPartialOrder =
                SortedMapPartialOrder[AssetName, Coin.Fractional, Int](
                    compareBoth = coinAlgebra.compare,
                    compareLeft = _.signum,
                    compareRight = -_.signum
                )
        }
    }
}
