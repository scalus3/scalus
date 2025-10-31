package scalus.cardano.ledger.value.multiasset

import scalus.cardano.ledger.AssetName
import scalus.cardano.ledger.value.coin.Coin
import scalus.cardano.ledger.value.multiasset.CanonicalSortedMap
import scalus.cardano.ledger.value.multiasset.CanonicalSortedMap.*
import spire.algebra.*
import spire.math.{Rational, SafeLong}
import spire.implicits.*
import spire.implicits.MapEq as _
import spire.implicits.{MapCSemiring as _, MapMonoid as _}
import spire.implicits.{MapCRng as _, MapGroup as _}
import spire.implicits.{MapInnerProductSpace as _, MapVectorSpace as _}

import scala.annotation.targetName
import scala.collection.immutable.SortedMap

private object MultiAssetInner {
    // ===================================
    // MultiAsset.Inner
    // ===================================

    type Inner = Inner.Inner

    object Inner {
        opaque type Inner = CanonicalSortedMap[AssetName, Coin]

        @targetName("applyWithCanonicalSortedMap")
        def apply(self: CanonicalSortedMap[AssetName, Coin]): Inner = self

        @targetName("applyWithSortedMap")
        def apply(sortedMap: SortedMap[AssetName, Coin]): Inner =
            CanonicalSortedMap(sortedMap)(using vMonoid = Coin.AdditiveMonoid)

        def zero: Inner = CanonicalSortedMap.empty

        extension (self: Inner)
            def underlying: CanonicalSortedMap[AssetName, Coin] = self

            def scaleIntegral[I](c: I)(using int: spire.math.Integral[I]): Unbounded =
                Unbounded(self.mapValues(_.scaleIntegral(c)))

            def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional = {
                Fractional(self.mapValues(_.scaleFractional(c)))
            }

            @targetName("addCoerce_Inner")
            infix def +~(other: Inner): Unbounded = Unbounded(
              combineWith[AssetName, Coin, Coin, Coin.Unbounded](_ +~ _, self, other)(using
                vSelfMonoid = Coin.AdditiveMonoid,
                vOtherMonoid = Coin.AdditiveMonoid
              )
            )

            @targetName("addCoerce_Unbounded")
            infix def +~(other: Unbounded): Unbounded = Unbounded(
              combineWith[AssetName, Coin, Coin.Unbounded, Coin.Unbounded](
                _ +~ _,
                self,
                other.underlying
              )(using vSelfMonoid = Coin.AdditiveMonoid)
            )

            @targetName("addCoerce_Fractional")
            infix def +~(other: Fractional): Fractional = Fractional(
              combineWith[AssetName, Coin, Coin.Fractional, Coin.Fractional](
                _ +~ _,
                self,
                other.underlying
              )(using vSelfMonoid = Coin.AdditiveMonoid)
            )

            @targetName("subtractCoerce_Inner")
            infix def -~(other: Inner): Unbounded = Unbounded(
              combineWith[AssetName, Coin, Coin, Coin.Unbounded](_ -~ _, self, other)(using
                vSelfMonoid = Coin.AdditiveMonoid,
                vOtherMonoid = Coin.AdditiveMonoid
              )
            )

            @targetName("subtractCoerce_Unbounded")
            infix def -~(other: Unbounded): Unbounded = Unbounded(
              combineWith[AssetName, Coin, Coin.Unbounded, Coin.Unbounded](
                _ -~ _,
                self,
                other.underlying
              )(using vSelfMonoid = Coin.AdditiveMonoid)
            )

            @targetName("subtractCoerce_Fractional")
            infix def -~(other: Fractional): Fractional = Fractional(
              combineWith[AssetName, Coin, Coin.Fractional, Coin.Fractional](
                _ -~ _,
                self,
                other.underlying
              )(using vSelfMonoid = Coin.AdditiveMonoid)
            )

            @targetName("negate")
            infix def unary_- : Unbounded = Unbounded(self.mapValues(-_))

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Inner] {
            override def eqv(self: Inner, other: Inner): Boolean = self == other

            override def partialCompare(self: Inner, other: Inner): Double =
                mapPartialOrder.partialCompare(self, other)

            private val mapPartialOrder =
                CanonicalSortedMapOrder[AssetName, Coin](Coin.algebra.compare)(using
                  vMonoid = Coin.AdditiveMonoid
                )
        }

        // This AdditiveMonoid is available for manual import, but it isn't implicitly given to users
        // because adding `Long` coins is unsafe (it can overflow/underflow without warning/error).
        object AdditiveMonoid extends AdditiveMonoid[Inner] {
            override def zero: Inner = Inner.zero
            override def plus(self: Inner, other: Inner): Inner =
                combineWith(Coin.AdditiveMonoid.plus, self, other)(using
                  vSelfMonoid = Coin.AdditiveMonoid,
                  vOtherMonoid = Coin.AdditiveMonoid,
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
        opaque type Unbounded = CanonicalSortedMap[AssetName, Coin.Unbounded]

        @targetName("applyWithCanonicalSortedMap")
        def apply(self: CanonicalSortedMap[AssetName, Coin.Unbounded]): Unbounded = self

        @targetName("applyWithSortedMap")
        def apply(sortedMap: SortedMap[AssetName, Coin.Unbounded]): Unbounded = CanonicalSortedMap(
          sortedMap
        )

        def zero: Unbounded = CanonicalSortedMap.empty

        import Coin.Unbounded.algebra as coinAlgebra

        extension (self: Unbounded)
            def underlying: CanonicalSortedMap[AssetName, Coin.Unbounded] = self

            def toInner: Either[Inner.ArithmeticError, Inner] =
                try {
                    Right(self.unsafeToInner)
                } catch {
                    case e: Inner.ArithmeticError => Left(e)
                }

            def unsafeToInner: Inner = Inner(
              self.mapValuesIndexed((assetName: AssetName, coinUnbounded: Coin.Unbounded) =>
                  try { coinUnbounded.unsafeToCoin }
                  catch {
                      case e: Coin.ArithmeticError =>
                          throw Inner.ArithmeticError.withAssetName(e, assetName)
                  }
              )(using vNewMonoid = Coin.AdditiveMonoid)
            )

            def toFractional: Fractional =
                Fractional(self.mapValues(_.toFractional))

            def scaleIntegral[I](c: I)(using int: spire.math.Integral[I]): Unbounded =
                self :* c.toSafeLong

            def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional = {
                Fractional(self.mapValues(_.scaleFractional(c)))
            }

            @targetName("addCoerce_Inner")
            infix def +~(other: Inner): Unbounded = Unbounded(
              combineWith[AssetName, Coin.Unbounded, Coin, Coin.Unbounded](
                _ +~ _,
                self,
                other.underlying
              )(using vOtherMonoid = Coin.AdditiveMonoid)
            )

            @targetName("addCoerce_Unbounded")
            infix def +~(other: Unbounded): Unbounded = self + other

            @targetName("addCoerce_Fractional")
            infix def +~(other: Fractional): Fractional = Fractional(
              combineWith[AssetName, Coin.Unbounded, Coin.Fractional, Coin.Fractional](
                _ +~ _,
                self,
                other.underlying
              )
            )

            @targetName("subtractCoerce_Inner")
            infix def -~(other: Inner): Unbounded = Unbounded(
              combineWith[AssetName, Coin.Unbounded, Coin, Coin.Unbounded](
                _ -~ _,
                self,
                other.underlying
              )(using vOtherMonoid = Coin.AdditiveMonoid)
            )

            @targetName("subtractCoerce_Unbounded")
            infix def -~(other: Unbounded): Unbounded = self - other

            @targetName("subtractCoerce_Fractional")
            infix def -~(other: Fractional): Fractional = Fractional(
              combineWith[AssetName, Coin.Unbounded, Coin.Fractional, Coin.Fractional](
                _ -~ _,
                self,
                other.underlying
              )
            )

        extension (self: Seq[Unbounded]) def sumMultiAssets: Unbounded = self.qsum

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Unbounded], CModule[Unbounded, SafeLong] {
            override def eqv(self: Unbounded, other: Unbounded): Boolean = self == other

            override def partialCompare(self: Unbounded, other: Unbounded): Double =
                mapPartialOrder.partialCompare(self, other)

            override def scalar: CRing[SafeLong] = CRing[SafeLong]

            override def zero: Unbounded = Unbounded.zero

            override def negate(self: Unbounded): Unbounded =
                self.mapValues(coinAlgebra.negate)

            override def plus(self: Unbounded, other: Unbounded): Unbounded =
                combineWith(coinAlgebra.plus, self, other)

            override def minus(self: Unbounded, other: Unbounded): Unbounded =
                combineWith(coinAlgebra.minus, self, other)

            override def timesl(s: SafeLong, self: Unbounded): Unbounded =
                self.mapValues(_ :* s)

            private val mapPartialOrder =
                CanonicalSortedMapOrder[AssetName, Coin.Unbounded](coinAlgebra.compare)
        }
    }

    // ===================================
    // MultiAsset.Inner.Fractional
    // ===================================

    type Fractional = Fractional.Fractional

    object Fractional {
        opaque type Fractional = CanonicalSortedMap[AssetName, Coin.Fractional]

        @targetName("applyWithCanonicalSortedMap")
        def apply(self: CanonicalSortedMap[AssetName, Coin.Fractional]): Fractional = self

        @targetName("applyWithSortedMap")
        def apply(sortedMap: SortedMap[AssetName, Coin.Fractional]): Fractional =
            CanonicalSortedMap(sortedMap)

        def zero: Fractional = CanonicalSortedMap.empty

        extension (self: Fractional)
            def underlying: CanonicalSortedMap[AssetName, Coin.Fractional] = self

            def toUnbounded: Unbounded =
                Unbounded(self.mapValues(_.toUnbounded))

            def toInner: Either[Inner.ArithmeticError, Inner] =
                try {
                    Right(self.unsafeToInner)
                } catch {
                    case e: Inner.ArithmeticError => Left(e)
                }

            def unsafeToInner: Inner = Inner(
              self.mapValuesIndexed((assetName: AssetName, coinFractional: Coin.Fractional) =>
                  try { coinFractional.unsafeToCoin }
                  catch {
                      case e: Coin.ArithmeticError =>
                          throw Inner.ArithmeticError.withAssetName(e, assetName)
                  }
              )(using vNewMonoid = Coin.AdditiveMonoid)
            )

            def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional =
                self :* c.toRational

            @targetName("addCoerce_Inner")
            infix def +~(other: Inner): Fractional = Fractional(
              combineWith[AssetName, Coin.Fractional, Coin, Coin.Fractional](
                _ +~ _,
                self,
                other.underlying
              )(using vOtherMonoid = Coin.AdditiveMonoid)
            )

            @targetName("addCoerce_Unbounded")
            infix def +~(other: Unbounded): Fractional = Fractional(
              combineWith[AssetName, Coin.Fractional, Coin.Unbounded, Coin.Fractional](
                _ +~ _,
                self,
                other.underlying
              )
            )

            @targetName("addCoerce_Fractional")
            infix def +~(other: Fractional): Fractional = self + other

            @targetName("subtractCoerce_Inner")
            infix def -~(other: Inner): Fractional = Fractional(
              combineWith[AssetName, Coin.Fractional, Coin, Coin.Fractional](
                _ -~ _,
                self,
                other.underlying
              )(using vOtherMonoid = Coin.AdditiveMonoid)
            )

            @targetName("subtractCoerce_Unbounded")
            infix def -~(other: Unbounded): Fractional = Fractional(
              combineWith[AssetName, Coin.Fractional, Coin.Unbounded, Coin.Fractional](
                _ -~ _,
                self,
                other.underlying
              )
            )

            @targetName("subtractCoerce_Fractional")
            infix def -~(other: Fractional): Fractional = self - other

        extension (self: Seq[Fractional]) def sumMultiAssets: Fractional = self.qsum

        import Coin.Fractional.algebra as coinAlgebra

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Fractional], VectorSpace[Fractional, Rational] {
            override def eqv(self: Fractional, other: Fractional): Boolean = self == other

            override def partialCompare(self: Fractional, other: Fractional): Double =
                mapPartialOrder.partialCompare(self, other)

            override def scalar: Field[Rational] = Field[Rational]

            override def zero: Fractional = Fractional.zero

            override def negate(self: Fractional): Fractional =
                self.mapValues(coinAlgebra.negate)

            override def plus(self: Fractional, other: Fractional): Fractional =
                combineWith(coinAlgebra.plus, self, other)

            override def minus(self: Fractional, other: Fractional): Fractional =
                combineWith(coinAlgebra.minus, self, other)

            override def timesl(s: Rational, self: Fractional): Fractional =
                self.mapValues(_ :* s)

            private val mapPartialOrder =
                CanonicalSortedMapOrder[AssetName, Coin.Fractional](coinAlgebra.compare)
        }
    }
}
