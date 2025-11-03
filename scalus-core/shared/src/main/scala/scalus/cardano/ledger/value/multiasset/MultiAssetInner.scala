package scalus.cardano.ledger.value.multiasset

import scalus.cardano.ledger.AssetName
import scalus.cardano.ledger.value.coin.Coin
import spire.algebra.*
import spire.math.{Rational, SafeLong}
import spire.implicits.{additiveGroupOps, additiveSemigroupOps, rms, seqOps, toRational, vectorSpaceOps, IntAlgebra}

import scala.annotation.targetName
import Multiset.*

import scala.collection.immutable.SortedMap

private object MultiAssetInner {

    // ===================================
    // MultiAsset.Inner
    // ===================================

    type Inner = Inner.Inner

    object Inner {
        import Coin.AlgebraFull as vAlgebra

        opaque type Inner = Multiset[AssetName, Coin, vAlgebra.type, Order[AssetName]]

        def from(m: Multiset[AssetName, Coin, vAlgebra.type, Order[AssetName]]): Inner = m

        def apply(sortedMap: SortedMap[AssetName, Coin]): Inner =
            Multiset(sortedMap)(using vMonoid = vAlgebra)

        def empty: Inner = Multiset.empty(using vMonoid = vAlgebra)
        def zero: Inner = empty

        extension (self: Inner)
            def underlying: Multiset[AssetName, Coin, vAlgebra.type, Order[AssetName]] = self

            def get(assetName: AssetName): Coin = underlying.get(assetName)

            def filter(f: (assetName: AssetName, coin: Coin) => Boolean): Inner =
                underlying.filter(f.tupled)

            def filterCoins(f: (coin: Coin) => Boolean): Inner =
                filter((_, c: Coin) => f(c))

            @targetName("negate")
            infix def unary_- : Unbounded = Unbounded.from(self.mapValues(-_))

            @targetName("addCoerce_Inner")
            infix def +~(other: Inner): Unbounded = Unbounded.from(
              Multiset.combineWith(self, other)(_ +~ _)(using
                vSelfMonoid = vAlgebra,
                vOtherMonoid = vAlgebra
              )
            )

            @targetName("addCoerce_Unbounded")
            infix def +~(other: Unbounded): Unbounded = Unbounded.from(
              Multiset.combineWith(self, other.underlying)(_ +~ _)(using vSelfMonoid = vAlgebra)
            )

            @targetName("addCoerce_Fractional")
            infix def +~(other: Fractional): Fractional = Fractional.from(
              Multiset.combineWith(self, other.underlying)(_ +~ _)(using vSelfMonoid = vAlgebra)
            )

            @targetName("subtractCoerce_Inner")
            infix def -~(other: Inner): Unbounded = Unbounded.from(
              Multiset.combineWith(self, other)(_ -~ _)(using
                vSelfMonoid = vAlgebra,
                vOtherMonoid = vAlgebra
              )
            )

            @targetName("subtractCoerce_Unbounded")
            infix def -~(other: Unbounded): Unbounded = Unbounded.from(
              Multiset.combineWith(self, other.underlying)(_ -~ _)(using vSelfMonoid = vAlgebra)
            )

            @targetName("subtractCoerce_Fractional")
            infix def -~(other: Fractional): Fractional = Fractional.from(
              Multiset.combineWith(self, other.underlying)(_ -~ _)(using vSelfMonoid = vAlgebra)
            )

            @targetName("scale")
            infix def *~(c: SafeLong): Unbounded = Unbounded.from(self.mapValues(_ *~ c))

            @targetName("scale")
            infix def *~(c: Rational): Fractional = Fractional.from(self.mapValues(_ *~ c))

            @targetName("div")
            infix def /~(c: SafeLong): Fractional = Fractional.from(self.mapValues(_ /~ c))

            @targetName("div")
            infix def /~(c: Rational): Fractional = Fractional.from(self.mapValues(_ /~ c))

        extension (self: Iterable[Inner])
            def multiAssetSum: Unbounded = self.foldRight(Unbounded.empty)(_ +~ _)

        given algebra: Algebra.type = Algebra

        object Algebra extends InnerPartialOrder

        /** This is available for manual import, but it isn't implicitly given to users because
          * adding `Long` Inners is unsafe (it can overflow/underflow without warning/error).
          */
        object AlgebraFull extends InnerPartialOrder, InnerAdditiveMonoid

        private object M
            extends Multiset.Algebra[AssetName, Coin, vAlgebra.type, Order[AssetName]](using
              vMonoid = vAlgebra
            )

        trait InnerPartialOrder extends PartialOrder[Inner] {
            private object A extends M.PartialOrder.OrderedElements(vAlgebra.compare)

            override def eqv(self: Inner, other: Inner): Boolean = A.eqv(self, other)
            override def partialCompare(x: Inner, y: Inner): Double =
                A.partialCompare(x, y)
        }

        trait InnerAdditiveMonoid extends AdditiveMonoid[Inner] {
            private object A extends M.AdditiveMonoid

            override def zero: Inner = A.zero
            override def plus(x: Inner, y: Inner): Inner = A.plus(x, y)
        }

        enum ArithmeticError extends Throwable:
            def assetName: AssetName
            case Underflow(assetName: AssetName) extends ArithmeticError
            case Overflow(assetName: AssetName) extends ArithmeticError

        given eqArithmeticError: Eq[ArithmeticError] = Eq.fromUniversalEquals

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
        import Coin.Unbounded.Algebra as vAlgebra

        opaque type Unbounded =
            Multiset[AssetName, Coin.Unbounded, vAlgebra.type, Order[AssetName]]

        def from(
            m: Multiset[AssetName, Coin.Unbounded, vAlgebra.type, Order[AssetName]]
        ): Unbounded = m

        def apply(sortedMap: SortedMap[AssetName, Coin.Unbounded]): Unbounded = Multiset(sortedMap)

        def empty: Unbounded = Multiset.empty
        def zero: Unbounded = empty

        extension (self: Unbounded)
            def underlying: Multiset[AssetName, Coin.Unbounded, vAlgebra.type, Order[AssetName]] =
                self

            def toInner: Either[Inner.ArithmeticError, Inner] =
                try {
                    Right(self.unsafeToInner)
                } catch {
                    case e: Inner.ArithmeticError => Left(e)
                }

            def unsafeToInner: Inner = Inner.from(
              self.mapValuesIndexed((assetName: AssetName, coinUnbounded: Coin.Unbounded) =>
                  try {
                      coinUnbounded.unsafeToCoin
                  } catch {
                      case e: Coin.ArithmeticError =>
                          throw Inner.ArithmeticError.withAssetName(e, assetName)
                  }
              )(using vNewMonoid = Coin.AlgebraFull)
            )

            def toFractional: Fractional =
                Fractional.from(self.mapValues(_.toFractional))

            def get(assetName: AssetName): Coin.Unbounded = underlying.get(assetName)

            def filter(f: (assetName: AssetName, coin: Coin.Unbounded) => Boolean): Unbounded =
                underlying.filter(f.tupled)

            def filterCoins(f: (coin: Coin.Unbounded) => Boolean): Unbounded =
                filter((_, c: Coin.Unbounded) => f(c))

            def positives: Unbounded = filterCoins(_.underlying > 0)

            def negatives: Unbounded = filterCoins(_.underlying < 0)

            @targetName("addCoerce_Inner")
            infix def +~(other: Inner): Unbounded = Unbounded.from(
              Multiset.combineWith(self, other.underlying)(_ +~ _)(using
                vOtherMonoid = Coin.AlgebraFull
              )
            )

            @targetName("addCoerce_Unbounded")
            infix def +~(other: Unbounded): Unbounded = self + other

            @targetName("addCoerce_Fractional")
            infix def +~(other: Fractional): Fractional = Fractional.from(
              Multiset.combineWith(self, other.underlying)(_ +~ _)
            )

            @targetName("subtractCoerce_Inner")
            infix def -~(other: Inner): Unbounded = Unbounded.from(
              Multiset.combineWith(self, other.underlying)(_ -~ _)(using
                vOtherMonoid = Coin.AlgebraFull
              )
            )

            @targetName("subtractCoerce_Unbounded")
            infix def -~(other: Unbounded): Unbounded = self - other

            @targetName("subtractCoerce_Fractional")
            infix def -~(other: Fractional): Fractional = Fractional.from(
              Multiset.combineWith(self, other.underlying)(_ -~ _)
            )

            @targetName("scale")
            infix def *~(c: SafeLong): Unbounded = self :* c

            @targetName("scale")
            infix def *~(c: Rational): Fractional = Fractional.from(self.mapValues(_ *~ c))

            @targetName("div")
            infix def /~(c: SafeLong): Fractional = Fractional.from(self.mapValues(_ /~ c))

            @targetName("div")
            infix def /~(c: Rational): Fractional = Fractional.from(self.mapValues(_ /~ c))

        extension (self: Iterable[Unbounded]) def multiAssetSum: Unbounded = self.qsum

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Unbounded], CModule[Unbounded, SafeLong] {
            private object M
                extends Multiset.Algebra[AssetName, Coin.Unbounded, vAlgebra.type, Order[AssetName]]

            private object A
                extends M.PartialOrder.OrderedElements(vAlgebra.compare),
                  M.CModule[SafeLong, CRing[SafeLong], vAlgebra.type]

            override def eqv(self: Unbounded, other: Unbounded): Boolean = A.eqv(self, other)
            override def partialCompare(x: Unbounded, y: Unbounded): Double =
                A.partialCompare(x, y)

            override def zero: Unbounded = A.zero
            override def plus(x: Unbounded, y: Unbounded): Unbounded = A.plus(x, y)

            override def negate(x: Unbounded): Unbounded = A.negate(x)
            override def minus(x: Unbounded, y: Unbounded): Unbounded = A.minus(x, y)

            override def scalar: CRing[SafeLong] = A.scalar
            override def timesl(r: SafeLong, v: Unbounded): Unbounded = A.timesl(r, v)
        }
    }

    // ===================================
    // MultiAsset.Inner.Fractional
    // ===================================

    type Fractional = Fractional.Fractional

    object Fractional {
        import Coin.Fractional.Algebra as vAlgebra

        opaque type Fractional =
            Multiset[AssetName, Coin.Fractional, vAlgebra.type, Order[AssetName]]

        def from(
            m: Multiset[AssetName, Coin.Fractional, vAlgebra.type, Order[AssetName]]
        ): Fractional = m

        def apply(sortedMap: SortedMap[AssetName, Coin.Fractional]): Fractional = Multiset(
          sortedMap
        )

        def empty: Fractional = Multiset.empty
        def zero: Fractional = empty

        extension (self: Fractional)
            def underlying: Multiset[AssetName, Coin.Fractional, vAlgebra.type, Order[
              AssetName
            ]] = self

            def toUnbounded: Unbounded =
                Unbounded.from(self.mapValues(_.toUnbounded))

            def toInner: Either[Inner.ArithmeticError, Inner] =
                try {
                    Right(self.unsafeToInner)
                } catch {
                    case e: Inner.ArithmeticError => Left(e)
                }

            def unsafeToInner: Inner = Inner.from(
              self.mapValuesIndexed((assetName: AssetName, coinFractional: Coin.Fractional) =>
                  try {
                      coinFractional.unsafeToCoin
                  } catch {
                      case e: Coin.ArithmeticError =>
                          throw Inner.ArithmeticError.withAssetName(e, assetName)
                  }
              )(using vNewMonoid = Coin.AlgebraFull)
            )

            def get(assetName: AssetName): Coin.Fractional = underlying.get(assetName)

            def filter(f: (assetName: AssetName, coin: Coin.Fractional) => Boolean): Fractional =
                underlying.filter(f.tupled)

            def filterCoins(f: (coin: Coin.Fractional) => Boolean): Fractional =
                filter((_, c: Coin.Fractional) => f(c))

            def positives: Fractional = filterCoins(_.underlying > 0)

            def negatives: Fractional = filterCoins(_.underlying < 0)

            @targetName("addCoerce_Inner")
            infix def +~(other: Inner): Fractional = Fractional.from(
              Multiset.combineWith(self, other.underlying)(_ +~ _)(using
                vOtherMonoid = Coin.AlgebraFull
              )
            )

            @targetName("addCoerce_Unbounded")
            infix def +~(other: Unbounded): Fractional = Fractional.from(
              Multiset.combineWith(self, other.underlying)(_ +~ _)
            )

            @targetName("addCoerce_Fractional")
            infix def +~(other: Fractional): Fractional = self + other

            @targetName("subtractCoerce_Inner")
            infix def -~(other: Inner): Fractional = Fractional.from(
              Multiset.combineWith(self, other.underlying)(_ -~ _)(using
                vOtherMonoid = Coin.AlgebraFull
              )
            )

            @targetName("subtractCoerce_Unbounded")
            infix def -~(other: Unbounded): Fractional = Fractional.from(
              Multiset.combineWith(self, other.underlying)(_ -~ _)
            )

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

        extension (self: Iterable[Fractional]) def multiAssetSum: Fractional = self.qsum

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Fractional], VectorSpace[Fractional, Rational] {
            private object M
                extends Multiset.Algebra[AssetName, Coin.Fractional, vAlgebra.type, Order[
                  AssetName
                ]]

            private object A
                extends M.PartialOrder.OrderedElements(vAlgebra.compare),
                  M.VectorSpace[Rational, Field[Rational], vAlgebra.type]

            override def eqv(self: Fractional, other: Fractional): Boolean = A.eqv(self, other)
            override def partialCompare(x: Fractional, y: Fractional): Double =
                A.partialCompare(x, y)

            override def zero: Fractional = A.zero
            override def plus(x: Fractional, y: Fractional): Fractional = A.plus(x, y)

            override def negate(x: Fractional): Fractional = A.negate(x)
            override def minus(x: Fractional, y: Fractional): Fractional = A.minus(x, y)

            override def scalar: Field[Rational] = A.scalar
            override def timesl(r: Rational, v: Fractional): Fractional = A.timesl(r, v)
        }

    }
}
