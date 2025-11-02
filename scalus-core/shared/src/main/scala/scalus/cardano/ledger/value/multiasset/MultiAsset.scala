package scalus.cardano.ledger.value.multiasset

import scalus.cardano.ledger.value.coin.Coin
import scalus.cardano.ledger.{AssetName, PolicyId}
import spire.algebra.*
import spire.math.{Rational, SafeLong}
import spire.implicits.{additiveGroupOps, additiveSemigroupOps, rms, seqOps, toRational, vectorSpaceOps, DoubleAlgebra}

import scala.annotation.targetName
import Multiset.*

import scala.collection.immutable.SortedMap

// ===================================
// MultiAsset
// ===================================

type MultiAsset = MultiAsset.MultiAsset

object MultiAsset {
    import Inner.AlgebraFull as vAlgebra

    type InnerType = Inner

    opaque type MultiAsset = Multiset[PolicyId, InnerType, vAlgebra.type, Order[PolicyId]]

    def from(m: Multiset[PolicyId, InnerType, vAlgebra.type, Order[PolicyId]]): MultiAsset = m

    def apply(s: SortedMap[PolicyId, InnerType]): MultiAsset = Multiset(s)(using vMonoid = vAlgebra)

    def empty: MultiAsset = Multiset.empty(using vMonoid = vAlgebra)
    def zero: MultiAsset = empty

    extension (self: MultiAsset)
        def underlying: Multiset[PolicyId, InnerType, vAlgebra.type, Order[PolicyId]] = self

        def get(policyId: PolicyId): Inner = underlying.get(policyId)

        def get(policyId: PolicyId, assetName: AssetName): Coin =
            underlying.get(policyId).get(assetName)

        @targetName("negate")
        infix def unary_- : Unbounded = Unbounded.from(self.mapValues(-_))

        @targetName("addCoerce_Inner")
        infix def +~(other: MultiAsset): Unbounded = Unbounded.from(
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
        infix def -~(other: MultiAsset): Unbounded = Unbounded.from(
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

    extension (self: Iterable[MultiAsset])
        def multiAssetSum: Unbounded = self.foldRight(Unbounded.empty)(_ +~ _)

    given algebra: Algebra.type = Algebra

    object Algebra extends InnerPartialOrder

    /** This is available for manual import, but it isn't implicitly given to users because adding
      * `Long` Inners is unsafe (it can overflow/underflow without warning/error).
      */
    object AlgebraFull extends InnerPartialOrder, InnerAdditiveMonoid

    private object M
        extends Multiset.Algebra[PolicyId, InnerType, vAlgebra.type, Order[PolicyId]](using
          vMonoid = vAlgebra
        )

    trait InnerPartialOrder extends PartialOrder[MultiAsset] {
        private object A extends M.PartialOrder.PartiallyOrderedElements(vAlgebra.partialCompare)

        override def eqv(self: MultiAsset, other: MultiAsset): Boolean = A.eqv(self, other)
        override def partialCompare(x: MultiAsset, y: MultiAsset): Double =
            A.partialCompare(x, y)
    }

    trait InnerAdditiveMonoid extends AdditiveMonoid[MultiAsset] {
        private object A extends M.AdditiveMonoid

        override def zero: MultiAsset = A.zero
        override def plus(x: MultiAsset, y: MultiAsset): MultiAsset = A.plus(x, y)
    }

    enum ArithmeticError extends Throwable:
        def policyId: PolicyId
        def assetName: AssetName

        case Underflow(policyId: PolicyId, assetName: AssetName) extends ArithmeticError
        case Overflow(policyId: PolicyId, assetName: AssetName) extends ArithmeticError

    given eqArithmeticError: Eq[ArithmeticError] = Eq.fromUniversalEquals

    object ArithmeticError {
        def withPolicyId(e: Inner.ArithmeticError, policyId: PolicyId): MultiAsset.ArithmeticError =
            e match {
                case Inner.ArithmeticError.Underflow(tn) =>
                    MultiAsset.ArithmeticError.Underflow(policyId, tn)
                case Inner.ArithmeticError.Overflow(tn) =>
                    MultiAsset.ArithmeticError.Overflow(policyId, tn)
            }
    }

    // ===================================
    // Re-exported nested objects and types
    // ===================================
    // We re-export these to avoid having them be directly defined in the root object,
    // so that the root's opaque type stays opaque to the nested objects.

    type Unbounded = MultiAssetVariant.Unbounded

    object Unbounded { export MultiAssetVariant.Unbounded.* }

    type Fractional = MultiAssetVariant.Fractional

    object Fractional { export MultiAssetVariant.Fractional.* }

    type Inner = MultiAssetInner.Inner

    object Inner {
        export MultiAssetInner.Inner.*

        type Unbounded = MultiAssetInner.Unbounded

        object Unbounded { export MultiAssetInner.Unbounded.* }

        type Fractional = MultiAssetInner.Fractional

        object Fractional { export MultiAssetInner.Fractional.* }
    }
}

private object MultiAssetVariant {
    import MultiAsset.Inner

    // ===================================
    // MultiAsset.Unbounded
    // ===================================

    type Unbounded = Unbounded.Unbounded

    object Unbounded {
        import Inner.Unbounded.Algebra as vAlgebra

        type InnerType = Inner.Unbounded

        opaque type Unbounded =
            Multiset[PolicyId, InnerType, vAlgebra.type, Order[PolicyId]]

        def from(
            m: Multiset[PolicyId, InnerType, vAlgebra.type, Order[PolicyId]]
        ): Unbounded = m

        def apply(s: SortedMap[PolicyId, InnerType]): MultiAsset.Unbounded = Multiset(s)

        def empty: Unbounded = Multiset.empty
        def zero: Unbounded = empty

        extension (self: Unbounded)
            def underlying: Multiset[PolicyId, InnerType, vAlgebra.type, Order[PolicyId]] =
                self

            def get(policyId: PolicyId): Inner.Unbounded = underlying.get(policyId)

            def get(policyId: PolicyId, assetName: AssetName): Coin.Unbounded =
                underlying.get(policyId).get(assetName)

            def toMultiAsset: Either[MultiAsset.ArithmeticError, MultiAsset] =
                try {
                    Right(self.unsafeToMultiAsset)
                } catch {
                    case e: MultiAsset.ArithmeticError => Left(e)
                }

            def unsafeToMultiAsset: MultiAsset = MultiAsset.from(
              self.mapValuesIndexed((policyId: PolicyId, innerUnbounded: InnerType) =>
                  try {
                      innerUnbounded.unsafeToInner
                  } catch {
                      case e: Inner.ArithmeticError =>
                          throw MultiAsset.ArithmeticError.withPolicyId(e, policyId)
                  }
              )(using vNewMonoid = Inner.AlgebraFull)
            )

            def toFractional: Fractional =
                Fractional.from(self.mapValues(_.toFractional))

            @targetName("addCoerce_Inner")
            infix def +~(other: MultiAsset): Unbounded = Unbounded.from(
              Multiset.combineWith(self, other.underlying)(_ +~ _)(using
                vOtherMonoid = Inner.AlgebraFull
              )
            )

            @targetName("addCoerce_Unbounded")
            infix def +~(other: Unbounded): Unbounded = self + other

            @targetName("addCoerce_Fractional")
            infix def +~(other: Fractional): Fractional = Fractional.from(
              Multiset.combineWith(self, other.underlying)(_ +~ _)
            )

            @targetName("subtractCoerce_Inner")
            infix def -~(other: MultiAsset): Unbounded = Unbounded.from(
              Multiset.combineWith(self, other.underlying)(_ -~ _)(using
                vOtherMonoid = Inner.AlgebraFull
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
                extends Multiset.Algebra[PolicyId, InnerType, vAlgebra.type, Order[PolicyId]]

            private object A
                extends M.PartialOrder.PartiallyOrderedElements(vAlgebra.partialCompare),
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
    // MultiAsset.Fractional
    // ===================================

    type Fractional = Fractional.Fractional

    object Fractional {
        import Inner.Fractional.Algebra as vAlgebra

        type InnerType = Inner.Fractional

        opaque type Fractional =
            Multiset[PolicyId, InnerType, vAlgebra.type, Order[PolicyId]]

        def from(
            m: Multiset[PolicyId, InnerType, vAlgebra.type, Order[PolicyId]]
        ): Fractional = m

        def apply(s: SortedMap[PolicyId, InnerType]): MultiAsset.Fractional = Multiset(s)

        def empty: Fractional = Multiset.empty
        def zero: Fractional = empty

        extension (self: Fractional)
            def underlying: Multiset[PolicyId, InnerType, vAlgebra.type, Order[
              PolicyId
            ]] = self

            def get(policyId: PolicyId): Inner.Fractional = underlying.get(policyId)

            def get(policyId: PolicyId, assetName: AssetName): Coin.Fractional =
                underlying.get(policyId).get(assetName)

            def toUnbounded: Unbounded =
                Unbounded.from(self.mapValues(_.toUnbounded))

            def toMultiAsset: Either[MultiAsset.ArithmeticError, MultiAsset] =
                try {
                    Right(self.unsafeToMultiAsset)
                } catch {
                    case e: MultiAsset.ArithmeticError => Left(e)
                }

            def unsafeToMultiAsset: MultiAsset = MultiAsset.from(
              self.mapValuesIndexed((policyId: PolicyId, innerFractional: InnerType) =>
                  try {
                      innerFractional.unsafeToInner
                  } catch {
                      case e: Inner.ArithmeticError =>
                          throw MultiAsset.ArithmeticError.withPolicyId(e, policyId)
                  }
              )(using vNewMonoid = Inner.AlgebraFull)
            )

            @targetName("addCoerce_Inner")
            infix def +~(other: MultiAsset): Fractional = Fractional.from(
              Multiset.combineWith(self, other.underlying)(_ +~ _)(using
                vOtherMonoid = Inner.AlgebraFull
              )
            )

            @targetName("addCoerce_Unbounded")
            infix def +~(other: Unbounded): Fractional = Fractional.from(
              Multiset.combineWith(self, other.underlying)(_ +~ _)
            )

            @targetName("addCoerce_Fractional")
            infix def +~(other: Fractional): Fractional = self + other

            @targetName("subtractCoerce_Inner")
            infix def -~(other: MultiAsset): Fractional = Fractional.from(
              Multiset.combineWith(self, other.underlying)(_ -~ _)(using
                vOtherMonoid = Inner.AlgebraFull
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
                extends Multiset.Algebra[PolicyId, InnerType, vAlgebra.type, Order[
                  PolicyId
                ]]

            private object A
                extends M.PartialOrder.PartiallyOrderedElements(vAlgebra.partialCompare),
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
