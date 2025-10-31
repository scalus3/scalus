package scalus.cardano.ledger.value.multiasset

import scalus.cardano.ledger.value.multiasset.CanonicalSortedMap
import scalus.cardano.ledger.value.multiasset.CanonicalSortedMap.*
import scalus.cardano.ledger.{AssetName, PolicyId}
import spire.algebra.*
import spire.math.{Rational, SafeLong}
import spire.implicits.*
import spire.implicits.MapEq as _
import spire.implicits.{MapCSemiring as _, MapMonoid as _}
import spire.implicits.{MapCRng as _, MapGroup as _}
import spire.implicits.{MapInnerProductSpace as _, MapVectorSpace as _}

import scala.annotation.targetName
import scala.collection.immutable.SortedMap

type MultiAsset = MultiAsset.MultiAsset

object MultiAsset {
    opaque type MultiAsset = CanonicalSortedMap[PolicyId, Inner]

    @targetName("applyWithCanonicalSortedMap")
    def apply(self: CanonicalSortedMap[PolicyId, Inner]): MultiAsset = self

    @targetName("applyWithSortedMap")
    def apply(sortedMap: SortedMap[PolicyId, Inner]): MultiAsset =
        CanonicalSortedMap(sortedMap)(using vMonoid = Inner.AdditiveMonoid)

    def zero: MultiAsset = CanonicalSortedMap.empty

    extension (self: MultiAsset)
        def underlying: CanonicalSortedMap[PolicyId, Inner] = self

        def scaleIntegral[I](c: I)(using int: spire.math.Integral[I]): Unbounded =
            Unbounded(self.mapValues(_.scaleIntegral(c)))

        def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional = {
            Fractional(self.mapValues(_.scaleFractional(c)))
        }

        @targetName("addCoerce_Inner")
        infix def +~(other: MultiAsset): Unbounded = Unbounded(
            combineWith[PolicyId, Inner, Inner, Inner.Unbounded](_ +~ _, self, other)(using
                vSelfMonoid = Inner.AdditiveMonoid,
                vOtherMonoid = Inner.AdditiveMonoid
            )
        )

        @targetName("addCoerce_Unbounded")
        infix def +~(other: Unbounded): Unbounded = Unbounded(
            combineWith[PolicyId, Inner, Inner.Unbounded, Inner.Unbounded](
                _ +~ _,
                self,
                other.underlying
            )(using vSelfMonoid = Inner.AdditiveMonoid)
        )

        @targetName("addCoerce_Fractional")
        infix def +~(other: Fractional): Fractional = Fractional(
            combineWith[PolicyId, Inner, Inner.Fractional, Inner.Fractional](
                _ +~ _,
                self,
                other.underlying
            )(using vSelfMonoid = Inner.AdditiveMonoid)
        )

        @targetName("subtractCoerce_Inner")
        infix def -~(other: MultiAsset): Unbounded = Unbounded(
            combineWith[PolicyId, Inner, Inner, Inner.Unbounded](_ -~ _, self, other)(using
                vSelfMonoid = Inner.AdditiveMonoid,
                vOtherMonoid = Inner.AdditiveMonoid
            )
        )

        @targetName("subtractCoerce_Unbounded")
        infix def -~(other: Unbounded): Unbounded = Unbounded(
            combineWith[PolicyId, Inner, Inner.Unbounded, Inner.Unbounded](
                _ -~ _,
                self,
                other.underlying
            )(using vSelfMonoid = Inner.AdditiveMonoid)
        )

        @targetName("subtractCoerce_Fractional")
        infix def -~(other: Fractional): Fractional = Fractional(
            combineWith[PolicyId, Inner, Inner.Fractional, Inner.Fractional](
                _ -~ _,
                self,
                other.underlying
            )(using vSelfMonoid = Inner.AdditiveMonoid)
        )

        @targetName("negate")
        infix def unary_- : Unbounded = Unbounded(self.mapValues(-_))

    given algebra: Algebra.type = Algebra

    object Algebra extends PartialOrder[MultiAsset] {
        override def eqv(self: MultiAsset, other: MultiAsset): Boolean = self == other

        override def partialCompare(self: MultiAsset, other: MultiAsset): Double =
            mapPartialOrder.partialCompare(self, other)

        private val mapPartialOrder = CanonicalSortedMapPartialOrder[PolicyId, Inner](
          MultiAsset.Inner.algebra.partialCompare
        )(using vMonoid = Inner.AdditiveMonoid)
    }

    // This AdditiveMonoid is available for manual import, but it isn't implicitly given to users
    // because adding `Long` Inners is unsafe (it can overflow/underflow without warning/error).
    object AdditiveMonoid extends AdditiveMonoid[MultiAsset] {
        override def zero: MultiAsset = MultiAsset.zero

        override def plus(self: MultiAsset, other: MultiAsset): MultiAsset =
            combineWith(Inner.AdditiveMonoid.plus, self, other)(using
              vSelfMonoid = Inner.AdditiveMonoid,
              vOtherMonoid = Inner.AdditiveMonoid,
              vResultMonoid = Inner.AdditiveMonoid
            )
    }

    enum ArithmeticError extends Throwable:
        def policyId: PolicyId
        def assetName: AssetName

        case Underflow(policyId: PolicyId, assetName: AssetName) extends ArithmeticError
        case Overflow(policyId: PolicyId, assetName: AssetName) extends ArithmeticError

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

    object Unbounded { export MultiAssetVariant.Unbounded.{*, given} }

    type Fractional = MultiAssetVariant.Fractional

    object Fractional { export MultiAssetVariant.Fractional.{*, given} }

    type Inner = MultiAssetInner.Inner

    object Inner {
        export MultiAssetInner.Inner.{*, given}

        type Unbounded = MultiAssetInner.Unbounded

        object Unbounded { export MultiAssetInner.Unbounded.{*, given} }

        type Fractional = MultiAssetInner.Fractional

        object Fractional { export MultiAssetInner.Fractional.{*, given} }
    }
}

private object MultiAssetVariant {
    import MultiAsset.Inner

    // ===================================
    // MultiAsset.Unbounded
    // ===================================
    type Unbounded = Unbounded.Unbounded

    object Unbounded {
        opaque type Unbounded = CanonicalSortedMap[PolicyId, Inner.Unbounded]

        @targetName("applyWithCanonicalSortedMap")
        def apply(self: CanonicalSortedMap[PolicyId, Inner.Unbounded]): Unbounded = self

        @targetName("applyWithSortedMap")
        def apply(sortedMap: SortedMap[PolicyId, Inner.Unbounded]): Unbounded = CanonicalSortedMap(
          sortedMap
        )

        def zero: Unbounded = CanonicalSortedMap.empty

        import Inner.Unbounded.algebra as innerAlgebra

        extension (self: Unbounded)
            def underlying: CanonicalSortedMap[PolicyId, Inner.Unbounded] = self

            def toMultiAsset: Either[MultiAsset.ArithmeticError, MultiAsset] =
                try { Right(self.unsafeToMultiAsset) }
                catch { case e: MultiAsset.ArithmeticError => Left(e) }

            def unsafeToMultiAsset: MultiAsset = MultiAsset(
              self.mapValuesIndexed((policyId: PolicyId, innerUnbounded: Inner.Unbounded) =>
                  try { innerUnbounded.unsafeToInner }
                  catch {
                      case e: Inner.ArithmeticError =>
                          throw MultiAsset.ArithmeticError.withPolicyId(e, policyId)
                  }
              )(using vNewMonoid = Inner.AdditiveMonoid)
            )

            def scaleIntegral[I](c: I)(using int: spire.math.Integral[I]): Unbounded =
                self :* c.toSafeLong

            def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional = {
                Fractional(self.mapValues(_.scaleFractional(c)))
            }

            @targetName("addCoerce_Inner")
            infix def +~(other: MultiAsset): Unbounded = Unbounded(
                combineWith[PolicyId, Inner.Unbounded, Inner, Inner.Unbounded](
                    _ +~ _,
                    self,
                    other.underlying
                )(using vOtherMonoid = Inner.AdditiveMonoid)
            )

            @targetName("addCoerce_Unbounded")
            infix def +~(other: Unbounded): Unbounded = self + other

            @targetName("addCoerce_Fractional")
            infix def +~(other: Fractional): Fractional = Fractional(
                combineWith[PolicyId, Inner.Unbounded, Inner.Fractional, Inner.Fractional](
                    _ +~ _,
                    self,
                    other.underlying
                )
            )

            @targetName("subtractCoerce_Inner")
            infix def -~(other: MultiAsset): Unbounded = Unbounded(
                combineWith[PolicyId, Inner.Unbounded, Inner, Inner.Unbounded](
                    _ -~ _,
                    self,
                    other.underlying
                )(using vOtherMonoid = Inner.AdditiveMonoid)
            )

            @targetName("subtractCoerce_Unbounded")
            infix def -~(other: Unbounded): Unbounded = self - other

            @targetName("subtractCoerce_Fractional")
            infix def -~(other: Fractional): Fractional = Fractional(
                combineWith[PolicyId, Inner.Unbounded, Inner.Fractional, Inner.Fractional](
                    _ -~ _,
                    self,
                    other.underlying
                )
            )
        
        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Unbounded], CModule[Unbounded, SafeLong] {
            override def eqv(self: Unbounded, other: Unbounded): Boolean = self == other

            override def partialCompare(self: Unbounded, other: Unbounded): Double =
                mapPartialOrder.partialCompare(self, other)

            override def scalar: CRing[SafeLong] = CRing[SafeLong]

            override def zero: Unbounded = Unbounded.zero

            override def negate(self: Unbounded): Unbounded =
                self.mapValues(innerAlgebra.negate)

            override def plus(self: Unbounded, other: Unbounded): Unbounded =
                combineWith(innerAlgebra.plus, self, other)

            override def minus(self: Unbounded, other: Unbounded): Unbounded =
                combineWith(innerAlgebra.minus, self, other)

            override def timesl(s: SafeLong, self: Unbounded): Unbounded =
                self.mapValues(_ :* s)

            private val mapPartialOrder = CanonicalSortedMapPartialOrder[PolicyId, Inner.Unbounded](
              innerAlgebra.partialCompare
            )
        }
    }

    // ===================================
    // MultiAsset.Fractional
    // ===================================

    type Fractional = Fractional.Fractional

    object Fractional {
        opaque type Fractional = CanonicalSortedMap[PolicyId, Inner.Fractional]

        @targetName("applyWithCanonicalSortedMap")
        def apply(self: CanonicalSortedMap[PolicyId, Inner.Fractional]): Fractional = self

        @targetName("applyWithSortedMap")
        def apply(sortedMap: SortedMap[PolicyId, Inner.Fractional]): Fractional =
            CanonicalSortedMap(sortedMap)

        def zero: Fractional = CanonicalSortedMap.empty

        extension (self: Fractional)
            def underlying: CanonicalSortedMap[PolicyId, Inner.Fractional] = self

            def toUnbounded: Unbounded =
                Unbounded(self.mapValues(_.toUnbounded))

            def toMultiAsset: Either[MultiAsset.ArithmeticError, MultiAsset] =
                try { Right(self.unsafeToMultiAsset) }
                catch { case e: MultiAsset.ArithmeticError => Left(e) }

            def unsafeToMultiAsset: MultiAsset = MultiAsset(
              self.mapValuesIndexed((policyId: PolicyId, innerFractional: Inner.Fractional) =>
                  try { innerFractional.unsafeToInner }
                  catch {
                      case e: Inner.ArithmeticError =>
                          throw MultiAsset.ArithmeticError.withPolicyId(e, policyId)
                  }
              )(using vNewMonoid = Inner.AdditiveMonoid)
            )

            def scaleFractional[F](c: F)(using spire.math.Fractional[F]): Fractional =
                self :* c.toRational

            @targetName("addCoerce_Inner")
            infix def +~(other: MultiAsset): Fractional = Fractional(
                combineWith[PolicyId, Inner.Fractional, Inner, Inner.Fractional](
                    _ +~ _,
                    self,
                    other.underlying
                )(using vOtherMonoid = Inner.AdditiveMonoid)
            )

            @targetName("addCoerce_Unbounded")
            infix def +~(other: Unbounded): Fractional = Fractional(
                combineWith[PolicyId, Inner.Fractional, Inner.Unbounded, Inner.Fractional](
                    _ +~ _,
                    self,
                    other.underlying
                )
            )

            @targetName("addCoerce_Fractional")
            infix def +~(other: Fractional): Fractional = self + other

            @targetName("subtractCoerce_Inner")
            infix def -~(other: MultiAsset): Fractional = Fractional(
                combineWith[PolicyId, Inner.Fractional, Inner, Inner.Fractional](
                    _ -~ _,
                    self,
                    other.underlying
                )(using vOtherMonoid = Inner.AdditiveMonoid)
            )

            @targetName("subtractCoerce_Unbounded")
            infix def -~(other: Unbounded): Fractional = Fractional(
                combineWith[PolicyId, Inner.Fractional, Inner.Unbounded, Inner.Fractional](
                    _ -~ _,
                    self,
                    other.underlying
                )
            )

            @targetName("subtractCoerce_Fractional")
            infix def -~(other: Fractional): Fractional = self - other

        import Inner.Fractional.algebra as innerAlgebra

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Fractional], VectorSpace[Fractional, Rational] {
            override def eqv(self: Fractional, other: Fractional): Boolean = self == other

            override def partialCompare(self: Fractional, other: Fractional): Double =
                mapPartialOrder.partialCompare(self, other)

            override def scalar: Field[Rational] = Field[Rational]

            override def zero: Fractional = Fractional.zero

            override def negate(self: Fractional): Fractional =
                self.mapValues(innerAlgebra.negate)

            override def plus(self: Fractional, other: Fractional): Fractional =
                combineWith(innerAlgebra.plus, self, other)

            override def minus(self: Fractional, other: Fractional): Fractional =
                combineWith(innerAlgebra.minus, self, other)

            override def timesl(s: Rational, self: Fractional): Fractional =
                self.mapValues(_ :* s)

            private val mapPartialOrder =
                CanonicalSortedMapPartialOrder[PolicyId, Inner.Fractional](
                  innerAlgebra.partialCompare
                )
        }
    }
}
