package scalus.cardano.ledger.value.multiasset

import scalus.cardano.ledger.value.multiasset.UtilsSortedMap.CombineWith.*
import scalus.cardano.ledger.value.multiasset.UtilsSortedMap.SortedMapPartialOrder
import scalus.cardano.ledger.{AssetName, PolicyId}
import spire.algebra.*
import spire.implicits.*
import spire.math.{Rational, SafeLong}

import scala.collection.immutable.SortedMap

type MultiAsset = MultiAsset.MultiAsset

object MultiAsset {
    opaque type MultiAsset = SortedMap[PolicyId, Inner]

    def apply(x: SortedMap[PolicyId, Inner]): MultiAsset = x

    def zero: MultiAsset = SortedMap.empty

    extension (self: MultiAsset)
        def underlying: SortedMap[PolicyId, Inner] = self

        def scaleIntegral[I](c: I)(using int: spire.math.Integral[I]): Unbounded =
            Unbounded(self.view.mapValues(_.scaleIntegral(c)).to(SortedMap))

        def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional = {
            Fractional(self.view.mapValues(_.scaleFractional(c)).to(SortedMap))
        }

    given algebra: Algebra.type = Algebra

    object Algebra extends PartialOrder[MultiAsset] {
        private val mapPartialOrder = SortedMapPartialOrder[PolicyId, Inner, Double](
          // If both keys exist, compare the values.
          // If only the left key exists, compare the left value against zero.
          // If only the right key exists, compare the right value against zero.
          compareBoth = MultiAsset.Inner.algebra.partialCompare,
          compareLeft = MultiAsset.Inner.algebra.partialCompare(_, Inner.zero),
          compareRight = MultiAsset.Inner.algebra.partialCompare(Inner.zero, _)
        )

        override def partialCompare(self: MultiAsset, other: MultiAsset): Double =
            mapPartialOrder.partialCompare(self, other)
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
        opaque type Unbounded = SortedMap[PolicyId, Inner.Unbounded]

        def apply(x: SortedMap[PolicyId, Inner.Unbounded]): Unbounded = x

        def zero: Unbounded = SortedMap.empty

        import Inner.Unbounded.algebra as innerAlgebra

        extension (self: Unbounded)
            def underlying: SortedMap[PolicyId, Inner.Unbounded] = self

            def toMultiAsset: Either[MultiAsset.ArithmeticError, MultiAsset] =
                try {
                    Right(self.unsafeToMultiAsset)
                } catch {
                    case e: MultiAsset.ArithmeticError => Left(e)
                }

            def unsafeToMultiAsset: MultiAsset = MultiAsset(
              self.view
                  .map((policyId: PolicyId, innerUnbounded: Inner.Unbounded) =>
                      try {
                          (policyId, innerUnbounded.unsafeToInner)
                      } catch {
                          case e: Inner.ArithmeticError =>
                              throw MultiAsset.ArithmeticError.withPolicyId(e, policyId)
                      }
                  )
                  .to(SortedMap)
            )

            def scaleIntegral[I](c: I)(using int: spire.math.Integral[I]): Unbounded =
                self :* c.toSafeLong

            def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional = {
                Fractional(self.view.mapValues(_.scaleFractional(c)).to(SortedMap))
            }

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Unbounded], CModule[Unbounded, SafeLong] {
            private val mapPartialOrder = SortedMapPartialOrder[PolicyId, Inner.Unbounded, Double](
              // If both keys exist, compare the values.
              // If only the left key exists, compare the left value against zero.
              // If only the right key exists, compare the right value against zero.
              compareBoth = innerAlgebra.partialCompare,
              compareLeft = innerAlgebra.partialCompare(_, Inner.Unbounded.zero),
              compareRight = innerAlgebra.partialCompare(Inner.Unbounded.zero, _)
            )

            override def partialCompare(self: Unbounded, other: Unbounded): Double =
                mapPartialOrder.partialCompare(self, other)

            override def scalar: CRing[SafeLong] = CRing[SafeLong]

            override def zero: Unbounded = Unbounded.zero

            override def negate(self: Unbounded): Unbounded =
                self.view.mapValues(innerAlgebra.negate).to(SortedMap)

            override def plus(self: Unbounded, other: Unbounded): Unbounded =
                combineWith(innerAlgebra.plus)(self, other)

            override def minus(self: Unbounded, other: Unbounded): Unbounded =
                combineWith(innerAlgebra.minus, identity, innerAlgebra.negate)(self, other)

            override def timesl(s: SafeLong, self: Unbounded): Unbounded =
                self.view.mapValues(_ :* s).to(SortedMap)
        }
    }

    // ===================================
    // MultiAsset.Fractional
    // ===================================

    type Fractional = Fractional.Fractional

    object Fractional {
        opaque type Fractional = SortedMap[PolicyId, Inner.Fractional]

        def apply(x: SortedMap[PolicyId, Inner.Fractional]): Fractional = x

        def zero: Fractional = SortedMap.empty

        extension (self: Fractional)
            def underlying: SortedMap[PolicyId, Inner.Fractional] = self

            def toUnbounded: Unbounded =
                Unbounded(self.view.mapValues(_.toUnbounded).to(SortedMap))

            def toMultiAsset: Either[MultiAsset.ArithmeticError, MultiAsset] =
                try {
                    Right(self.unsafeToMultiAsset)
                } catch {
                    case e: MultiAsset.ArithmeticError => Left(e)
                }

            def unsafeToMultiAsset: MultiAsset = MultiAsset(
              self.view
                  .map((policyId: PolicyId, innerFractional: Inner.Fractional) =>
                      try {
                          (policyId, innerFractional.unsafeToInner)
                      } catch {
                          case e: Inner.ArithmeticError =>
                              throw MultiAsset.ArithmeticError.withPolicyId(e, policyId)
                      }
                  )
                  .to(SortedMap)
            )

            def scaleFractional[F](c: F)(using spire.math.Fractional[F]): Fractional =
                self :* c.toRational

        import Inner.Fractional.algebra as innerAlgebra

        given algebra: Algebra.type = Algebra

        object Algebra extends PartialOrder[Fractional], VectorSpace[Fractional, Rational] {
            private val mapPartialOrder =
                SortedMapPartialOrder[PolicyId, Inner.Fractional, Double](
                  // If both keys exist, compare the values.
                  // If only the left key exists, compare the left value against zero.
                  // If only the right key exists, compare the right value against zero.
                  compareBoth = innerAlgebra.partialCompare,
                  compareLeft = innerAlgebra.partialCompare(_, Inner.Fractional.zero),
                  compareRight = innerAlgebra.partialCompare(Inner.Fractional.zero, _)
                )

            override def partialCompare(self: Fractional, other: Fractional): Double =
                mapPartialOrder.partialCompare(self, other)

            override def scalar: Field[Rational] = Field[Rational]

            override def zero: Fractional = Fractional.zero

            override def negate(self: Fractional): Fractional =
                self.view.mapValues(innerAlgebra.negate).to(SortedMap)

            override def plus(self: Fractional, other: Fractional): Fractional =
                combineWith(innerAlgebra.plus)(self, other)

            override def minus(self: Fractional, other: Fractional): Fractional =
                combineWith(innerAlgebra.minus, identity, innerAlgebra.negate)(self, other)

            override def timesl(s: Rational, self: Fractional): Fractional =
                self.view.mapValues(_ :* s).to(SortedMap)
        }
    }
}
