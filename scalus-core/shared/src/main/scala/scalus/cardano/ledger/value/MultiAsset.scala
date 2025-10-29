package scalus.cardano.ledger.value

import scalus.cardano.ledger.{AssetName, PolicyId}
import spire.algebra.*
import spire.implicits.*
import spire.math.{Rational, SafeLong}

import scala.collection.immutable.SortedMap

type MultiAsset = MultiAsset.MultiAsset

object MultiAsset {
    import CombineWith.*

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

    sealed trait ArithmeticError extends Throwable:
        def policyId: PolicyId

        def assetName: AssetName

    type Unbounded = Unbounded.Unbounded

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

    type Fractional = Fractional.Fractional

    object Unbounded {
        opaque type Unbounded = SortedMap[PolicyId, Inner.Unbounded]

        def apply(x: SortedMap[PolicyId, Inner.Unbounded]): Unbounded = x

        def zero: Unbounded = SortedMap.empty

        import Inner.Unbounded.algebra as innerAlgebra

        extension (self: Unbounded)
            def underlying: SortedMap[PolicyId, Inner.Unbounded] = self

            def toMultiAsset: Either[MultiAsset.ArithmeticError, MultiAsset] =
                try { Right(self.unsafeToMultiAsset) }
                catch { case e: MultiAsset.ArithmeticError => Left(e) }

            def unsafeToMultiAsset: MultiAsset = self.view
                .map((policyId: PolicyId, innerUnbounded: Inner.Unbounded) =>
                    try { (policyId, innerUnbounded.unsafeToInner) }
                    catch {
                        case e: Inner.ArithmeticError =>
                            throw MultiAsset.ArithmeticError.withPolicyId(e, policyId)
                    }
                )
                .to(SortedMap)

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

    object Fractional {
        opaque type Fractional = SortedMap[PolicyId, Inner.Fractional]

        def apply(x: SortedMap[PolicyId, Inner.Fractional]): Fractional = x

        def zero: Fractional = SortedMap.empty

        extension (self: Fractional)
            def underlying: SortedMap[PolicyId, Inner.Fractional] = self

            def toUnbounded: Unbounded =
                Unbounded(self.view.mapValues(_.toUnbounded).to(SortedMap))

            def toMultiAsset: Either[MultiAsset.ArithmeticError, MultiAsset] =
                try { Right(self.unsafeToMultiAsset) }
                catch { case e: MultiAsset.ArithmeticError => Left(e) }

            def unsafeToMultiAsset: MultiAsset = self.view
                .map((policyId: PolicyId, innerFractional: Inner.Fractional) =>
                    try { (policyId, innerFractional.unsafeToInner) }
                    catch {
                        case e: Inner.ArithmeticError =>
                            throw MultiAsset.ArithmeticError.withPolicyId(e, policyId)
                    }
                )
                .to(SortedMap)

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

    object ArithmeticError {
        final case class Underflow(policyId: PolicyId, assetName: AssetName) extends ArithmeticError

        final case class Overflow(policyId: PolicyId, assetName: AssetName) extends ArithmeticError

        def withPolicyId(e: Inner.ArithmeticError, policyId: PolicyId): MultiAsset.ArithmeticError =
            e match {
                case Inner.ArithmeticError.Underflow(tn) =>
                    MultiAsset.ArithmeticError.Underflow(policyId, tn)
                case Inner.ArithmeticError.Overflow(tn) =>
                    MultiAsset.ArithmeticError.Overflow(policyId, tn)
            }
    }

    type Inner = Inner.Inner

    object Inner {
        opaque type Inner = SortedMap[AssetName, Coin]

        def apply(x: SortedMap[AssetName, Coin]): Inner = x

        def zero: Inner = SortedMap.empty

        extension (self: Inner)
            def underlying: SortedMap[AssetName, Coin] = self

            def scaleIntegral[I](c: I)(using int: spire.math.Integral[I]): Unbounded =
                Unbounded(self.view.mapValues(_.scaleIntegral(c)).to(SortedMap))

            def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional = {
                Fractional(self.view.mapValues(_.scaleFractional(c)).to(SortedMap))
            }

        given algebra: Algebra.type = Algebra

        sealed trait ArithmeticError extends Throwable:
            def assetName: AssetName

        type Unbounded = Unbounded.Unbounded

        object Algebra extends PartialOrder[Inner] {
            private val mapPartialOrder = SortedMapPartialOrder[AssetName, Coin, Int](
              // If both keys exist, compare the values.
              // If only the left key exists, compare the left value against zero.
              // If only the right key exists, compare the right value against zero.
              compareBoth = Coin.algebra.compare,
              compareLeft = _.signum,
              compareRight = -_.signum
            )

            override def partialCompare(self: Inner, other: Inner): Double =
                mapPartialOrder.partialCompare(self, other)
        }

        type Fractional = Fractional.Fractional

        object Unbounded {
            opaque type Unbounded = SortedMap[AssetName, Coin.Unbounded]

            def apply(x: SortedMap[AssetName, Coin.Unbounded]): Unbounded = x

            def zero: Unbounded = SortedMap.empty

            import Coin.Unbounded.algebra as coinAlgebra

            extension (self: Unbounded)
                def underlying: SortedMap[AssetName, Coin.Unbounded] = self

                def toInner: Either[Inner.ArithmeticError, Inner] =
                    try { Right(self.unsafeToInner) }
                    catch { case e: Inner.ArithmeticError => Left(e) }

                def unsafeToInner: Inner = self.view
                    .map((assetName: AssetName, coinUnbounded: Coin.Unbounded) =>
                        try { (assetName, coinUnbounded.unsafeToCoin) }
                        catch {
                            case e: Coin.ArithmeticError =>
                                throw Inner.ArithmeticError.withAssetName(e, assetName)
                        }
                    )
                    .to(SortedMap)

                def toFractional: Fractional =
                    Fractional(self.view.mapValues(_.toCoinFractional).to(SortedMap))

                def scaleIntegral[I](c: I)(using int: spire.math.Integral[I]): Unbounded =
                    self :* c.toSafeLong

                def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional = {
                    Fractional(self.view.mapValues(_.scaleFractional(c)).to(SortedMap))
                }

            given algebra: Algebra.type = Algebra

            object Algebra extends PartialOrder[Unbounded], CModule[Unbounded, SafeLong] {
                private val mapPartialOrder =
                    MultiAsset.SortedMapPartialOrder[AssetName, Coin.Unbounded, Int](
                      // If both keys exist, compare the values.
                      // If only the left key exists, compare the left value against zero.
                      // If only the right key exists, compare the right value against zero.
                      compareBoth = coinAlgebra.compare,
                      compareLeft = _.signum,
                      compareRight = -_.signum
                    )

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
            }
        }

        object Fractional {
            opaque type Fractional = SortedMap[AssetName, Coin.Fractional]

            def apply(x: SortedMap[AssetName, Coin.Fractional]): Fractional = x

            def zero: Fractional = SortedMap.empty

            extension (self: Fractional)
                def underlying: SortedMap[AssetName, Coin.Fractional] = self

                def toUnbounded: Unbounded =
                    Unbounded(self.view.mapValues(_.toUnbounded).to(SortedMap))

                def toInner: Either[Inner.ArithmeticError, Inner] =
                    try { Right(self.unsafeToInner) }
                    catch { case e: Inner.ArithmeticError => Left(e) }

                def unsafeToInner: Inner = self.view
                    .map((assetName: AssetName, coinFractional: Coin.Fractional) =>
                        try { (assetName, coinFractional.unsafeToCoin) }
                        catch {
                            case e: Coin.ArithmeticError =>
                                throw Inner.ArithmeticError.withAssetName(e, assetName)
                        }
                    )
                    .to(SortedMap)

                def scaleFractional[F](c: F)(using frac: spire.math.Fractional[F]): Fractional =
                    self :* c.toRational

            import Coin.Fractional.algebra as coinAlgebra

            given algebra: Algebra.type = Algebra

            object Algebra extends PartialOrder[Fractional], VectorSpace[Fractional, Rational] {
                private val mapPartialOrder =
                    SortedMapPartialOrder[AssetName, Coin.Fractional, Int](
                      compareBoth = coinAlgebra.compare,
                      compareLeft = _.signum,
                      compareRight = -_.signum
                    )

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
            }
        }

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

            final case class Underflow(assetName: AssetName) extends ArithmeticError

            final case class Overflow(assetName: AssetName) extends ArithmeticError
        }
    }

    private object CombineWith {
        def combineWithSimple[K, V](
            opBoth: (V, V) => V
        )(self: SortedMap[K, V], other: SortedMap[K, V])(using
            ev: AdditiveMonoid[V],
            ord: Ordering[K]
        ): SortedMap[K, V] = {
            (self.keySet ++ other.keySet).view
                .map { key =>
                    val combinedValue =
                        opBoth(self.getOrElse(key, ev.zero), other.getOrElse(key, ev.zero))
                    key -> combinedValue
                }
                .to(SortedMap)
        }

        def combineWith[K, VCommon, VResult, VSelf, VOther](
            opBoth: (VCommon, VCommon) => VResult,
            opSelf: VCommon => VResult = identity[VResult],
            opOther: VCommon => VResult = identity[VResult],
            preMapSelf: VSelf => VCommon = identity[VCommon],
            preMapOther: VOther => VCommon = identity[VCommon]
        )(
            self: SortedMap[K, VSelf],
            other: SortedMap[K, VOther]
        )(using Ordering[K]): SortedMap[K, VResult] = {
            import scala.annotation.tailrec
            import scala.collection.mutable
            import scala.math.Ordered.orderingToOrdered

            val selfIterator: Iterator[(K, VSelf)] = self.iterator
            val otherIterator: Iterator[(K, VOther)] = other.iterator
            val resultBuilder: mutable.Builder[(K, VResult), SortedMap[K, VResult]] =
                SortedMap.newBuilder

            inline def processBoth(x: (K, VSelf), y: (K, VOther)): (K, VResult) =
                (x._1, opBoth(preMapSelf(x._2), preMapOther(y._2)))
            inline def processSelf(x: (K, VSelf)): (K, VResult) =
                (x._1, opSelf(preMapSelf(x._2)))
            inline def processOther(y: (K, VOther)): (K, VResult) =
                (y._1, opOther(preMapOther(y._2)))

            // Warning: this function mutates its arguments!
            @tailrec
            def loop(
                x: (K, VSelf),
                xs: Iterator[(K, VSelf)],
                y: (K, VOther),
                ys: Iterator[(K, VOther)],
                builder: mutable.Builder[(K, VResult), SortedMap[K, VResult]]
            ): SortedMap[K, VResult] = {
                if x._1 < y._1 then {
                    // Process `x` now and `y` later
                    builder += processSelf(x)
                    if xs.hasNext then {
                        // Continue looping with the next `x` and same `y`
                        val xNext = xs.next()
                        loop(xNext, xs, y, ys, builder)
                    } else {
                        // Process `y` and the remaining `ys` and return
                        builder += processOther(y)
                        builder ++= ys.map(processOther)
                        builder.result()
                    }
                } else if x._1 == y._1 then {
                    // Process both `x` and `y` now
                    builder += processBoth(x, y)
                    if xs.hasNext then
                        if ys.hasNext then {
                            // Continue looping with the next `x` and next `y`
                            val xNext = xs.next()
                            val yNext = ys.next()
                            loop(xNext, xs, yNext, ys, builder)
                        } else {
                            // Process the remaining `xs` and return
                            builder ++= xs.map(processSelf)
                            builder.result()
                        }
                    else {
                        // Process the remaining `ys` and return
                        builder ++= ys.map(processOther)
                        builder.result()
                    }
                } else {
                    // Process `y` now and `x` later
                    builder += processOther(y)
                    if ys.hasNext then {
                        // Continue looping with the same `x` and next `y`
                        val yNext = ys.next()
                        loop(x, xs, yNext, ys, builder)
                    } else {
                        // Process `x` and the remaining `xs` and return
                        builder += processSelf(x)
                        builder ++= xs.map(processSelf)
                        builder.result()
                    }
                }
            }

            if selfIterator.hasNext then {
                if otherIterator.hasNext then {
                    // Start looping with the first entries in `self` and `other`
                    val selfFirst = selfIterator.next()
                    val otherFirst = otherIterator.next()
                    loop(selfFirst, selfIterator, otherFirst, otherIterator, resultBuilder)
                } else {
                    // Process the entries in `self` and return
                    resultBuilder ++= selfIterator.map(processSelf)
                    resultBuilder.result()
                }
            } else {
                // Process the entries in `other` and return
                resultBuilder ++= otherIterator.map(processOther)
                resultBuilder.result()
            }
        }
    }

    private final class SortedMapPartialOrder[K, V, @specialized(Int, Double) I](
        compareBoth: (V, V) => I,
        compareLeft: V => I,
        compareRight: V => I
    )(using
        eqI: Eq[I],
        keyOrdering: Ordering[K],
        toDouble: ToDouble[I]
    ) extends PartialOrder[SortedMap[K, V]] {
        override def partialCompare(self: SortedMap[K, V], other: SortedMap[K, V]): Double = {
            val comparisons: Iterable[I] =
                combineWith(compareBoth, compareLeft, compareRight)(self, other).values
            // If both maps are empty, then they are equal.
            // If all element-wise comparisons are the same, then the maps are comparable.
            // Otherwise, the maps are incomparable.
            comparisons.headOption.fold(0d)(first =>
                val monotonic = comparisons.forall(_ === first)
                if monotonic then toDouble.toDouble(first) else Double.NaN
            )
        }
    }

    private sealed trait ToDouble[I]:
        def toDouble: I => Double

    private given ToDouble[Double] with
        override def toDouble: Double => Double = identity

    private given ToDouble[Int] with
        override def toDouble: Int => Double = _.toDouble
}
