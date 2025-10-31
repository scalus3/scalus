package scalus.cardano.ledger.value.multiasset

import spire.algebra.*

import scala.collection.immutable.SortedMap

/** A canonical sorted map is a [[SortedMap]] with an additively monoidal value type, such that all
  * keys corresponding to the monoid's additive identity are removed from the map. This ensures that
  * the map itself can be an additive monoid, because [[SortedMap.empty]] can be the unique additive
  * identity.
  */
type CanonicalSortedMap[K, V] = CanonicalSortedMap.CanonicalSortedMap[K, V]

private object CanonicalSortedMap {
    opaque type CanonicalSortedMap[K, V] = SortedMap[K, V]

    /** Canonicalize a [[SortedMap]] by removing all keys corresponding to zero value. */
    def apply[K, V](sortedMap: SortedMap[K, V])(using
        vMonoid: AdditiveMonoid[V]
    ): CanonicalSortedMap[K, V] =
        sortedMap.filterNot(_._2 == vMonoid.zero)

    def empty[K, V](using Ordering[K]): CanonicalSortedMap[K, V] = SortedMap.empty

    extension [K, V](self: CanonicalSortedMap[K, V])
        def underlying: SortedMap[K, V] = self

        /** Apply a function to every value in the canonical sorted map. */
        def mapValues[VNew](f: V => VNew)(using
            kOrdering: Ordering[K],
            vNewMonoid: AdditiveMonoid[VNew]
        ): CanonicalSortedMap[K, VNew] = {
            val newSortedMap: SortedMap[K, VNew] = self.view.mapValues(v => f(v)).to(SortedMap)
            apply(newSortedMap)(using vMonoid = vNewMonoid)
        }

        /** Apply a function to every value in the canonical sorted map, exposing the value's key to
          * the function as an additional argument.
          */
        def mapValuesIndexed[VNew](f: (K, V) => VNew)(using
            kOrdering: Ordering[K],
            vNewMonoid: AdditiveMonoid[VNew]
        ): CanonicalSortedMap[K, VNew] = {
            def fIndexed(kv: (K, V)): (K, VNew) = (kv._1, f(kv._1, kv._2))
            val newSortedMap: SortedMap[K, VNew] = self.view.map(fIndexed).to(SortedMap)
            apply(newSortedMap)(using vMonoid = vNewMonoid)
        }

    //    def combineWithSimple[K, V](
    //        opBoth: (V, V) => V
    //    )(self: CanonicalSortedMap[K, V], other: CanonicalSortedMap[K, V])(using
    //        ev: AdditiveMonoid[V],
    //        ord: Ordering[K]
    //    ): CanonicalSortedMap[K, V] = {
    //        (self.keySet ++ other.keySet).view
    //            .map { key =>
    //                val combinedValue =
    //                    opBoth(self.getOrElse(key, ev.zero), other.getOrElse(key, ev.zero))
    //                key -> combinedValue
    //            }
    //            .to(SortedMap)
    //    }

    def combineWith[K, VCommon, VResult, VSelf, VOther](
        opBoth: (VCommon, VCommon) => VResult,
        opSelf: VCommon => VResult = identity[VResult],
        opOther: VCommon => VResult = identity[VResult],
        preMapSelf: VSelf => VCommon = identity[VCommon],
        preMapOther: VOther => VCommon = identity[VCommon]
    )(
        self: CanonicalSortedMap[K, VSelf],
        other: CanonicalSortedMap[K, VOther]
    )(using
        kOrdering: Ordering[K],
        vResultMonoid: AdditiveMonoid[VResult]
    ): CanonicalSortedMap[K, VResult] = {
        import scala.annotation.tailrec
        import scala.collection.mutable
        import scala.math.Ordered.orderingToOrdered

        val selfIterator: Iterator[(K, VSelf)] = self.iterator
        val otherIterator: Iterator[(K, VOther)] = other.iterator
        val resultBuilder: mutable.Builder[(K, VResult), CanonicalSortedMap[K, VResult]] =
            SortedMap.newBuilder

        inline def processBoth(x: (K, VSelf), y: (K, VOther)): (K, VResult) =
            (x._1, opBoth(preMapSelf(x._2), preMapOther(y._2)))

        inline def processSelf(x: (K, VSelf)): (K, VResult) =
            (x._1, opSelf(preMapSelf(x._2)))

        inline def processOther(y: (K, VOther)): (K, VResult) =
            (y._1, opOther(preMapOther(y._2)))

        inline def appendNonZero(
            builder: mutable.Builder[(K, VResult), CanonicalSortedMap[K, VResult]],
            z: (K, VResult)
        ): Unit = if z._2 == vResultMonoid.zero then () else builder += z

        inline def concatNonZero(
            builder: mutable.Builder[(K, VResult), CanonicalSortedMap[K, VResult]],
            m: Iterator[(K, VResult)]
        ): Unit = builder ++= m.iterator.filterNot(_._2 == vResultMonoid.zero)

        // Warning: this function mutates its arguments!
        @tailrec
        def loop(
            x: (K, VSelf),
            xs: Iterator[(K, VSelf)],
            y: (K, VOther),
            ys: Iterator[(K, VOther)],
            builder: mutable.Builder[(K, VResult), CanonicalSortedMap[K, VResult]]
        ): CanonicalSortedMap[K, VResult] = {
            if x._1 < y._1 then {
                // Process `x` now and `y` later
                appendNonZero(builder, processSelf(x))
                if xs.hasNext then {
                    // Continue looping with the next `x` and same `y`
                    val xNext = xs.next()
                    loop(xNext, xs, y, ys, builder)
                } else {
                    // Process `y` and the remaining `ys` and return
                    appendNonZero(builder, processOther(y))
                    concatNonZero(builder, ys.map(processOther))
                    builder.result()
                }
            } else if x._1 == y._1 then {
                // Process both `x` and `y` now
                appendNonZero(builder, processBoth(x, y))
                if xs.hasNext then
                    if ys.hasNext then {
                        // Continue looping with the next `x` and next `y`
                        val xNext = xs.next()
                        val yNext = ys.next()
                        loop(xNext, xs, yNext, ys, builder)
                    } else {
                        // Process the remaining `xs` and return
                        concatNonZero(builder, xs.map(processSelf))
                        builder.result()
                    }
                else {
                    // Process the remaining `ys` and return
                    concatNonZero(builder, ys.map(processOther))
                    builder.result()
                }
            } else {
                // Process `y` now and `x` later
                appendNonZero(builder, processOther(y))
                if ys.hasNext then {
                    // Continue looping with the same `x` and next `y`
                    val yNext = ys.next()
                    loop(x, xs, yNext, ys, builder)
                } else {
                    // Process `x` and the remaining `xs` and return
                    appendNonZero(builder, processSelf(x))
                    concatNonZero(builder, xs.map(processSelf))
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
                concatNonZero(resultBuilder, selfIterator.map(processSelf))
                resultBuilder.result()
            }
        } else {
            // Process the entries in `other` and return
            concatNonZero(resultBuilder, otherIterator.map(processOther))
            resultBuilder.result()
        }
    }

    trait CanonicalSortedMapPartialOrderAbstract[K, V, I](toDouble: I => Double)(using
        iMonoid: AdditiveMonoid[I],
        kOrdering: Ordering[K],
        vMonoid: AdditiveMonoid[V]
    ) extends PartialOrder[CanonicalSortedMap[K, V]] {
        def compareElements: (V, V) => I

        override def partialCompare(
            self: CanonicalSortedMap[K, V],
            other: CanonicalSortedMap[K, V]
        ): Double = {
            val comparisons: Iterable[I] =
                // If both keys exist, compare the values.
                // If only the left key exists, compare the left value against zero.
                // If only the right key exists, compare the right value against zero.
                combineWith(
                  compareElements,
                  compareElements(_, vMonoid.zero),
                  compareElements(vMonoid.zero, _)
                )(self, other).values
            // If both maps are empty, then they are equal.
            // If all element-wise comparisons are the same, then the maps are comparable.
            // Otherwise, the maps are incomparable.
            comparisons.headOption.fold(0d)(first =>
                val monotonic = comparisons.forall(_ == first)
                if monotonic then toDouble(first) else Double.NaN
            )
        }
    }

    final case class CanonicalSortedMapOrder[K, V](
        override val compareElements: (V, V) => Int,
    )(using kOrdering: Ordering[K], vMonoid: AdditiveMonoid[V])
        extends PartialOrder[CanonicalSortedMap[K, V]],
          CanonicalSortedMapPartialOrderAbstract[K, V, Int](intToDouble)(using
            iMonoid = spire.implicits.IntAlgebra
          )

    final case class CanonicalSortedMapPartialOrder[K, V](
        override val compareElements: (V, V) => Double,
    )(using kOrdering: Ordering[K], vMonoid: AdditiveMonoid[V])
        extends PartialOrder[CanonicalSortedMap[K, V]],
          CanonicalSortedMapPartialOrderAbstract[K, V, Double](identity[Double])(using
            iMonoid = spire.implicits.DoubleAlgebra
          )

    private def intToDouble(x: Int): Double = x.toDouble

    private def compareLeftDefault[V, I](using
        vMonoid: AdditiveMonoid[V]
    )(compareBoth: (V, V) => I)(x: V) = compareBoth(x, vMonoid.zero)

    private def compareRightDefault[V, I](using
        vMonoid: AdditiveMonoid[V]
    )(compareBoth: (V, V) => I)(x: V) = compareBoth(x, vMonoid.zero)
}
