package scalus.cardano.ledger.value.multiasset

import org.scalatest.funsuite.AnyFunSuite
import spire.algebra.*

import scala.collection.immutable.SortedMap

type CanonicalSortedMap[K,V, M <: Monoid[V] & Singleton] =
    CanonicalSortedMap.CanonicalSortedMap[K,V, M]

/**
 * A canonical sorted map is:
 * - a [[SortedMap]] with an associated "zero" element, provided by a [[Monoid]],
 * - such that zero elements are removed from the map.
 */
object CanonicalSortedMap {

    opaque type CanonicalSortedMap[K,V, M <: Monoid[V] & Singleton] = SortedMap[K,V]

    /**
     * Canonicalize a [[SortedMap]], using the underlying [[Monoid]] on [[V]] to determine the 0 element.
     */
    def fromSortedMap[K, V](
        self: SortedMap[K, V]
    )(using vMonoid: Monoid[V]): CanonicalSortedMap[K, V, vMonoid.type ] =
        self.filterNot(_._2 == vMonoid.empty)


    def empty[K,V, M <: Monoid[V] & Singleton](using Ordering[K]) : CanonicalSortedMap[K,V, M] = SortedMap.empty

    extension [K,V, M <: Monoid[V] & Singleton](self : CanonicalSortedMap[K,V, M])
        def toSortedMap: SortedMap[K,V] = identity(self)

        def map[K1, V1](f : (K, V) => (K1, V1))
                       (using ordering : Ordering[K1], monoid: Monoid[V1]):
            CanonicalSortedMap[K1, V1, monoid.type ]
            = fromSortedMap(self.map(kv => f(kv._1, kv._2)))

        def mapValues[V1](f: V => V1)
                       (using ordering: Ordering[K], monoid: Monoid[V1]): CanonicalSortedMap[K, V1,
                monoid.type ] =
            fromSortedMap(self.view.mapValues(f).to(SortedMap))

    /**
     * Extension methods from a non-canoncialized sorted map to a canonicalized one
     */
    extension [K, V](self: SortedMap[K, V])
        /**
         * Apply a mapping function (fmap) to a non-canoncial SortedMap and canonicalize the results, using
         * the [[AdditiveMonoid]] from [[V1]] to determine the zero element
         */
        def mapCanonicalize[K1, V1](f: (K, V) => (K1, V1))
                                   (using v1Monoid: Monoid[V1],
                                    k1Ord: Ordering[K1]): CanonicalSortedMap[K1, V1, v1Monoid.type ] =
            fromSortedMap(self.map(kv => f(kv._1, kv._2)))

        /**
         * Apply a mapping function (fmap) to a non-canonical SortedMap's values and canonicalize the results, using
         * the [[AdditiveMonoid]] from [[V1]] to determine the zero element
         */
        def mapValuesCanonicalize[V1](f: V => V1)
                                     (using v1Monoid: Monoid[V1],
                                      ordering: Ordering[K]
                                     ): CanonicalSortedMap[K, V1, v1Monoid.type ] =
            fromSortedMap(self.view.mapValues(f).to(SortedMap))


    object CombineWith {
//        def combineWithSimple[K, V](
//            opBoth: (V, V) => V
//        )(self: SortedMap[K, V], other: SortedMap[K, V])(using
//            ev: AdditiveMonoid[V],
//            ord: Ordering[K]
//        ): SortedMap[K, V] = {
//            (self.keySet ++ other.keySet).view
//                .map { key =>
//                    val combinedValue =
//                        opBoth(self.getOrElse(key, ev.zero), other.getOrElse(key, ev.zero))
//                    key -> combinedValue
//                }
//                .to(SortedMap)
//        }

        def combineWith[K, VCommon, VResult, VSelf, VOther](
            opBoth: (VCommon, VCommon) => VResult,
            opSelf: VCommon => VResult = identity[VResult],
            opOther: VCommon => VResult = identity[VResult],
            preMapSelf: VSelf => VCommon = identity[VCommon],
            preMapOther: VOther => VCommon = identity[VCommon]
        )(using
          kOrdering: Ordering[K],
          vSelfMonoid : Monoid[VSelf],
          vOtherMonoid: Monoid[VOther],
          vResultMonoid: Monoid[VResult],
                                                           )(
            self: CanonicalSortedMap[K, VSelf, vSelfMonoid.type ],
            other: CanonicalSortedMap[K, VOther, vOtherMonoid.type ]
        ): CanonicalSortedMap[K, VResult, vResultMonoid.type ] = {
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

            inline def appendNonZero(
                builder: mutable.Builder[(K, VResult), SortedMap[K, VResult]],
                z: (K, VResult)
            ): Unit = {
                if z._2 == vResultMonoid.empty then () else builder += z
            }

            inline def concatNonZero(
                builder: mutable.Builder[(K, VResult), SortedMap[K, VResult]],
                m: IterableOnce[(K, VResult)]
            ): Unit = builder ++= m.iterator.filterNot(_._2 == vResultMonoid.empty)

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
    }

    final class CanonicalSortedMapPartialOrder[K, V, @specialized(Int, Double) I, M <: Monoid[V] & Singleton, IM <: Monoid[I] & Singleton](
        compareBoth: (V, V) => I,
        compareLeft: V => I,
        compareRight: V => I
    )(using
        iEq: Eq[I],
        kOrdering: Ordering[K],
        toDouble: ToDouble[I],
        iMonoid : IM,
        vMonoid: M
    ) extends PartialOrder[CanonicalSortedMap[K, V, M]] {
        override def partialCompare(self: CanonicalSortedMap[K, V, M], other: CanonicalSortedMap[K, V, M]): Double = {
            val comparisons: Iterable[I] =
                CombineWith.combineWith[K, V, I, V, V](compareBoth, compareLeft, compareRight)(self, other).values
            // If both maps are empty, then they are equal.
            // If all element-wise comparisons are the same, then the maps are comparable.
            // Otherwise, the maps are incomparable.
            comparisons.headOption.fold(0d)(first =>
                val monotonic = comparisons.forall(_ == first)
                if monotonic then toDouble.toDouble(first) else Double.NaN
            )
        }
    }

    sealed trait ToDouble[I]:
        def toDouble: I => Double

    given ToDouble[Double] with
        override def toDouble: Double => Double = identity

    given ToDouble[Int] with
        override def toDouble: Int => Double = _.toDouble
}