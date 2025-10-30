package scalus.cardano.ledger.value.multiasset

import spire.algebra.*
import spire.implicits.*

import scala.collection.immutable.SortedMap

private object UtilsSortedMap {
    object Canonical {
        def sortedMap[K, V](
            self: SortedMap[K, V]
        )(using vMonoid: AdditiveMonoid[V], vEq: Eq[V]): SortedMap[K, V] =
            Canonical.sortedMap(self, vMonoid.zero)

        def sortedMap[K, V](self: SortedMap[K, V], zero: V)(using vEq: Eq[V]): SortedMap[K, V] =
            self.filterNot(_._2 === zero)
    }

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
        )(
            self: SortedMap[K, VSelf],
            other: SortedMap[K, VOther]
        )(using
            kOrdering: Ordering[K],
            vResultMonoid: AdditiveMonoid[VResult],
            vResultEq: Eq[VResult]
        ): SortedMap[K, VResult] = {
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
            ): Unit = if z._2.isZero then () else builder += z

            inline def concatNonZero(
                builder: mutable.Builder[(K, VResult), SortedMap[K, VResult]],
                m: IterableOnce[(K, VResult)]
            ): Unit = builder ++= m.iterator.filterNot(_._2.isZero)

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

    final class SortedMapPartialOrder[K, V, @specialized(Int, Double) I](
        compareBoth: (V, V) => I,
        compareLeft: V => I,
        compareRight: V => I
    )(using
        iEq: Eq[I],
        iMonoid: AdditiveMonoid[I],
        kOrdering: Ordering[K],
        toDouble: ToDouble[I],
    ) extends PartialOrder[SortedMap[K, V]] {
        override def partialCompare(self: SortedMap[K, V], other: SortedMap[K, V]): Double = {
            val comparisons: Iterable[I] =
                CombineWith.combineWith(compareBoth, compareLeft, compareRight)(self, other).values
            // If both maps are empty, then they are equal.
            // If all element-wise comparisons are the same, then the maps are comparable.
            // Otherwise, the maps are incomparable.
            comparisons.headOption.fold(0d)(first =>
                val monotonic = comparisons.forall(_ === first)
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
