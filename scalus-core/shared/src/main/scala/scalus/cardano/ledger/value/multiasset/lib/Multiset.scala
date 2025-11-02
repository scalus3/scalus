package scalus.cardano.ledger.value.multiasset.lib

import cats.implicits.catsKernelOrderingForOrder
import spire.algebra.*

import scala.collection.immutable.SortedMap

/** A multiset (AKA a "bag") allows for multiple instances to exist for each of its elements,
  * generalizing the concept of a set (which only allows one instance per element).
  *
  * The number of instances for each element in a multiset is called the "multiplicity" of that
  * element in the set, which generalizes the indicator function of sets.
  *
  * The multiplicity function is implemented via a map from keys to values. In the simplest case,
  * the values are natural numbers directly representing the number of instances per key, defaulting
  * to zero for missing keys. However, the value type is only required to be an additive monoid in
  * this class, which allows for more general concepts of multiplicity (finite, signed,
  * rational-valued, nested, etc.).
  *
  * For the purposes of defining a multiset's multiplicity function, two maps are equivalent if they
  * give the same value for a given key. Therefore, this class canonicalizes its multiplicity map by
  * sorting its keys, removing zero-valued entries, and setting the default to zero.
  *
  * @param multiplicityMap
  *   a sorted map with additively monoidal values (only the additive identity is used here).
  */
case class Multiset[K, V, M <: AdditiveMonoid[V], O <: Order[K]] private (
    multiplicityMap: SortedMap[K, V]
)(using vMonoid: M, kOrder: O) {

    /** Lookup the value in the multiset's canonical map that corresponds to the given key.
      */
    def get(k: K): V = multiplicityMap.getOrElse(k, vMonoid.zero)

    /** Apply a function to every value in the multiset's canonical map. */
    def mapValues[VNew, MNew <: AdditiveMonoid[VNew]](
        f: V => VNew
    )(using vNewMonoid: MNew): Multiset[K, VNew, MNew, O] = {
        Multiset.apply(multiplicityMap.view.mapValues(f).to(SortedMap))
    }

    /** Apply a function to every value in the canonical sorted map, exposing the value's key to the
      * function as an additional argument.
      */
    def mapValuesIndexed[VNew, MNew <: AdditiveMonoid[VNew]](
        f: (K, V) => VNew
    )(using vNewMonoid: MNew): Multiset[K, VNew, MNew, O] = {
        def g(kv: (K, V)): (K, VNew) = (kv._1, f(kv._1, kv._2))
        Multiset.apply(multiplicityMap.view.map(g).to(SortedMap))
    }

    def combineWith[
        VOther,
        VResult,
        MOther <: AdditiveMonoid[VOther],
        MResult <: AdditiveMonoid[VResult],
    ](other: Multiset[K, VOther, MOther, O])(combiner: (V, VOther) => VResult)(using
        vOtherMonoid: MOther,
        vResultMonoid: MResult
    ): Multiset[K, VResult, MResult, O] = MultisetOps.combineWith(this, other)(combiner)

    // Copied from immutable.TreeMap
    override def equals(obj: Any): Boolean = obj match {
        case that: Multiset[?, ?, ?, ?] => multiplicityMap == that.multiplicityMap
        case _                          => false
    }
}

object Multiset {

    /** Create a multiset.
      * @param sortedMap
      *   a sorted map with additively monoidal values (only the additive identity is used here).
      *
      * @return
      *   a multiset with a [[V]]-valued multiplicity function corresponding to the canonicalized
      *   sorted map. Canonicalizing a sorted map means removing all zero-valued entries and setting
      *   its default value for missing keys set to zero.
      */
    def apply[K, V, M <: AdditiveMonoid[V], O <: Order[K]](
        sortedMap: SortedMap[K, V]
    )(using vMonoid: M, kOrder: O): Multiset[K, V, M, O] = {
        val canonicalMap = sortedMap.filterNot(_._2 == vMonoid.zero).withDefaultValue(vMonoid.zero)
        new Multiset[K, V, M, O](canonicalMap)
    }

    def empty[K, V, M <: AdditiveMonoid[V], O <: Order[K]](using
        vMonoid: M,
        kOrder: O
    ): Multiset[K, V, M, O] =
        new Multiset[K, V, M, O](SortedMap.empty)

    export MultisetAlgebra.*
    export MultisetOps.*
}

private object MultisetAlgebra {
    import spire.algebra
    import MultisetOps.*

    class Algebra[K, V, M <: algebra.AdditiveMonoid[V], O <: algebra.Order[K]](using
        vMonoid: M,
        kOrder: O
    ) {
        trait PartialOrder[I, MI <: algebra.AdditiveMonoid[I]](
            comparer: (V, V) => I,
            toDouble: I => Double
        )(using iMonoid: MI)
            extends algebra.PartialOrder[Multiset[K, V, M, O]],
              org.scalactic.Equality[Multiset[K, V, M, O]] {
            override def areEqual(self: Multiset[K, V, M, O], other: Any): Boolean =
                self.equals(other)

            override def eqv(self: Multiset[K, V, M, O], other: Multiset[K, V, M, O]): Boolean =
                self.equals(other)

            override def partialCompare(
                self: Multiset[K, V, M, O],
                other: Multiset[K, V, M, O]
            ): Double =
                val comparisons: Iterable[I] =
                    // If both keys exist, compare the values.
                    // If only the left key exists, compare the left value against zero.
                    // If only the right key exists, compare the right value against zero.
                    MultisetOps.combineWith(self, other)(comparer).multiplicityMap.values
                // If both maps are empty, then they are equal.
                // If all element-wise comparisons are the same, then the maps are comparable.
                // Otherwise, the maps are incomparable.
                comparisons.headOption.fold(0d)(first =>
                    val monotonic = comparisons.forall(_ == first)
                    if monotonic then toDouble(first) else Double.NaN
                )
        }

        object PartialOrder {
            class OrderedElements(comparer: (V, V) => Int)(using
                algebra.AdditiveMonoid[Int]
            ) extends PartialOrder[Int, algebra.AdditiveMonoid[Int]](comparer, _.toDouble)

            class PartiallyOrderedElements(comparer: (V, V) => Double)(using
                algebra.AdditiveMonoid[Double]
            ) extends PartialOrder[Double, algebra.AdditiveMonoid[Double]](comparer, identity)
        }

        trait AdditiveMonoid extends algebra.AdditiveMonoid[Multiset[K, V, M, O]] {
            override def zero: Multiset[K, V, M, O] = Multiset.empty
            override def plus(
                self: Multiset[K, V, M, O],
                other: Multiset[K, V, M, O]
            ): Multiset[K, V, M, O] = combineWith(self, other)(vMonoid.plus)
        }

        trait AdditiveCommutativeGroup[VG <: algebra.AdditiveAbGroup[V]](using vAbGroup: VG)
            extends AdditiveMonoid,
              algebra.AdditiveAbGroup[Multiset[K, V, M, O]] {
            override def negate(self: Multiset[K, V, M, O]): Multiset[K, V, M, O] =
                self.mapValues(vAbGroup.negate)
            override def minus(
                self: Multiset[K, V, M, O],
                other: Multiset[K, V, M, O]
            ): Multiset[K, V, M, O] = combineWith(self, other)(vAbGroup.minus)

        }

        trait CModule[S, SR <: CRing[S], VG <: algebra.CModule[V, S]](using
            sCRing: SR,
            vCModule: VG
        ) extends AdditiveCommutativeGroup[VG],
              algebra.CModule[Multiset[K, V, M, O], S] {
            override def scalar: SR = sCRing
            override def timesl(s: S, self: Multiset[K, V, M, O]): Multiset[K, V, M, O] =
                self.mapValues(vCModule.timesl(s, _))
        }

        trait VectorSpace[S, SF <: Field[S], VS <: algebra.VectorSpace[V, S]](using sField: SF)
            extends CModule[S, SF, VS],
              algebra.VectorSpace[Multiset[K, V, M, O], S] {
            override def scalar: SF = sField
        }
    }
}

private object MultisetOps {
    def combineWith[
        K,
        VSelf,
        VOther,
        VResult,
        MSelf <: AdditiveMonoid[VSelf],
        MOther <: AdditiveMonoid[VOther],
        MResult <: AdditiveMonoid[VResult],
        O <: Order[K]
    ](
        self: Multiset[K, VSelf, MSelf, O],
        other: Multiset[K, VOther, MOther, O]
    )(combiner: (VSelf, VOther) => VResult)(using
        kOrder: O,
        vSelfMonoid: MSelf,
        vOtherMonoid: MOther,
        vResultMonoid: MResult
    ): Multiset[K, VResult, MResult, O] = {
        import scala.annotation.tailrec
        import scala.collection.mutable
        import scala.math.Ordered.orderingToOrdered

        val selfIterator: Iterator[(K, VSelf)] = self.multiplicityMap.iterator
        val otherIterator: Iterator[(K, VOther)] = other.multiplicityMap.iterator
        val resultBuilder: mutable.Builder[(K, VResult), SortedMap[K, VResult]] =
            SortedMap.newBuilder

        inline def processBoth(x: (K, VSelf), y: (K, VOther)): (K, VResult) =
            (x._1, combiner(x._2, y._2))

        inline def processSelf(x: (K, VSelf)): (K, VResult) =
            (x._1, combiner(x._2, vOtherMonoid.zero))

        inline def processOther(y: (K, VOther)): (K, VResult) =
            (y._1, combiner(vSelfMonoid.zero, y._2))

        inline def appendNonZero(
            builder: mutable.Builder[(K, VResult), SortedMap[K, VResult]],
            z: (K, VResult)
        ): Unit = if z._2 == vResultMonoid.zero then () else builder += z

        inline def concatNonZero(
            builder: mutable.Builder[(K, VResult), SortedMap[K, VResult]],
            m: Iterator[(K, VResult)]
        ): Unit = builder ++= m.iterator.filterNot(_._2 == vResultMonoid.zero)

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

        val newCanonicalMap = if selfIterator.hasNext then {
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

        Multiset(newCanonicalMap)
    }
}
