package scalus.cardano.ledger.value

import scala.math.BigDecimal.RoundingMode.RoundingMode
import spire.algebra.*
import spire.implicits.*
import spire.math.SafeLong

import scala.collection.immutable.SortedMap
import scalus.ledger.api.v3.{PolicyId, TokenName}

type MultiAsset = MultiAsset.MultiAsset

object MultiAsset {
    import CombineWith.*

    opaque type MultiAsset = SortedMap[PolicyId, Inner]

    def apply(x: SortedMap[PolicyId, Inner]): MultiAsset = x

    def zero: MultiAsset = SortedMap.empty

    given partialOrder: PartialOrder[MultiAsset] = {
        // If both keys exist, compare the values.
        // If only the left key exists, compare the left value against zero.
        // If only the right key exists, compare the right value against zero.
        SortedMapPartialOrder[TokenName, Inner, Double](
          compareBoth = MultiAsset.Inner.partialOrder.partialCompare,
          compareLeft = MultiAsset.Inner.partialOrder.partialCompare(_, Inner.zero),
          compareRight = MultiAsset.Inner.partialOrder.partialCompare(Inner.zero, _)
        )
    }

    type Unbounded = Unbounded.Unbounded

    object Unbounded {
        opaque type Unbounded = SortedMap[PolicyId, Inner.Unbounded]

        def apply(x: SortedMap[PolicyId, Inner.Unbounded]): Unbounded = x

        def zero: Unbounded = SortedMap.empty

        extension (self: Unbounded)
            def toMultiAsset: Either[MultiAsset.ArithmeticError, MultiAsset] = try {
                Right(self.unsafeToMultiAsset)
            } catch {
                case e: MultiAsset.ArithmeticError => Left(e)
            }

            def unsafeToMultiAsset: MultiAsset = self.view
                .map((policyId: PolicyId, innerUnbounded: Inner.Unbounded) =>
                    try {
                        (policyId, innerUnbounded.unsafeToInner)
                    } catch {
                        case e: Inner.ArithmeticError =>
                            throw MultiAsset.ArithmeticError.withPolicyId(e, policyId)
                    }
                )
                .to(SortedMap)

            def scale(s: SafeLong): Unbounded = ops.timesl(s, self)

            def scale(s: BigDecimal): Fractional =
                Fractional(self.view.mapValues(_.scale(s)).to(SortedMap))

        import Inner.Unbounded.ops as innerOps
        import Inner.Unbounded.partialOrder as innerPartialOrder

        given partialOrder: PartialOrder[Unbounded] = {
            // If both keys exist, compare the values.
            // If only the left key exists, compare the left value against zero.
            // If only the right key exists, compare the right value against zero.
            SortedMapPartialOrder[TokenName, Inner.Unbounded, Double](
              compareBoth = innerPartialOrder.partialCompare,
              compareLeft = innerPartialOrder.partialCompare(_, Inner.Unbounded.zero),
              compareRight = innerPartialOrder.partialCompare(Inner.Unbounded.zero, _)
            )
        }

        given ops: CModule[Unbounded, SafeLong] with {
            override def scalar: CRing[SafeLong] = CRing[SafeLong]

            override def zero: Unbounded = Unbounded.zero

            override def negate(self: Unbounded): Unbounded =
                self.view.mapValues(innerOps.negate).to(SortedMap)

            override def plus(self: Unbounded, other: Unbounded): Unbounded =
                combineWith(innerOps.plus)(self, other)

            override def minus(self: Unbounded, other: Unbounded): Unbounded =
                combineWith(innerOps.plus, identity, innerOps.negate)(self, other)

            override def timesl(s: SafeLong, self: Unbounded): Unbounded =
                self.view.mapValues(_.scale(s)).to(SortedMap)
        }
    }

    type Fractional = Fractional.Fractional

    object Fractional {
        opaque type Fractional = SortedMap[PolicyId, Inner.Fractional]

        def apply(x: SortedMap[PolicyId, Inner.Fractional]): Fractional = x

        def zero: Fractional = ops.zero

        extension (self: Fractional)
            def round(mode: RoundingMode): Unbounded =
                Unbounded(self.view.mapValues(_.round(mode)).to(SortedMap))

            def toMultiAsset(mode: RoundingMode): Either[MultiAsset.ArithmeticError, MultiAsset] =
                try {
                    Right(self.unsafeToMultiAsset(mode))
                } catch {
                    case e: MultiAsset.ArithmeticError => Left(e)
                }

            def unsafeToMultiAsset(mode: RoundingMode): MultiAsset = self.view
                .map((policyId: PolicyId, innerFractional: Inner.Fractional) =>
                    try {
                        (policyId, innerFractional.unsafeToInner(mode))
                    } catch {
                        case e: Inner.ArithmeticError =>
                            throw MultiAsset.ArithmeticError.withPolicyId(e, policyId)
                    }
                )
                .to(SortedMap)

            def scale(s: BigDecimal): Fractional = ops.timesl(s, self)

        import Inner.Fractional.ops as innerOps
        import Inner.Fractional.partialOrder as innerPartialOrder

        given partialOrder: PartialOrder[Fractional] = {
            // If both keys exist, compare the values.
            // If only the left key exists, compare the left value against zero.
            // If only the right key exists, compare the right value against zero.
            SortedMapPartialOrder[TokenName, Inner.Fractional, Double](
              compareBoth = innerPartialOrder.partialCompare,
              compareLeft = innerPartialOrder.partialCompare(_, Inner.Fractional.zero),
              compareRight = innerPartialOrder.partialCompare(Inner.Fractional.zero, _)
            )
        }

        given ops: VectorSpace[Fractional, BigDecimal] with
            override def scalar: Field[BigDecimal] = Field[BigDecimal]

            override def zero: Fractional = Fractional.zero

            override def negate(self: Fractional): Fractional =
                self.view.mapValues(innerOps.negate).to(SortedMap)

            override def plus(self: Fractional, other: Fractional): Fractional =
                combineWith(innerOps.plus)(self, other)

            override def minus(self: Fractional, other: Fractional): Fractional =
                combineWith(innerOps.minus, identity, innerOps.negate)(self, other)

            override def timesl(s: BigDecimal, self: Fractional): Fractional =
                self.view.mapValues(_.scale(s)).to(SortedMap)
    }

    sealed trait ArithmeticError extends Throwable:
        def policyId: PolicyId
        def tokenName: TokenName

    object ArithmeticError {
        final case class Underflow(policyId: PolicyId, tokenName: TokenName) extends ArithmeticError
        final case class Overflow(policyId: PolicyId, tokenName: TokenName) extends ArithmeticError

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
        opaque type Inner = SortedMap[TokenName, Coin]

        def apply(x: SortedMap[TokenName, Coin]): Inner = x

        def zero: Inner = SortedMap.empty

        type Unbounded = Unbounded.Unbounded

        given partialOrder: PartialOrder[Inner] = {
            // If both keys exist, compare the values.
            // If only the left key exists, compare the left value against zero.
            // If only the right key exists, compare the right value against zero.
            SortedMapPartialOrder[TokenName, Coin, Int](
              compareBoth = Coin.order.compare,
              compareLeft = _.signum,
              compareRight = -_.signum
            )
        }

        object Unbounded {
            opaque type Unbounded = SortedMap[TokenName, Coin.Unbounded]

            def apply(x: SortedMap[TokenName, Coin.Unbounded]): Unbounded = x

            def zero: Unbounded = SortedMap.empty

            extension (self: Unbounded)
                def toInner: Either[Inner.ArithmeticError, Inner] = try {
                    Right(self.unsafeToInner)
                } catch {
                    case e: Inner.ArithmeticError => Left(e)
                }

                def unsafeToInner: Inner = self.view
                    .map((tokenName: TokenName, coinUnbounded: Coin.Unbounded) =>
                        try {
                            (tokenName, coinUnbounded.unsafeToCoin)
                        } catch {
                            case e: Coin.ArithmeticError =>
                                throw Inner.ArithmeticError.withTokenName(e, tokenName)
                        }
                    )
                    .to(SortedMap)

                def toFractional: Fractional =
                    Fractional(self.view.mapValues(_.toCoinFractional).to(SortedMap))

                def scale(s: SafeLong): Unbounded = ops.timesl(s, self)

                def scale(s: BigDecimal): Fractional =
                    Fractional(self.view.mapValues(_.scale(s)).to(SortedMap))

            import Coin.Unbounded.ops as coinOps
            import Coin.Unbounded.order as coinOrder

            given partialOrder: PartialOrder[Unbounded] =
                // If both keys exist, compare the values.
                // If only the left key exists, compare the left value against zero.
                // If only the right key exists, compare the right value against zero.
                SortedMapPartialOrder[TokenName, Coin.Unbounded, Int](
                  compareBoth = coinOrder.compare,
                  compareLeft = _.signum,
                  compareRight = -_.signum
                )

            given ops: CModule[Unbounded, SafeLong] with {
                override def scalar: CRing[SafeLong] = CRing[SafeLong]

                override def zero: Unbounded = Unbounded.zero

                override def negate(self: Unbounded): Unbounded =
                    self.view.mapValues(coinOps.negate).to(SortedMap)

                override def plus(self: Unbounded, other: Unbounded): Unbounded =
                    combineWith(coinOps.plus)(self, other)

                override def minus(self: Unbounded, other: Unbounded): Unbounded =
                    combineWith(coinOps.plus, identity, coinOps.negate)(self, other)

                override def timesl(s: SafeLong, self: Unbounded): Unbounded =
                    self.view.mapValues(_.scale(s)).to(SortedMap)
            }
        }

        type Fractional = Fractional.Fractional

        object Fractional {
            opaque type Fractional = SortedMap[TokenName, Coin.Fractional]

            def apply(x: SortedMap[TokenName, Coin.Fractional]): Fractional = x

            def zero: Fractional = SortedMap.empty

            extension (self: Fractional)
                def round(mode: RoundingMode): Unbounded =
                    Unbounded(self.view.mapValues(_.round(mode)).to(SortedMap))

                def toInner(mode: RoundingMode): Either[Inner.ArithmeticError, Inner] = try {
                    Right(self.unsafeToInner(mode))
                } catch {
                    case e: Inner.ArithmeticError => Left(e)
                }

                def unsafeToInner(mode: RoundingMode): Inner = self.view
                    .map((tokenName: TokenName, coinFractional: Coin.Fractional) =>
                        try {
                            (tokenName, coinFractional.unsafeToCoin(mode))
                        } catch {
                            case e: Coin.ArithmeticError =>
                                throw Inner.ArithmeticError.withTokenName(e, tokenName)
                        }
                    )
                    .to(SortedMap)

                def scale(s: BigDecimal): Fractional = ops.timesl(s, self)

            import Coin.Fractional.ops as coinOps
            import Coin.Fractional.order as coinOrder

            given partialOrder: PartialOrder[Fractional] =
                // If both keys exist, compare the values.
                // If only the left key exists, compare it against zero.
                // If only the right key exists, compare it against zero.
                SortedMapPartialOrder[TokenName, Coin.Fractional, Int](
                  compareBoth = coinOrder.compare,
                  compareLeft = _.signum,
                  compareRight = -_.signum
                )

            given ops: VectorSpace[Fractional, BigDecimal] with
                override def scalar: Field[BigDecimal] = Field[BigDecimal]

                override def zero: Fractional = Fractional.zero

                override def negate(self: Fractional): Fractional =
                    self.view.mapValues(coinOps.negate).to(SortedMap)

                override def plus(self: Fractional, other: Fractional): Fractional =
                    combineWith(coinOps.plus)(self, other)

                override def minus(self: Fractional, other: Fractional): Fractional =
                    combineWith(coinOps.minus, identity, coinOps.negate)(self, other)

                override def timesl(s: BigDecimal, self: Fractional): Fractional =
                    self.view.mapValues(_.scale(s)).to(SortedMap)
        }

        sealed trait ArithmeticError extends Throwable:
            def tokenName: TokenName

        object ArithmeticError {
            final case class Underflow(tokenName: TokenName) extends ArithmeticError
            final case class Overflow(tokenName: TokenName) extends ArithmeticError

            def withTokenName(
                e: Coin.ArithmeticError,
                tokenName: TokenName
            ): Inner.ArithmeticError =
                e match {
                    case Coin.ArithmeticError.Underflow =>
                        Inner.ArithmeticError.Underflow(tokenName)
                    case Coin.ArithmeticError.Overflow => Inner.ArithmeticError.Overflow(tokenName)
                }
        }
    }

    private object CombineWith {
        def combineWith[K, V](opBoth: (V, V) => V)(
            self: SortedMap[K, V],
            other: SortedMap[K, V]
        )(using Ordering[K]): SortedMap[K, V] = {
            combineWith(opBoth, identity, identity)(self, other)
        }

        def combineWith[K, V, V2](opBoth: (V, V) => V2, opSelf: V => V2, opOther: V => V2)(
            self: SortedMap[K, V],
            other: SortedMap[K, V]
        )(using Ordering[K]): SortedMap[K, V2] = {
            import scala.annotation.tailrec
            import scala.collection.mutable
            import scala.math.Ordered.orderingToOrdered

            val selfIterator: Iterator[(K, V)] = self.iterator
            val otherIterator: Iterator[(K, V)] = other.iterator
            val resultBuilder: mutable.Builder[(K, V2), SortedMap[K, V2]] = SortedMap.newBuilder

            inline def mapVal(f: V => V2)(z: (K, V)): (K, V2) = (z._1, f(z._2))

            // Warning: this function mutates its arguments!
            @tailrec
            def loop(
                x: (K, V),
                xs: Iterator[(K, V)],
                y: (K, V),
                ys: Iterator[(K, V)],
                builder: mutable.Builder[(K, V2), SortedMap[K, V2]]
            ): SortedMap[K, V2] = {
                if x._1 < y._1 then {
                    // Process `x` now and `y` later
                    builder += mapVal(opSelf)(x)
                    if xs.hasNext then {
                        // Continue looping with the next `x` and same `y`
                        val xNext = xs.next()
                        loop(xNext, xs, y, ys, builder)
                    } else {
                        // Process the remaining `ys` and return
                        builder ++= ys.map(mapVal(opOther))
                        builder.result()
                    }
                } else if x._1 == y._1 then {
                    // Process both `x` and `y` now
                    builder += mapVal(opBoth(x._2, _))(y)
                    if xs.hasNext then
                        if ys.hasNext then {
                            // Continue looping with the next `x` and next `y`
                            val xNext = xs.next()
                            val yNext = ys.next()
                            loop(xNext, xs, yNext, ys, builder)
                        } else {
                            // Process the remaining `xs` and return
                            builder ++= xs.map(mapVal(opSelf))
                            builder.result()
                        }
                    else {
                        // Process the remaining `ys` and return
                        builder ++= ys.map(mapVal(opOther))
                        builder.result()
                    }
                } else {
                    // Process `y` now and `x` later
                    val yMapped = mapVal(opOther)(y)
                    builder += yMapped
                    if ys.hasNext then {
                        // Continue looping with the same `x` and next `y`
                        val yNext = ys.next()
                        loop(x, xs, yNext, ys, builder)
                    } else {
                        // Process the remaining `xs` and return
                        builder ++= xs.map(mapVal(opSelf))
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
                    // Process the remaining entries in `self` and return
                    resultBuilder ++= selfIterator.map(mapVal(opSelf))
                    resultBuilder.result()
                }
            } else {
                // Process the remaining entries in `other` and return
                resultBuilder ++= otherIterator.map(mapVal(opOther))
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

    private trait ToDouble[I]:
        def toDouble: I => Double

    private given ToDouble[Double] with
        override def toDouble: Double => Double = identity

    private given ToDouble[Int] with
        override def toDouble: Int => Double = _.toDouble
}
