package scalus.cardano.ledger.value.coin

import spire.algebra.*
import spire.implicits.*
import spire.math.{Rational, SafeLong}

private object Aggregate {
    def average[T <: SafeLong | Rational, R](
        self: IterableOnce[T],
        convert: T => R = identity[R]
    )(using evT: AdditiveMonoid[T], evR: VectorSpace[R, Rational]): Option[R] = {
        val (sum, length) = Aggregate.sumLength(self)

        Option.when(length > 0)(convert(sum) :/ length)
    }

    def max[T](self: IterableOnce[T])(using ev: Ordering[T]): T = self.iterator.max

    def min[T](self: IterableOnce[T])(using ev: Ordering[T]): T = self.iterator.min

    def sum[T <: SafeLong | Rational](self: IterableOnce[T])(using ev: AdditiveMonoid[T]): T =
        self.iterator.foldRight(ev.zero)(ev.plus)

    private def sumLength[T <: SafeLong | Rational](
        self: IterableOnce[T]
    )(using ev: AdditiveMonoid[T]): (T, Int) = {
        type Acc = (T, Int)

        def f(x: T, acc: Acc): Acc = (acc._1 + x, acc._2 + 1)

        self.iterator.foldRight((ev.zero, 0))(f)
    }
}
