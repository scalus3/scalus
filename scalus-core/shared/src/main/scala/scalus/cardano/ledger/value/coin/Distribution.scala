package scalus.cardano.ledger.value.coin

import cats.data.NonEmptyList
import spire.implicits.*
import spire.math.{Rational, SafeLong}

object Distribution {

    /** A non-empty distribution of rational weights that sum to one.
      */
    opaque type NormalizedWeights = NonEmptyList[Rational]

    /** Construct a non-empty distribution of rational weights that sum to one.
      *
      * @param weights
      *   a non-empty sequence of rational weights that don't necessarily sum to one.
      *
      * @return
      *   If the weights' sum is non-zero, a non-empty distribution the [[weights]] have been
      *   divided by their sum.
      */
    def normalizeWeights[Record](
        weights: NonEmptyList[Record],
        selector: Record => Rational = identity[Rational]
    ): Option[NormalizedWeights] = {
        val weightsRational = weights.map(selector)
        val totalWeight = sumWeights(weightsRational)
        Option.when(totalWeight > 0 && weightsRational.forall(_ >= 0))(
          weightsRational.map(_ / totalWeight)
        )
    }

    /** For testing purposes only. */
    def unsafeNormalizeWeights[Record](
        weights: NonEmptyList[Record],
        selector: Record => Rational = identity[Rational]
    ): NormalizedWeights =
        normalizeWeights(weights, selector).getOrElse(throw ArithmeticException("/ by zero"))

    private def sumWeights(weightsRational: NonEmptyList[Rational]): Rational =
        weightsRational.foldLeft(Rational(0))(_ + _)

    extension (weights: NormalizedWeights)
        def underlying: NonEmptyList[Rational] = weights

        def numberOfWeights: Int = weights.length

        def totalWeight: Rational = sumWeights(weights)

        /** Divide a given amount of fungible items into integral shares according to the
          * distribution of rational weights. If there is a surplus amount left after this initial
          * distribution, it is evenly spread across the shares (in order) until it is exhausted.
          *
          * The sum of the shares is always equal to the amount that was apportioned.
          *
          * @param amount
          *   the amount of fungible items to distribute
          */
        def distribute[I](amount: SafeLong): NonEmptyList[SafeLong] = {
            val amountRational: Rational = amount.toRational

            def calcQuotaFloored(w: Rational): SafeLong = amount * w.numerator / w.denominator

            val quotasFloored = weights.map(calcQuotaFloored)

            val surplus: SafeLong = amount - quotasFloored.foldLeft(SafeLong(0))(_ + _)

            val surplusDiv: SafeLong = surplus / weights.length

            val surplusRem: SafeLong = surplus % weights.length

            /** To each quota:
              *   - add the surplusDiv (if any) every time
              *   - add one item from surplusRem while it lasts, first-come first-served.
              */
            def allocator(quota: SafeLong, index: Int) =
                quota + surplusDiv + (if index < surplusRem.abs then 1 else 0) * surplusRem.signum

            quotasFloored.zipWithIndex.map(allocator.tupled)
        }
}
