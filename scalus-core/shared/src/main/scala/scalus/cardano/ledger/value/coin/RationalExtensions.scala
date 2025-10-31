package scalus.cardano.ledger.value.coin

import spire.math.{Rational, SafeLong}

object RationalExtensions {
    extension (self: Rational)
        /** Half-even rounding (also called banker's rounding). Rounds the to the nearest integer;
          * if equidistant between two integers, then rounds to the even integer.
          *
          * Implementation ported from Haskell's `GHC.Real.RealFrac.{properFraction, round}`:
          *
          * [[https://hackage.haskell.org/package/base-4.21.0.0/docs/GHC-Real.html#t:RealFrac]]
          *
          * @return
          *   the rounded integer. Note that this is different from [[Rational.round]], which
          *   returns a [[Rational]] with a denominator of one.
          */
        def roundHalfEven: SafeLong = {
            // The whole part of the proper fraction.
            // Divide the numerator by the denominator, truncating toward zero.
            val whole = self.numerator / self.denominator

            // The fractional part of the proper fraction.
            val fraction = Rational(self.numerator % self.denominator, self.denominator)

            // The whole part incremented away from zero.
            val awayFromZero = if whole.signum < 0 then whole - 1 else whole + 1

            // The distance between the absolute fractional part and one half.
            val distanceToHalf = fraction.abs - Rational(1, 2)

            distanceToHalf.signum match {
                // Round toward zero if absolute fractional part is less than half.
                case -1 => whole
                // Round away from zero if absolute fractional part is more than half.
                case 1 => awayFromZero
                // Break ties by rounding toward whichever nearest integer is even.
                case 0 => if whole.isEven then whole else awayFromZero
            }
        }
}
