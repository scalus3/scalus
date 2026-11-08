package scalus.cardano.ledger.internal

private[ledger] object RationalToDouble {

    /** Converts a non-negative Long numerator and positive Long denominator with one binary
      * rounding step. Every positive ratio in this domain rounds to a finite, normal double.
      */
    def apply(numerator: Long, denominator: Long): Double = {
        if numerator == 0 then 0.0
        else {
            val n = BigInt(numerator)
            val d = BigInt(denominator)
            val estimate = n.bitLength - d.bitLength
            val belowEstimate =
                if estimate >= 0 then n < (d << estimate)
                else (n << -estimate) < d
            val exponent = if belowEstimate then estimate - 1 else estimate

            // Scaling to the significand keeps the quotient and remainder exact until rounding.
            val shift = 52 - exponent
            val scaledNumerator = if shift >= 0 then n << shift else n
            val scaledDenominator = if shift >= 0 then d else d << -shift
            val (quotient, remainder) = scaledNumerator /% scaledDenominator
            val midpointComparison = (remainder << 1).compare(scaledDenominator)
            val roundUp = midpointComparison > 0 ||
                (midpointComparison == 0 && quotient.testBit(0))
            val significand = if roundUp then quotient + 1 else quotient

            // A significand carry increments the exponent through the addition.
            val fraction = significand.toLong - (1L << 52)
            java.lang.Double.longBitsToDouble(((exponent + 1023).toLong << 52) + fraction)
        }
    }
}
