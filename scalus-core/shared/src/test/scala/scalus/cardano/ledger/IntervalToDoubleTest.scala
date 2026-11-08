package scalus.cardano.ledger

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class IntervalToDoubleTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    private def assertBits(actual: Double, expected: Double): Unit =
        assert(
          java.lang.Double.doubleToLongBits(actual) == java.lang.Double.doubleToLongBits(expected)
        )

    private def checkBoth(n: Long, d: Long, expected: Double): Unit = {
        assertBits(NonNegativeInterval(n, d).toDouble, expected)
        assertNearest(n, d, expected)
        if n <= d then assertBits(UnitInterval(n, d).toDouble, expected)
    }

    test("toDouble rounds the fraction below one without first rounding its terms") {
        checkBoth(9999999999999999L, 10000000000000000L, Math.nextDown(1.0))
        assert(
          java.lang.Double.doubleToLongBits(
            UnitInterval(9999999999999999L, 10000000000000000L).toDouble
          ) ==
              0x3fefffffffffffffL
        )
    }

    test("toDouble avoids double rounding through a decimal MathContext") {
        checkBoth(
          7622910750321615326L,
          9213255080030466181L,
          java.lang.Double.longBitsToDouble(0x3fea79f07e598426L)
        )
    }

    test("toDouble handles terms above two to the power of 53") {
        checkBoth(9007199254740993L, 9007199254740995L, Math.nextDown(Math.nextDown(1.0)))
        checkBoth(Long.MaxValue - 1, Long.MaxValue, 1.0)
        checkBoth(9007199254740993L, 1L, 9007199254740992.0)
    }

    test("toDouble rounds exact binary midpoints to the even significand") {
        checkBoth(9007199254740993L, 18014398509481984L, 0.5)
        checkBoth(9007199254740995L, 18014398509481984L, Math.nextUp(Math.nextUp(0.5)))
        checkBoth(18014398509481983L, 18014398509481984L, 1.0)
        checkBoth(9007199254740995L, 1L, 9007199254740996.0)
    }

    test("toDouble handles zero, exact halves, powers of two and Long extremes") {
        checkBoth(0L, Long.MaxValue, 0.0)
        checkBoth(1L, 2L, 0.5)
        checkBoth(1L << 61, 1L << 62, 0.5)
        checkBoth(Long.MaxValue, Long.MaxValue, 1.0)
        checkBoth(1L, Long.MaxValue, Math.pow(2.0, -63))
        checkBoth(Long.MaxValue, 1L, Math.pow(2.0, 63))
        checkBoth(1L << 62, 1L, Math.pow(2.0, 62))
    }

    // Decode the result and its neighbours as exact dyadic rationals, independently of conversion.
    private def exactDouble(value: Double): (BigInt, BigInt) = {
        val bits = java.lang.Double.doubleToLongBits(value)
        val biasedExponent = ((bits >>> 52) & 0x7ff).toInt
        val fraction = BigInt(bits & 0x000fffffffffffffL)
        val significand = if biasedExponent == 0 then fraction else fraction + (BigInt(1) << 52)
        val exponent = if biasedExponent == 0 then -1074 else biasedExponent - 1075
        if exponent >= 0 then (significand << exponent, BigInt(1))
        else (significand, BigInt(1) << -exponent)
    }

    private def assertNearest(n: Long, d: Long, result: Double): Unit = {
        assert(result >= 0.0 && !result.isInfinity && !result.isNaN)
        if n == 0 then assertBits(result, 0.0)
        else {
            assert(result > 0.0)
            val (rn, rd) = exactDouble(result)
            val even = (java.lang.Double.doubleToLongBits(result) & 1L) == 0L
            for neighbour <- Seq(Math.nextDown(result), Math.nextUp(result)) do {
                val (nn, nd) = exactDouble(neighbour)
                val resultDistance = (BigInt(n) * rd - rn * d).abs
                val neighbourDistance = (BigInt(n) * nd - nn * d).abs
                val comparison = (resultDistance * nd).compare(neighbourDistance * rd)
                assert(
                  comparison < 0 || (comparison == 0 && even),
                  s"$n/$d -> $result, neighbour $neighbour"
                )
            }
        }
    }

    private val numerator = Gen.chooseNum(0L, Long.MaxValue)
    private val denominator = Gen.chooseNum(1L, Long.MaxValue)

    test("NonNegativeInterval.toDouble is nearest with ties to even for random Long fractions") {
        forAll(numerator, denominator, minSuccessful(10000)) { (n, d) =>
            assertNearest(n, d, NonNegativeInterval(n, d).toDouble)
        }
    }

    test("UnitInterval.toDouble is nearest with ties to even for random Long fractions") {
        val fractions = denominator.flatMap(d => Gen.chooseNum(0L, d).map(n => (n, d)))
        forAll(fractions, minSuccessful(10000)) { case (n, d) =>
            assertNearest(n, d, UnitInterval(n, d).toDouble)
        }
    }
}
