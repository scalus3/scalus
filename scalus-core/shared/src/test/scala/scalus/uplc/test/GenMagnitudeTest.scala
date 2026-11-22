package scalus.uplc
package test

import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite

/** `genMagnitude`: spec `[gen.magnitude.range]`, `[gen.magnitude.sign-split]` and
  * `[gen.magnitude.bit-length]` of docs/internal/test-refactor/d5-generators.md.
  *
  * Samples with fixed seeds, as `ArbitraryInstancesBoundsTest` does, so a share check cannot be
  * flaky.
  */
class GenMagnitudeTest extends AnyFunSuite {
    import ArbitraryInstances.genMagnitude

    private val Samples = 5000

    private def sample[A](gen: Gen[A]): Seq[A] = {
        val params = Gen.Parameters.default.withSize(100)
        Iterator
            .iterate(Seed(42L))(_.slide)
            .map(seed => gen(params, seed))
            .collect { case Some(value) => value }
            .take(Samples)
            .toSeq
    }

    private def share[A](values: Seq[A])(p: A => Boolean): Double =
        values.count(p).toDouble / values.size

    test("rejects min > max") {
        assertThrows[IllegalArgumentException](genMagnitude(BigInt(1), BigInt(0)))
        assertThrows[IllegalArgumentException](genMagnitude(1L, 0L))
    }

    test("stays in the range, including a range of one value") {
        val ranges = Seq[(BigInt, BigInt)](
          (0, 0),
          (5, 5),
          (-7, -7),
          (0, 1000),
          (3, 1000),
          (-1000, -3),
          (-5, 1000),
          (0, BigInt(2).pow(64)),
          (-BigInt(2).pow(64) - 1, BigInt(2).pow(64))
        )
        for (min, max) <- ranges do
            val values = sample(genMagnitude(min, max))
            assert(values.forall(v => min <= v && v <= max), (min, max))
    }

    test("picks each bit length of genMagnitude(0, 1000) with chance 1/11") {
        val values = sample(genMagnitude(0L, 1000L))
        for bits <- 0 to 10 do
            val got = share(values)(v => BigInt(v).bitLength == bits)
            assert(got > 0.07 && got < 0.12, s"bit length $bits: $got")
    }

    test("gives the bounds of a range when they are the only value of their bit length") {
        val values = sample(genMagnitude(BigInt(0), BigInt(2).pow(64)))
        assert(values.contains(BigInt(0)))
        assert(values.contains(BigInt(1)))
        assert(values.contains(BigInt(2).pow(64)))
    }

    test("picks the negative or the non-negative part with equal chance") {
        val values = sample(genMagnitude(-1000000L, 10L))
        val negative = share(values)(_ < 0)
        assert(negative > 0.45 && negative < 0.55, negative)
        // the negative part works on the absolute values: small negatives are as likely as large
        assert(values.contains(-1L))
    }

    test("the Long form gives the values of the BigInt form") {
        val longs = sample(genMagnitude(-50L, 1L << 40))
        val bigs = sample(genMagnitude(BigInt(-50), BigInt(1L << 40)))
        assert(longs.map(BigInt(_)) == bigs)
    }
}
