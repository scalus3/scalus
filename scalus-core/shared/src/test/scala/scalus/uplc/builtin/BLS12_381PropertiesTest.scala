package scalus.uplc.builtin

import org.scalacheck.Gen
import org.scalactic.anyvals.PosInt
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.builtin.PlatformSpecific.{bls12_381_G1_compressed_zero, bls12_381_G2_compressed_zero, bls12_381_scalar_period}
import scalus.uplc.builtin.bls12_381.{G1Element, G2Element}
import scalus.uplc.test.ArbitraryInstances

/** Algebraic properties of the BLS12-381 builtins of the platform. The library is the oracle for
  * the group law itself; these properties tie the builtins to each other: MSM to scalarMul and add,
  * uncompress to compress, neg to add, and the pairing to scalarMul.
  */
class BLS12_381PropertiesTest
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances {
    private val r = bls12_381_scalar_period

    /** 0, r (which acts as 0), or any value in [-3r, 3r]. */
    private val genScalar: Gen[BigInt] =
        Gen.oneOf(Gen.const(BigInt(0)), Gen.const(r), Gen.choose[BigInt](-3 * r, 3 * r))

    /** One group with its builtins. G2 is slower, notably with noble on JS, so it runs fewer cases.
      */
    private case class Group[P](
        name: String,
        point: Gen[P],
        zero: P,
        add: (P, P) => P,
        scalarMul: (BigInt, P) => P,
        neg: P => P,
        compress: P => ByteString,
        uncompress: ByteString => P,
        msm: (Seq[BigInt], Seq[P]) => P,
        runs: PosInt
    ) {

        /** A point that is the zero point one time in five. */
        val pointOrZero: Gen[P] = Gen.frequency((1, Gen.const(zero)), (4, point))
    }

    private val g1 = Group[G1Element](
      "G1",
      g1ElementArbitrary.arbitrary,
      platform.bls12_381_G1_uncompress(bls12_381_G1_compressed_zero),
      platform.bls12_381_G1_add,
      platform.bls12_381_G1_scalarMul,
      platform.bls12_381_G1_neg,
      platform.bls12_381_G1_compress,
      platform.bls12_381_G1_uncompress,
      platform.bls12_381_G1_multiScalarMul,
      runs = PosInt(50)
    )

    private val g2 = Group[G2Element](
      "G2",
      g2ElementArbitrary.arbitrary,
      platform.bls12_381_G2_uncompress(bls12_381_G2_compressed_zero),
      platform.bls12_381_G2_add,
      platform.bls12_381_G2_scalarMul,
      platform.bls12_381_G2_neg,
      platform.bls12_381_G2_compress,
      platform.bls12_381_G2_uncompress,
      platform.bls12_381_G2_multiScalarMul,
      runs = PosInt(20)
    )

    private def properties[P](g: Group[P]): Unit = {
        test(s"${g.name}: multiScalarMul equals the sum of scalarMul") {
            val genPairs =
                Gen.choose(0, 5).flatMap(n => Gen.listOfN(n, Gen.zip(genScalar, g.pointOrZero)))
            forAll(genPairs, minSuccessful(g.runs)) { pairs =>
                val (scalars, points) = pairs.unzip
                val sum = pairs.foldLeft(g.zero) { case (acc, (s, p)) =>
                    g.add(acc, g.scalarMul(s, p))
                }
                assert(g.msm(scalars, points) == sum)
            }
        }

        test(s"${g.name}: uncompress(compress(p)) == p") {
            forAll(g.pointOrZero, minSuccessful(g.runs)) { p =>
                assert(g.uncompress(g.compress(p)) == p)
            }
        }

        test(s"${g.name}: p + neg(p) == zero") {
            forAll(g.pointOrZero, minSuccessful(g.runs)) { p =>
                assert(g.add(p, g.neg(p)) == g.zero)
            }
        }
    }

    properties(g1)
    properties(g2)

    test("finalVerify(millerLoop([a]P, Q), millerLoop(P, [b]Q)) holds exactly when a = b mod r") {
        // b is congruent to a about half the time, so both outcomes occur.
        val genAB = for
            a <- genScalar
            b <- Gen.frequency((1, Gen.oneOf(a, a + r, a - r)), (1, genScalar))
        yield (a, b)
        var outcomes = Set.empty[Boolean]
        forAll(g1.point, g2.point, genAB, minSuccessful(20)) { (p, q, ab) =>
            val (a, b) = ab
            val lhs = platform.bls12_381_millerLoop(platform.bls12_381_G1_scalarMul(a, p), q)
            val rhs = platform.bls12_381_millerLoop(p, platform.bls12_381_G2_scalarMul(b, q))
            val congruent = a.mod(r) == b.mod(r)
            assert(platform.bls12_381_finalVerify(lhs, rhs) == congruent)
            outcomes += congruent
        }
        assert(outcomes == Set(true, false))
    }
}
