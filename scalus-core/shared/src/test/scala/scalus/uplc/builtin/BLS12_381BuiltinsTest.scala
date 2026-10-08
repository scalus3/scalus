package scalus.uplc.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.PlatformSpecific.{bls12_381_G1_compressed_generator, bls12_381_G1_compressed_zero, bls12_381_G2_compressed_generator, bls12_381_G2_compressed_zero}
import scalus.uplc.builtin.bls12_381.G1Element.g1
import scalus.uplc.builtin.bls12_381.G2Element.g2
import scalus.uplc.builtin.bls12_381.MLResult

import scala.language.implicitConversions

/** The BLS12-381 builtins of the platform: JVM (scalus-crypto-jni), JS (@noble/curves) and Native
  * (blst through FFI).
  */
class BLS12_381BuiltinsTest extends AnyFunSuite {
    private def zeroG1 = bls12_381_G1_uncompress(bls12_381_G1_compressed_zero)
    private def genG1 = bls12_381_G1_uncompress(bls12_381_G1_compressed_generator)
    private def zeroG2 = bls12_381_G2_uncompress(bls12_381_G2_compressed_zero)
    private def genG2 = bls12_381_G2_uncompress(bls12_381_G2_compressed_generator)

    private val msg = ByteString.fromString("p")
    private val dst = ByteString.fromString("DST")

    test("G1: uncompress then compress gives the zero encoding") {
        assert(
          zeroG1.toCompressedByteString == hex"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        )
    }

    test("G1: zero + zero == zero") {
        assert(bls12_381_G1_equal(bls12_381_G1_add(zeroG1, zeroG1), zeroG1))
    }

    test("G1: zero + p == p") {
        assert(bls12_381_G1_equal(bls12_381_G1_add(zeroG1, genG1), genG1))
    }

    // cardano-crypto-class adds with blst's add_or_double, which also handles p + p.
    test("G1: p + p == 2p") {
        val p = bls12_381_G1_hashToGroup(msg, dst)
        assert(bls12_381_G1_add(p, p) == bls12_381_G1_scalarMul(BigInt(2), p))
    }

    test("G1: add, scalarMul and neg leave their arguments unchanged") {
        val p = genG1
        bls12_381_G1_add(p, p)
        bls12_381_G1_scalarMul(BigInt(2), p)
        bls12_381_G1_neg(p)
        assert(bls12_381_G1_equal(p, genG1))
    }

    test("G1: equal points have equal hashCodes") {
        val a = genG1
        val b = genG1
        assert(a == b)
        assert(a.hashCode == b.hashCode)
        assert(Set(a).contains(b))
    }

    test("G2: uncompress then compress gives the zero encoding") {
        assert(
          zeroG2.toCompressedByteString == hex"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        )
    }

    test("G2: zero + zero == zero") {
        assert(bls12_381_G2_equal(bls12_381_G2_add(zeroG2, zeroG2), zeroG2))
    }

    test("G2: zero + p == p") {
        assert(bls12_381_G2_equal(bls12_381_G2_add(zeroG2, genG2), genG2))
    }

    test("G2: p + p == 2p") {
        val p = bls12_381_G2_hashToGroup(msg, dst)
        assert(bls12_381_G2_add(p, p) == bls12_381_G2_scalarMul(BigInt(2), p))
    }

    test("G2: add, scalarMul and neg leave their arguments unchanged") {
        val p = genG2
        bls12_381_G2_add(p, p)
        bls12_381_G2_scalarMul(BigInt(2), p)
        bls12_381_G2_neg(p)
        assert(bls12_381_G2_equal(p, genG2))
    }

    test("G2: equal points have equal hashCodes") {
        val a = genG2
        val b = genG2
        assert(a == b)
        assert(a.hashCode == b.hashCode)
        assert(Set(a).contains(b))
    }

    private def millerLoopOfGenerators: MLResult = bls12_381_millerLoop(genG1, genG2)

    test("ML result: mulMlResult leaves its arguments unchanged") {
        val a = millerLoopOfGenerators
        val b = millerLoopOfGenerators
        bls12_381_mulMlResult(a, b)
        assert(a == millerLoopOfGenerators)
        assert(b == millerLoopOfGenerators)
    }

    test("ML result: equal results have equal hashCodes") {
        val a = millerLoopOfGenerators
        val b = millerLoopOfGenerators
        assert(a == b)
        assert(a.hashCode == b.hashCode)
        assert(Set(a).contains(b))
    }

    test("g1 interpolator: builds the generator from hex") {
        val gen =
            g1"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
        assert(bls12_381_G1_equal(gen, genG1))
    }

    test("g1 interpolator: accepts spaces") {
        val zero =
            g1"c0000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
        assert(bls12_381_G1_equal(zero, zeroG1))
    }

    test("g1 interpolator: rejects a wrong size") {
        assertThrows[IllegalArgumentException] {
            g1"deadbeef" // 4 bytes, not 48
        }
    }

    test("g2 interpolator: builds the generator from hex") {
        val gen =
            g2"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"
        assert(bls12_381_G2_equal(gen, genG2))
    }

    test("g2 interpolator: accepts spaces") {
        val zero =
            g2"c0000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
        assert(bls12_381_G2_equal(zero, zeroG2))
    }

    test("g2 interpolator: rejects a wrong size") {
        assertThrows[IllegalArgumentException] {
            g2"deadbeef" // 4 bytes, not 96
        }
    }
}
