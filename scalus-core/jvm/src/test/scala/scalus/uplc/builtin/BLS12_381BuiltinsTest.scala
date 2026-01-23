package scalus.uplc.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.PlatformSpecific.bls12_381_G1_compressed_zero
import scalus.uplc.builtin.PlatformSpecific.bls12_381_G2_compressed_zero
import scalus.uplc.builtin.PlatformSpecific.bls12_381_G1_compressed_generator
import scalus.uplc.builtin.PlatformSpecific.bls12_381_G2_compressed_generator
import scalus.uplc.builtin.ByteString.*

import scala.language.implicitConversions

// TODO: move to shared when BLS12-381 builtins on JS are implemented
class BLS12_381BuiltinsTest extends AnyFunSuite {
    test("uncompress zero G1") {
        assert(
          bls12_381_G1_uncompress(
            bls12_381_G1_compressed_zero
          ).toCompressedByteString == hex"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        )
    }

    test("add zeros is zero in G1") {
        val zero = bls12_381_G1_uncompress(bls12_381_G1_compressed_zero)
        val sumZero = bls12_381_G1_add(zero, zero)
        assert(bls12_381_G1_equal(zero, sumZero))
    }

    test("add anything and zero is anything in G1") {
        val zero = bls12_381_G1_uncompress(bls12_381_G1_compressed_zero)
        val g1 = bls12_381_G1_uncompress(bls12_381_G1_compressed_generator)
        val sum = bls12_381_G1_add(zero, g1)
        assert(bls12_381_G1_equal(g1, sum))
    }

    test("primitive behaves as value type in G1") {
        val g1 = bls12_381_G1_uncompress(bls12_381_G1_compressed_generator)
        val g2 = bls12_381_G1_uncompress(bls12_381_G1_compressed_generator)
        bls12_381_G1_add(g1, g1)
        bls12_381_G1_scalarMul(BigInt(2), g1)
        bls12_381_G1_neg(g1)
        assert(bls12_381_G1_equal(g1, g2))
    }

    test("uncompress zero G2") {
        assert(
          bls12_381_G2_uncompress(
            bls12_381_G2_compressed_zero
          ).toCompressedByteString == hex"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        )
    }

    test("add zeros is zero in G2") {
        val zero = bls12_381_G2_uncompress(bls12_381_G2_compressed_zero)
        val sumZero = bls12_381_G2_add(zero, zero)
        assert(bls12_381_G2_equal(zero, sumZero))
    }

    test("add anything and zero is anything in G2") {
        val zero = bls12_381_G2_uncompress(bls12_381_G2_compressed_zero)
        val g2 = bls12_381_G2_uncompress(bls12_381_G2_compressed_generator)
        val sum = bls12_381_G2_add(zero, g2)
        assert(bls12_381_G2_equal(g2, sum))
    }

    test("primitive behaves as value type in G2") {
        val g1 = bls12_381_G2_uncompress(bls12_381_G2_compressed_generator)
        val g2 = bls12_381_G2_uncompress(bls12_381_G2_compressed_generator)
        bls12_381_G2_add(g1, g1)
        bls12_381_G2_scalarMul(BigInt(2), g1)
        bls12_381_G2_neg(g1)
        assert(bls12_381_G2_equal(g1, g2))
    }

    test("pairing primitive behaves as value type") {
        def gt(): BLS12_381_MlResult = {
            val gG1 = bls12_381_G1_uncompress(bls12_381_G1_compressed_generator)
            val gG2 = bls12_381_G2_uncompress(bls12_381_G2_compressed_generator)
            bls12_381_millerLoop(gG1, gG2)
        }

        val gt1 = gt()
        val gt2 = gt()
        val gtResult = bls12_381_mulMlResult(gt1, gt2)

        assert(gtResult.value ne gt1.value)
        assert(gtResult.value ne gt2.value)
    }
}
