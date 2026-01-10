package scalus.prelude.crypto.bls12_381

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.*
import scalus.builtin.PlatformSpecific.bls12_381_G1_compressed_zero
import scalus.builtin.{Builtins, ByteString}
import scalus.prelude.*
import scalus.prelude.crypto.bls12_381.G1.{*, given}
import scalus.testing.kit.EvalTestKit

class BLS12_381_G1Test extends AnyFunSuite with EvalTestKit {

    test("zero") {
        assert(bls12_381_G1_equal(zero, bls12_381_G1_uncompress(bls12_381_G1_compressed_zero)))
    }

    test("generator") {
        assert(
          bls12_381_G1_equal(generator, bls12_381_G1_uncompress(G1.generatorCompressed))
        )
    }

    test("equal") {
        assertEval(zero equal zero)
        assertEval(generator equal generator)
        assertEval(!(generator equal zero))
        assertEval(zero === zero)
        assertEval(zero !== generator)
    }

    test("add") {
        assertEval {
            generator + generator === Builtins.bls12_381_G1_add(generator, generator)
        }
    }

    test("scalar multiplication") {
        assertEval {
            generator.scale(2) === bls12_381_G1_scalarMul(2, generator)
        }
    }

    test("negation") {
        assertEval {
            generator + -generator === zero
        }
    }

    test("compress and uncompress") {
        assertEval {
            uncompress(generator.compress) === generator
        }
    }

    test("hash to group") {
        assertEval {
            val input = ByteString.fromHex("deadbeef")
            val dst = ByteString.fromHex("123456")
            val hashed = bls12_381_G1_hashToGroup(input, dst)
            // Ensure the result is not the zero element
            (hashed !== zero) && hashToGroup(input, dst) === hashed
        }
    }
}
