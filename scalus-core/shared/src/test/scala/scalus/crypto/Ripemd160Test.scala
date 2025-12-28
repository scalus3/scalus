package scalus.crypto

import org.scalatest.funsuite.AnyFunSuite
import scalus.utils.Hex.*

/** Tests for RIPEMD-160 implementation.
  *
  * Test vectors are from the official RIPEMD-160 specification by Dobbertin, Bosselaers, and
  * Preneel.
  *
  * @see
  *   https://homes.esat.kuleuven.be/~bosselae/ripemd160.html
  */
class Ripemd160Test extends AnyFunSuite {

    // ==================== Official RIPEMD-160 Test Vectors ====================
    // From https://homes.esat.kuleuven.be/~bosselae/ripemd160.html

    test("ripemd160: empty input") {
        val result = Ripemd160.ripemd160(Array.emptyByteArray)
        val expected = "9c1185a5c5e9fc54612808977ee8f548b2258d31"
        assertResult(expected)(result.toHex)
    }

    test("ripemd160: 'a'") {
        val result = Ripemd160.ripemd160("a".getBytes("UTF-8"))
        val expected = "0bdc9d2d256b3ee9daae347be6f4dc835a467ffe"
        assertResult(expected)(result.toHex)
    }

    test("ripemd160: 'abc'") {
        val result = Ripemd160.ripemd160("abc".getBytes("UTF-8"))
        val expected = "8eb208f7e05d987a9b044a8e98c6b087f15a0bfc"
        assertResult(expected)(result.toHex)
    }

    test("ripemd160: 'message digest'") {
        val result = Ripemd160.ripemd160("message digest".getBytes("UTF-8"))
        val expected = "5d0689ef49d2fae572b881b123a85ffa21595f36"
        assertResult(expected)(result.toHex)
    }

    test("ripemd160: 'a-z'") {
        val result = Ripemd160.ripemd160("abcdefghijklmnopqrstuvwxyz".getBytes("UTF-8"))
        val expected = "f71c27109c692c1b56bbdceb5b9d2865b3708dbc"
        assertResult(expected)(result.toHex)
    }

    test("ripemd160: 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'") {
        val result = Ripemd160.ripemd160(
          "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq".getBytes("UTF-8")
        )
        val expected = "12a053384a9c0c88e405a06c27dcf49ada62eb2b"
        assertResult(expected)(result.toHex)
    }

    test("ripemd160: 'A-Za-z0-9'") {
        val result = Ripemd160.ripemd160(
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789".getBytes("UTF-8")
        )
        val expected = "b0e20b6e3116640286ed3a87a5713079b21f5189"
        assertResult(expected)(result.toHex)
    }

    test("ripemd160: 8 times '1234567890'") {
        val result = Ripemd160.ripemd160(("1234567890" * 8).getBytes("UTF-8"))
        val expected = "9b752e45573d4b39f4dbd3323cab82bf63326bfb"
        assertResult(expected)(result.toHex)
    }

    test("ripemd160: 1 million 'a'") {
        val result = Ripemd160.ripemd160(Array.fill(1000000)('a'.toByte))
        val expected = "52783243c1697bdbe16d37f97f68f08325dc1528"
        assertResult(expected)(result.toHex)
    }

    // ==================== Well-Known Phrase Test Vectors ====================
    // Used in existing Scalus tests

    test("ripemd160: 'The quick brown fox jumps over the lazy dog'") {
        val result =
            Ripemd160.ripemd160("The quick brown fox jumps over the lazy dog".getBytes("UTF-8"))
        val expected = "37f332f68db77bd9d7edd4969571ad671cf9dd3b"
        assertResult(expected)(result.toHex)
    }

    test("ripemd160: 'The quick brown fox jumps over the lazy cog'") {
        val result =
            Ripemd160.ripemd160("The quick brown fox jumps over the lazy cog".getBytes("UTF-8"))
        val expected = "132072df690933835eb8b6ad0b77e7b6f14acad7"
        assertResult(expected)(result.toHex)
    }

    // ==================== Edge Cases ====================

    test("ripemd160: single byte 0x00") {
        val result = Ripemd160.ripemd160(Array(0x00.toByte))
        assert(result.length == 20)
    }

    test("ripemd160: 55 bytes (padding fits in one block)") {
        // 55 bytes + 1 (0x80) + 8 (length) = 64 bytes = one block
        val input = Array.fill(55)(0x42.toByte)
        val result = Ripemd160.ripemd160(input)
        assert(result.length == 20)
    }

    test("ripemd160: 56 bytes (triggers two-block processing)") {
        // 56 bytes + 1 (0x80) + 8 (length) = 65 bytes > 64, needs second block
        val input = Array.fill(56)(0x42.toByte)
        val result = Ripemd160.ripemd160(input)
        assert(result.length == 20)
    }

    test("ripemd160: 64 bytes (exactly one full block, padding in second)") {
        val input = Array.fill(64)(0x42.toByte)
        val result = Ripemd160.ripemd160(input)
        assert(result.length == 20)
    }

    test("ripemd160: 128 bytes (exactly two blocks, padding in third)") {
        val input = Array.fill(128)(0x42.toByte)
        val result = Ripemd160.ripemd160(input)
        assert(result.length == 20)
    }

    test("ripemd160: 200 bytes (Plutus conformance pattern)") {
        val input = Array.fill(200)(0xa3.toByte)
        val result = Ripemd160.ripemd160(input)
        assert(result.length == 20)
    }

    // ==================== Determinism ====================

    test("ripemd160: same input produces same output") {
        val input = "deterministic".getBytes("UTF-8")
        val result1 = Ripemd160.ripemd160(input)
        val result2 = Ripemd160.ripemd160(input)
        assert(java.util.Arrays.equals(result1, result2))
    }
}
