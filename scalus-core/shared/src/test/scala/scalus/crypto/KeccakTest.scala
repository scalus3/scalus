package scalus.crypto

import org.scalatest.funsuite.AnyFunSuite
import scalus.utils.Hex.*

/** Tests for Keccak-256 and SHA3-256 implementations.
  *
  * Test vectors are from:
  *   - NIST FIPS 202 for SHA3-256
  *   - Ethereum (uses Keccak-256) for Keccak-256
  */
class KeccakTest extends AnyFunSuite {

    // ==================== Keccak-256 Test Vectors ====================
    // These are from Ethereum which uses Keccak-256 (not SHA3-256)

    test("keccak_256: empty input") {
        val result = Keccak.keccak256(Array.emptyByteArray)
        val expected = "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
        assertResult(expected)(result.toHex)
    }

    test("keccak_256: 'abc'") {
        val result = Keccak.keccak256("abc".getBytes("UTF-8"))
        val expected = "4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45"
        assertResult(expected)(result.toHex)
    }

    test("keccak_256: longer input") {
        // "The quick brown fox jumps over the lazy dog"
        val input = "The quick brown fox jumps over the lazy dog".getBytes("UTF-8")
        val result = Keccak.keccak256(input)
        val expected = "4d741b6f1eb29cb2a9b9911c82f56fa8d73b04959d3d9d222895df6c0b28aa15"
        assertResult(expected)(result.toHex)
    }

    test("keccak_256: 200 bytes input") {
        // 200 bytes of 0xa3 (from Plutus conformance tests pattern)
        val input = Array.fill(200)(0xa3.toByte)
        val result = Keccak.keccak256(input)
        // Cross-verify this with BouncyCastle on JVM
        assert(result.length == 32)
    }

    // ==================== SHA3-256 Test Vectors ====================
    // These are from NIST FIPS 202

    test("sha3_256: empty input") {
        val result = Keccak.sha3_256(Array.emptyByteArray)
        val expected = "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a"
        assertResult(expected)(result.toHex)
    }

    test("sha3_256: 'abc'") {
        val result = Keccak.sha3_256("abc".getBytes("UTF-8"))
        val expected = "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532"
        assertResult(expected)(result.toHex)
    }

    test("sha3_256: longer input (448 bits)") {
        // NIST test vector: "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
        val input = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq".getBytes("UTF-8")
        val result = Keccak.sha3_256(input)
        val expected = "41c0dba2a9d6240849100376a8235e2c82e1b9998a999e21db32dd97496d3376"
        assertResult(expected)(result.toHex)
    }

    test("sha3_256: longer input (896 bits)") {
        // NIST test vector
        val input =
            "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
                .getBytes("UTF-8")
        val result = Keccak.sha3_256(input)
        val expected = "916f6061fe879741ca6469b43971dfdb28b1a32dc36cb3254e812be27aad1d18"
        assertResult(expected)(result.toHex)
    }

    test("sha3_256: 200 bytes input") {
        val input = Array.fill(200)(0xa3.toByte)
        val result = Keccak.sha3_256(input)
        // Verify length
        assert(result.length == 32)
    }

    // ==================== Differential Tests ====================
    // Verify Keccak-256 and SHA3-256 produce different results

    test("keccak_256 and sha3_256 produce different results for same input") {
        val input = "test".getBytes("UTF-8")
        val keccakResult = Keccak.keccak256(input)
        val sha3Result = Keccak.sha3_256(input)
        assert(!java.util.Arrays.equals(keccakResult, sha3Result))
    }

    test("keccak_256 and sha3_256 produce different results for empty input") {
        val keccakResult = Keccak.keccak256(Array.emptyByteArray)
        val sha3Result = Keccak.sha3_256(Array.emptyByteArray)
        assert(!java.util.Arrays.equals(keccakResult, sha3Result))
    }

    // ==================== Edge Cases ====================

    test("keccak_256: single byte") {
        val result = Keccak.keccak256(Array(0x00.toByte))
        assert(result.length == 32)
    }

    test("sha3_256: single byte") {
        val result = Keccak.sha3_256(Array(0x00.toByte))
        assert(result.length == 32)
    }

    test("keccak_256: rate boundary (136 bytes - exactly one block)") {
        val input = Array.fill(135)(0x42.toByte) // 135 + 1 padding = 136 = rate
        val result = Keccak.keccak256(input)
        assert(result.length == 32)
    }

    test("sha3_256: rate boundary (136 bytes)") {
        val input = Array.fill(135)(0x42.toByte)
        val result = Keccak.sha3_256(input)
        assert(result.length == 32)
    }

    test("keccak_256: multiple blocks") {
        val input = Array.fill(300)(0x42.toByte)
        val result = Keccak.keccak256(input)
        assert(result.length == 32)
    }

    // ==================== Determinism ====================

    test("keccak_256: same input produces same output") {
        val input = "deterministic".getBytes("UTF-8")
        val result1 = Keccak.keccak256(input)
        val result2 = Keccak.keccak256(input)
        assert(java.util.Arrays.equals(result1, result2))
    }

    test("sha3_256: same input produces same output") {
        val input = "deterministic".getBytes("UTF-8")
        val result1 = Keccak.sha3_256(input)
        val result2 = Keccak.sha3_256(input)
        assert(java.util.Arrays.equals(result1, result2))
    }
}
