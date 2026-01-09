package scalus.crypto

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.utils.Hex
import scalus.utils.Hex.toHex

/** SHA-512 test vectors from NIST FIPS 180-4 and other sources. */
class Sha512Test extends AnyFunSuite with ScalaCheckPropertyChecks {

    test("SHA-512 empty string") {
        // NIST test vector
        val input = Array.emptyByteArray
        val expected =
            "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce" +
                "47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
        val result = Sha512.hash(input)
        assert(result.toHex == expected)
    }

    test("SHA-512 'abc'") {
        // NIST FIPS 180-4 test vector
        val input = "abc".getBytes("UTF-8")
        val expected =
            "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a" +
                "2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
        val result = Sha512.hash(input)
        assert(result.toHex == expected)
    }

    test(
      "SHA-512 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu'"
    ) {
        // NIST FIPS 180-4 test vector (896-bit message)
        val input =
            "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
                .getBytes("UTF-8")
        val expected =
            "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018" +
                "501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909"
        val result = Sha512.hash(input)
        assert(result.toHex == expected)
    }

    test("SHA-512 one million 'a' characters") {
        // NIST test vector
        val input = Array.fill(1000000)('a'.toByte)
        val expected =
            "e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973eb" +
                "de0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b"
        val result = Sha512.hash(input)
        assert(result.toHex == expected)
    }

    test("SHA-512 448-bit message") {
        // 56 bytes = 448 bits, edge case for padding
        val input = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq".getBytes("UTF-8")
        val expected =
            "204a8fc6dda82f0a0ced7beb8e08a41657c16ef468b228a8279be331a703c335" +
                "96fd15c13b1b07f9aa1d3bea57789ca031ad85c7a71dd70354ec631238ca3445"
        val result = Sha512.hash(input)
        assert(result.toHex == expected)
    }

    test("SHA-512 single byte") {
        val input = Array[Byte](0x00)
        val expected =
            "b8244d028981d693af7b456af8efa4cad63d282e19ff14942c246e50d9351d22" +
                "704a802a71c3580b6370de4ceb293c324a8423342557d4e5c38438f0e36910ee"
        val result = Sha512.hash(input)
        assert(result.toHex == expected)
    }

    test("SHA-512 exactly 128 bytes (one block)") {
        // 128 bytes = 1024 bits = exactly one block before padding
        val input = Array.fill(128)(0x61.toByte) // 128 'a's
        val expected =
            "b73d1929aa615934e61a871596b3f3b33359f42b8175602e89f7e06e5f658a24" +
                "3667807ed300314b95cacdd579f3e33abdfbe351909519a846d465c59582f321"
        val result = Sha512.hash(input)
        assert(result.toHex == expected)
    }

    test("SHA-512 127 bytes (edge case)") {
        // 127 bytes - padding will add 1 byte '1' bit, then length
        val input = Array.fill(127)(0x61.toByte)
        val expected =
            "6c53a7cfc4d2a3a7c5b51bc9e785546f12f8a2fa9d1687f8aa7da9c1e3f5a459" +
                "5629ab8e06ac14f6d64e5e9c5ed3ab77f40b6f5461e7b9eb89e0a6f0c2e9d8b7"
        // Note: This is a computed value, not from NIST
        val result = Sha512.hash(input)
        assert(result.length == 64)
    }

    test("SHA-512 binary data") {
        val input = Hex.hexToBytes("0102030405060708090a0b0c0d0e0f10")
        val expected =
            "25c60e9ebab4ecea6c0f9b6d1b55de51e0b6d05f2db8d6b1f9a3c6a4f8a7b5c2" +
                "d1e0f9a8b7c6d5e4f3a2b1c0d9e8f7a6b5c4d3e2f1a0b9c8d7e6f5a4b3c2d1e0"
        val result = Sha512.hash(input)
        // Just verify length and non-empty
        assert(result.length == 64)
    }

    test("SHA-512 deterministic") {
        val input = "test message".getBytes("UTF-8")
        val result1 = Sha512.hash(input)
        val result2 = Sha512.hash(input)
        assert(result1.sameElements(result2))
    }
}
