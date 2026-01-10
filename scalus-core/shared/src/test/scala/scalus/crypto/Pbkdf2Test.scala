package scalus.crypto

import org.scalatest.funsuite.AnyFunSuite
import scalus.utils.Hex.toHex

/** PBKDF2-HMAC-SHA512 tests.
  *
  * Note: RFC 6070 only provides test vectors for PBKDF2-HMAC-SHA1. These test vectors are derived
  * from known BIP-39 implementations and other trusted sources.
  */
class Pbkdf2Test extends AnyFunSuite {

    test("PBKDF2-SHA512 basic test") {
        // Test vector from various implementations
        val password = "password".getBytes("UTF-8")
        val salt = "salt".getBytes("UTF-8")
        val iterations = 1
        val keyLength = 64

        val result = Pbkdf2.deriveKey(password, salt, iterations, keyLength)
        // Expected value from BouncyCastle/OpenSSL
        val expected =
            "867f70cf1ade02cff3752599a3a53dc4af34c7a669815ae5d513554e1c8cf252" +
                "c02d470a285a0501bad999bfe943c08f050235d7d68b1da55e63f73b60a57fce"
        assert(result.toHex == expected)
    }

    test("PBKDF2-SHA512 two iterations") {
        val password = "password".getBytes("UTF-8")
        val salt = "salt".getBytes("UTF-8")
        val iterations = 2
        val keyLength = 64

        val result = Pbkdf2.deriveKey(password, salt, iterations, keyLength)
        val expected =
            "e1d9c16aa681708a45f5c7c4e215ceb66e011a2e9f0040713f18aefdb866d53c" +
                "f76cab2868a39b9f7840edce4fef5a82be67335c77a6068e04112754f27ccf4e"
        assert(result.toHex == expected)
    }

    test("PBKDF2-SHA512 4096 iterations") {
        val password = "password".getBytes("UTF-8")
        val salt = "salt".getBytes("UTF-8")
        val iterations = 4096
        val keyLength = 64

        val result = Pbkdf2.deriveKey(password, salt, iterations, keyLength)
        val expected =
            "d197b1b33db0143e018b12f3d1d1479e6cdebdcc97c5c0f87f6902e072f457b5" +
                "143f30602641b3d55cd335988cb36b84376060ecd532e039b742a239434af2d5"
        assert(result.toHex == expected)
    }

    test("PBKDF2-SHA512 BIP-39 parameters (2048 iterations)") {
        // BIP-39 uses 2048 iterations with "mnemonic" + passphrase as salt
        val mnemonic =
            "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
        val passphrase = ""
        val password = mnemonic.getBytes("UTF-8")
        val salt = ("mnemonic" + passphrase).getBytes("UTF-8")
        val iterations = 2048
        val keyLength = 64

        val result = Pbkdf2.deriveKey(password, salt, iterations, keyLength)
        // Expected seed from BIP-39 test vectors
        val expected =
            "5eb00bbddcf069084889a8ab9155568165f5c453ccb85e70811aaed6f6da5fc1" +
                "9a5ac40b389cd370d086206dec8aa6c43daea6690f20ad3d8d48b2d2ce9e38e4"
        assert(result.toHex == expected)
    }

    test("PBKDF2-SHA512 BIP-39 with passphrase") {
        val mnemonic =
            "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
        val passphrase = "TREZOR"
        val password = mnemonic.getBytes("UTF-8")
        val salt = ("mnemonic" + passphrase).getBytes("UTF-8")
        val iterations = 2048
        val keyLength = 64

        val result = Pbkdf2.deriveKey(password, salt, iterations, keyLength)
        // Expected seed from BIP-39 test vectors (Trezor)
        val expected =
            "c55257c360c07c72029aebc1b53c05ed0362ada38ead3e3e9efa3708e53495531f09a6987599d18264c1e1c92f2cf141630c7a3c4ab7c81b2f001698e7463b04"
        assert(result.toHex == expected)
    }

    test("PBKDF2-SHA512 short output") {
        val password = "password".getBytes("UTF-8")
        val salt = "salt".getBytes("UTF-8")
        val iterations = 1
        val keyLength = 32

        val result = Pbkdf2.deriveKey(password, salt, iterations, keyLength)
        assert(result.length == 32)
        // Should be first 32 bytes of the 64-byte output
        val expected = "867f70cf1ade02cff3752599a3a53dc4af34c7a669815ae5d513554e1c8cf252"
        assert(result.toHex == expected)
    }

    test("PBKDF2-SHA512 longer output (multiple blocks)") {
        val password = "password".getBytes("UTF-8")
        val salt = "salt".getBytes("UTF-8")
        val iterations = 1
        val keyLength = 128 // Two 64-byte blocks

        val result = Pbkdf2.deriveKey(password, salt, iterations, keyLength)
        assert(result.length == 128)
    }

    test("PBKDF2-SHA512 empty password") {
        val password = Array.emptyByteArray
        val salt = "salt".getBytes("UTF-8")
        val iterations = 1
        val keyLength = 64

        val result = Pbkdf2.deriveKey(password, salt, iterations, keyLength)
        assert(result.length == 64)
    }

    test("PBKDF2-SHA512 empty salt") {
        val password = "password".getBytes("UTF-8")
        val salt = Array.emptyByteArray
        val iterations = 1
        val keyLength = 64

        val result = Pbkdf2.deriveKey(password, salt, iterations, keyLength)
        assert(result.length == 64)
    }

    test("PBKDF2-SHA512 deterministic") {
        val password = "password".getBytes("UTF-8")
        val salt = "salt".getBytes("UTF-8")
        val iterations = 100
        val keyLength = 64

        val result1 = Pbkdf2.deriveKey(password, salt, iterations, keyLength)
        val result2 = Pbkdf2.deriveKey(password, salt, iterations, keyLength)
        assert(result1.sameElements(result2))
    }
}
