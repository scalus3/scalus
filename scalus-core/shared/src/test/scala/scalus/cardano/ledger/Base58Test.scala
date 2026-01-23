package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Test suite for Base58 encoding/decoding */
class Base58Test extends AnyFunSuite with ScalaCheckPropertyChecks {

    test("Base58 encode should handle empty input") {
        assert(Base58.encode(Array.empty) == "")
    }

    test("Base58 decode should handle empty input") {
        assert(Base58.decode("").sameElements(Array.empty[Byte]))
    }

    test("Base58 encode should handle leading zeros") {
        // Leading zeros map to '1' characters
        assert(Base58.encode(Array(0, 0, 0)) == "111")
        assert(Base58.encode(Array(0, 0, 1)) == "112")
        assert(Base58.encode(Array(0)) == "1")
    }

    test("Base58 decode should handle leading 1s") {
        // Leading '1's map to zero bytes
        assert(Base58.decode("111").sameElements(Array[Byte](0, 0, 0)))
        assert(Base58.decode("1").sameElements(Array[Byte](0)))
    }

    test("Base58 should encode known test vectors") {
        // From Bitcoin test vectors
        assert(Base58.encode("Hello World".getBytes("UTF-8")) == "JxF12TrwUP45BMd")
        assert(Base58.encode(Array[Byte](0, 0, 0, 0)) == "1111")
        assert(Base58.encode(Array[Byte](0xff.toByte)) == "5Q")
    }

    test("Base58 should decode known test vectors") {
        assert(new String(Base58.decode("JxF12TrwUP45BMd"), "UTF-8") == "Hello World")
        assert(Base58.decode("1111").sameElements(Array[Byte](0, 0, 0, 0)))
        assert(Base58.decode("5Q").sameElements(Array[Byte](0xff.toByte)))
    }

    test("Base58 should round-trip") {
        val testCases = List(
          Array[Byte](0x00, 0x01, 0x02, 0x03),
          Array[Byte](0xff.toByte, 0xfe.toByte, 0xfd.toByte),
          "Hello World".getBytes("UTF-8"),
          Array.fill(32)(0x42.toByte), // 32 bytes of 0x42
          Array[Byte](0, 0, 0, 1, 2, 3), // Leading zeros
          Array[Byte](0), // Single zero
          Array.fill(100)(0xaa.toByte) // Large input
        )

        for bytes <- testCases do {
            val encoded = Base58.encode(bytes)
            val decoded = Base58.decode(encoded)
            assert(decoded.sameElements(bytes))
        }
    }

    test("Base58 decode should reject invalid characters") {
        // '0' is not in Base58 alphabet
        assertThrows[IllegalArgumentException] {
            Base58.decode("Invalid0Char")
        }

        // 'O' is not in Base58 alphabet
        assertThrows[IllegalArgumentException] {
            Base58.decode("InvalidOChar")
        }

        // 'I' is not in Base58 alphabet
        assertThrows[IllegalArgumentException] {
            Base58.decode("InvalidIChar")
        }

        // 'l' is not in Base58 alphabet
        assertThrows[IllegalArgumentException] {
            Base58.decode("Invalidlchar")
        }
    }

    test("Base58 should handle all valid alphabet characters") {
        val alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
        // Each single character should decode without error
        for c <- alphabet do {
            Base58.decode(c.toString) // Should not throw
        }
    }

    test("Base58 property: encode then decode is identity") {
        forAll { (bytes: Array[Byte]) =>
            val encoded = Base58.encode(bytes)
            val decoded = Base58.decode(encoded)
            assert(decoded.sameElements(bytes))
        }
    }

    test("Base58 should handle real Byron address bytes") {
        // A real mainnet Byron address encoded bytes (Base58 decoded)
        val byronBase58 = "Ae2tdPwUPEZFRbyhz3cpfC2CumGzNkFBN2L42rcUc2yjQpEkxDbkPodpMAi"

        // Should decode without error
        val decoded = Base58.decode(byronBase58)
        assert(decoded.length > 0)

        // Should round-trip
        assert(Base58.encode(decoded) == byronBase58)
    }
}
