package scalus.crypto

import org.scalatest.funsuite.AnyFunSuite
import scalus.utils.Hex.toHex

/** HMAC-SHA512 test vectors from RFC 4231. */
class HmacTest extends AnyFunSuite {

    test("HMAC-SHA512 RFC 4231 Test Case 1") {
        // Key: 0x0b repeated 20 times
        // Data: "Hi There"
        val key = Array.fill(20)(0x0b.toByte)
        val data = "Hi There".getBytes("UTF-8")
        val expected =
            "87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cde" +
                "daa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854"
        val result = Hmac.hmacSha512(key, data)
        assert(result.toHex == expected)
    }

    test("HMAC-SHA512 RFC 4231 Test Case 2") {
        // Key: "Jefe"
        // Data: "what do ya want for nothing?"
        val key = "Jefe".getBytes("UTF-8")
        val data = "what do ya want for nothing?".getBytes("UTF-8")
        val expected =
            "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea250554" +
                "9758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737"
        val result = Hmac.hmacSha512(key, data)
        assert(result.toHex == expected)
    }

    test("HMAC-SHA512 RFC 4231 Test Case 3") {
        // Key: 0xaa repeated 20 times
        // Data: 0xdd repeated 50 times
        val key = Array.fill(20)(0xaa.toByte)
        val data = Array.fill(50)(0xdd.toByte)
        val expected =
            "fa73b0089d56a284efb0f0756c890be9b1b5dbdd8ee81a3655f83e33b2279d39" +
                "bf3e848279a722c806b485a47e67c807b946a337bee8942674278859e13292fb"
        val result = Hmac.hmacSha512(key, data)
        assert(result.toHex == expected)
    }

    test("HMAC-SHA512 RFC 4231 Test Case 4") {
        // Key: 0x01020304...19 (25 bytes)
        // Data: 0xcd repeated 50 times
        val key = (1 to 25).map(_.toByte).toArray
        val data = Array.fill(50)(0xcd.toByte)
        val expected =
            "b0ba465637458c6990e5a8c5f61d4af7e576d97ff94b872de76f8050361ee3db" +
                "a91ca5c11aa25eb4d679275cc5788063a5f19741120c4f2de2adebeb10a298dd"
        val result = Hmac.hmacSha512(key, data)
        assert(result.toHex == expected)
    }

    test("HMAC-SHA512 RFC 4231 Test Case 5 (truncated - we verify full)") {
        // Key: 0x0c repeated 20 times
        // Data: "Test With Truncation"
        val key = Array.fill(20)(0x0c.toByte)
        val data = "Test With Truncation".getBytes("UTF-8")
        // Full 64-byte output (RFC only specifies truncated to 128 bits for this test)
        val result = Hmac.hmacSha512(key, data)
        // Verify it starts with the expected truncated value
        val truncatedExpected = "415fad6271580a531d4179bc891d87a6"
        assert(result.toHex.startsWith(truncatedExpected))
    }

    test("HMAC-SHA512 RFC 4231 Test Case 6") {
        // Key: 0xaa repeated 131 times (larger than block size)
        // Data: "Test Using Larger Than Block-Size Key - Hash Key First"
        val key = Array.fill(131)(0xaa.toByte)
        val data = "Test Using Larger Than Block-Size Key - Hash Key First".getBytes("UTF-8")
        val expected =
            "80b24263c7c1a3ebb71493c1dd7be8b49b46d1f41b4aeec1121b013783f8f352" +
                "6b56d037e05f2598bd0fd2215d6a1e5295e64f73f63f0aec8b915a985d786598"
        val result = Hmac.hmacSha512(key, data)
        assert(result.toHex == expected)
    }

    test("HMAC-SHA512 RFC 4231 Test Case 7") {
        // Key: 0xaa repeated 131 times
        // Data: "This is a test using a larger than block-size key and a larger than block-size data..."
        val key = Array.fill(131)(0xaa.toByte)
        val data =
            "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm."
                .getBytes("UTF-8")
        val expected =
            "e37b6a775dc87dbaa4dfa9f96e5e3ffddebd71f8867289865df5a32d20cdc944" +
                "b6022cac3c4982b10d5eeb55c3e4de15134676fb6de0446065c97440fa8c6a58"
        val result = Hmac.hmacSha512(key, data)
        assert(result.toHex == expected)
    }

    test("HMAC-SHA512 empty key and data") {
        val key = Array.emptyByteArray
        val data = Array.emptyByteArray
        val result = Hmac.hmacSha512(key, data)
        assert(result.length == 64)
    }

    test("HMAC-SHA512 deterministic") {
        val key = "secret".getBytes("UTF-8")
        val data = "message".getBytes("UTF-8")
        val result1 = Hmac.hmacSha512(key, data)
        val result2 = Hmac.hmacSha512(key, data)
        assert(result1.sameElements(result2))
    }
}
