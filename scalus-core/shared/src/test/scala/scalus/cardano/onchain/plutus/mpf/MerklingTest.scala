package scalus.cardano.onchain.plutus.mpf

import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck.{Arbitrary, Gen}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import scalus.cardano.onchain.plutus.mpf.Merkling.*
import scalus.testing.kit.EvalTestKit

class MerklingTest extends AnyFunSuite with EvalTestKit {
    // Custom generator 32 random bytes ByteString
    private val byteArrayGen: Gen[ByteString] =
        Gen.containerOfN[Array, Byte](32, Arbitrary.arbByte.arbitrary).map(ByteString.fromArray)

    test("combine generates expected hashes for null sequences") {
        assertEvalEq(combine(NullHash, NullHash), NullHash2)
        assertEvalEq(combine(NullHash2, NullHash2), NullHash4)
        assertEvalEq(combine(NullHash4, NullHash4), NullHash8)
    }

    test("suffix generates expected values for specific examples") {
        assert(suffix(hex"abcd456789", 0) == hex"ffabcd456789")
        assert(suffix(hex"abcd456789", 1) == hex"000bcd456789")
        assert(suffix(hex"abcd456789", 2) == hex"ffcd456789")
        assert(suffix(hex"abcd456789", 4) == hex"ff456789")
        assert(suffix(hex"abcd456789", 5) == hex"00056789")
        assert(suffix(hex"abcd456789", 10) == hex"ff")
    }

    test("suffix always starts with either 0x00 or 0xff") {
        check { (_: Int) =>
            forAll(byteArrayGen, Gen.choose(0, 31)) { (bytes, index) =>
                val result = suffix(bytes, index)
                val head = indexByteString(result, 0)

                if head == 0 then assert(indexByteString(result, 1) < 16)
                else assert(head == 255)
            }
            true
        }
    }

    test("nibbles extracts correct sequences for specific examples") {
        assert(nibbles(hex"0123456789", 2, 2) == ByteString.empty)
        assert(nibbles(hex"0123456789", 2, 3) == hex"02")
        assert(nibbles(hex"0123456789", 4, 8) == hex"04050607")
        assert(nibbles(hex"0123456789", 3, 6) == hex"030405")
        assert(nibbles(hex"0123456789", 1, 7) == hex"010203040506")
    }

    test("nibble extracts correct values for specific examples") {
        assert(nibble(hex"ab", 0) == BigInt(10)) // 'a' high nibble
        assert(nibble(hex"ab", 1) == BigInt(11)) // 'b' low nibble
    }

    test("nearby nibbles combine to form original byte") {
        check { (_: Int) =>
            forAll(byteArrayGen, Gen.choose(0, 31)) { (bytes, index) =>
                val msb = nibble(bytes, index * 2)
                val lsb = nibble(bytes, index * 2 + 1)

                assert(msb * 16 + lsb == indexByteString(bytes, index))
            }
            true
        }
    }

    test("nibble always returns values between 0 and 15") {
        check { (_: Int) =>
            forAll(byteArrayGen, Gen.choose(0, 63)) { (bytes, index) =>
                val digit = nibble(bytes, index)
                assert(digit >= 0 && digit <= 15)
            }
            true
        }
    }

    test("merkle4 constructs correct root hash for all positions") {
        check { (_: Int) =>
            forAll(byteArrayGen, byteArrayGen, byteArrayGen, byteArrayGen) { (a, b, c, d) =>
                // Calculate expected root hash
                val root = combine(combine(a, b), combine(c, d))

                // Test all positions
                assert(merkle4(0, a, combine(c, d), b) == root)
                assert(merkle4(1, b, combine(c, d), a) == root)
                assert(merkle4(2, c, combine(a, b), d) == root)
                assert(merkle4(3, d, combine(a, b), c) == root)
            }
            true
        }
    }
}
