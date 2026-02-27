package scalus.cardano.onchain.plutus.mpt

import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck.{Arbitrary, Gen}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import scalus.cardano.onchain.plutus.mpt.BinaryMerkling.*
import scalus.testing.kit.EvalTestKit

class BinaryMerklingTest extends AnyFunSuite with EvalTestKit {
    private val byteArrayGen: Gen[ByteString] =
        Gen.containerOfN[Array, Byte](32, Arbitrary.arbByte.arbitrary).map(ByteString.fromArray)

    test("combine generates expected hash for null hashes") {
        val combined = combine(NullHash, NullHash)
        assert(combined == blake2b_256(appendByteString(NullHash, NullHash)))
        // idempotency
        assert(combine(NullHash, NullHash) == combined)
    }

    test("bit extracts correct values for hex 80") {
        // hex"80" = 10000000 in binary
        assert(bit(hex"80", 0) == BigInt(1))
        assert(bit(hex"80", 1) == BigInt(0))
        assert(bit(hex"80", 7) == BigInt(0))
    }

    test("bit extracts correct values for hex 01") {
        // hex"01" = 00000001 in binary
        assert(bit(hex"01", 0) == BigInt(0))
        assert(bit(hex"01", 7) == BigInt(1))
    }

    test("bit extracts correct values for hex ff") {
        // hex"ff" = 11111111
        for i <- 0 until 8 do assert(bit(hex"ff", i) == BigInt(1), s"bit $i should be 1")
    }

    test("bit extracts correct values for hex 00") {
        // hex"00" = 00000000
        for i <- 0 until 8 do assert(bit(hex"00", i) == BigInt(0), s"bit $i should be 0")
    }

    test("bit always returns 0 or 1") {
        check { (_: Int) =>
            forAll(byteArrayGen, Gen.choose(0, 255)) { (bytes, index) =>
                val b = bit(bytes, index)
                assert(b == BigInt(0) || b == BigInt(1))
            }
            true
        }
    }

    test("8 consecutive bits reconstruct original byte") {
        check { (_: Int) =>
            forAll(byteArrayGen, Gen.choose(0, 31)) { (bytes, byteIndex) =>
                var byte = BigInt(0)
                for i <- 0 until 8 do byte = byte * 2 + bit(bytes, byteIndex * 8 + i)
                assert(byte == indexByteString(bytes, byteIndex))
            }
            true
        }
    }

    test("suffixBit byte-aligned cursor") {
        val path = hex"abcd"
        // cursor = 0: marker 0xff + all bytes
        assert(suffixBit(path, 0) == hex"ffabcd")
        // cursor = 8: marker 0xff + bytes from offset 1
        assert(suffixBit(path, 8) == hex"ffcd")
        // cursor = 16: marker 0xff + empty (end of 2-byte path)
        assert(suffixBit(path, 16) == hex"ff")
    }

    test("suffixBit non-aligned cursor") {
        val path = hex"abcd"
        // cursor = 3: marker 0x03 + bytes from offset 0
        assert(suffixBit(path, 3) == hex"03abcd")
        // cursor = 5: marker 0x05 + bytes from offset 0
        assert(suffixBit(path, 5) == hex"05abcd")
        // cursor = 11: marker 0x03 + bytes from offset 1
        assert(suffixBit(path, 11) == hex"03cd")
    }

    test("suffixBit always starts with either 0xff or 1..7") {
        check { (_: Int) =>
            forAll(byteArrayGen, Gen.choose(0, 255)) { (bytes, cursor) =>
                val result = suffixBit(bytes, cursor)
                val head = indexByteString(result, 0)
                assert(
                  head == 255 || (head >= 1 && head <= 7),
                  s"Unexpected marker: $head at cursor=$cursor"
                )
            }
            true
        }
    }

}
