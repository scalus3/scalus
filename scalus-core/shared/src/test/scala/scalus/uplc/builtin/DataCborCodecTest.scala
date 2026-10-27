package scalus.uplc.builtin

import io.bullet.borer.{Cbor, Tag}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.builtin
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.*
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.test.ArbitraryInstances
import scalus.utils.Hex.{hexToBytes, toHex}
import scalus.utils.Utils

class DataCborCodecTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
    private def encodeHex(d: Data) = Utils.bytesToHex(Cbor.encode(d).toByteArray)
    private def decodeHex(d: String) = Cbor.decode(d.hexToBytes).to[Data].value

    private def roundtrip(d: Data): Unit =
        val ba = d.toCbor
        val dd = Data.fromCbor(ba)
        assert(d == dd)

    test("BigInt encoding/decoding of < 64 bits") {
        val bigInt = BigInt("1234567890111213141516")
        val expected = "c24942ed123b08fe58fe0c"
        assert(encodeHex(I(bigInt)) == expected)
        assert(decodeHex(expected) == I(bigInt))
    }

    test("BigInt encoding/decoding of > 64 bits is chunked") {
        val twoTo520 = BigInt(2).pow(520)
        val expectedTwo =
            "c25f584001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000420000ff"
        assert(encodeHex(I(twoTo520)) == expectedTwo)
        assert(decodeHex(expectedTwo) == I(twoTo520))

        val minusTwoTo520 = -BigInt(2).pow(520)
        val expectedMinusTwo =
            "c35f5840ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff41ffff"
        assert(encodeHex(I(minusTwoTo520)) == expectedMinusTwo)
        assert(decodeHex(expectedMinusTwo) == I(minusTwoTo520))
    }

    test("Fix of bug with encoding large negative BigInt into CBOR") {
        val negative = BigInt("-249855028377480970714957382941817618225")
        val expectedNegative = "c350bbf853fdc212d25fb51f31598036bf30"
        assert(encodeHex(I(negative)) == expectedNegative)
    }

    test("Fix of bug with encoding large BigInt into CBOR") {
        /* We take byte array representation of the BigInt, and encode it as a CBOR
         * byte string.
         * We use `BigInteger.toByteArray`, which returns a byte array.
         * The issue was that `BigInteger.toByteArray` returns a byte array has a place for a sign bit,
         * even if the number is positive.
         * This results in an extra '00' byte at the beginning of the byte array which is not needed.
         */
        val bs =
            hex"aa7bd5108b0ad89b36b70f8555d44f0c63da331eb0f4dc85f0df96bac171a117048ba43520412412a8870182b4f454429de39635a54079e695a2253f5977c6bfe521b19066a58c54574e691211b0264d96493cc7ed664fcb21ea45c4c8866745511a50b6bae29fbf63c72d4c265bf04e4d18a830fa0e1fd5f9b3bd46d71003aa"
        val bigInt = BigInt(bs.toHex, 16)
        val data = I(bigInt)
        val encoded = data.toCbor
        val I(decoded) = Data.fromCbor(encoded): @unchecked
        assert(decoded == bigInt)
        val expectedHex =
            "c25f5840"
                + "aa7bd5108b0ad89b36b70f8555d44f0c63da331eb0f4dc85f0df96bac171a117048ba43520412412a8870182b4f454429de39635a54079e695a2253f5977c6bf"
                + "5840"
                + "e521b19066a58c54574e691211b0264d96493cc7ed664fcb21ea45c4c8866745511a50b6bae29fbf63c72d4c265bf04e4d18a830fa0e1fd5f9b3bd46d71003aa"
                + "ff"
        assert(encoded.toHex == expectedHex)
    }

    test("All BigInt values are encoded/decoded correctly") {
        forAll { (bigInt: BigInt) =>
            roundtrip(I(bigInt))
        }
    }

    test("Encoding/decoding of Constrs") {
        assert(encodeHex(Constr(3, PList(Constr(3, PList.Nil)))) == "d87c9fd87c80ff")
        assert(decodeHex("d8 7c 9f d8 7c 80 ff") == Constr(3, PList(Constr(3, PList.Nil))))

        assert(encodeHex(List(PList(Constr(3, PList.Nil)))) == "9fd87c80ff")
        assert(decodeHex("9f d8 7c 80 ff") == List(PList(Constr(3, PList.Nil))))

        assert(encodeHex(List(PList(Constr(7, PList.Nil)))) == "9fd9050080ff")
        assert(decodeHex("9f d9 05 00 80 ff") == List(PList(Constr(7, PList.Nil))))

        assert(encodeHex(List(PList(Constr(1234567890, PList.Nil)))) == "9fd866821a499602d280ff")
        assert(
          decodeHex("9f d8 66 82 1a 49 96 02 d2 80 ff") == List(
            PList(Constr(1234567890, PList.Nil))
          )
        )
    }

    /* The tag-102 form encodes the constructor index as a CBOR unsigned 64-bit integer, so the
     * whole 0 .. 2^64-1 range is valid. The node reads it with `decodeWord64` and requires the
     * tagged array to hold exactly two elements, definite- or indefinite-length; see
     * `decodeConstrExtended` in PlutusCore.Data.
     */
    private val twoTo63 = BigInt(1) << 63
    private val twoTo64 = BigInt(1) << 64

    private def assertRejects(hex: String): Unit =
        assertThrows[io.bullet.borer.Borer.Error[?]](decodeHex(hex))

    test("Constr index around the signed 64-bit boundary") {
        assert(encodeHex(Constr(twoTo63 - 1, PList.Nil)) == "d866821b7fffffffffffffff80")
        assert(decodeHex("d8 66 82 1b 7fffffffffffffff 80") == Constr(twoTo63 - 1, PList.Nil))

        // Long.MaxValue + 1: still a valid unsigned 64-bit index, and the value at which a
        // signed write silently wrapped into a CBOR negative integer.
        assert(encodeHex(Constr(twoTo63, PList.Nil)) == "d866821b800000000000000080")
        assert(decodeHex("d8 66 82 1b 8000000000000000 80") == Constr(twoTo63, PList.Nil))

        assert(encodeHex(Constr(twoTo64 - 1, PList.Nil)) == "d866821bffffffffffffffff80")
        assert(decodeHex("d8 66 82 1b ffffffffffffffff 80") == Constr(twoTo64 - 1, PList.Nil))
    }

    test("Constr index above the unsigned 64-bit range does not round-trip") {
        // Outside the Plutus Data contract, since the index is a CBOR unsigned 64-bit integer.
        // Both implementations emit a bignum here and neither reads it back, so what matters is
        // that the value fails loudly instead of wrapping into a different one.
        val encoded = encodeHex(Constr(twoTo64, PList.Nil))
        assert(encoded.startsWith("d86682c2"))
        assertRejects(encoded)
        assertRejects("d8 66 82 c2 49 010000000000000000 80")
    }

    /* `encodeData` writes an index beyond 64 bits with cborg's `encodeInteger`: tag 2 and ONE
     * definite byte string of the minimal magnitude, not chunked like the integer of `I`. Borer's
     * BigInt encoder added Java's sign byte `00` whenever the top bit of the magnitude was set.
     * The expected bytes are `serialiseData` of plutus-ledger-api 1.70.0.0, reported in #398.
     */
    test("Constr index beyond 64 bits is one definite byte string without a sign byte") {
        def expected(magnitude: String) = {
            val length = magnitude.length / 2
            val head = if length < 24 then f"${0x40 + length}%02x" else f"58$length%02x"
            "d86682c2" + head + magnitude + "80"
        }
        val cases = Seq(
          65 -> ("01" + "ff" * 8),
          72 -> "ff" * 9,
          512 -> "ff" * 64,
          513 -> ("01" + "ff" * 64),
          600 -> "ff" * 75
        )
        for (bits, magnitude) <- cases do
            val d = Constr((BigInt(1) << bits) - 1, PList.Nil)
            assert(encodeHex(d) == expected(magnitude), s"index of $bits bits")
            assert(Utils.bytesToHex(Builtins.serialiseData(d).bytes) == expected(magnitude))
        assert(encodeHex(Constr(twoTo64, PList.Nil)) == expected("01" + "00" * 8))
    }

    test("Constr index must be a non-negative integer") {
        assertRejects("d8 66 82 20 80") // -1
        assertRejects("d8 66 82 3b 8000000000000000 80") // -2^63 - 1, a negative overlong
    }

    test("tag 102 rejects a three-element array") {
        // Wrapped in an outer two-element array so the stray item is consumed there instead of
        // surfacing later as trailing input: this used to decode as `List(Constr(0, []), I(0))`.
        assertRejects("82 d8 66 83 00 80 00")
    }

    test("tag 102 rejects a one-element array") {
        assertRejects("d8 66 81 00")
    }

    test("tag 102 rejects an indefinite-length array of the wrong arity") {
        assertRejects("d8 66 9f 00 80 00 ff")
    }

    test("tag 102 accepts an indefinite-length array") {
        assert(decodeHex("d8 66 9f 00 80 ff") == Constr(0, PList.Nil))
        assert(decodeHex("d8 66 9f 1b 8000000000000000 80 ff") == Constr(twoTo63, PList.Nil))
    }

    test("Encoding/decoding of Lists") {
        assert(
          encodeHex(
            List(
              PList(I(0), I(-1), I(100), I(1000), I(BigInt("1234567890111213141516")))
            ): Data
          ) == "9f002018641903e8c24942ed123b08fe58fe0cff"
        )
        assert(
          decodeHex("9f 00 20 18 64 19 03 e8 c2 49 42 ed 12 3b 08 fe 58 fe 0c ff") ==
              (List(
                PList(I(0), I(-1), I(100), I(1000), I(BigInt("1234567890111213141516")))
              ): Data)
        )
    }

    test("ByteString <= 64") {
        val longBs = "1234567890"
        val expected = "4a31323334353637383930"
        assert(
          encodeHex(B(scalus.uplc.builtin.ByteString.unsafeFromArray(longBs.getBytes))) == expected
        )
        assert(
          decodeHex(expected) == B(scalus.uplc.builtin.ByteString.unsafeFromArray(longBs.getBytes))
        )
    }

    test("ByteString chunks") {
        val longBs = "1234567890" * 7
        val expected =
            "5f58403132333435363738393031323334353637383930313233343536373839303132333435363738393031323334353637383930313233343536373839303132333446353637383930ff"
        assert(encodeHex(B(scalus.uplc.builtin.ByteString.fromArray(longBs.getBytes))) == expected)
        assert(decodeHex(expected) == B(scalus.uplc.builtin.ByteString.fromArray(longBs.getBytes)))
    }

    test("All ByteString values are encoded/decoded correctly") {
        forAll { (bs: ByteString) =>
            roundtrip(B(bs))
        }
    }

    test("Map can have duplicate keys") {
        val data =
            Map(
              PList(
                (
                  Map(
                    PList(
                      (
                        Map(PList.Nil),
                        B(
                          hex"95800c7f4d004080660000dec201ffd1ff01ff9d00ce9058267fa001807f62bd7f7f80baffbccf567fffbb06ff9b7f4eff39807fc7010b0001ffca00d0c1f8"
                        )
                      ),
                      (
                        Map(PList.Nil),
                        B(hex"4f0138e6010000b2017f7f01de01")
                      )
                    )
                  ),
                  B(
                    hex"6a017fd28a7f007fff80009cb6997f8bbcb2f080ff97ff0eca0a0142ffff007f808480ffbd6c5e606580ab6101fffffba13b7f7f0101dbffff62"
                  )
                )
              )
            )
        roundtrip(data)
    }

    test("Encoder <-> Decoder roundtrip") {
        forAll { (d: Data) =>
            roundtrip(d)
        }
    }

    test("the bytes are those of the recursive encoder it replaced") {
        forAll { (d: Data) =>
            val expected = Utils.bytesToHex(Cbor.encode(d)(using RecursiveEncoder).toByteArray)
            assert(Utils.bytesToHex(d.toCbor) == expected)
            assert(Utils.bytesToHex(Builtins.serialiseData(d).bytes) == expected)
        }
    }

    /** The recursive encoder that `dataCborEncoder` replaced (#398), kept as an oracle: the bytes
      * must not change, except for a `Constr` index beyond 64 bits, which the generator does not
      * produce.
      */
    private object RecursiveEncoder extends io.bullet.borer.Encoder[Data]:
        def write(w: io.bullet.borer.Writer, data: Data): io.bullet.borer.Writer = {
            given io.bullet.borer.Encoder[Data] = this
            def bytes(value: Array[Byte]) = {
                if value.length <= 64 then w.writeBytes(value)
                else w.writeBytesIterator(value.grouped(64))
            }
            data match {
                case Constr(c, args) =>
                    if c < 7 then w.writeTag(Tag.Other((121 + c).toLong))
                    else if c < 128 then w.writeTag(Tag.Other((1280 + (c - 7)).toLong))
                    else w.writeTag(Tag.Other(102)).writeArrayHeader(2).write(c)
                    w.writeLinearSeq(args.toScalaList)
                case Map(entries) =>
                    val pairs = entries.toScalaList
                    if pairs.isEmpty then w.writeEmptyMap()
                    else
                        w.writeMapHeader(pairs.size)
                        pairs.foreach((k, v) => w.write(k).write(v))
                        w
                case List(items)               => w.writeLinearSeq(items.toScalaList)
                case I(x) if x.bitLength <= 64 => w.write(x)
                case I(x) =>
                    val magnitude = x.toByteArray
                    if x.signum < 0 then magnitude.mapInPlace(b => (~b.toInt).toByte)
                    w.writeTag(if x.signum < 0 then Tag.NegativeBigNum else Tag.PositiveBigNum)
                    bytes(if magnitude.head == 0 then magnitude.tail else magnitude)
                case B(value) => bytes(value.bytes)
            }
        }
