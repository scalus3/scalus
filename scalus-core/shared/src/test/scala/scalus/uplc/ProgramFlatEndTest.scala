package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{MajorProtocolVersion, PlutusScript, Script}
import scalus.serialization.cbor.Cbor
import scalus.serialization.flat.FlatDecodingError
import scalus.uplc.builtin.ByteString
import scalus.utils.Hex.hexToBytes

/** A flat-encoded program must end the bytes it is read from, as `unflat` in Plutus requires. The
  * filler after the program is any number of 0 bits and then a 1 bit (`dFiller`), and the input
  * must end right behind it, on a byte boundary (`strictDecoder`).
  */
class ProgramFlatEndTest extends AnyFunSuite {

    /** `(program 1.1.0 (con unit))`. The program takes 34 bits: its last byte, `0x81`, holds the
      * last two bits of the program and then the filler `000001`.
      */
    private val exact = "0101004981".hexToBytes

    /** The same program with a filler that runs on into one more byte: `000000 00000001`. */
    private val longerFiller = "010100498001".hexToBytes

    private val trailingZero = "010100498100".hexToBytes
    private val trailingFF = "0101004981ff".hexToBytes

    /** The filler has no 1 bit. */
    private val noFiller = "0101004980".hexToBytes

    /** The filler `00001` ends one bit before the end of the byte. */
    private val misalignedFiller = "0101004982".hexToBytes

    private val unit = Term.Const(Constant.Unit)

    test("a program that ends right behind its filler decodes, also with a longer filler") {
        assert(DeBruijnedProgram.fromFlatEncoded(exact).term == unit)
        assert(DeBruijnedProgram.fromFlatEncoded(exact).flatEncoded.sameElements(exact))
        assert(DeBruijnedProgram.fromFlatEncoded(longerFiller).term == unit)
    }

    test("bytes after the filler are refused") {
        for bytes <- Seq(trailingZero, trailingFF) do
            assertThrows[FlatDecodingError](ProgramFlatCodec.decodeFlat(bytes))
            assertThrows[FlatDecodingError](DeBruijnedProgram.fromFlatEncoded(bytes))
            assertThrows[FlatDecodingError](Program.fromFlatEncoded(bytes))
            assertThrows[FlatDecodingError](DeBruijnedProgram.fromCbor(Cbor.encode(bytes)))
            assertThrows[FlatDecodingError](DeBruijnedProgram.fromCborPrefix(Cbor.encode(bytes)))
    }

    test("a missing filler or one that ends inside a byte is refused, also with remaining bytes") {
        for bytes <- Seq(noFiller, misalignedFiller) do
            assertThrows[FlatDecodingError](DeBruijnedProgram.fromFlatEncoded(bytes))
            assertThrows[FlatDecodingError](ProgramFlatCodec.decodeFlatWithRemainingBytes(bytes))
            assertThrows[FlatDecodingError](
              DeBruijnedProgram.fromCborWithRemainingBytes(Cbor.encode(bytes))
            )
    }

    test("decodeFlatWithRemainingBytes returns the bytes after the filler") {
        val result = ProgramFlatCodec.decodeFlatWithRemainingBytes(trailingFF)
        assert(result.program.term == unit)
        assert(result.remainder.sameElements(Array(0xff.toByte)))
    }

    test("the program of a script refuses bytes after its filler, in every language") {
        val script = ByteString.fromArray(Cbor.encode(trailingZero))
        for
            make <- Seq[ByteString => PlutusScript](
              Script.PlutusV1(_),
              Script.PlutusV2(_),
              Script.PlutusV3(_),
              Script.PlutusV4(_)
            )
            decode <- Seq[PlutusScript => Any](_.program, _.deBruijnedProgram)
        do assertThrows[FlatDecodingError](decode(make(script)))
    }

    test("in V1 and V2, bytes after the CBOR byte string do not excuse bytes after the filler") {
        val good = Cbor.encode(exact)
        val bad = Cbor.encode(trailingZero)
        for make <- Seq[ByteString => PlutusScript](Script.PlutusV1(_), Script.PlutusV2(_)) do
            // the flat rule applies inside the first byte string, whatever comes after it
            val badFirst = make(ByteString.fromArray(bad ++ good))
            assertThrows[FlatDecodingError](badFirst.deBruijnedProgram)
            assert(!badFirst.isWellFormed(MajorProtocolVersion.dijkstraPV))
            // the bytes after the first byte string are not read as flat
            val goodFirst = make(ByteString.fromArray(good ++ bad))
            assert(goodFirst.deBruijnedProgram.term == unit)
            assert(goodFirst.isWellFormed(MajorProtocolVersion.dijkstraPV))
    }
}
