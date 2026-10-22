package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.serialization.cbor.Cbor
import scalus.uplc.builtin.ByteString
import scalus.uplc.{DeBruijnedProgram, Program}

/** [[PlutusScript.isWellFormed]] reads a script as `deserialiseScript` in plutus-ledger-api does.
  * Bytes after the script's CBOR byte string are allowed in Plutus V1 and V2 only; bytes after the
  * program inside the byte string are allowed in no language.
  */
class PlutusScriptWellFormedTest extends AnyFunSuite {

    private val languages = List(Language.PlutusV1, Language.PlutusV2, Language.PlutusV3)

    /** `(program 1.0.0 (con integer 1))`. Its last byte, `0x81`, holds the last two bits of the
      * program and then the padding `000001`.
      */
    private val flat: Array[Byte] =
        Program.parseUplc("(program 1.0.0 (con integer 1))").toOption.get.flatEncoded

    private val script: Array[Byte] = Cbor.encode(flat)

    /** For V1, V2 and V3, whether `bytes` is a well-formed script in protocol version 9. */
    private def wellFormed(bytes: Array[Byte]): List[Boolean] =
        languages.map(
          PlutusScript.isWellFormed(ByteString.fromArray(bytes), _, MajorProtocolVersion.changPV)
        )

    private def withLastByte(b: Int): Array[Byte] = flat.updated(flat.length - 1, b.toByte)

    test("a script that is exactly one CBOR byte string is well formed") {
        assert(flat.last == 0x81.toByte)
        assert(wellFormed(script) == List(true, true, true))
    }

    test("bytes after the CBOR byte string are allowed in Plutus V1 and V2 only") {
        assert(wellFormed(script ++ script) == List(true, true, false))
        assert(wellFormed(script :+ 0.toByte) == List(true, true, false))
    }

    test("bytes after the program inside the CBOR byte string are allowed in no language") {
        assert(wellFormed(Cbor.encode(flat :+ 0.toByte)) == List(false, false, false))
    }

    test("the padding must end on a byte boundary") {
        // `0x82` ends the padding, `00001`, one bit before the end of the byte
        val badPadding = withLastByte(0x82)
        assert(
          DeBruijnedProgram.fromFlatEncoded(badPadding) == DeBruijnedProgram.fromFlatEncoded(flat)
        )
        assert(wellFormed(Cbor.encode(badPadding)) == List(false, false, false))
    }

    test("the padding may run on into further bytes") {
        // flat's `Filler` is any number of 0 bits and then a 1 bit, so `0x80 0x01` pads as `0x81`
        assert(wellFormed(Cbor.encode(withLastByte(0x80) :+ 1.toByte)) == List(true, true, true))
    }

    test("the CBOR byte string must have a definite length") {
        val indefinite = Array(0x5f.toByte) ++ script :+ 0xff.toByte
        assert(wellFormed(indefinite) == List(false, false, false))
    }

    test("the length of the CBOR byte string may take more bytes than it needs") {
        val longHeader = Array(0x59, 0x00, flat.length).map(_.toByte) ++ flat
        assert(wellFormed(longHeader) == List(true, true, true))
    }

    test("program and deBruijnedProgram ignore the bytes after the CBOR byte string") {
        val expected = DeBruijnedProgram.fromCbor(script)
        val twice = ByteString.fromArray(script ++ script)
        assert(Script.PlutusV2(twice).deBruijnedProgram == expected)
        assert(Script.PlutusV2(twice).program.deBruijnedProgram == expected)
    }
}
