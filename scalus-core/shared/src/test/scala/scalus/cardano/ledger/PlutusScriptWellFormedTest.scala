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

    private val languages =
        List(Language.PlutusV1, Language.PlutusV2, Language.PlutusV3, Language.PlutusV4)

    /** `(program 1.0.0 (con integer 1))`. Its last byte, `0x81`, holds the last two bits of the
      * program and then the padding `000001`.
      */
    private val flat: Array[Byte] =
        Program.parseUplc("(program 1.0.0 (con integer 1))").toOption.get.flatEncoded

    private val script: Array[Byte] = Cbor.encode(flat)

    /** For V1, V2, V3 and V4, whether `bytes` is a well-formed script in protocol version 12, the
      * first with all four languages.
      */
    private def wellFormed(bytes: Array[Byte]): List[Boolean] =
        languages.map(
          PlutusScript.isWellFormed(ByteString.fromArray(bytes), _, MajorProtocolVersion.dijkstraPV)
        )

    private def withLastByte(b: Int): Array[Byte] = flat.updated(flat.length - 1, b.toByte)

    private val all = List(true, true, true, true)
    private val none = List(false, false, false, false)

    test("a script that is exactly one CBOR byte string is well formed") {
        assert(flat.last == 0x81.toByte)
        assert(wellFormed(script) == all)
    }

    test("bytes after the CBOR byte string are allowed in Plutus V1 and V2 only") {
        assert(wellFormed(script ++ script) == List(true, true, false, false))
        assert(wellFormed(script :+ 0.toByte) == List(true, true, false, false))
    }

    test("bytes after the program inside the CBOR byte string are allowed in no language") {
        assert(wellFormed(Cbor.encode(flat :+ 0.toByte)) == none)
    }

    test("the padding must end on a byte boundary") {
        // `0x82` ends the padding, `00001`, one bit before the end of the byte
        val badPadding = withLastByte(0x82)
        assert(
          DeBruijnedProgram.fromFlatEncoded(badPadding) == DeBruijnedProgram.fromFlatEncoded(flat)
        )
        assert(wellFormed(Cbor.encode(badPadding)) == none)
    }

    test("the padding may run on into further bytes") {
        // flat's `Filler` is any number of 0 bits and then a 1 bit, so `0x80 0x01` pads as `0x81`
        assert(wellFormed(Cbor.encode(withLastByte(0x80) :+ 1.toByte)) == all)
    }

    test("the CBOR byte string must have a definite length") {
        val indefinite = Array(0x5f.toByte) ++ script :+ 0xff.toByte
        assert(wellFormed(indefinite) == none)
    }

    test("the length of the CBOR byte string may take more bytes than it needs") {
        val longHeader = Array(0x59, 0x00, flat.length).map(_.toByte) ++ flat
        assert(wellFormed(longHeader) == all)
    }

    test("program and deBruijnedProgram ignore bytes after the CBOR byte string in V1 and V2") {
        val expected = DeBruijnedProgram.fromCbor(script)
        val twice = ByteString.fromArray(script ++ script)
        for plutusScript <- List(Script.PlutusV1(twice), Script.PlutusV2(twice)) do
            assert(plutusScript.deBruijnedProgram == expected)
        for plutusScript <- List(Script.PlutusV1(twice), Script.PlutusV2(twice)) do
            assert(plutusScript.program.deBruijnedProgram == expected)
    }

    test("program and deBruijnedProgram fail on bytes after the CBOR byte string in V3 and V4") {
        val twice = ByteString.fromArray(script ++ script)
        for plutusScript <- List(Script.PlutusV3(twice), Script.PlutusV4(twice)) do
            assertThrows[IllegalArgumentException](plutusScript.deBruijnedProgram)
        for plutusScript <- List(Script.PlutusV3(twice), Script.PlutusV4(twice)) do
            assertThrows[IllegalArgumentException](plutusScript.program)
    }
}
