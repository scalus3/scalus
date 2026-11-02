package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.serialization.flat.FlatDecodingError
import scalus.uplc.builtin.{Builtins, ByteString}
import scalus.uplc.FlatDecodeOracle.{allAt9To11, assertAgrees, Row}
import scalus.uplc.FlatProgramBuilder.*

/** A `string` constant must be UTF-8: the reference decodes it with the strict `decodeUtf8'`
  * (`dUTF8`, `PlutusCore/Flat/Decoder/Strict.hs`) and fails on anything else, where Scalus used to
  * replace it with U+FFFD and go on.
  */
class FlatStringConstantTest extends AnyFunSuite {

    // The reference's verdicts on these scripts, see `FlatDecodeOracle`.
    private val rows = Seq(
      Row(
        "string-utf8",
        "e6294e949d137df98e84717a98626c01d3072e5bbff14e536e97b1159a0ddda3",
        allAt9To11,
        ""
      ),
      Row(
        "string-not-utf8",
        "705dab8b18863e0a99bdb43d209da15cbf0a97c86b94a8f5f11a26549d0b473b",
        "",
        allAt9To11
      ),
      Row(
        "string-surrogate",
        "b4e09b65e2de565a64c00a93fd97fdef67b9190904858d30c6e6f19ededa9646",
        "",
        allAt9To11
      ),
    )

    private val constants: Map[String, Array[Byte]] = Map(
      "string-utf8" -> "ok".getBytes("UTF-8"),
      "string-not-utf8" -> Array[Byte](0xff.toByte, 0xfe.toByte),
      // U+D800, a surrogate, written as three bytes
      "string-surrogate" -> Array[Byte](0xed.toByte, 0xa0.toByte, 0x80.toByte)
    )

    private val scripts: Map[String, Array[Byte]] =
        constants.map { case (id, raw) => id -> script(discard(string(raw))) }

    test("scripts with string constants are admitted as the reference admits them") {
        assertAgrees(rows, scripts)
    }

    // The boundaries of Table 3-7 of the Unicode standard. This test is shared, so JVM, Scala.js
    // and Scala Native must all decide them as `decodeUtf8'` does.
    private val wellFormed = Seq(
      "" -> "",
      "6f6b" -> "ok",
      "00" -> "\u0000",
      "7f" -> "\u007f",
      "c280" -> "\u0080",
      "c3a9" -> "é",
      "dfbf" -> "߿",
      "e0a080" -> "ࠀ",
      "e282ac" -> "€",
      "ed9fbf" -> "퟿",
      "ee8080" -> "",
      "efbfbf" -> "￿",
      "f0908080" -> "𐀀", // U+10000
      "f09d849e" -> "𝄞",
      "f48fbfbf" -> "􏿿" // U+10FFFF
    )

    private val illFormed = Seq(
      "80", // a continuation byte without a lead
      "bf",
      "c0af", // overlong encodings
      "c1bf",
      "e080af",
      "f08080af",
      "c328", // a lead without its continuation
      "e282",
      "f09d84",
      "eda080", // surrogates U+D800 and U+DFFF
      "edbfbf",
      "f4908080", // above U+10FFFF
      "f5808080",
      "ff",
      "6fff6b",
      "fffe"
    )

    test("well-formed UTF-8 is decoded by the flat decoder and by decodeUtf8") {
        import scalus.utils.Hex.hexToBytes
        for (hex, text) <- wellFormed do
            assert(decodeDiscarded(string(hex.hexToBytes)) == Constant.String(text), hex)
            assert(Builtins.decodeUtf8(ByteString.unsafeFromArray(hex.hexToBytes)) == text, hex)
    }

    test("ill-formed UTF-8 is refused by the flat decoder and by decodeUtf8") {
        import scalus.utils.Hex.hexToBytes
        for hex <- illFormed do
            withClue(hex) {
                assertThrows[FlatDecodingError](decodeDiscarded(string(hex.hexToBytes)))
                assertThrows[IllegalArgumentException](
                  Builtins.decodeUtf8(ByteString.unsafeFromArray(hex.hexToBytes))
                )
            }
    }
}
