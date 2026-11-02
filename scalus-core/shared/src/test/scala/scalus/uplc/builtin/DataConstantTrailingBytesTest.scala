package scalus.uplc.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.FlatDecodeOracle.{allAt9To11, assertAgrees, Row}
import scalus.uplc.FlatProgramBuilder.*
import scalus.serialization.flat.FlatDecodingError
import scalus.uplc.Constant

/** The flat decoder reads the first CBOR item of a `data` constant and drops what follows it: the
  * reference reads the constant through `FlatViaSerialise`, whose `deserialiseOrFail` returns the
  * value as soon as the decoder is done (`Codec/Serialise.hs` 134-143 of serialise 0.2.6.1).
  */
class DataConstantTrailingBytesTest extends AnyFunSuite {

    // The reference's verdicts on these scripts, see `FlatDecodeOracle`.
    private val rows = Seq(
      Row(
        "data-trailing-zero",
        "07cb370d4ec3ab07ab86f9c5220cdfd900f144e2c23bb0107743be0e0bbcadc8",
        allAt9To11,
        ""
      ),
      Row(
        "data-trailing-bytes",
        "5f0cd28f7c9140991f222bf9838cf9399a5136593e3a4bd3f00fb4c55b77c50d",
        allAt9To11,
        ""
      ),
      Row(
        "data-trailing-truncated-item",
        "77c36ce20911033d29f2f4f8d5260d5c0a012bb7d427493e11fbd4837560e09b",
        allAt9To11,
        ""
      ),
      Row(
        "data-truncated",
        "94b1d53f3b068952f4c8dbeb396b0741f20c8015b71a87a853c787c731034150",
        "",
        allAt9To11
      ),
    )

    private val constants: Map[String, Array[Byte]] = Map(
      "data-trailing-zero" -> Array[Byte](0x00, 0x00),
      "data-trailing-bytes" -> Array[Byte](0x00, 0xff.toByte, 0x1c),
      "data-trailing-truncated-item" -> Array[Byte](0x00, 0x58),
      "data-truncated" -> Array[Byte](0x82.toByte, 0x00)
    )

    private val scripts: Map[String, Array[Byte]] =
        constants.map { case (id, cbor) => id -> script(discard(data(cbor))) }

    test(
      "scripts with bytes after a data constant's CBOR item are admitted as the reference does"
    ) {
        assertAgrees(rows, scripts)
    }

    test("the bytes after the first CBOR item do not change the decoded constant") {
        for id <- Seq("data-trailing-zero", "data-trailing-bytes", "data-trailing-truncated-item")
        do assert(decodeDiscarded(data(constants(id))) == Constant.Data(Data.I(0)), id)
    }

    test("a data constant that ends inside its first CBOR item is refused") {
        assertThrows[FlatDecodingError](decodeDiscarded(data(constants("data-truncated"))))
    }
}
