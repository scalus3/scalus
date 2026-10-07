package scalus.testing.conformance

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.DatumOption
import scalus.uplc.builtin.ByteString

class MempackParserTest extends AnyFunSuite {

    test("an inline datum keeps its original bytes") {
        // d879811a0000000a is valid CBOR for Constr 0 [10], but not minimal
        val probe = ByteString.fromHex("d879811a0000000a").bytes
        val address = 0x61.toByte +: Array.fill[Byte](28)(1) // enterprise key address, mainnet
        val tag4 = Array[Byte](4, address.length.toByte) ++ address ++
            Array[Byte](0, 5) ++ // coin-only value, 5 lovelace
            (probe.length.toByte +: probe)

        MempackParser.parseOutput(tag4).datumOption match
            case Some(inline: DatumOption.Inline) =>
                assert(inline.binaryData.raw.sameElements(probe))
            case other => fail(s"expected an inline datum, got $other")
    }
}
