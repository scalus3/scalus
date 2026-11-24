package scalus.testing.conformance

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.ledger.{DataHash, DatumOption}
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

    test("a DataHash32 decodes to the datum hash of the transaction output") {
        // The tag-3 TxOut of the vector "UTXOW.Fails with PPViewHashesDontMatch before PV 11",
        // 0.json, in scalus-cardano-ledger-it/src/test/resources/vectors.tar.gz. Mempack writes
        // Addr28Extra and DataHash32 as four Word64 in little-endian order.
        val tag3 = ByteString.fromHex(
          "030107b06f489181db0f3083b7c39de533cb94e1b16fe4a96ed4b79f8ac853b660ea0d10c132d9203e" +
              "b3ad323e23272007f759830ca300000000a0003eec00bd8440b7b797752e0a17039a131d39054cd8e3" +
              "c0d88687e757b1621413114ccf9df282"
        )
        // The address and the datum hash of output 0 of the transaction body
        val bodyAddress = Address.fromByteString(
          ByteString.fromHex(
            "1032c1100dea60b653233e32adb33e20d9a30c8359f7072027ec3e00a0" +
                "07b06f489181db0f3083b7c39de533cb94e1b16fe4a96ed4b79f8ac8"
          )
        )
        val bodyDatumHash =
            DataHash.fromHex("03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314")

        val output = MempackParser.parseOutput(tag3.bytes)

        assert(output.datumOption.contains(DatumOption.Hash(bodyDatumHash)))
        assert(output.address == bodyAddress)
    }
}
