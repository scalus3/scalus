package scalus.cardano.ledger

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.uplc.builtin.{Builtins, ByteString, Data}
import scalus.utils.Hex

/** Inline datums keep their original CBOR, spec [SC-13] to [SC-13h]. */
class DatumOptionTest extends AnyFunSuite {

    /** `Constr 0 [10]` with the integer in 5 bytes: valid, but not the minimal encoding. */
    private val probeDatumHex = "d879811a0000000a"
    private val canonicalDatumHex = "d8799f0aff"
    private val probeData: Data = Data.fromCbor(Hex.hexToBytes(canonicalDatumHex))

    private val address: Address = ShelleyAddress(
      Network.Testnet,
      ShelleyPaymentPart.Key(AddrKeyHash.fromHex("a" * 56)),
      ShelleyDelegationPart.Null
    )

    /** An output holding the probe datum inline, as CBOR hex. */
    private val probeOutputHex: String = {
        val canonical = Hex.bytesToHex(
          Cbor.encode(
            TransactionOutput.Babbage(
              address,
              Value.ada(2),
              Some(DatumOption.Inline(probeData)),
              None
            ): TransactionOutput
          ).toByteArray
        )
        // tag 24 wraps a byte string: 0x45 holds 5 bytes, 0x48 holds 8
        val wrapped = "d81845" + canonicalDatumHex
        assert(canonical.contains(wrapped))
        canonical.replace(wrapped, "d81848" + probeDatumHex)
    }

    private def decodeOutput(hex: String): TransactionOutput =
        Cbor.decode(Hex.hexToBytes(hex)).to[TransactionOutput].value

    private def encodeOutput(output: TransactionOutput): String =
        Hex.bytesToHex(Cbor.encode(output).toByteArray)

    private def probeInline: DatumOption =
        decodeOutput(probeOutputHex).datumOption.getOrElse(fail("the output has no datum"))

    test("an output with a non-minimal inline datum re-encodes byte for byte") {
        // spec [SC-13], [SC-13c], [SC-13d]
        assert(encodeOutput(decodeOutput(probeOutputHex)) == probeOutputHex)
    }

    test("case Inline(d) binds the decoded Data") {
        // spec [SC-13f]
        probeInline match
            case DatumOption.Inline(d) => assert(d == probeData)
            case other                 => fail(s"expected an inline datum, got $other")
    }

    test("Inline(data) builds an inline datum with the canonical bytes") {
        // spec [SC-13e]
        val output =
            TransactionOutput.Babbage(address, Value.ada(2), Some(DatumOption.Inline(probeData)))
        assert(encodeOutput(output).contains("d81845" + canonicalDatumHex))
    }

    test("inline datums with the same Data and different bytes are not equal") {
        // spec [SC-13g]
        assert(probeInline != DatumOption.Inline(probeData))
        assert(probeInline == decodeOutput(probeOutputHex).datumOption.get)
    }

    test("contentEquals compares two inline datums by their Data") {
        // spec [SC-13h]
        assert(probeInline.contentEquals(DatumOption.Inline(probeData)))
    }

    test("contentEquals matches a hash against the hash of the original bytes") {
        // spec [SC-13l]: the chain hashes the bytes, not re-encoded Data
        val original = DatumOption.Hash(
          DataHash.fromByteString(Builtins.blake2b_256(ByteString.fromHex(probeDatumHex)))
        )
        val canonical = DatumOption.Hash(DataHash.fromByteString(probeData.dataHash))
        assert(original.contentEquals(probeInline))
        assert(probeInline.contentEquals(original))
        assert(!canonical.contentEquals(probeInline))
        assert(!probeInline.contentEquals(canonical))
    }

    test("the hash of an inline datum is the hash of its original bytes") {
        // spec [SC-13i]: Haskell hashes the memoized bytes, not re-encoded Data
        val expected =
            DataHash.fromByteString(Builtins.blake2b_256(ByteString.fromHex(probeDatumHex)))
        assert(probeInline.dataHash == expected)
    }

    test("a datum hash still round-trips") {
        val hash = DatumOption.Hash(DataHash.fromByteString(ByteString.fromHex("b" * 64)))
        val output = TransactionOutput.Babbage(address, Value.ada(2), Some(hash))
        assert(decodeOutput(encodeOutput(output)) == output)
    }
}
