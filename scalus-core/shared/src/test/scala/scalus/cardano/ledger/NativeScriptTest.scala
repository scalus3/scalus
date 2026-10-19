package scalus.cardano.ledger

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.serialization.cbor.Cbor as KeepRawCbor
import scalus.uplc.builtin.{platform, ByteString}
import scalus.utils.Hex

/** Native scripts keep their original CBOR, as Haskell's `MemoBytes` does. */
class NativeScriptTest extends AnyFunSuite {

    /** `RequireTimeAfter 10` with the slot in 5 bytes: valid, but not the minimal encoding. */
    private val probeHex = "82041a0000000a"
    private val canonicalHex = "82040a"
    private val probeTimelock = Timelock.TimeStart(10)

    private val address: Address = ShelleyAddress(
      Network.Testnet,
      ShelleyPaymentPart.Key(AddrKeyHash.fromHex("a" * 56)),
      ShelleyDelegationPart.Null
    )

    private def probe: Script.Native = Script.Native.fromCbor(Hex.hexToBytes(probeHex))

    /** `blake2b_224(0x00 ++ bytes)`, `hashScript` of a timelock in cardano-ledger. */
    private def hashOf(hex: String): ScriptHash =
        Hash(platform.blake2b_224(ByteString.fromHex("00" + hex)))

    /** An output holding the probe script as its reference script, as CBOR hex. */
    private val probeOutputHex: String = {
        val output: TransactionOutput = TransactionOutput.Babbage(
          address,
          Value.ada(2),
          None,
          Some(ScriptRef(Script.Native(probeTimelock)))
        )
        val canonical = Hex.bytesToHex(Cbor.encode(output).toByteArray)
        // tag 24 wraps a byte string of `[0, timelock]`: 0x45 holds 5 bytes, 0x49 holds 9
        val wrapped = "d81845" + "8200" + canonicalHex
        assert(canonical.contains(wrapped))
        canonical.replace(wrapped, "d81849" + "8200" + probeHex)
    }

    private def decodeOutput(hex: String): TransactionOutput =
        Cbor.decode(Hex.hexToBytes(hex)).to[TransactionOutput].value

    test("a native script decoded from CBOR binds its timelock") {
        probe match
            case Script.Native(timelock) => assert(timelock == probeTimelock)
    }

    test("the hash of a native script is the hash of its original bytes") {
        assert(probe.scriptHash == hashOf(probeHex))
        assert(Script.Native(probeTimelock).scriptHash == hashOf(canonicalHex))
    }

    test("Native(timelock) encodes the timelock canonically") {
        assert(Hex.bytesToHex(KeepRawCbor.encode(Script.Native(probeTimelock))) == canonicalHex)
    }

    test("borer's validating writer encodes a canonical native script") {
        val script: Script = Script.Native(probeTimelock)
        assert(Hex.bytesToHex(Cbor.encode(script).toByteArray) == "8200" + canonicalHex)
    }

    test("native scripts with the same timelock and different bytes are not equal") {
        assert(probe != Script.Native(probeTimelock))
        assert(probe == Script.Native.fromCbor(Hex.hexToBytes(probeHex)))
    }

    test("an output with a non-minimal native reference script re-encodes byte for byte") {
        val output = decodeOutput(probeOutputHex)
        assert(Hex.bytesToHex(Cbor.encode(output).toByteArray) == probeOutputHex)
        assert(output.scriptRef.map(_.script.scriptHash).contains(hashOf(probeHex)))
    }

    test("a witness set keys a non-minimal native script by the hash of its original bytes") {
        val tx = Transaction(
          TransactionBody(
            TaggedSortedSet(
              TransactionInput(TransactionHash.fromByteString(ByteString.fromHex("1" * 64)), 0)
            ),
            IndexedSeq.empty,
            Coin.zero
          ),
          TransactionWitnessSet(Seq(Script.Native(probeTimelock)), None, Set.empty, Seq.empty)
        )
        val canonical = Hex.bytesToHex(tx.toCbor)
        // witness set key 1 holds a one-element array of timelocks
        val wrapped = "81" + canonicalHex
        assert(canonical.contains(wrapped))
        val decoded =
            Transaction.fromCbor(Hex.hexToBytes(canonical.replace(wrapped, "81" + probeHex)))
        val scripts = decoded.witnessSet.nativeScripts.toMap
        assert(scripts.keySet == Set(hashOf(probeHex)))
        assert(Hex.bytesToHex(KeepRawCbor.encode(decoded.witnessSet)).contains("81" + probeHex))
    }

    /** `RequireAllOf [RequireTimeAfter 10]` with every array of indefinite length. Haskell reads
      * the sum (`decodeRecordSum`) and the list (`decodeStrictSeq`) with `decodeListLenOrIndef`.
      */
    private val indefiniteHex = "9f019f9f040affffff"
    private val nestedCanonicalHex = "8201818204" + "0a"
    private val nestedTimelock = Timelock.AllOf(IndexedSeq(Timelock.TimeStart(10)))

    /** Tag 24 around a CBOR byte string shorter than 24 bytes. */
    private def tag24(hex: String): String = f"d818${0x40 + hex.length / 2}%02x$hex"

    test("a native script with indefinite-length arrays decodes and keeps its bytes") {
        val script = Script.Native.fromCbor(Hex.hexToBytes(indefiniteHex))
        assert(script.script == nestedTimelock)
        assert(script.scriptHash == hashOf(indefiniteHex))
        assert(Hex.bytesToHex(KeepRawCbor.encode(script)) == indefiniteHex)
    }

    test("an output with an indefinite-length native reference script re-encodes byte for byte") {
        val output: TransactionOutput = TransactionOutput.Babbage(
          address,
          Value.ada(2),
          None,
          Some(ScriptRef(Script.Native(nestedTimelock)))
        )
        val canonical = Hex.bytesToHex(Cbor.encode(output).toByteArray)
        val wrapped = tag24("8200" + nestedCanonicalHex)
        assert(canonical.contains(wrapped))
        val hex = canonical.replace(wrapped, tag24("8200" + indefiniteHex))
        val decoded = decodeOutput(hex)
        assert(Hex.bytesToHex(Cbor.encode(decoded).toByteArray) == hex)
        assert(decoded.scriptRef.map(_.script.scriptHash).contains(hashOf(indefiniteHex)))
    }

    test("a witness set keeps an indefinite-length native script byte for byte") {
        val tx = Transaction(
          TransactionBody(
            TaggedSortedSet(
              TransactionInput(TransactionHash.fromByteString(ByteString.fromHex("1" * 64)), 0)
            ),
            IndexedSeq.empty,
            Coin.zero
          ),
          TransactionWitnessSet(Seq(Script.Native(nestedTimelock)), None, Set.empty, Seq.empty)
        )
        val canonical = Hex.bytesToHex(tx.toCbor)
        val wrapped = "81" + nestedCanonicalHex
        assert(canonical.contains(wrapped))
        val decoded =
            Transaction.fromCbor(Hex.hexToBytes(canonical.replace(wrapped, "81" + indefiniteHex)))
        assert(decoded.witnessSet.nativeScripts.toMap.keySet == Set(hashOf(indefiniteHex)))
        assert(
          Hex.bytesToHex(KeepRawCbor.encode(decoded.witnessSet)).contains("81" + indefiniteHex)
        )
    }

    test("a transaction keeps the bytes of a non-canonical native script in its auxiliary data") {
        // indefinite-length arrays and the slot in 5 bytes
        val scriptHex = "9f019f9f041a0000000affffff"
        // tag 259, a map of one entry: key 1 holds a one-element array of timelocks
        val canonicalAuxHex = "d90103a10181" + nestedCanonicalHex
        val rawAuxHex = "d90103a10181" + scriptHex
        def auxHash(hex: String): AuxiliaryDataHash =
            AuxiliaryDataHash.fromByteString(platform.blake2b_256(ByteString.fromHex(hex)))
        val tx = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet(
              TransactionInput(TransactionHash.fromByteString(ByteString.fromHex("1" * 64)), 0)
            ),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            auxiliaryDataHash = Some(auxHash(rawAuxHex))
          ),
          TransactionWitnessSet.empty,
          AuxiliaryData.AlonzoFormat(None, IndexedSeq(nestedTimelock))
        )
        val canonical = Hex.bytesToHex(tx.toCbor)
        assert(canonical.contains(canonicalAuxHex))
        val hex = canonical.replace(canonicalAuxHex, rawAuxHex)
        val decoded = Transaction.fromCbor(Hex.hexToBytes(hex))
        assert(
          decoded.auxiliaryData.map(_.value.getNativeScripts) == Some(IndexedSeq(nestedTimelock))
        )
        assert(Hex.bytesToHex(decoded.toCbor) == hex)
        // the hash covers the bytes as sent, so it matches the body only for the raw bytes
        val raw = decoded.auxiliaryData.map(aux => Hex.bytesToHex(aux.raw))
        assert(raw == Some(rawAuxHex))
        assert(raw.map(auxHash) == decoded.body.value.auxiliaryDataHash)
        assert(auxHash(rawAuxHex) != auxHash(canonicalAuxHex))
    }

    test("an MOf script with indefinite-length arrays decodes and keeps its bytes") {
        // `RequireMOf 1 [RequireSignature h]`, every array of indefinite length
        val keyHash = "c" * 56
        val hex = s"9f03019f9f00581c${keyHash}ffffff"
        val script = Script.Native.fromCbor(Hex.hexToBytes(hex))
        assert(
          script.script == Timelock.MOf(
            1,
            IndexedSeq(Timelock.Signature(AddrKeyHash.fromHex(keyHash)))
          )
        )
        assert(script.scriptHash == hashOf(hex))
        assert(Hex.bytesToHex(KeepRawCbor.encode(script)) == hex)
    }

    test("a definite-length script holding an indefinite-length list decodes and keeps its bytes") {
        val hex = "82019f82040aff"
        val script = Script.Native.fromCbor(Hex.hexToBytes(hex))
        assert(script.script == nestedTimelock)
        assert(script.scriptHash == hashOf(hex))
        assert(Hex.bytesToHex(KeepRawCbor.encode(script)) == hex)
    }

    test("a definite-length script whose array size does not fit its constructor is rejected") {
        // `matchSize` in cardano-ledger's `decodeRecordSum`: 3 for MOf, 2 for the others
        val malformed = Seq(
          s"8300581c${"c" * 56}", // Signature with a size of 3
          "83040a", // TimeStart with a size of 3
          "82030180" // MOf with a size of 2
        )
        for hex <- malformed do
            withClue(hex) {
                val error = intercept[io.bullet.borer.Borer.Error[?]](
                  Timelock.fromCbor(Hex.hexToBytes(hex))
                )
                assert(error.getMessage.contains("Timelock array of size"), error)
            }
    }
}
