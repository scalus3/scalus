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
}
