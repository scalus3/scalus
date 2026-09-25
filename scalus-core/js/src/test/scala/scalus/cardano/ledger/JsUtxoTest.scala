package scalus.cardano.ledger

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.utils.Hex

import scalus.utils.scalajs.internal.*

import scala.scalajs.js

class JsUtxoTest extends AnyFunSuite {

    // Read a `js.UndefOr` inside `assert` through `.toOption`: ScalaTest's macro decomposing a raw
    // `js.UndefOr` chain, such as `assert(r.errorRule.contains(x))`, crashes the Scala.js backend
    // ("Cannot emit primitive conversion ... to Lscala/scalajs/js/$bar;").

    private val hash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
    private val address = Address.fromString(
      "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw"
    )

    test("a wrapped UTxO exposes hex ids and a Value handle") {
        val utxo = JsUtxo.wrap(TransactionInput(hash, 3), TransactionOutput(address, Value.ada(7)))
        assert(utxo.txHash == "0" * 64)
        assert(utxo.outputIndex == 3.0)
        assert(utxo.address == address.encode.get)
        assert(utxo.value.coin.toString == "7000000")
        assert(utxo.datumHash.toOption.isEmpty)
        assert(utxo.inlineDatum.toOption.isEmpty)
        assert(utxo.scriptRef.toOption.isEmpty)
        assert(utxo.scriptLanguage.toOption.isEmpty)
    }

    test("a UTxO built in JavaScript round-trips to the same ledger value") {
        val built = new JsUtxo("0" * 64, 1.0, address.encode.get, JsValue.ada(js.BigInt("4")))
        assert(built.output.value == Value.ada(4))
        assert(built.input.index == 1)
    }

    test("a UTxO built from an invalid address is rejected by the constructor") {
        // At the call that passed the bad argument, not later from a getter. The message has to
        // name the value and say what was expected: `Address.fromString` on its own reports
        // whichever of bech32/Base58 got furthest, which for `not-a-valid-address` is an
        // unhelpful complaint about a Base58 character.
        val e = intercept[IllegalArgumentException](
          new JsUtxo("0" * 64, 0.0, "not-a-valid-address", JsValue.ada(js.BigInt("1")))
        )
        assert(e.getMessage.contains("not-a-valid-address"))
        assert(e.getMessage.contains("not a Cardano address"))
    }

    test("a UTxO built from a malformed transaction hash is rejected by the constructor") {
        assertThrows[Exception](
          new JsUtxo("nothex", 0.0, address.encode.get, JsValue.ada(js.BigInt("1")))
        )
    }

    test("the bech32 the caller passed is the string address gives back, without re-encoding") {
        val bech32 = address.encode.get
        val built = new JsUtxo("0" * 64, 0.0, bech32, JsValue.ada(js.BigInt("1")))
        assert(built.address == bech32)
    }

    test("address throws rather than silently returning hex when bech32 encoding is impossible") {
        // `Network.Other` is a real, reachable case (a devnet or Yaci-style custom network id),
        // and bech32 has no defined human-readable prefix for it, so `Address.encode` fails. The
        // accessor must fail loudly, not degrade to a hex string that does not look like an
        // address and gives no signal to the caller.
        val unrepresentableAddress = ShelleyAddress(
          Network.Other(3),
          ShelleyPaymentPart.Key(
            Hash[Blake2b_224, HashPurpose.KeyHash](ByteString.fromHex("0" * 56))
          ),
          ShelleyDelegationPart.Null
        )
        val utxo =
            JsUtxo.wrap(
              TransactionInput(hash, 0),
              TransactionOutput(unrepresentableAddress, Value.ada(1))
            )
        assertThrows[IllegalStateException](utxo.address)
    }

    test("wrap stores the ledger pair by reference, so a round trip copies nothing") {
        val input = TransactionInput(hash, 5)
        val output = TransactionOutput(address, Value.ada(2))
        val wrapped = JsUtxo.wrap(input, output)
        assert(wrapped.input eq input)
        assert(wrapped.output eq output)
    }

    test("CBOR round-trip preserves the UTxO") {
        val utxo = JsUtxo.wrap(TransactionInput(hash, 0), TransactionOutput(address, Value.ada(1)))
        val back = JsUtxo.fromCbor(utxo.toCbor())
        assert(back.txHash == utxo.txHash)
        assert(back.value.coin.toString == utxo.value.coin.toString)
    }

    test("toCbor writes [input, output]; fromCbor also reads the one-entry map of 1.2") {
        val (in, out) = (TransactionInput(hash, 2), TransactionOutput(address, Value.ada(1)))
        val cbor = JsUtxo.wrap(in, out).toCbor().toByteArray
        assert(cbor.sameElements(Cbor.encode((in, out)).toByteArray))
        assert(cbor(0) == 0x82.toByte)
        val oldMap = Cbor.encode(Map(in -> out)).toByteArray
        for bytes <- Seq(cbor, oldMap) do
            val back = JsUtxo.fromCbor(bytes.toUint8Array)
            assert(back.input == in && back.output == out)
        val twoEntries = Cbor.encode(Map(in -> out, TransactionInput(hash, 3) -> out)).toByteArray
        assertThrows[IllegalArgumentException](JsUtxo.fromCbor(twoEntries.toUint8Array))
    }

    test("Utxos decoders read the ledger map, [input, output] pairs, or either") {
        val (a, b) = (TransactionInput(hash, 0), TransactionInput(hash, 1))
        val (x, y) =
            (TransactionOutput(address, Value.ada(1)), TransactionOutput(address, Value.ada(2)))
        val map = Cbor.encode(Map(a -> x, b -> y)).toByteArray
        val pairs = Cbor.encode(Vector(Utxo(a, x), Utxo(b, y), Utxo(a, y))).toByteArray
        assert(Cbor.decode(map).to(using Utxos.mapDecoder).value == Map(a -> x, b -> y))
        // A later pair with the same input replaces an earlier one.
        assert(Cbor.decode(pairs).to(using Utxos.pairsDecoder).value == Map(a -> y, b -> y))
        assert(Cbor.decode(map).to(using Utxos.mapOrPairsDecoder).value == Map(a -> x, b -> y))
        assert(Cbor.decode(pairs).to(using Utxos.mapOrPairsDecoder).value == Map(a -> y, b -> y))
    }

    test("withScriptRef takes { type, script } with the Plutus script in any wrapping") {
        val flat = Array[Byte](1, 1, 0, 0x33, 0x70)
        val single = Cbor.encode(flat).toByteArray
        val double = Cbor.encode(single).toByteArray
        val utxo = JsUtxo.wrap(TransactionInput(hash, 0), TransactionOutput(address, Value.ada(1)))
        val expected = ScriptRef(Script.PlutusV2(ByteString.unsafeFromArray(single)))
        for script <- Seq[js.Any](flat.toUint8Array, Hex.bytesToHex(single), double.toUint8Array) do
            val record = js.Dynamic.literal(`type` = "PlutusV2", script = script)
            assert(utxo.withScriptRef(record).output.scriptRef.contains(expected))

        val native = Timelock.TimeStart(5)
        val record = js.Dynamic.literal(`type` = "Native", script = Hex.bytesToHex(native.toCbor))
        assert(
          utxo.withScriptRef(record).output.scriptRef.contains(ScriptRef(Script.Native(native)))
        )

        for bad <- Seq[js.Any](
              js.Dynamic.literal(`type` = "PlutusV4", script = "00"),
              js.Dynamic.literal(`type` = "Native", script = "zz"),
              "00"
            )
        do
            val e = intercept[js.JavaScriptException](utxo.withScriptRef(bad)).exception
            assert(e.isInstanceOf[js.TypeError], e)
    }

    test("scriptHash ignores the wrapping; dataHash hashes the CBOR as given") {
        val flat = Array[Byte](1, 1, 0, 0x33, 0x70)
        val single = Cbor.encode(flat).toByteArray
        val expected = Script.PlutusV3(ByteString.unsafeFromArray(single)).scriptHash.toHex
        for script <- Seq[js.Any](flat.toUint8Array, Hex.bytesToHex(single)) do
            assert(
              JsHashes.scriptHash(
                js.Dynamic.literal(`type` = "PlutusV3", script = script)
              ) == expected
            )
        val scriptRef =
            JsCbor.encode(ScriptRef(Script.PlutusV3(ByteString.unsafeFromArray(single))))
        assert(JsHashes.scriptHash(scriptRef) == expected)
        val native = Timelock.TimeStart(5)
        val nativeRecord =
            js.Dynamic.literal(`type` = "Native", script = Hex.bytesToHex(native.toCbor))
        assert(JsHashes.scriptHash(nativeRecord) == native.scriptHash.toHex)

        val data: Data = Data.I(42)
        assert(JsHashes.dataHash("182a") == DatumOption.Inline(data).dataHash.toHex)
        // A non-canonical encoding of the same integer hashes differently, as on the ledger.
        assert(JsHashes.dataHash("19002a") != JsHashes.dataHash("182a"))
    }

    test("withDatumHash returns a new handle carrying that hash, leaving the original untouched") {
        val utxo = JsUtxo.wrap(TransactionInput(hash, 0), TransactionOutput(address, Value.ada(1)))
        val hashHex = "1" * 64
        val updated = utxo.withDatumHash(hashHex)
        assert(updated.output.datumOption.contains(DatumOption.Hash(DataHash.fromHex(hashHex))))
        assert(updated.datumHash.toOption.contains(hashHex))
        assert(updated.inlineDatum.toOption.isEmpty)
        assert(utxo.datumHash.toOption.isEmpty)
    }

    test("withInlineDatum returns a new handle carrying the decoded inline datum") {
        val data: Data = Data.I(42)
        val cbor = JsCbor.encode(data)
        val utxo = JsUtxo.wrap(TransactionInput(hash, 0), TransactionOutput(address, Value.ada(1)))
        val updated = utxo.withInlineDatum(cbor)
        assert(updated.output.datumOption.contains(DatumOption.Inline(data)))
        assert(updated.inlineDatum.toOption.isDefined)
        assert(updated.datumHash.toOption.isEmpty)
    }

    test("withScriptRef returns a new handle carrying the decoded reference script") {
        val script = Script.PlutusV3(ByteString.fromHex("00"))
        val scriptRefCbor =
            JsCbor.encode(ScriptRef(script))
        val utxo = JsUtxo.wrap(TransactionInput(hash, 0), TransactionOutput(address, Value.ada(1)))
        val updated = utxo.withScriptRef(scriptRefCbor)
        assert(updated.output.scriptRef.contains(ScriptRef(script)))
        assert(updated.scriptLanguage.toOption.contains("PlutusV3"))
    }

    test("toObject yields own enumerable properties, which the handle does not") {
        val utxo = JsUtxo.wrap(TransactionInput(hash, 3), TransactionOutput(address, Value.ada(7)))
        assert(js.Object.keys(utxo).length == 0, "a handle exposes nothing to spread or toEqual")
        val plain = utxo.toObject()
        assert(
          js.Object.keys(plain).toSet == Set(
            "txHash",
            "outputIndex",
            "address",
            "value",
            "datumHash",
            "inlineDatum",
            "scriptRef",
            "scriptLanguage"
          )
        )
        assert(plain.txHash == utxo.txHash)
        assert(plain.outputIndex == utxo.outputIndex)
        assert(plain.address == utxo.address)
        assert(plain.value.coin.toString == "7000000")
    }
}
