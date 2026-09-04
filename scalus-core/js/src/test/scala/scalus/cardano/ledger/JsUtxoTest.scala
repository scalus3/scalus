package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.uplc.builtin.{ByteString, Data}

import scala.scalajs.js
import scala.scalajs.js.typedarray.{byteArray2Int8Array, Uint8Array}

class JsUtxoTest extends AnyFunSuite {

    private val hash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
    private val address = Address.fromString(
      "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw"
    )

    private def bytesOf(a: Array[Byte]): Uint8Array = new Uint8Array(byteArray2Int8Array(a).buffer)

    // A `js.UndefOr[_]` member access read directly inside `assert(...)` crashes the Scala.js JVM
    // backend ("Cannot emit primitive conversion from Ljava/lang/Object; to
    // Lscala/scalajs/js/$bar;") - the ScalaTest `assert` macro's expression-capturing does not
    // survive it. Verified: `assert(utxo.datumHash.isEmpty)` crashes; routing the same value
    // through this helper first, so the macro only ever sees a plain `Option[A]`, does not. A bare
    // `assert(x.toOption.isEmpty)` was NOT tried and is not known to be safe - use this helper.
    private def optionOf[A](u: js.UndefOr[A]): Option[A] = u.toOption

    test("a wrapped UTxO exposes hex ids and a Value handle") {
        val utxo = JsUtxo.wrap(TransactionInput(hash, 3), TransactionOutput(address, Value.ada(7)))
        assert(utxo.txHash == "0" * 64)
        assert(utxo.outputIndex == 3.0)
        assert(utxo.address == address.encode.get)
        assert(utxo.value.coin.toString == "7000000")
        assert(optionOf(utxo.datumHash).isEmpty)
        assert(optionOf(utxo.inlineDatum).isEmpty)
        assert(optionOf(utxo.scriptRef).isEmpty)
        assert(optionOf(utxo.scriptLanguage).isEmpty)
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

    test("withDatumHash returns a new handle carrying that hash, leaving the original untouched") {
        val utxo = JsUtxo.wrap(TransactionInput(hash, 0), TransactionOutput(address, Value.ada(1)))
        val hashHex = "1" * 64
        val updated = utxo.withDatumHash(hashHex)
        assert(updated.output.datumOption.contains(DatumOption.Hash(DataHash.fromHex(hashHex))))
        assert(optionOf(updated.datumHash).contains(hashHex))
        assert(optionOf(updated.inlineDatum).isEmpty)
        assert(optionOf(utxo.datumHash).isEmpty)
    }

    test("withInlineDatum returns a new handle carrying the decoded inline datum") {
        val data: Data = Data.I(42)
        val cbor = bytesOf(io.bullet.borer.Cbor.encode(data).toByteArray)
        val utxo = JsUtxo.wrap(TransactionInput(hash, 0), TransactionOutput(address, Value.ada(1)))
        val updated = utxo.withInlineDatum(cbor)
        assert(updated.output.datumOption.contains(DatumOption.Inline(data)))
        assert(optionOf(updated.inlineDatum).isDefined)
        assert(optionOf(updated.datumHash).isEmpty)
    }

    test("withScriptRef returns a new handle carrying the decoded reference script") {
        val script = Script.PlutusV3(ByteString.fromHex("00"))
        val scriptRefCbor = bytesOf(io.bullet.borer.Cbor.encode(ScriptRef(script)).toByteArray)
        val utxo = JsUtxo.wrap(TransactionInput(hash, 0), TransactionOutput(address, Value.ada(1)))
        val updated = utxo.withScriptRef(scriptRefCbor)
        assert(updated.output.scriptRef.contains(ScriptRef(script)))
        assert(optionOf(updated.scriptLanguage).contains("PlutusV3"))
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
