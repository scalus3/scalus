package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import io.bullet.borer.Cbor
import scalus.cardano.ledger.*
import scalus.uplc.builtin.ByteString

class BlockfrostScriptRefTest extends AnyFunSuite {

    // Single-CBOR bytes of the always-fails script (flat wrapped once as a CBOR bytestring)
    private val singleCbor = ByteString.fromHex("46010000222601")
    private val expectedV2 = Script.PlutusV2(singleCbor)

    test("referenceScriptHash extracts the field when present") {
        val hex = expectedV2.scriptHash.toHex
        val json = ujson.Obj("reference_script_hash" -> ujson.Str(hex))
        assert(
          BlockfrostProvider.referenceScriptHash(json) == Some(ScriptHash.fromHex(hex))
        )
        assert(BlockfrostProvider.referenceScriptHash(ujson.Obj()) == None)
        assert(
          BlockfrostProvider.referenceScriptHash(
            ujson.Obj("reference_script_hash" -> ujson.Null)
          ) == None
        )
    }

    test("buildPlutusScript accepts the script bytes directly") {
        val script =
            BlockfrostProvider.buildPlutusScript("plutusV2", expectedV2.scriptHash, singleCbor)
        assert(script == expectedV2)
    }

    test("buildPlutusScript unwraps an extra CBOR bytestring layer") {
        val wrapped = ByteString.unsafeFromArray(Cbor.encode(singleCbor.bytes).toByteArray)
        val script =
            BlockfrostProvider.buildPlutusScript("plutusV2", expectedV2.scriptHash, wrapped)
        assert(script == expectedV2)
    }

    test("buildPlutusScript rejects bytes that do not hash to the expected hash") {
        val wrongHash = Script.PlutusV3(singleCbor).scriptHash
        assertThrows[RuntimeException](
          BlockfrostProvider.buildPlutusScript("plutusV2", wrongHash, singleCbor)
        )
    }

    test("buildPlutusScript rejects unknown script types") {
        assertThrows[RuntimeException](
          BlockfrostProvider.buildPlutusScript("plutusV9", expectedV2.scriptHash, singleCbor)
        )
    }

    test("buildNativeScript parses Blockfrost timelock JSON and verifies the hash") {
        val keyHash = "c0ffee0123456789abcdef0123456789abcdef0123456789abcdef01"
        val timelock = Timelock.Signature(AddrKeyHash.fromHex(keyHash))
        val expected = Script.Native(timelock)
        val json = ujson.Obj("type" -> "sig", "keyHash" -> keyHash)
        assert(BlockfrostProvider.buildNativeScript(expected.scriptHash, json) == expected)

        val wrongHash = expectedV2.scriptHash
        assertThrows[RuntimeException](BlockfrostProvider.buildNativeScript(wrongHash, json))
    }

    test("parseUtxoItemsWithRefHashes collects reference script hashes per input") {
        val refHash = expectedV2.scriptHash.toHex
        def utxoJson(idx: Int, ref: Option[String]): ujson.Obj = {
            val obj = ujson.Obj(
              "tx_hash" -> "a" * 64,
              "output_index" -> idx,
              "address" -> "addr1q9d34spgg2kdy47n82e7x9pdd6vql6d2engxmpj20jmhuc2047yqd4xnh7u6u5jp4t0q3fkxzckph4tgnzvamlu7k5psuahzcp",
              "amount" -> ujson.Arr(ujson.Obj("unit" -> "lovelace", "quantity" -> "1000000"))
            )
            ref.foreach(r => obj("reference_script_hash") = r)
            obj
        }
        val (utxos, refs) = BlockfrostProvider.parseUtxoItemsWithRefHashes(
          Seq(utxoJson(0, Some(refHash)), utxoJson(1, None))
        )
        assert(utxos.size == 2)
        assert(utxos.values.forall(_.scriptRef.isEmpty))
        assert(refs == Map(Input(TransactionHash.fromHex("a" * 64), 0) -> expectedV2.scriptHash))
    }
}
