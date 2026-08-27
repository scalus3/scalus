package scalus.examples.cape

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.v1.IntervalBoundType
import scalus.cardano.onchain.plutus.prelude.{List, Option}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.{fromData, toData}

/** Unit tests for the shared CAPE v3 loader (`CapeTestSuite`) against a small inline JSON fixture.
  *
  * These tests exercise the loader's parsing rules and the `ScriptContextBuilder` semantics that
  * must mirror `lib/Cape/ScriptContextBuilder.hs` / `lib/Cape/Tests.hs` from the upstream UPLC-CAPE
  * repository exactly (prepend ordering, default datum/redeemer, empty-bytes pubkey for non-own
  * inputs, inclusive interval bounds, own-input updates the SpendingScript TxOutRef).
  */
class CapeTestSuiteTest extends AnyFunSuite {

    private val suiteJson = """{
      "version": "3.0.0",
      "description": "test",
      "data_structures": {
        "pk": {"type": "builtin_data", "value": "B #aaaa"},
        "datum_text": {"type": "builtin_data", "value": "Constr 0 [Constr 1 [], I 1000]"},
        "datum_json": {"type": "builtin_data",
          "value": {"constructor": 0, "fields": [{"constructor": 1, "fields": []}, {"int": 1000}]}}
      },
      "measurements": [
        {"name": "m1", "description": "d", "expected": {"type": "value", "content": "(con unit ())"},
         "inputs": [{"type": "script_context", "script_context": {"baseline": "spending", "patches": [
            {"op": "set_redeemer", "redeemer": "I 1"},
            {"op": "set_script_datum", "datum": "@datum_text"},
            {"op": "add_signature", "pubkey_hash": "@pk"},
            {"op": "add_signature", "pubkey_hash": "#bbbb"},
            {"op": "set_valid_range", "from_time": 11},
            {"op": "add_input_utxo", "utxo_ref": "3333333333333333333333333333333333333333333333333333333333333333:0",
             "value": {"lovelace": 2000000, "assets": [{"currency_symbol": "#dddd", "token_name": "#76657374", "quantity": 1000}]},
             "is_own_input": true, "datum": "@datum_text"},
            {"op": "add_input_utxo", "utxo_ref": "4444444444444444444444444444444444444444444444444444444444444444:1",
             "value": {"lovelace": 5}, "is_own_input": false}
         ]}}]}
      ],
      "checks": [
        {"name": "c1", "description": "d", "expected": {"type": "error"},
         "inputs": [{"type": "builtin_data", "value": "I 42"}]}
      ]
    }"""

    private val suite = CapeTestSuite.fromString(suiteJson)

    /** Loads `json`, takes the first case's first (sole) `script_context` input, and decodes it
      * back to a `ScriptContext`.
      */
    private def firstContext(json: String): ScriptContext =
        CapeTestSuite
            .fromString(json)
            .cases
            .head
            .inputs
            .head
            .asInstanceOf[CapeInput.Ctx]
            .data
            .to[ScriptContext]

    test("both Data formats parse to the same value") {
        assert(suite.dataStructure("datum_text") == suite.dataStructure("datum_json"))
    }

    test("measurements and checks are loaded with flags") {
        assert(suite.cases.map(_.name) == Seq("m1", "c1"))
        assert(suite.cases.head.isMeasurement && !suite.cases.last.isMeasurement)
        assert(suite.cases.last.expectError)
    }

    test("script context matches upstream builder semantics") {
        val sc = suite.cases.head.inputs.head.asInstanceOf[CapeInput.Ctx].data.to[ScriptContext]
        // redeemer replaced
        assert(sc.redeemer == Data.I(1))
        // datum set via set_script_datum
        assert(
          sc.scriptInfo == ScriptInfo.SpendingScript(
            TxOutRef(
              TxId(hex"3333333333333333333333333333333333333333333333333333333333333333"),
              0
            ),
            Option.Some(suite.dataStructure("datum_text"))
          )
        )
        // signatures PREPENDED: last-added first
        assert(
          fromData[List[PubKeyHash]](sc.txInfo.signatories.toData).toScalaList
              .map(_.hash) == scala.List(hex"bbbb", hex"aaaa")
        )
        // inputs PREPENDED; non-own input first, own second
        val ins = sc.txInfo.inputs.toScalaList
        assert(ins.size == 2)
        assert(
          ins.head.resolved.address.credential ==
              Credential.PubKeyCredential(PubKeyHash(ByteString.empty))
        )
        assert(
          ins(1).resolved.address.credential == Credential.ScriptCredential(
            hex"1111111111111111111111111111111111111111111111111111111111"
          )
        )
        // own input value carries the asset
        assert(ins(1).resolved.value.quantityOf(hex"dddd", hex"76657374") == BigInt(1000))
        // valid range: [11, +inf), both inclusive
        assert(sc.txInfo.validRange.from.boundType == IntervalBoundType.Finite(BigInt(11)))
        assert(sc.txInfo.validRange.from.isInclusive)
        assert(sc.txInfo.validRange.to.boundType == IntervalBoundType.PosInf)
        assert(sc.txInfo.validRange.to.isInclusive)
    }

    test("spending baseline defaults match upstream") {
        val minimal = """{"version":"3.0.0","description":"","data_structures":{},
          "measurements":[{"name":"m","description":"","expected":{"type":"error"},
            "inputs":[{"type":"script_context","script_context":{"baseline":"spending","patches":[]}}]}],
          "checks":[]}"""
        val sc = CapeTestSuite
            .fromString(minimal)
            .cases
            .head
            .inputs
            .head
            .asInstanceOf[CapeInput.Ctx]
            .data
            .to[ScriptContext]
        assert(sc.redeemer == ().toData)
        assert(
          sc.scriptInfo == ScriptInfo.SpendingScript(
            TxOutRef(
              TxId(hex"0000000000000000000000000000000000000000000000000000000000000000"),
              0
            ),
            Option.Some(().toData)
          )
        )
        assert(
          sc.txInfo.inputs.isEmpty && sc.txInfo.outputs.isEmpty && sc.txInfo.signatories.isEmpty
        )
    }

    test(
      "add_output_utxo prepends and resolves script_hash as bare hex, #hex, and @ref (both address types, with/without datum)"
    ) {
        val json = """{"version":"3.0.0","description":"","data_structures":{
          "out_hash_ref": {"type": "builtin_data", "value": "B #ffff"},
          "datum_out": {"type": "builtin_data", "value": "I 7"}
        },
        "measurements":[{"name":"m","description":"","expected":{"type":"error"},
          "inputs":[{"type":"script_context","script_context":{"baseline":"spending","patches":[
            {"op":"add_output_utxo","address":{"type":"pubkey","pubkey_hash":"#cccc"},"value":{"lovelace":1}},
            {"op":"add_output_utxo","address":{"type":"script","script_hash":"eeee"},"value":{"lovelace":2}},
            {"op":"add_output_utxo","address":{"type":"script","script_hash":"#eeff"},"value":{"lovelace":3},"datum":"@datum_out"},
            {"op":"add_output_utxo","address":{"type":"script","script_hash":"@out_hash_ref"},"value":{"lovelace":4}}
          ]}}]}],
        "checks":[]}"""
        val outs = firstContext(json).txInfo.outputs.toScalaList
        // PREPEND: final order is the reverse of patch application order
        assert(outs.size == 4)
        assert(outs.head.address.credential == Credential.ScriptCredential(hex"ffff")) // @ref
        assert(outs.head.datum == OutputDatum.NoOutputDatum)
        assert(outs(1).address.credential == Credential.ScriptCredential(hex"eeff")) // #hex
        assert(outs(1).datum == OutputDatum.OutputDatum(Data.I(7)))
        assert(outs(2).address.credential == Credential.ScriptCredential(hex"eeee")) // bare hex
        assert(outs(2).datum == OutputDatum.NoOutputDatum)
        assert(
          outs(3).address.credential == Credential.PubKeyCredential(PubKeyHash(hex"cccc"))
        )
        assert(outs(3).datum == OutputDatum.NoOutputDatum)
    }

    test("remove_output_utxo deletes by index") {
        val json = """{"version":"3.0.0","description":"","data_structures":{},
        "measurements":[{"name":"m","description":"","expected":{"type":"error"},
          "inputs":[{"type":"script_context","script_context":{"baseline":"spending","patches":[
            {"op":"add_output_utxo","address":{"type":"pubkey","pubkey_hash":"#aaaa"},"value":{"lovelace":1}},
            {"op":"add_output_utxo","address":{"type":"pubkey","pubkey_hash":"#bbbb"},"value":{"lovelace":2}},
            {"op":"add_output_utxo","address":{"type":"pubkey","pubkey_hash":"#cccc"},"value":{"lovelace":3}},
            {"op":"remove_output_utxo","index":1}
          ]}}]}],
        "checks":[]}"""
        val outs = firstContext(json).txInfo.outputs.toScalaList
        // before removal (prepend order): [cccc, bbbb, aaaa]; index 1 deletes bbbb
        assert(
          outs.map(_.address.credential) == scala.List(
            Credential.PubKeyCredential(PubKeyHash(hex"cccc")),
            Credential.PubKeyCredential(PubKeyHash(hex"aaaa"))
          )
        )
    }

    test("remove_signature filters by pkh") {
        val json = """{"version":"3.0.0","description":"","data_structures":{},
        "measurements":[{"name":"m","description":"","expected":{"type":"error"},
          "inputs":[{"type":"script_context","script_context":{"baseline":"spending","patches":[
            {"op":"add_signature","pubkey_hash":"#aaaa"},
            {"op":"add_signature","pubkey_hash":"#bbbb"},
            {"op":"add_signature","pubkey_hash":"#cccc"},
            {"op":"remove_signature","pubkey_hash":"#bbbb"}
          ]}}]}],
        "checks":[]}"""
        val sc = firstContext(json)
        assert(
          fromData[List[PubKeyHash]](sc.txInfo.signatories.toData).toScalaList
              .map(_.hash) == scala.List(hex"cccc", hex"aaaa")
        )
    }

    test("@ref baseline composes with outer patches") {
        val json = """{"version":"3.0.0","description":"","data_structures":{
          "datum_base": {"type": "builtin_data", "value": "I 1"},
          "datum_outer": {"type": "builtin_data", "value": "I 2"},
          "base_ctx": {"type": "script_context", "script_context": {"baseline": "spending", "patches": [
            {"op": "set_script_datum", "datum": "@datum_base"},
            {"op": "add_signature", "pubkey_hash": "#1111"}
          ]}}
        },
        "measurements":[{"name":"m","description":"","expected":{"type":"error"},
          "inputs":[{"type":"script_context","script_context":{"baseline":"@base_ctx","patches":[
            {"op":"add_signature","pubkey_hash":"#2222"},
            {"op":"set_script_datum","datum":"@datum_outer"}
          ]}}]}],
        "checks":[]}"""
        val sc = firstContext(json)
        // outer patches apply ON TOP of the @ref baseline's own patches: signature prepended in
        // front of the baseline's signature, datum overridden by the outer patch (distinct refs
        // from the baseline's datum, so cross-wiring the two would be caught).
        assert(
          fromData[List[PubKeyHash]](sc.txInfo.signatories.toData).toScalaList
              .map(_.hash) == scala.List(hex"2222", hex"1111")
        )
        assert(
          sc.scriptInfo == ScriptInfo.SpendingScript(
            TxOutRef(
              TxId(hex"0000000000000000000000000000000000000000000000000000000000000000"),
              0
            ),
            Option.Some(Data.I(2))
          )
        )
    }
}
