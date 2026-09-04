package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import upickle.default.write

class ProtocolParamsJsonTest extends AnyFunSuite {

    private val mainnet = CardanoInfo.mainnet.protocolParams

    test("Blockfrost codec round-trips ProtocolParams (write then read)") {
        val json = write(mainnet)(using ProtocolParams.blockfrostParamsReadWriter)
        val back = ProtocolParams.fromBlockfrostJson(json)
        assert(back == mainnet)
    }

    test(
      "Blockfrost writer emits canonical language names and array cost models under cost_models_raw"
    ) {
        // `cost_models_raw` is real Blockfrost's array-shaped field; `cost_models` is its
        // deprecated, object-shaped (opName -> cost) field, which the writer does not produce.
        val json = write(mainnet)(using ProtocolParams.blockfrostParamsReadWriter)
        val tree = ujson.read(json)
        assert(!tree.obj.contains("cost_models"))
        val obj = tree.obj("cost_models_raw").obj
        assert(obj.keySet == Set("PlutusV1", "PlutusV2", "PlutusV3"))
        assert(obj("PlutusV3").arrOpt.isDefined)
        // and the array values round-trip back to the original cost model
        val back = ProtocolParams.fromBlockfrostJson(json)
        assert(back.costModels == mainnet.costModels)
    }

    test(
      "Blockfrost reader falls back to object-shaped cost_models when cost_models_raw is absent"
    ) {
        // Sources that only send the deprecated field (e.g. Yaci DevKit) must still parse.
        val json = write(mainnet)(using ProtocolParams.blockfrostParamsReadWriter)
        val tree = ujson.read(json)
        val objShaped = tree.obj("cost_models_raw").obj.map { case (lang, arr) =>
            lang -> ujson.Obj.from(
              arr.arr.zipWithIndex.map { case (v, i) => s"op$i" -> v }
            )
        }
        tree.obj.remove("cost_models_raw")
        tree("cost_models") = ujson.Obj.from(objShaped)
        val back = ProtocolParams.fromBlockfrostJson(ujson.write(tree))
        assert(back.costModels == mainnet.costModels)
    }

    test("cardano-cli codec round-trips ProtocolParams (write then read)") {
        val json = write(mainnet)(using ProtocolParams.cardanoCliParamsReadWriter)
        val back = ProtocolParams.fromCardanoCliJson(json)
        assert(back == mainnet)
    }

    test("cardano-cli codec preserves tiny priceSteps (7.21e-5, not truncated to 7.2e-5)") {
        val json = write(mainnet)(using ProtocolParams.cardanoCliParamsReadWriter)
        val back = ProtocolParams.fromCardanoCliJson(json)
        assert(back.executionUnitPrices.priceSteps == mainnet.executionUnitPrices.priceSteps)
        assert(back.executionUnitPrices.priceSteps.toDouble == 7.21e-5)
    }

    // Failure messages. Both codecs read fields straight off a `ujson.Value`, so anything short of
    // a complete parameter set fails deep inside upickle, whose own message is the JSON path.

    test("an empty Blockfrost object names the field it could not find") {
        val thrown = intercept[IllegalArgumentException](ProtocolParams.fromBlockfrostJson("{}"))
        val message = thrown.getMessage
        assert(message.contains("collateral_percent"), message)
        assert(message.contains("Blockfrost"), message)
        assert(message != "$", message)
    }

    test("a Blockfrost response missing one field names that field") {
        val json = write(mainnet)(using ProtocolParams.blockfrostParamsReadWriter)
        val tree = ujson.read(json)
        tree.obj.remove("min_fee_ref_script_cost_per_byte")
        val thrown =
            intercept[IllegalArgumentException](
              ProtocolParams.fromBlockfrostJson(ujson.write(tree))
            )
        assert(thrown.getMessage.contains("min_fee_ref_script_cost_per_byte"), thrown.getMessage)
    }

    test("an empty cardano-cli object says which format was expected") {
        val thrown = intercept[IllegalArgumentException](ProtocolParams.fromCardanoCliJson("{}"))
        assert(thrown.getMessage.contains("cardano-cli"), thrown.getMessage)
    }
}
