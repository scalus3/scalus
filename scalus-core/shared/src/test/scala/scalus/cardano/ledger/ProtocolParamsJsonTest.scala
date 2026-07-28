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

    test("Blockfrost writer emits canonical language names and array cost models") {
        val json = write(mainnet)(using ProtocolParams.blockfrostParamsReadWriter)
        val obj = ujson.read(json).obj("cost_models").obj
        assert(obj.keySet == Set("PlutusV1", "PlutusV2", "PlutusV3"))
        assert(obj("PlutusV3").arrOpt.isDefined)
        // and the array values round-trip back to the original cost model
        val back = ProtocolParams.fromBlockfrostJson(json)
        assert(back.costModels == mainnet.costModels)
    }

    test("Blockfrost reader still accepts object-shaped cost model values") {
        // Real Blockfrost `cost_models` values are objects (opName -> cost), not arrays.
        val json = write(mainnet)(using ProtocolParams.blockfrostParamsReadWriter)
        val tree = ujson.read(json)
        val objShaped = tree.obj("cost_models").obj.map { case (lang, arr) =>
            lang -> ujson.Obj.from(
              arr.arr.zipWithIndex.map { case (v, i) => s"op$i" -> v }
            )
        }
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
}
