package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import upickle.default.write

class ProtocolParamsJsonTest extends AnyFunSuite {

    private val mainnet = CardanoInfo.mainnet.protocolParams

    test("Blockfrost writer rounds monetary expansion below one to the nearest double") {
        val params =
            mainnet.copy(monetaryExpansion = UnitInterval(9999999999999999L, 10000000000000000L))
        val json = write(params)(using ProtocolParams.blockfrostParamsReadWriter)
        assert(ujson.read(json)("rho").num == Math.nextDown(1.0))
        assert(
          ProtocolParams.fromBlockfrostJson(json).monetaryExpansion ==
              UnitInterval(9999999999999999L, 10000000000000000L)
        )
    }

    test("cardano-cli writer rounds monetary expansion below one to the nearest double") {
        val params =
            mainnet.copy(monetaryExpansion = UnitInterval(9999999999999999L, 10000000000000000L))
        val json = write(params)(using ProtocolParams.cardanoCliParamsReadWriter)
        assert(ujson.read(json)("monetaryExpansion").num == Math.nextDown(1.0))
        assert(
          ProtocolParams.fromCardanoCliJson(json).monetaryExpansion ==
              UnitInterval(9999999999999999L, 10000000000000000L)
        )
    }

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

    // spec [DM-18] to [DM-20a]: rho, tau and a0 are exact rationals, read from the decimal.

    private val networks = Seq(
      "mainnet" -> CardanoInfo.mainnet.protocolParams,
      "preprod" -> CardanoInfo.preprod.protocolParams,
      "preview" -> CardanoInfo.preview.protocolParams
    )

    private def assertExactRewardParams(network: String, p: ProtocolParams): Unit = {
        assert(p.monetaryExpansion == UnitInterval(3, 1000), network)
        assert(p.treasuryCut == UnitInterval(1, 5), network)
        // NonNegativeInterval equality reduces both sides, so pin the terms too.
        assert(p.poolPledgeInfluence.numerator == 3L, network)
        assert(p.poolPledgeInfluence.denominator == 10L, network)
    }

    test("CardanoInfo carries rho 3/1000, tau 1/5 and a0 3/10 on every network") {
        // spec [DM-20a]
        // The compile-time read of the Blockfrost resources, then ToExpr.
        for (network, params) <- networks do assertExactRewardParams(network, params)
    }

    test("Blockfrost write-then-read keeps rho, tau and a0 exact") {
        // spec [DM-20a], [DM-21a]
        for (network, params) <- networks do
            val json = ProtocolParams.toBlockfrostJson(params)
            val tree = ujson.read(json)
            // spec [DM-21a]: the writer emits JSON numbers, as Blockfrost does.
            assert(tree("rho") == ujson.Num(0.003), network)
            assert(tree("tau") == ujson.Num(0.2), network)
            assert(tree("a0") == ujson.Num(0.3), network)
            assertExactRewardParams(network, ProtocolParams.fromBlockfrostJson(json))
    }

    test("cardano-cli write-then-read keeps rho, tau and a0 exact") {
        // spec [DM-20b]
        for (network, params) <- networks do
            val json = write(params)(using ProtocolParams.cardanoCliParamsReadWriter)
            val tree = ujson.read(json)
            assert(tree("monetaryExpansion") == ujson.Num(0.003), network)
            assertExactRewardParams(network, ProtocolParams.fromCardanoCliJson(json))
    }

    private def blockfrostWith(field: String, value: ujson.Value): String = {
        val tree = ujson.read(ProtocolParams.toBlockfrostJson(mainnet))
        tree(field) = value
        ujson.write(tree)
    }

    test("Blockfrost rho as the string \"0.003\" and as the number 3e-3 reads 3/1000") {
        val fromString =
            ProtocolParams.fromBlockfrostJson(blockfrostWith("rho", ujson.Str("0.003")))
        assert(fromString.monetaryExpansion == UnitInterval(3, 1000))
        val written = ProtocolParams.toBlockfrostJson(mainnet)
        assert(written.contains("\"rho\":0.003"))
        val fromNumber =
            ProtocolParams.fromBlockfrostJson(written.replace("\"rho\":0.003", "\"rho\":3e-3"))
        assert(fromNumber.monetaryExpansion == UnitInterval(3, 1000))
    }

    test("Blockfrost reads a decimal past Double precision exactly, in lowest terms") {
        // A Double holds 0.123456789012345678 as 0.12345678901234568; the string text is exact.
        val p = ProtocolParams.fromBlockfrostJson(
          blockfrostWith("tau", ujson.Str("0.123456789012345678"))
        )
        assert(p.treasuryCut == UnitInterval(61728394506172839L, 500000000000000000L))
    }

    test("cardano-cli reads a0 given as a JSON string exactly") {
        // spec [DM-20a]
        val tree = ujson.read(write(mainnet)(using ProtocolParams.cardanoCliParamsReadWriter))
        tree("poolPledgeInfluence") = ujson.Str("0.30")
        val p = ProtocolParams.fromCardanoCliJson(ujson.write(tree))
        assert(p.poolPledgeInfluence.numerator == 3L)
        assert(p.poolPledgeInfluence.denominator == 10L)
    }

    test("a term that does not fit a Long fails and names the field") {
        // spec [DM-20a]
        val thrown = intercept[IllegalArgumentException](
          ProtocolParams.fromBlockfrostJson(blockfrostWith("rho", ujson.Str("1e-20")))
        )
        assert(thrown.getMessage.contains("rho"), thrown.getMessage)
    }

    test("a negative a0 fails and names the field") {
        // spec [DM-20a]
        val thrown = intercept[IllegalArgumentException](
          ProtocolParams.fromBlockfrostJson(blockfrostWith("a0", ujson.Num(-0.3)))
        )
        assert(thrown.getMessage.contains("a0: -0.3: "), thrown.getMessage)
    }

    test("cardano-cli: a negative poolPledgeInfluence fails and names the field") {
        // spec [DM-20a]
        val tree = ujson.read(write(mainnet)(using ProtocolParams.cardanoCliParamsReadWriter))
        tree("poolPledgeInfluence") = ujson.Num(-0.3)
        val thrown = intercept[IllegalArgumentException](
          ProtocolParams.fromCardanoCliJson(ujson.write(tree))
        )
        assert(thrown.getMessage.contains("poolPledgeInfluence: -0.3: "), thrown.getMessage)
    }

    test("a rho above 1 fails and names the field") {
        val thrown = intercept[IllegalArgumentException](
          ProtocolParams.fromBlockfrostJson(blockfrostWith("rho", ujson.Num(1.5)))
        )
        assert(thrown.getMessage.contains("rho"), thrown.getMessage)
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
