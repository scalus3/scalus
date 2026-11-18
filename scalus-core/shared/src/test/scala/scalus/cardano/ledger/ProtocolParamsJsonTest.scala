package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.internal.JsonTree

class ProtocolParamsJsonTest extends AnyFunSuite {

    private val mainnet = CardanoInfo.mainnet.protocolParams

    private val rewardJsonCodecs
        : Seq[(String, ProtocolParams => String, String => ProtocolParams)] =
        Seq(
          ("Blockfrost", ProtocolParams.toBlockfrostJson, ProtocolParams.fromBlockfrostJson),
          ("cardano-cli", ProtocolParams.toCardanoCliJson, ProtocolParams.fromCardanoCliJson)
        )

    for (format, encode, decode) <- rewardJsonCodecs do {
        test(s"$format: reward decimals with denominators through 10^18 still round-trip") {
            val values = (0 to 18).map(scale => UnitInterval(1, BigInt(10).pow(scale).toLong)) ++
                Seq(
                  UnitInterval.zero,
                  UnitInterval.half,
                  UnitInterval(7, 1000000000000000000L),
                  UnitInterval(12345678901234568L, 100000000000000000L),
                  UnitInterval(9999999999999999L, 10000000000000000L),
                  mainnet.monetaryExpansion,
                  mainnet.treasuryCut
                )
            for value <- values do {
                val params =
                    mainnet.copy(monetaryExpansion = value.reduce, treasuryCut = value.reduce)
                assert(decode(encode(params)) == params, s"$format: $value")
            }
        }

        test(s"$format: writes monetary expansion below one as its exact decimal") {
            // spec [DM-21b]. As a Double, the number is the nearest one below 1.0.
            val value = UnitInterval(9999999999999999L, 10000000000000000L)
            val json = encode(mainnet.copy(monetaryExpansion = value))
            assert(json.contains(":0.9999999999999999,"), json)
            assert(decode(json).monetaryExpansion == value)
        }
    }

    /** No decimal of at most 19 places: in lowest terms it is 10^13 / (10^17 + 3). */
    private val noShortDecimal = UnitInterval(100000000000000L, 1000000000000000030L)

    test("Blockfrost: a fraction without a short decimal is written as its nearest double") {
        // spec [DM-21b]: Blockfrost's shape has no exact form for it, and the exact decimal of
        // that double has a reduced denominator beyond a Long, so the reader refuses it.
        val json = ProtocolParams.toBlockfrostJson(mainnet.copy(monetaryExpansion = noShortDecimal))
        assert(ujson.read(json)("rho").num == 0.00009999999999999999)
        val thrown = intercept[IllegalArgumentException](ProtocolParams.fromBlockfrostJson(json))
        assert(
          thrown.getMessage.contains(
            "rho: 0.00009999999999999999: requirement failed: Reduced fraction " +
                "(9999999999999999 / 100000000000000000000) overflows Long"
          ),
          thrown.getMessage
        )

        val oldWriterValue = ujson.read(json)
        oldWriterValue("rho") = ujson.Num(0.0001)
        assert(
          ProtocolParams
              .fromBlockfrostJson(ujson.write(oldWriterValue))
              .monetaryExpansion == UnitInterval(1, 10000)
        )
    }

    test("cardano-cli: a fraction without a short decimal is written exactly and reads back") {
        // spec [DM-21b], as cardano-ledger's ToJSON for a bounded ratio
        val json = ProtocolParams.toCardanoCliJson(mainnet.copy(monetaryExpansion = noShortDecimal))
        assert(
          json.contains(
            "\"monetaryExpansion\":{\"numerator\":10000000000000,\"denominator\":100000000000000003}"
          ),
          json
        )
        assert(
          ProtocolParams.fromCardanoCliJson(json).monetaryExpansion ==
              UnitInterval(10000000000000L, 100000000000000003L)
        )
    }

    test("Blockfrost codec round-trips ProtocolParams (write then read)") {
        val json = ProtocolParams.toBlockfrostJson(mainnet)
        val back = ProtocolParams.fromBlockfrostJson(json)
        assert(back == mainnet)
    }

    test(
      "Blockfrost writer emits canonical language names and array cost models under cost_models_raw"
    ) {
        // `cost_models_raw` is real Blockfrost's array-shaped field; `cost_models` is its
        // deprecated, object-shaped (opName -> cost) field, which the writer does not produce.
        val json = ProtocolParams.toBlockfrostJson(mainnet)
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
        val json = ProtocolParams.toBlockfrostJson(mainnet)
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
        val json = ProtocolParams.toCardanoCliJson(mainnet)
        val back = ProtocolParams.fromCardanoCliJson(json)
        assert(back == mainnet)
    }

    test("cardano-cli codec preserves tiny priceSteps (7.21e-5, not truncated to 7.2e-5)") {
        val json = ProtocolParams.toCardanoCliJson(mainnet)
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
        // spec [DM-20c]
        for (network, params) <- networks do
            val json = ProtocolParams.toCardanoCliJson(params)
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
        val tree = ujson.read(ProtocolParams.toCardanoCliJson(mainnet))
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
        val tree = ujson.read(ProtocolParams.toCardanoCliJson(mainnet))
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

    // Failure messages: anything short of a complete parameter set names the first field it lacks
    // and the format that was expected.

    test("an empty Blockfrost object names the field it could not find") {
        val thrown = intercept[IllegalArgumentException](ProtocolParams.fromBlockfrostJson("{}"))
        val message = thrown.getMessage
        assert(message.contains("collateral_percent"), message)
        assert(message.contains("Blockfrost"), message)
        assert(message != "$", message)
    }

    test("a Blockfrost response missing one field names that field") {
        val json = ProtocolParams.toBlockfrostJson(mainnet)
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

    // spec [DM-20c], [DM-21b]: every number exact, every fraction in lowest terms.

    /** `json` with the value at `path` replaced by the JSON text `value`, through [[JsonTree]],
      * which keeps every number exact.
      */
    private def withValue(json: String, path: List[String], value: String): String = {
        def update(tree: JsonTree, path: List[String]): JsonTree = path match
            case Nil => JsonTree.parse(value)
            case key :: rest =>
                JsonTree.Obj(
                  tree.obj.updated(key, update(tree.get(key).getOrElse(JsonTree.Null), rest))
                )
        JsonTree.render(update(JsonTree.parse(json), path))
    }

    private def cliWith(path: List[String], value: String): String =
        withValue(ProtocolParams.toCardanoCliJson(mainnet), path, value)

    private def blockfrostWithValue(fields: (String, String)*): String =
        fields.foldLeft(ProtocolParams.toBlockfrostJson(mainnet)) { case (json, (field, value)) =>
            withValue(json, List(field), value)
        }

    test("cardano-cli reads the {numerator, denominator} fractions cardano-ledger writes") {
        // eras/conway/impl/golden/pparams.json in cardano-ledger: the numerator is past 2^53
        val json = cliWith(
          List("poolVotingThresholds", "hardForkInitiation"),
          """{"denominator":2000000000000000000,"numerator":694018963415406037}"""
        )
        assert(
          ProtocolParams.fromCardanoCliJson(json).poolVotingThresholds.hardForkInitiation ==
              UnitInterval(694018963415406037L, 2000000000000000000L)
        )
    }

    test("cardano-cli reads a {numerator, denominator} fraction in lowest terms") {
        val json = cliWith(List("treasuryCut"), """{"numerator":2,"denominator":10}""")
        assert(ProtocolParams.fromCardanoCliJson(json).treasuryCut == UnitInterval(1, 5))
    }

    test("cardano-cli: a {numerator, denominator} fraction without a denominator names it") {
        val json = cliWith(List("treasuryCut"), """{"numerator":2}""")
        val thrown = intercept[IllegalArgumentException](ProtocolParams.fromCardanoCliJson(json))
        assert(thrown.getMessage.contains("treasuryCut.denominator"), thrown.getMessage)
    }

    test("cardano-cli: a nested object of the wrong kind names its path") {
        val json = cliWith(List("maxTxExecutionUnits"), "null")
        val thrown = intercept[IllegalArgumentException](ProtocolParams.fromCardanoCliJson(json))
        assert(
          thrown.getMessage.contains("maxTxExecutionUnits: expected an object, got null"),
          thrown.getMessage
        )
    }

    test("cardano-cli round-trips fractions that have no short decimal") {
        val params = mainnet.copy(
          poolPledgeInfluence = NonNegativeInterval(1, 7),
          treasuryCut = UnitInterval(1, 3),
          dRepVotingThresholds =
              mainnet.dRepVotingThresholds.copy(committeeNormal = UnitInterval(2, 3)),
          executionUnitPrices = ExUnitPrices(
            priceMemory = NonNegativeInterval(1, 3000),
            priceSteps = NonNegativeInterval(721, 10000000)
          )
        )
        val json = ProtocolParams.toCardanoCliJson(params)
        assert(json.contains("\"treasuryCut\":{\"numerator\":1,\"denominator\":3}"), json)
        assert(json.contains("\"priceSteps\":0.0000721"), json)
        assert(ProtocolParams.fromCardanoCliJson(json) == params)
    }

    test("cardano-cli writes integers past 2^53 as JSON numbers and reads them back exactly") {
        val big = (1L << 53) + 1
        val params = mainnet.copy(stakePoolDeposit = big, maxTxSize = big)
        val json = ProtocolParams.toCardanoCliJson(params)
        assert(json.contains(s"\"stakePoolDeposit\":$big,"), json)
        assert(ProtocolParams.fromCardanoCliJson(json) == params)
    }

    test("minFeeRefScriptCostPerByte, a fraction in the ledger, reads as its whole part") {
        // cardano-ledger's golden parameters have 23896.4809186040265
        val golden = cliWith(List("minFeeRefScriptCostPerByte"), "23896.4809186040265")
        assert(ProtocolParams.fromCardanoCliJson(golden).minFeeRefScriptCostPerByte == 23896L)
        val ratio =
            cliWith(List("minFeeRefScriptCostPerByte"), """{"numerator":25,"denominator":2}""")
        assert(ProtocolParams.fromCardanoCliJson(ratio).minFeeRefScriptCostPerByte == 12L)
        val blockfrost = blockfrostWithValue("min_fee_ref_script_cost_per_byte" -> "12.5")
        assert(ProtocolParams.fromBlockfrostJson(blockfrost).minFeeRefScriptCostPerByte == 12L)
    }

    test("Blockfrost reads an integer past 2^53 exactly, as a number or as a string") {
        val big = (1L << 53) + 1
        val params = mainnet.copy(stakePoolDeposit = big, maxTxSize = big)
        val json = ProtocolParams.toBlockfrostJson(params)
        assert(json.contains(s"\"max_tx_size\":$big,"), json)
        assert(json.contains(s"\"pool_deposit\":\"$big\","), json)
        assert(ProtocolParams.fromBlockfrostJson(json) == params)
    }

    test("Blockfrost reads a JSON number past Double precision exactly") {
        val json = blockfrostWithValue("tau" -> "0.123456789012345678")
        assert(
          ProtocolParams.fromBlockfrostJson(json).treasuryCut ==
              UnitInterval(61728394506172839L, 500000000000000000L)
        )
    }

    test("Blockfrost reads voting thresholds and prices exactly, in lowest terms") {
        val threshold =
            ProtocolParams.fromBlockfrostJson(blockfrostWithValue("dvt_committee_normal" -> "0.67"))
        assert(threshold.dRepVotingThresholds.committeeNormal == UnitInterval(67, 100))
        val price =
            ProtocolParams.fromBlockfrostJson(blockfrostWithValue("price_step" -> "7.21e-5"))
        assert(price.executionUnitPrices.priceSteps.numerator == 721L)
        assert(price.executionUnitPrices.priceSteps.denominator == 10000000L)
    }

    test("Blockfrost writes a fraction with a long terminating decimal exactly") {
        val tiny = UnitInterval(1, 1L << 60)
        val params = mainnet.copy(
          dRepVotingThresholds = mainnet.dRepVotingThresholds.copy(committeeNormal = tiny)
        )
        // 2^-60 has 60 decimal places; its nearest Double would read back as 8673617379884035/10^34
        val json = ProtocolParams.toBlockfrostJson(params)
        assert(json.contains(":8.67361737988403547205962240695953369140625E-19,"), json)
        assert(ProtocolParams.fromBlockfrostJson(json) == params)
    }

    test("Blockfrost: a price or threshold without Long terms reads rounded to 18 places") {
        // spec [DM-20c]: as the codecs this replaces, which read them at a fixed precision; the
        // writer gives 1/3000 as its nearest Double, 3.333333333333333E-4 = 3333333333333333/10^19
        val params = mainnet.copy(
          executionUnitPrices =
              mainnet.executionUnitPrices.copy(priceMemory = NonNegativeInterval(1, 3000)),
          poolVotingThresholds =
              mainnet.poolVotingThresholds.copy(committeeNormal = UnitInterval(1, 3000))
        )
        val back = ProtocolParams.fromBlockfrostJson(ProtocolParams.toBlockfrostJson(params))
        val rounded = NonNegativeInterval(333333333333333L, 1000000000000000000L)
        assert(back.executionUnitPrices.priceMemory == rounded)
        assert(
          back.poolVotingThresholds.committeeNormal == UnitInterval(
            333333333333333L,
            1000000000000000000L
          )
        )
    }

    test("Blockfrost reads a pre-Conway null voting threshold as zero") {
        val p =
            ProtocolParams.fromBlockfrostJson(blockfrostWithValue("pvt_committee_normal" -> "null"))
        assert(p.poolVotingThresholds.committeeNormal == UnitInterval.zero)
    }

    test("Blockfrost: a null name of a two-name threshold does not hide the other") {
        val json = blockfrostWithValue(
          "pvtpp_security_group" -> "null",
          "pvt_p_p_security_group" -> "0.51"
        )
        val p = ProtocolParams.fromBlockfrostJson(json)
        assert(p.poolVotingThresholds.ppSecurityGroup == UnitInterval(51, 100))
    }

    test("Blockfrost: a null cost_models_raw falls back to cost_models") {
        val objectShaped = JsonTree.Obj(
          scala.collection.immutable.VectorMap
              .from(mainnet.costModels.models.toSeq.sortBy(_._1).map { (languageId, costs) =>
                  Language.fromId(languageId).toString -> JsonTree.Obj(
                    scala.collection.immutable.VectorMap.from(
                      costs.zipWithIndex.map((cost, i) => s"op$i" -> JsonTree.num(cost))
                    )
                  )
              })
        )
        val json = blockfrostWithValue(
          "cost_models_raw" -> "null",
          "cost_models" -> JsonTree.render(objectShaped)
        )
        assert(ProtocolParams.fromBlockfrostJson(json).costModels == mainnet.costModels)
    }

    test("a fractional value for an integer field fails and names the field") {
        val thrown = intercept[IllegalArgumentException](
          ProtocolParams.fromBlockfrostJson(blockfrostWithValue("max_tx_size" -> "16384.5"))
        )
        assert(thrown.getMessage.contains("max_tx_size: 16384.5 is not a whole number"))
    }

    test("an overlong number string fails before it is parsed") {
        val thrown = intercept[IllegalArgumentException](
          ProtocolParams.fromBlockfrostJson(
            blockfrostWithValue("pool_deposit" -> ("\"" + "1" * 100000 + "\""))
          )
        )
        assert(
          thrown.getMessage.contains("pool_deposit: a number of 100000 characters is too long")
        )
    }

    test("malformed JSON fails with the format that was expected") {
        val thrown =
            intercept[IllegalArgumentException](ProtocolParams.fromBlockfrostJson("""{"rho": }"""))
        assert(thrown.getMessage.contains("Blockfrost"), thrown.getMessage)
    }

    test("the readers agree with the deprecated upickle codecs on every network") {
        import LegacyProtocolParamsCodecs.*
        for (network, params) <- networks do
            val blockfrost = ProtocolParams.toBlockfrostJson(params)
            assert(
              reducedThresholds(ProtocolParams.fromBlockfrostJson(blockfrost)) ==
                  reducedThresholds(legacyBlockfrost(blockfrost)),
              network
            )
            val cli = legacyCardanoCliJson(params)
            assert(
              reducedThresholds(ProtocolParams.fromCardanoCliJson(cli)) ==
                  reducedThresholds(legacyCardanoCli(cli)),
              network
            )
            assert(ProtocolParams.fromCardanoCliJson(cli) == params, network)
    }
}
