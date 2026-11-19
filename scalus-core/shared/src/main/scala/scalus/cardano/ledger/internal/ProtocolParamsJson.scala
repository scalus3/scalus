package scalus.cardano.ledger.internal

import scalus.cardano.ledger.{CostModels, DRepVotingThresholds, ExUnitPrices, ExUnits, Language, NonNegativeInterval, PoolVotingThresholds, ProtocolParams, ProtocolVersion, UnitInterval}
import scalus.cardano.ledger.internal.JsonTree.{num, obj, Arr, Null, Num, Obj, Str}

import scala.collection.immutable.VectorMap

/** The Blockfrost and `cardano-cli` JSON shapes of [[ProtocolParams]], read and written through
  * [[JsonTree]] so that every number is exact. spec [DM-20c], [DM-21b], D129
  *
  * A fraction reads in lowest terms from a JSON number, a decimal string, or the
  * `{"numerator": n, "denominator": d}` object that cardano-ledger writes for a fraction with no
  * short decimal. Integers read from a JSON number or a decimal string.
  */
private[ledger] object ProtocolParamsJson {

    /** cardano-ledger's `maxDecimalsWord64`: its writer gives a fraction as a decimal of at most
      * this many places, and its reader refuses a decimal with more (`BaseTypes.hs`).
      */
    private val MaxCliDecimalPlaces = 19

    /** The places a voting threshold or price is rounded to when its exact value has no `Long`
      * terms: `10^18` is the largest power of ten a `Long` holds.
      */
    private val RoundedPlaces = 18

    /** A decimal whose scale is beyond this cannot reduce to `Long` terms. The bound keeps a
      * decimal string such as `"1e-999999999"` from building a billion-digit power of ten.
      */
    private val MaxDecimalScale = 6178

    /** The longest decimal or integer string read. A JSON number is bounded at 308 digits by
      * [[JsonTree]]; this keeps a string from feeding a superlinear `BigInteger` parse.
      */
    private val MaxNumberText = 400

    // ---------------------------------------------------------------------------------------
    // Values

    /** The exact fraction, in lowest terms, of `decimal`. spec [DM-20a] */
    def fractionOf(field: String, decimal: BigDecimal): NonNegativeInterval = {
        val (num, den) = terms(field, decimal)
        reduced(field, decimal.toString, num, den)
    }

    /** `r` as a [[UnitInterval]], failing with the field name when it is above one. */
    def toUnitInterval(field: String, r: NonNegativeInterval): UnitInterval = {
        require(
          r.numerator <= r.denominator,
          s"$field: ${r.numerator}/${r.denominator} is not in the unit interval [0, 1]"
        )
        UnitInterval(r.numerator, r.denominator)
    }

    /** `decimal` as `numerator / 10^scale`, not yet reduced. */
    private def terms(field: String, decimal: BigDecimal): (BigInt, BigInt) = {
        val scale = decimal.scale
        if math.abs(scale) > MaxDecimalScale then
            throw new IllegalArgumentException(s"$field: $decimal: too many decimal places")
        val unscaled = BigInt(decimal.bigDecimal.unscaledValue)
        if scale >= 0 then (unscaled, BigInt(10).pow(scale))
        else (unscaled * BigInt(10).pow(-scale), BigInt(1))
    }

    private def reduced(
        field: String,
        shown: String,
        num: BigInt,
        den: BigInt
    ): NonNegativeInterval = {
        // no cause: rootCauseMessage reports the deepest message, which has no field
        try NonNegativeInterval.reduceBigInt(num, den)
        catch
            case e: IllegalArgumentException =>
                throw new IllegalArgumentException(s"$field: $shown: ${e.getMessage}")
    }

    /** An exact fraction from a JSON number, a decimal string, or a `{numerator, denominator}`
      * object.
      */
    private def fraction(field: String, value: JsonTree): NonNegativeInterval = value match
        case Num(_) | Str(_) => fractionOf(field, decimal(field, value))
        case Obj(fields) =>
            def term(name: String) = integer(
              s"$field.$name",
              fields.getOrElse(
                name,
                throw new NoSuchElementException(s"key not found: $field.$name")
              )
            )
            val n = term("numerator")
            val d = term("denominator")
            reduced(field, s"$n/$d", n, d)
        case other =>
            throw new IllegalArgumentException(
              s"$field: expected a number, a decimal string or a {numerator, denominator} " +
                  s"object, got ${other.kind}"
            )

    /** As [[fraction]], but a decimal whose exact value has no `Long` terms reads rounded to
      * [[RoundedPlaces]] places instead of failing. For the voting thresholds and the prices, which
      * the codecs this replaces read at a fixed precision: a writer with only a `Double` for a
      * fraction without a short decimal, as Blockfrost's, emits values such as
      * `3.333333333333333E-4`, whose exact terms are `3333333333333333 / 10^19`. spec [DM-20c]
      */
    private def roundedFraction(field: String, value: JsonTree): NonNegativeInterval =
        value match
            case Num(_) | Str(_) =>
                val exact = decimal(field, value)
                if exact.signum > 0 && exact.scale > RoundedPlaces && !hasLongTerms(field, exact)
                then
                    fractionOf(
                      field,
                      exact.setScale(RoundedPlaces, BigDecimal.RoundingMode.HALF_EVEN)
                    )
                else fractionOf(field, exact)
            case _ => fraction(field, value)

    private def hasLongTerms(field: String, decimal: BigDecimal): Boolean = {
        val (num, den) = terms(field, decimal)
        val g = num.gcd(den)
        (num / g).isValidLong && (den / g).isValidLong
    }

    private def unitInterval(field: String, value: JsonTree): UnitInterval =
        toUnitInterval(field, fraction(field, value))

    private def roundedUnitInterval(field: String, value: JsonTree): UnitInterval =
        toUnitInterval(field, roundedFraction(field, value))

    /** The decimal of a JSON number or decimal string. */
    private def decimal(field: String, value: JsonTree): BigDecimal = value match
        case Num(n) => n
        case Str(text) =>
            try BigDecimal(new java.math.BigDecimal(boundedText(field, text)))
            catch
                case _: NumberFormatException =>
                    throw new IllegalArgumentException(s"$field: \"$text\" is not a decimal")
        case other =>
            throw new IllegalArgumentException(s"$field: expected a number, got ${other.kind}")

    private def boundedText(field: String, text: String): String = {
        if text.length > MaxNumberText then
            throw new IllegalArgumentException(
              s"$field: a number of ${text.length} characters is too long"
            )
        text
    }

    private def integer(field: String, value: JsonTree): BigInt = value match
        case Num(n) =>
            if math.abs(n.scale) > MaxDecimalScale then
                throw new IllegalArgumentException(s"$field: $n is not a whole number")
            n.toBigIntExact.getOrElse(
              throw new IllegalArgumentException(s"$field: $n is not a whole number")
            )
        case Str(text) =>
            try BigInt(boundedText(field, text))
            catch
                case _: NumberFormatException =>
                    throw new IllegalArgumentException(s"$field: \"$text\" is not a whole number")
        case other =>
            throw new IllegalArgumentException(
              s"$field: expected a whole number, got ${other.kind}"
            )

    private def long(field: String, value: JsonTree): Long = {
        val n = integer(field, value)
        if n.isValidLong then n.toLong
        else throw new IllegalArgumentException(s"$field: $n does not fit a Long")
    }

    private def int(field: String, value: JsonTree): Int = {
        val n = integer(field, value)
        if n.isValidInt then n.toInt
        else throw new IllegalArgumentException(s"$field: $n does not fit an Int")
    }

    /** `minFeeRefScriptCostPerByte`, a fraction in the ledger and a `Long` here: its whole part, as
      * the reader this replaces truncated it. cardano-ledger's golden parameters have
      * `23896.4809186040265`.
      */
    private def wholePart(field: String, value: JsonTree): Long =
        roundedFraction(field, value).floor

    /** The fields of the object `value`, which is at `path`. */
    private def fieldsAt(path: String, value: JsonTree): VectorMap[String, JsonTree] =
        value match
            case Obj(fields) => fields
            case other =>
                throw new IllegalArgumentException(
                  s"$path: expected an object, got ${other.kind}"
                )

    /** The value of `key` in the object `json`, which is at `path`. */
    private def member(json: JsonTree, path: String, key: String): JsonTree =
        fieldsAt(path, json).getOrElse(
          key,
          throw new NoSuchElementException(s"key not found: $path.$key")
        )

    /** The id of the Plutus language named `name` ("PlutusV1", ...), at `path`. */
    private def languageId(path: String, name: String): Int = {
        // no cause: rootCauseMessage reports the deepest message, which has no path
        try Language.valueOf(name).languageId
        catch
            case _: IllegalArgumentException =>
                throw new IllegalArgumentException(s"$path: unknown Plutus language \"$name\"")
    }

    private def termsOf(r: UnitInterval | NonNegativeInterval): NonNegativeInterval = r match
        case u: UnitInterval        => NonNegativeInterval.reduce(u.numerator, u.denominator)
        case n: NonNegativeInterval => NonNegativeInterval.reduce(n.numerator, n.denominator)

    /** The decimal of `r`, given in lowest terms, if it has one of at most `maxPlaces` places: its
      * denominator is `2^a * 5^b`, and the places are `max(a, b)`.
      */
    private def decimalOf(r: NonNegativeInterval, maxPlaces: Int): Option[BigDecimal] = {
        var rest = r.denominator
        var twos = 0
        while rest % 2 == 0 do
            rest /= 2
            twos += 1
        var fives = 0
        while rest % 5 == 0 do
            rest /= 5
            fives += 1
        val places = math.max(twos, fives)
        if rest != 1 || places > maxPlaces then None
        else
            val unscaled = BigInt(r.numerator) * BigInt(2).pow(places - twos) *
                BigInt(5).pow(places - fives)
            Some(BigDecimal(unscaled, places))
    }

    // ---------------------------------------------------------------------------------------
    // Blockfrost: GET /epochs/{n}/parameters

    /** The exact decimal when the fraction has one. Blockfrost's shape has no exact form for any
      * other fraction, so that is written as its nearest `Double`, and reads back approximately. No
      * Cardano network carries one.
      */
    private def blockfrostFraction(fraction: UnitInterval | NonNegativeInterval): JsonTree = {
        val r = termsOf(fraction)
        Num(
          decimalOf(r, Int.MaxValue)
              .getOrElse(BigDecimal.decimal(RationalToDouble(r.numerator, r.denominator)))
        )
    }

    /** A `Long` field that may be a JSON number or a decimal string. */
    private def longAt(json: JsonTree, key: String): Long = long(key, json(key))

    /** As [[longAt]], but `null` or any other non-numeric value gives `default`. The key must be
      * present.
      */
    private def longOr(json: JsonTree, key: String, default: Long): Long = json(key) match
        case value @ (Num(_) | Str(_)) => long(key, value)
        case _                         => default

    /** A voting threshold: the key must be present, and `null` (pre-Conway) gives zero. */
    private def threshold(json: JsonTree, key: String): UnitInterval = json(key) match
        case Null  => UnitInterval.zero
        case value => roundedUnitInterval(key, value)

    /** A voting threshold Blockfrost has under two names: the first one with a value that is not
      * `null`. Absent or `null` under both gives zero.
      */
    private def threshold(json: JsonTree, key: String, alias: String): UnitInterval = {
        Seq(key, alias).flatMap(name => json.get(name).map(name -> _)).find(_._2 != Null) match
            case Some((name, value)) => roundedUnitInterval(name, value)
            case None                => UnitInterval.zero
    }

    def readBlockfrost(json: JsonTree): ProtocolParams = {
        // Local defs, not vals: the fields read in order, so a parameter set that lacks several
        // names the first one.
        def costModels(): CostModels = {
            // Prefer `cost_models_raw` (the array-shaped field real Blockfrost, and this codec's
            // own writer, emit) and fall back to the deprecated, object-shaped `cost_models` for
            // sources (Yaci DevKit, older fixtures) that only send that key, or send
            // `cost_models_raw: null`.
            val (key, models) = json.get("cost_models_raw").filter(_ != Null) match
                case Some(raw) => ("cost_models_raw", raw)
                case None      => ("cost_models", json("cost_models"))
            CostModels(
              fieldsAt(key, models).map { (language, costs) =>
                  // Arrays under `cost_models_raw`, objects (opName -> cost) under
                  // `cost_models`. Accept both.
                  val path = s"$key.$language"
                  val values = costs.arrOpt.getOrElse(fieldsAt(path, costs).values.toVector)
                  languageId(path, language) -> values.map(long(path, _)).toIndexedSeq
              }.toMap
            )
        }
        // As `longOr`: the key must be present, and `null` (pre-Conway) gives zero.
        def minFeeRefScriptCostPerByte(): Long = json("min_fee_ref_script_cost_per_byte") match
            case value @ (Num(_) | Str(_)) => wholePart("min_fee_ref_script_cost_per_byte", value)
            case _                         => 0L
        ProtocolParams(
          collateralPercentage = longAt(json, "collateral_percent"),
          committeeMaxTermLength = longOr(json, "committee_max_term_length", 0L),
          committeeMinSize = longOr(json, "committee_min_size", 0L),
          costModels = costModels(),
          dRepActivity = longOr(json, "drep_activity", 0L),
          dRepDeposit = longOr(json, "drep_deposit", 0L),
          dRepVotingThresholds = DRepVotingThresholds(
            motionNoConfidence = threshold(json, "dvt_motion_no_confidence"),
            committeeNormal = threshold(json, "dvt_committee_normal"),
            committeeNoConfidence = threshold(json, "dvt_committee_no_confidence"),
            updateToConstitution = threshold(json, "dvt_update_to_constitution"),
            hardForkInitiation = threshold(json, "dvt_hard_fork_initiation"),
            ppNetworkGroup = threshold(json, "dvt_p_p_network_group", "dvt_ppnetwork_group"),
            ppEconomicGroup = threshold(json, "dvt_p_p_economic_group", "dvt_ppeconomic_group"),
            ppTechnicalGroup = threshold(json, "dvt_p_p_technical_group", "dvt_pptechnical_group"),
            ppGovGroup = threshold(json, "dvt_p_p_gov_group", "dvt_ppgov_group"),
            treasuryWithdrawal = threshold(json, "dvt_treasury_withdrawal")
          ),
          executionUnitPrices = ExUnitPrices(
            priceMemory = roundedFraction("price_mem", json("price_mem")),
            priceSteps = roundedFraction("price_step", json("price_step"))
          ),
          govActionDeposit = longOr(json, "gov_action_deposit", 0L),
          govActionLifetime = longOr(json, "gov_action_lifetime", 0L),
          maxBlockBodySize = longAt(json, "max_block_size"),
          maxBlockExecutionUnits = ExUnits(
            memory = longAt(json, "max_block_ex_mem"),
            steps = longAt(json, "max_block_ex_steps")
          ),
          maxBlockHeaderSize = longAt(json, "max_block_header_size"),
          maxCollateralInputs = longAt(json, "max_collateral_inputs"),
          maxTxExecutionUnits = ExUnits(
            memory = longAt(json, "max_tx_ex_mem"),
            steps = longAt(json, "max_tx_ex_steps")
          ),
          maxTxSize = longAt(json, "max_tx_size"),
          maxValueSize = longAt(json, "max_val_size"),
          minFeeRefScriptCostPerByte = minFeeRefScriptCostPerByte(),
          minPoolCost = longAt(json, "min_pool_cost"),
          monetaryExpansion = unitInterval("rho", json("rho")),
          poolPledgeInfluence = fraction("a0", json("a0")),
          poolRetireMaxEpoch = longAt(json, "e_max"),
          poolVotingThresholds = PoolVotingThresholds(
            motionNoConfidence = threshold(json, "pvt_motion_no_confidence"),
            committeeNormal = threshold(json, "pvt_committee_normal"),
            committeeNoConfidence = threshold(json, "pvt_committee_no_confidence"),
            hardForkInitiation = threshold(json, "pvt_hard_fork_initiation"),
            ppSecurityGroup = threshold(json, "pvt_p_p_security_group", "pvtpp_security_group")
          ),
          protocolVersion = ProtocolVersion(
            major = int("protocol_major_ver", json("protocol_major_ver")),
            minor = int("protocol_minor_ver", json("protocol_minor_ver"))
          ),
          stakeAddressDeposit = longAt(json, "key_deposit"),
          stakePoolDeposit = longAt(json, "pool_deposit"),
          stakePoolTargetNum = longAt(json, "n_opt"),
          treasuryCut = unitInterval("tau", json("tau")),
          txFeeFixed = longAt(json, "min_fee_b"),
          txFeePerByte = longAt(json, "min_fee_a"),
          utxoCostPerByte = longAt(json, "coins_per_utxo_size")
        )
    }

    /** Blockfrost's schema types some fields as JSON numbers and others, the ones that can exceed
      * 2^53, as decimal strings; `ProtocolParamsTest` pins each field's JSON type.
      */
    def writeBlockfrost(params: ProtocolParams): JsonTree = {
        val dvt = params.dRepVotingThresholds
        val pvt = params.poolVotingThresholds
        def str(value: Long): JsonTree = Str(value.toString)
        obj(
          "collateral_percent" -> num(params.collateralPercentage),
          "committee_max_term_length" -> str(params.committeeMaxTermLength),
          "committee_min_size" -> str(params.committeeMinSize),
          // Real Blockfrost keys the deprecated, object-shaped (opName -> cost) form as
          // `cost_models`, and the array-shaped form this codec produces as `cost_models_raw`.
          // Emit only the latter: building the former needs the per-language Plutus
          // operation-name tables, which we don't have and MeshJS/Evolution SDK don't need (they
          // read `cost_models_raw`).
          "cost_models_raw" -> writeCostModels(params.costModels),
          "drep_activity" -> str(params.dRepActivity),
          "drep_deposit" -> str(params.dRepDeposit),
          "dvt_motion_no_confidence" -> blockfrostFraction(dvt.motionNoConfidence),
          "dvt_committee_normal" -> blockfrostFraction(dvt.committeeNormal),
          "dvt_committee_no_confidence" -> blockfrostFraction(dvt.committeeNoConfidence),
          "dvt_update_to_constitution" -> blockfrostFraction(dvt.updateToConstitution),
          "dvt_hard_fork_initiation" -> blockfrostFraction(dvt.hardForkInitiation),
          "dvt_p_p_network_group" -> blockfrostFraction(dvt.ppNetworkGroup),
          "dvt_p_p_economic_group" -> blockfrostFraction(dvt.ppEconomicGroup),
          "dvt_p_p_technical_group" -> blockfrostFraction(dvt.ppTechnicalGroup),
          "dvt_p_p_gov_group" -> blockfrostFraction(dvt.ppGovGroup),
          "dvt_treasury_withdrawal" -> blockfrostFraction(dvt.treasuryWithdrawal),
          "price_mem" -> blockfrostFraction(params.executionUnitPrices.priceMemory),
          "price_step" -> blockfrostFraction(params.executionUnitPrices.priceSteps),
          "gov_action_deposit" -> str(params.govActionDeposit),
          "gov_action_lifetime" -> str(params.govActionLifetime),
          "max_block_size" -> num(params.maxBlockBodySize),
          "max_block_ex_mem" -> str(params.maxBlockExecutionUnits.memory),
          "max_block_ex_steps" -> str(params.maxBlockExecutionUnits.steps),
          "max_block_header_size" -> num(params.maxBlockHeaderSize),
          "max_collateral_inputs" -> num(params.maxCollateralInputs),
          "max_tx_ex_mem" -> str(params.maxTxExecutionUnits.memory),
          "max_tx_ex_steps" -> str(params.maxTxExecutionUnits.steps),
          "max_tx_size" -> num(params.maxTxSize),
          "max_val_size" -> str(params.maxValueSize),
          "min_fee_ref_script_cost_per_byte" -> num(params.minFeeRefScriptCostPerByte),
          "min_pool_cost" -> str(params.minPoolCost),
          "rho" -> blockfrostFraction(params.monetaryExpansion),
          "a0" -> blockfrostFraction(params.poolPledgeInfluence),
          "e_max" -> num(params.poolRetireMaxEpoch),
          "pvt_motion_no_confidence" -> blockfrostFraction(pvt.motionNoConfidence),
          "pvt_committee_normal" -> blockfrostFraction(pvt.committeeNormal),
          "pvt_committee_no_confidence" -> blockfrostFraction(pvt.committeeNoConfidence),
          "pvt_hard_fork_initiation" -> blockfrostFraction(pvt.hardForkInitiation),
          // Blockfrost's schema requires both the deprecated `pvtpp_security_group` and its
          // replacement `pvt_p_p_security_group`; emit both.
          "pvtpp_security_group" -> blockfrostFraction(pvt.ppSecurityGroup),
          "pvt_p_p_security_group" -> blockfrostFraction(pvt.ppSecurityGroup),
          "protocol_major_ver" -> num(params.protocolVersion.major.toLong),
          "protocol_minor_ver" -> num(params.protocolVersion.minor.toLong),
          "key_deposit" -> str(params.stakeAddressDeposit),
          "pool_deposit" -> str(params.stakePoolDeposit),
          "n_opt" -> num(params.stakePoolTargetNum),
          "tau" -> blockfrostFraction(params.treasuryCut),
          "min_fee_b" -> num(params.txFeeFixed),
          "min_fee_a" -> num(params.txFeePerByte),
          "coins_per_utxo_size" -> str(params.utxoCostPerByte)
        )
    }

    /** Cost models keyed by the canonical language name ("PlutusV1", ...), in language order, each
      * an array of its parameters: the shape of Blockfrost's `cost_models_raw` and of cardano-cli's
      * `costModels`.
      */
    def writeCostModels(costModels: CostModels): JsonTree = {
        val byLanguage = costModels.models.toSeq.sortBy(_._1).map { (languageId, costs) =>
            Language.fromId(languageId).toString -> Arr(costs.iterator.map(num).toVector)
        }
        Obj(VectorMap.from(byLanguage))
    }

    // ---------------------------------------------------------------------------------------
    // cardano-cli: `cardano-cli query protocol-parameters`, keyed by the field names

    /** A fraction as cardano-ledger writes it: the decimal when it has at most 19 places, else the
      * exact `{"numerator": n, "denominator": d}` in lowest terms (`BaseTypes.hs`, `ToJSON`).
      */
    private def cliFraction(fraction: UnitInterval | NonNegativeInterval): JsonTree = {
        val r = termsOf(fraction)
        decimalOf(r, MaxCliDecimalPlaces) match
            case Some(decimal) => Num(decimal)
            case None => obj("numerator" -> num(r.numerator), "denominator" -> num(r.denominator))
    }

    private def exUnits(json: JsonTree, key: String): ExUnits = {
        val units = json(key)
        ExUnits(
          memory = long(s"$key.memory", member(units, key, "memory")),
          steps = long(s"$key.steps", member(units, key, "steps"))
        )
    }

    /** Cost models in cardano-cli's shape, at `path`: an object from language name ("PlutusV1",
      * ...) to the array of that language's parameters.
      */
    def readCostModels(path: String, json: JsonTree): CostModels = {
        CostModels(
          fieldsAt(path, json).map { (language, costs) =>
              val languagePath = s"$path.$language"
              val values = costs.arrOpt.getOrElse(
                throw new IllegalArgumentException(
                  s"$languagePath: expected an array, got ${costs.kind}"
                )
              )
              languageId(languagePath, language) -> values.map(long(languagePath, _)).toIndexedSeq
          }.toMap
        )
    }

    def readCardanoCli(json: JsonTree): ProtocolParams = {
        def longField(key: String): Long = long(key, json(key))
        val dvt = json("dRepVotingThresholds")
        def dRep(key: String): UnitInterval = roundedUnitInterval(
          s"dRepVotingThresholds.$key",
          member(dvt, "dRepVotingThresholds", key)
        )
        val pvt = json("poolVotingThresholds")
        def pool(key: String): UnitInterval = roundedUnitInterval(
          s"poolVotingThresholds.$key",
          member(pvt, "poolVotingThresholds", key)
        )
        val prices = json("executionUnitPrices")
        def price(key: String): NonNegativeInterval = roundedFraction(
          s"executionUnitPrices.$key",
          member(prices, "executionUnitPrices", key)
        )
        val version = json("protocolVersion")
        ProtocolParams(
          collateralPercentage = longField("collateralPercentage"),
          committeeMaxTermLength = longField("committeeMaxTermLength"),
          committeeMinSize = longField("committeeMinSize"),
          costModels = readCostModels("costModels", json("costModels")),
          dRepActivity = longField("dRepActivity"),
          dRepDeposit = longField("dRepDeposit"),
          dRepVotingThresholds = DRepVotingThresholds(
            motionNoConfidence = dRep("motionNoConfidence"),
            committeeNormal = dRep("committeeNormal"),
            committeeNoConfidence = dRep("committeeNoConfidence"),
            updateToConstitution = dRep("updateToConstitution"),
            hardForkInitiation = dRep("hardForkInitiation"),
            ppNetworkGroup = dRep("ppNetworkGroup"),
            ppEconomicGroup = dRep("ppEconomicGroup"),
            ppTechnicalGroup = dRep("ppTechnicalGroup"),
            ppGovGroup = dRep("ppGovGroup"),
            treasuryWithdrawal = dRep("treasuryWithdrawal")
          ),
          executionUnitPrices = ExUnitPrices(
            priceMemory = price("priceMemory"),
            priceSteps = price("priceSteps")
          ),
          govActionDeposit = longField("govActionDeposit"),
          govActionLifetime = longField("govActionLifetime"),
          maxBlockBodySize = longField("maxBlockBodySize"),
          maxBlockExecutionUnits = exUnits(json, "maxBlockExecutionUnits"),
          maxBlockHeaderSize = longField("maxBlockHeaderSize"),
          maxCollateralInputs = longField("maxCollateralInputs"),
          maxTxExecutionUnits = exUnits(json, "maxTxExecutionUnits"),
          maxTxSize = longField("maxTxSize"),
          maxValueSize = longField("maxValueSize"),
          minFeeRefScriptCostPerByte =
              wholePart("minFeeRefScriptCostPerByte", json("minFeeRefScriptCostPerByte")),
          minPoolCost = longField("minPoolCost"),
          monetaryExpansion = unitInterval("monetaryExpansion", json("monetaryExpansion")),
          poolPledgeInfluence = fraction("poolPledgeInfluence", json("poolPledgeInfluence")),
          poolRetireMaxEpoch = longField("poolRetireMaxEpoch"),
          poolVotingThresholds = PoolVotingThresholds(
            motionNoConfidence = pool("motionNoConfidence"),
            committeeNormal = pool("committeeNormal"),
            committeeNoConfidence = pool("committeeNoConfidence"),
            hardForkInitiation = pool("hardForkInitiation"),
            ppSecurityGroup = pool("ppSecurityGroup")
          ),
          protocolVersion = ProtocolVersion(
            major = int("protocolVersion.major", member(version, "protocolVersion", "major")),
            minor = int("protocolVersion.minor", member(version, "protocolVersion", "minor"))
          ),
          stakeAddressDeposit = longField("stakeAddressDeposit"),
          stakePoolDeposit = longField("stakePoolDeposit"),
          stakePoolTargetNum = longField("stakePoolTargetNum"),
          treasuryCut = unitInterval("treasuryCut", json("treasuryCut")),
          txFeeFixed = longField("txFeeFixed"),
          txFeePerByte = longField("txFeePerByte"),
          utxoCostPerByte = longField("utxoCostPerByte")
        )
    }

    /** Fields in declaration order, as the `macroRW` writer this replaces emitted them. Integers
      * are JSON numbers at any size, as cardano-cli writes them.
      */
    def writeCardanoCli(params: ProtocolParams): JsonTree = {
        val dvt = params.dRepVotingThresholds
        val pvt = params.poolVotingThresholds
        def exUnits(units: ExUnits): JsonTree =
            obj("memory" -> num(units.memory), "steps" -> num(units.steps))
        obj(
          "collateralPercentage" -> num(params.collateralPercentage),
          "committeeMaxTermLength" -> num(params.committeeMaxTermLength),
          "committeeMinSize" -> num(params.committeeMinSize),
          "costModels" -> writeCostModels(params.costModels),
          "dRepActivity" -> num(params.dRepActivity),
          "dRepDeposit" -> num(params.dRepDeposit),
          "dRepVotingThresholds" -> obj(
            "motionNoConfidence" -> cliFraction(dvt.motionNoConfidence),
            "committeeNormal" -> cliFraction(dvt.committeeNormal),
            "committeeNoConfidence" -> cliFraction(dvt.committeeNoConfidence),
            "updateToConstitution" -> cliFraction(dvt.updateToConstitution),
            "hardForkInitiation" -> cliFraction(dvt.hardForkInitiation),
            "ppNetworkGroup" -> cliFraction(dvt.ppNetworkGroup),
            "ppEconomicGroup" -> cliFraction(dvt.ppEconomicGroup),
            "ppTechnicalGroup" -> cliFraction(dvt.ppTechnicalGroup),
            "ppGovGroup" -> cliFraction(dvt.ppGovGroup),
            "treasuryWithdrawal" -> cliFraction(dvt.treasuryWithdrawal)
          ),
          "executionUnitPrices" -> obj(
            "priceMemory" -> cliFraction(params.executionUnitPrices.priceMemory),
            "priceSteps" -> cliFraction(params.executionUnitPrices.priceSteps)
          ),
          "govActionDeposit" -> num(params.govActionDeposit),
          "govActionLifetime" -> num(params.govActionLifetime),
          "maxBlockBodySize" -> num(params.maxBlockBodySize),
          "maxBlockExecutionUnits" -> exUnits(params.maxBlockExecutionUnits),
          "maxBlockHeaderSize" -> num(params.maxBlockHeaderSize),
          "maxCollateralInputs" -> num(params.maxCollateralInputs),
          "maxTxExecutionUnits" -> exUnits(params.maxTxExecutionUnits),
          "maxTxSize" -> num(params.maxTxSize),
          "maxValueSize" -> num(params.maxValueSize),
          "minFeeRefScriptCostPerByte" -> num(params.minFeeRefScriptCostPerByte),
          "minPoolCost" -> num(params.minPoolCost),
          "monetaryExpansion" -> cliFraction(params.monetaryExpansion),
          "poolPledgeInfluence" -> cliFraction(params.poolPledgeInfluence),
          "poolRetireMaxEpoch" -> num(params.poolRetireMaxEpoch),
          "poolVotingThresholds" -> obj(
            "motionNoConfidence" -> cliFraction(pvt.motionNoConfidence),
            "committeeNormal" -> cliFraction(pvt.committeeNormal),
            "committeeNoConfidence" -> cliFraction(pvt.committeeNoConfidence),
            "hardForkInitiation" -> cliFraction(pvt.hardForkInitiation),
            "ppSecurityGroup" -> cliFraction(pvt.ppSecurityGroup)
          ),
          "protocolVersion" -> obj(
            "major" -> num(params.protocolVersion.major.toLong),
            "minor" -> num(params.protocolVersion.minor.toLong)
          ),
          "stakeAddressDeposit" -> num(params.stakeAddressDeposit),
          "stakePoolDeposit" -> num(params.stakePoolDeposit),
          "stakePoolTargetNum" -> num(params.stakePoolTargetNum),
          "treasuryCut" -> cliFraction(params.treasuryCut),
          "txFeeFixed" -> num(params.txFeeFixed),
          "txFeePerByte" -> num(params.txFeePerByte),
          "utxoCostPerByte" -> num(params.utxoCostPerByte)
        )
    }
}
