package scalus.cardano.ledger

import upickle.default.*

import java.io.InputStream

/** Protocol parameters for the Cardano blockchain of Babbage era Field names are taken from the
  * `cardano-cli query protocol-parameters` output
  * @note
  *   These names are different from CIP-55, don't ask me why.
  */
case class ProtocolParams(
    collateralPercentage: Long,
    committeeMaxTermLength: Long,
    committeeMinSize: Long,
    costModels: CostModels,
    dRepActivity: Long,
    dRepDeposit: Long,
    dRepVotingThresholds: DRepVotingThresholds,
    executionUnitPrices: ExUnitPrices,
    govActionDeposit: Long,
    govActionLifetime: Long,
    maxBlockBodySize: Long,
    maxBlockExecutionUnits: ExUnits,
    maxBlockHeaderSize: Long,
    maxCollateralInputs: Long,
    maxTxExecutionUnits: ExUnits,
    maxTxSize: Long,
    maxValueSize: Long,
    minFeeRefScriptCostPerByte: Long,
    minPoolCost: Long,
    monetaryExpansion: Double,
    poolPledgeInfluence: Double,
    poolRetireMaxEpoch: Long,
    poolVotingThresholds: PoolVotingThresholds,
    protocolVersion: ProtocolVersion,
    stakeAddressDeposit: Long,
    stakePoolDeposit: Long,
    stakePoolTargetNum: Long,
    treasuryCut: Double,
    txFeeFixed: Long,
    txFeePerByte: Long,
    utxoCostPerByte: Long
)

object ProtocolParams {

    /** A single field difference between two ProtocolParams */
    case class ParamDiff(field: String, expected: String, actual: String)

    /** Compare two ProtocolParams and return all field-level differences. Returns empty Seq if they
      * are equal. Uses productElementNames/productIterator for automatic field enumeration.
      */
    def diff(expected: ProtocolParams, actual: ProtocolParams): Seq[ParamDiff] = {
        expected.productElementNames
            .zip(expected.productIterator)
            .zip(actual.productIterator)
            .collect {
                case ((name, exp), act) if exp != act =>
                    ParamDiff(name, exp.toString, act.toString)
            }
            .toSeq
    }

    /** Extension to parse JSON values that may be either strings or numbers. Some APIs (like
      * Blockfrost) return numeric values as strings, while others (like Yaci DevKit) return them as
      * numbers.
      */
    extension (v: ujson.Value)
        /** Parse as Long, handling both string and number formats */
        private def asLong: Long = v.strOpt.map(_.toLong).getOrElse(v.num.toLong)

        /** Parse as Long with default, handling both string and number formats */
        private def asLongOr(default: Long): Long =
            v.strOpt.map(_.toLong).orElse(v.numOpt.map(_.toLong)).getOrElse(default)

    /** Reads ProtocolParams from JSON string in Blockfrost format */
    def fromBlockfrostJson(json: String): ProtocolParams = {
        read[ProtocolParams](json)(using blockfrostParamsReadWriter)
    }

    /** Reads ProtocolParams from JSON string in Blockfrost format */
    def fromBlockfrostJson(json: InputStream): ProtocolParams = {
        read[ProtocolParams](json)(using blockfrostParamsReadWriter)
    }

    /** Reads ProtocolParams from JSON string in Cardano CLI format */
    def fromCardanoCliJson(json: String): ProtocolParams = {
        read[ProtocolParams](json)(using cardanoCliParamsReadWriter)
    }

    /** Reads ProtocolParams from JSON string in Cardano CLI format */
    def fromCardanoCliJson(json: InputStream): ProtocolParams = {
        read[ProtocolParams](json)(using cardanoCliParamsReadWriter)
    }

    val blockfrostParamsReadWriter: ReadWriter[ProtocolParams] =
        readwriter[ujson.Value].bimap[ProtocolParams](
          params =>
              ujson.Obj(
                "collateral_percent" -> params.collateralPercentage,
                "committee_max_term_length" -> params.committeeMaxTermLength.toString,
                "committee_min_size" -> params.committeeMinSize.toString,
                "cost_models" -> params.costModels.models.map { (k, v) =>
                    // Use the canonical language name ("PlutusV1", ...) so the value round-trips
                    // through this reader and matches Blockfrost's own `cost_models` key naming.
                    Language.fromId(k).toString -> v.map(v => ujson.Num(v.toDouble))
                },
                "drep_activity" -> params.dRepActivity.toString,
                "drep_deposit" -> params.dRepDeposit.toString,
                "dvt_motion_no_confidence" -> params.dRepVotingThresholds.motionNoConfidence.toDouble,
                "dvt_committee_normal" -> params.dRepVotingThresholds.committeeNormal.toDouble,
                "dvt_committee_no_confidence" -> params.dRepVotingThresholds.committeeNoConfidence.toDouble,
                "dvt_update_to_constitution" -> params.dRepVotingThresholds.updateToConstitution.toDouble,
                "dvt_hard_fork_initiation" -> params.dRepVotingThresholds.hardForkInitiation.toDouble,
                "dvt_p_p_network_group" -> params.dRepVotingThresholds.ppNetworkGroup.toDouble,
                "dvt_p_p_economic_group" -> params.dRepVotingThresholds.ppEconomicGroup.toDouble,
                "dvt_p_p_technical_group" -> params.dRepVotingThresholds.ppTechnicalGroup.toDouble,
                "dvt_p_p_gov_group" -> params.dRepVotingThresholds.ppGovGroup.toDouble,
                "dvt_treasury_withdrawal" -> params.dRepVotingThresholds.treasuryWithdrawal.toDouble,
                "price_mem" -> params.executionUnitPrices.priceMemory.toDouble,
                "price_step" -> params.executionUnitPrices.priceSteps.toDouble,
                "gov_action_deposit" -> params.govActionDeposit.toString,
                "gov_action_lifetime" -> params.govActionLifetime.toString,
                "max_block_size" -> params.maxBlockBodySize,
                "max_block_ex_mem" -> params.maxBlockExecutionUnits.memory.toString,
                "max_block_ex_steps" -> params.maxBlockExecutionUnits.steps.toString,
                "max_block_header_size" -> params.maxBlockHeaderSize,
                "max_collateral_inputs" -> params.maxCollateralInputs,
                "max_tx_ex_mem" -> params.maxTxExecutionUnits.memory.toString,
                "max_tx_ex_steps" -> params.maxTxExecutionUnits.steps.toString,
                "max_tx_size" -> params.maxTxSize,
                "max_val_size" -> params.maxValueSize.toString,
                "min_fee_ref_script_cost_per_byte" -> params.minFeeRefScriptCostPerByte,
                "min_pool_cost" -> params.minPoolCost.toString,
                "rho" -> params.monetaryExpansion,
                "a0" -> params.poolPledgeInfluence,
                "e_max" -> params.poolRetireMaxEpoch,
                "pvt_motion_no_confidence" -> params.poolVotingThresholds.motionNoConfidence.toDouble,
                "pvt_committee_normal" -> params.poolVotingThresholds.committeeNormal.toDouble,
                "pvt_committee_no_confidence" -> params.poolVotingThresholds.committeeNoConfidence.toDouble,
                "pvt_hard_fork_initiation" -> params.poolVotingThresholds.hardForkInitiation.toDouble,
                "pvtpp_security_group" -> params.poolVotingThresholds.ppSecurityGroup.toDouble,
                "protocol_major_ver" -> params.protocolVersion.major,
                "protocol_minor_ver" -> params.protocolVersion.minor,
                "key_deposit" -> params.stakeAddressDeposit.toString,
                "pool_deposit" -> params.stakePoolDeposit.toString,
                "n_opt" -> params.stakePoolTargetNum,
                "tau" -> params.treasuryCut,
                "min_fee_b" -> params.txFeeFixed,
                "min_fee_a" -> params.txFeePerByte,
                "coins_per_utxo_size" -> params.utxoCostPerByte.toString
              ),
          json =>
              ProtocolParams(
                collateralPercentage = json("collateral_percent").asLong,
                committeeMaxTermLength = json("committee_max_term_length").asLongOr(0L),
                committeeMinSize = json("committee_min_size").asLongOr(0L),
                costModels = CostModels(
                  json("cost_models").obj.map { case (k, v) =>
                      // Blockfrost's `cost_models` values are objects (opName -> cost), while its
                      // `cost_models_raw` and this codec's own writer emit plain arrays. Accept both.
                      val costs = v.arrOpt
                          .map(_.iterator.map(_.num.toLong).toIndexedSeq)
                          .getOrElse(v.obj.values.map(_.num.toLong).toIndexedSeq)
                      Language.valueOf(k).languageId -> costs
                  }.toMap
                ),
                dRepActivity = json("drep_activity").asLongOr(0L),
                dRepDeposit = json("drep_deposit").asLongOr(0L),
                dRepVotingThresholds = DRepVotingThresholds(
                  motionNoConfidence =
                      UnitInterval.fromDouble(json("dvt_motion_no_confidence").numOpt.getOrElse(0)),
                  committeeNormal =
                      UnitInterval.fromDouble(json("dvt_committee_normal").numOpt.getOrElse(0)),
                  committeeNoConfidence = UnitInterval.fromDouble(
                    json("dvt_committee_no_confidence").numOpt.getOrElse(0)
                  ),
                  updateToConstitution = UnitInterval.fromDouble(
                    json("dvt_update_to_constitution").numOpt.getOrElse(0)
                  ),
                  hardForkInitiation =
                      UnitInterval.fromDouble(json("dvt_hard_fork_initiation").numOpt.getOrElse(0)),
                  ppNetworkGroup = json.obj
                      .get("dvt_p_p_network_group")
                      .orElse(json.obj.get("dvt_ppnetwork_group"))
                      .flatMap(_.numOpt)
                      .map(UnitInterval.fromDouble)
                      .getOrElse(UnitInterval.zero),
                  ppEconomicGroup = json.obj
                      .get("dvt_p_p_economic_group")
                      .orElse(json.obj.get("dvt_ppeconomic_group"))
                      .flatMap(_.numOpt)
                      .map(UnitInterval.fromDouble)
                      .getOrElse(UnitInterval.zero),
                  ppTechnicalGroup = json.obj
                      .get("dvt_p_p_technical_group")
                      .orElse(json.obj.get("dvt_pptechnical_group"))
                      .flatMap(_.numOpt)
                      .map(UnitInterval.fromDouble)
                      .getOrElse(UnitInterval.zero),
                  ppGovGroup = json.obj
                      .get("dvt_p_p_gov_group")
                      .orElse(json.obj.get("dvt_ppgov_group"))
                      .flatMap(_.numOpt)
                      .map(UnitInterval.fromDouble)
                      .getOrElse(UnitInterval.zero),
                  treasuryWithdrawal =
                      UnitInterval.fromDouble(json("dvt_treasury_withdrawal").numOpt.getOrElse(0))
                ),
                executionUnitPrices = ExUnitPrices(
                  // Use precision=15 to preserve tiny values like 7.21e-5
                  priceMemory = NonNegativeInterval(json("price_mem").num, precision = 15),
                  priceSteps = NonNegativeInterval(json("price_step").num, precision = 15)
                ),
                govActionDeposit = json("gov_action_deposit").asLongOr(0L),
                govActionLifetime = json("gov_action_lifetime").asLongOr(0L),
                maxBlockBodySize = json("max_block_size").asLong,
                maxBlockExecutionUnits = ExUnits(
                  memory = json("max_block_ex_mem").asLong,
                  steps = json("max_block_ex_steps").asLong
                ),
                maxBlockHeaderSize = json("max_block_header_size").asLong,
                maxCollateralInputs = json("max_collateral_inputs").asLong,
                maxTxExecutionUnits = ExUnits(
                  memory = json("max_tx_ex_mem").asLong,
                  steps = json("max_tx_ex_steps").asLong
                ),
                maxTxSize = json("max_tx_size").asLong,
                maxValueSize = json("max_val_size").asLong,
                minFeeRefScriptCostPerByte = json("min_fee_ref_script_cost_per_byte").asLongOr(0L),
                minPoolCost = json("min_pool_cost").asLong,
                monetaryExpansion = json("rho").num,
                poolPledgeInfluence = json("a0").num,
                poolRetireMaxEpoch = json("e_max").asLong,
                poolVotingThresholds = PoolVotingThresholds(
                  motionNoConfidence = json("pvt_motion_no_confidence").numOpt
                      .map(UnitInterval.fromDouble)
                      .getOrElse(UnitInterval.zero),
                  committeeNormal = json("pvt_committee_normal").numOpt
                      .map(UnitInterval.fromDouble)
                      .getOrElse(UnitInterval.zero),
                  committeeNoConfidence = json("pvt_committee_no_confidence").numOpt
                      .map(UnitInterval.fromDouble)
                      .getOrElse(UnitInterval.zero),
                  hardForkInitiation = json("pvt_hard_fork_initiation").numOpt
                      .map(UnitInterval.fromDouble)
                      .getOrElse(UnitInterval.zero),
                  ppSecurityGroup = json.obj
                      .get("pvtpp_security_group")
                      .orElse(json.obj.get("pvt_p_p_security_group"))
                      .flatMap(_.numOpt)
                      .map(UnitInterval.fromDouble)
                      .getOrElse(UnitInterval.zero)
                ),
                protocolVersion = ProtocolVersion(
                  major = json("protocol_major_ver").num.toInt,
                  minor = json("protocol_minor_ver").num.toInt
                ),
                stakeAddressDeposit = json("key_deposit").asLong,
                stakePoolDeposit = json("pool_deposit").asLong,
                stakePoolTargetNum = json("n_opt").asLong,
                treasuryCut = json("tau").num,
                txFeeFixed = json("min_fee_b").asLong,
                txFeePerByte = json("min_fee_a").asLong,
                utxoCostPerByte = json("coins_per_utxo_size").asLong
              )
        )

    val cardanoCliParamsReadWriter: ReadWriter[ProtocolParams] = {
        // Provide implicit ReadWriter for CostModels in Cardano CLI format
        given ReadWriter[CostModels] = CostModels.cardanoCliReadWriter
        // NonNegativeInterval's default upickle codec rebuilds from Double at precision 6, which
        // truncates tiny values such as priceSteps = 7.21e-5 to 7.2e-5. Read at precision 15
        // (matching the Blockfrost reader) so the Double round-trip is exact. ExUnitPrices must be
        // re-derived here so it picks up this override instead of the precision-6 instance baked
        // into its `derives UpickleReadWriter` companion.
        given ReadWriter[NonNegativeInterval] =
            readwriter[Double].bimap[NonNegativeInterval](
              _.toDouble,
              d => NonNegativeInterval(d, precision = 15)
            )
        // UnitInterval fields (pool/dRep voting thresholds) intentionally keep their default
        // precision-1e6 codec: unlike NonNegativeInterval (reduced-value equality) it has plain
        // case-class equality, and both codecs build it via fromDouble, so it round-trips exactly
        // for the standard threshold values (which have few decimal places).
        given ReadWriter[ExUnitPrices] = macroRW
        macroRW
    }
}
