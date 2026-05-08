package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.Dom.Element

/** Conway protocol parameters as encoded by `cardano-ledger`'s `EncCBOR (PParams Conway)` instance
  * — the same shape returned by the `LocalStateQuery` mini-protocol's `GetCurrentPParams` and the
  * on-disk `pparams-by-hash` files used by conformance test vectors.
  *
  * Conway PParams is encoded as an array of 31 elements in this order (from `eraPParams` in
  * `cardano-ledger/Conway/PParams.hs`):
  *   - 0: minFeeA (Coin)
  *   - 1: minFeeB (Coin)
  *   - 2: maxBBSize (Word32)
  *   - 3: maxTxSize (Word32)
  *   - 4: maxBHSize (Word16)
  *   - 5: keyDeposit (Coin)
  *   - 6: poolDeposit (CompactForm Coin)
  *   - 7: eMax (EpochInterval)
  *   - 8: nOpt (Word16)
  *   - 9: a0 (NonNegativeInterval)
  *   - 10: rho (UnitInterval)
  *   - 11: tau (UnitInterval)
  *   - 12: protocolVersion (ProtVer)
  *   - 13: minPoolCost (Coin)
  *   - 14: coinsPerUTxOByte (CoinPerByte)
  *   - 15: costModels (CostModels)
  *   - 16: prices (Prices - [memPrice, stepPrice])
  *   - 17: maxTxExUnits (ExUnits)
  *   - 18: maxBlockExUnits (ExUnits)
  *   - 19: maxValSize (Word32)
  *   - 20: collateralPercentage (Word16)
  *   - 21: maxCollateralInputs (Word16)
  *   - 22: poolVotingThresholds
  *   - 23: dRepVotingThresholds
  *   - 24: committeeMinSize (Word16)
  *   - 25: committeeMaxTermLength (EpochInterval)
  *   - 26: govActionLifetime (EpochInterval)
  *   - 27: govActionDeposit (Coin)
  *   - 28: dRepDeposit (CompactForm Coin)
  *   - 29: dRepActivity (EpochInterval)
  *   - 30: minFeeRefScriptCostPerByte (NonNegativeInterval)
  *
  * `poolVotingThresholdsRaw` and `dRepVotingThresholdsRaw` are kept as raw `Dom.Element` because
  * the typed CBOR codecs for `PoolVotingThresholds` / `DRepVotingThresholds` don't yet exist in
  * scalus; [[toProtocolParams]] currently fills those typed fields from a Blockfrost-shaped
  * fallback. Wire those decoders and the fallback drops out.
  */
case class ConwayProtocolParams(
    minFeeA: Long,
    minFeeB: Long,
    maxBBSize: Long,
    maxTxSize: Long,
    maxBHSize: Long,
    keyDeposit: Long, // stakeAddressDeposit
    poolDeposit: Long,
    eMax: Long,
    nOpt: Long,
    a0Numerator: Long, // pool pledge influence numerator
    a0Denominator: Long,
    rhoNumerator: Long, // monetary expansion numerator
    rhoDenominator: Long,
    tauNumerator: Long, // treasury cut numerator
    tauDenominator: Long,
    protocolVersionMajor: Long,
    protocolVersionMinor: Long,
    minPoolCost: Long,
    coinsPerUTxOByte: Long,
    costModelsRaw: Element, // CostModels as DOM element
    pricesMemNumerator: Long,
    pricesMemDenominator: Long,
    pricesStepNumerator: Long,
    pricesStepDenominator: Long,
    maxTxExMem: Long,
    maxTxExSteps: Long,
    maxBlockExMem: Long,
    maxBlockExSteps: Long,
    maxValSize: Long,
    collateralPercentage: Long,
    maxCollateralInputs: Long,
    poolVotingThresholdsRaw: Element,
    dRepVotingThresholdsRaw: Element,
    committeeMinSize: Long,
    committeeMaxTermLength: Long,
    govActionLifetime: Long,
    govActionDeposit: Long,
    dRepDeposit: Long,
    dRepActivity: Long,
    minFeeRefScriptCostPerByteNum: Long,
    minFeeRefScriptCostPerByteDen: Long
):
    /** Parse cost models from raw DOM element. Returns None if parsing fails, allowing fallback to
      * default cost models.
      */
    private def parseCostModels: Option[CostModels] =
        try
            costModelsRaw match
                case mapElem: Dom.MapElem =>
                    val models = mapElem.toMap.flatMap { case (key, value) =>
                        val langIdOpt = key match
                            case Dom.IntElem(v)  => Some(v)
                            case Dom.LongElem(v) => Some(v.toInt)
                            case _               => None
                        val costsOpt = value match
                            case arr: Dom.ArrayElem =>
                                Some(
                                  arr.elems.map {
                                      case Dom.IntElem(v)  => v.toLong
                                      case Dom.LongElem(v) => v
                                      case _               => 0L
                                  }.toIndexedSeq
                                )
                            case _ => None
                        for langId <- langIdOpt; costs <- costsOpt yield langId -> costs
                    }
                    Some(CostModels(models))
                case _ => None
        catch case _: Exception => None

    /** Convert to scalus's [[ProtocolParams]] type.
      *
      * Fields not covered by the Conway CBOR encoding (pool-voting / drep-voting thresholds — see
      * class doc) come from a static Blockfrost-shaped baseline shipped with scalus-core
      * (`/blockfrost-params-epoch-544.json`). Override-fields from the CBOR-decoded values replace
      * the baseline.
      */
    def toProtocolParams: ProtocolParams =
        val baseParams = ProtocolParams.fromBlockfrostJson(
          this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        )
        val costModels = parseCostModels.getOrElse(baseParams.costModels)
        baseParams.copy(
          txFeePerByte = minFeeA,
          txFeeFixed = minFeeB,
          maxBlockBodySize = maxBBSize,
          maxTxSize = maxTxSize,
          maxBlockHeaderSize = maxBHSize,
          stakeAddressDeposit = keyDeposit,
          stakePoolDeposit = poolDeposit,
          poolRetireMaxEpoch = eMax,
          stakePoolTargetNum = nOpt,
          poolPledgeInfluence =
              if a0Denominator == 0 then 0.0 else a0Numerator.toDouble / a0Denominator,
          monetaryExpansion =
              if rhoDenominator == 0 then 0.0 else rhoNumerator.toDouble / rhoDenominator,
          treasuryCut = if tauDenominator == 0 then 0.0 else tauNumerator.toDouble / tauDenominator,
          protocolVersion = ProtocolVersion(protocolVersionMajor.toInt, protocolVersionMinor.toInt),
          minPoolCost = minPoolCost,
          utxoCostPerByte = coinsPerUTxOByte,
          costModels = costModels,
          executionUnitPrices = ExUnitPrices(
            priceMemory = NonNegativeInterval(
              if pricesMemDenominator == 0 then 0.0
              else pricesMemNumerator.toDouble / pricesMemDenominator
            ),
            priceSteps = NonNegativeInterval(
              if pricesStepDenominator == 0 then 0.0
              else pricesStepNumerator.toDouble / pricesStepDenominator
            )
          ),
          maxTxExecutionUnits = ExUnits(maxTxExMem, maxTxExSteps),
          maxBlockExecutionUnits = ExUnits(maxBlockExMem, maxBlockExSteps),
          maxValueSize = maxValSize,
          collateralPercentage = collateralPercentage,
          maxCollateralInputs = maxCollateralInputs,
          committeeMinSize = committeeMinSize,
          committeeMaxTermLength = committeeMaxTermLength,
          govActionLifetime = govActionLifetime,
          govActionDeposit = govActionDeposit,
          dRepDeposit = dRepDeposit,
          dRepActivity = dRepActivity,
          minFeeRefScriptCostPerByte =
              if minFeeRefScriptCostPerByteDen == 0 then 0L
              else minFeeRefScriptCostPerByteNum / minFeeRefScriptCostPerByteDen
        )

object ConwayProtocolParams:
    def fromCbor(cbor: Array[Byte]): ConwayProtocolParams =
        Cbor.decode(cbor).to[ConwayProtocolParams].value

    given Decoder[ConwayProtocolParams] with
        def read(r: Reader): ConwayProtocolParams =
            val size = r.readArrayHeader()
            // Can be 31 (without minFeeRefScriptCostPerByte) or 31+ with it
            require(size >= 31, s"Expected at least 31 elements, got $size")

            val minFeeA = r.readLong()
            val minFeeB = r.readLong()
            val maxBBSize = r.readLong()
            val maxTxSize = r.readLong()
            val maxBHSize = r.readLong()
            val keyDeposit = r.readLong()
            val poolDeposit = r.readLong()
            val eMax = r.readLong()
            val nOpt = r.readLong()

            // a0 = NonNegativeInterval = Tagged(30, [num, denom])
            val a0Tag = r.read[Dom.Element]()
            val (a0Num, a0Denom) = parseTaggedRational(a0Tag)

            // rho = UnitInterval = Tagged(30, [num, denom])
            val rhoTag = r.read[Dom.Element]()
            val (rhoNum, rhoDenom) = parseTaggedRational(rhoTag)

            // tau = UnitInterval = Tagged(30, [num, denom])
            val tauTag = r.read[Dom.Element]()
            val (tauNum, tauDenom) = parseTaggedRational(tauTag)

            // protocolVersion = [major, minor]
            r.readArrayHeader(2)
            val pvMajor = r.readLong()
            val pvMinor = r.readLong()

            val minPoolCost = r.readLong()
            val coinsPerUTxOByte = r.readLong()

            // costModels - keep as DOM element
            val costModels = r.read[Dom.Element]()

            // prices = [memPrice, stepPrice] where each is Tagged(30, [num, denom])
            r.readArrayHeader(2)
            val memPriceTag = r.read[Dom.Element]()
            val (memPriceNum, memPriceDenom) = parseTaggedRational(memPriceTag)
            val stepPriceTag = r.read[Dom.Element]()
            val (stepPriceNum, stepPriceDenom) = parseTaggedRational(stepPriceTag)

            // maxTxExUnits = [mem, steps]
            r.readArrayHeader(2)
            val maxTxExMem = r.readLong()
            val maxTxExSteps = r.readLong()

            // maxBlockExUnits = [mem, steps]
            r.readArrayHeader(2)
            val maxBlockExMem = r.readLong()
            val maxBlockExSteps = r.readLong()

            val maxValSize = r.readLong()
            val collateralPercentage = r.readLong()
            val maxCollateralInputs = r.readLong()

            // poolVotingThresholds - keep as DOM
            val poolVotingThresholds = r.read[Dom.Element]()

            // dRepVotingThresholds - keep as DOM
            val dRepVotingThresholds = r.read[Dom.Element]()

            val committeeMinSize = r.readLong()
            val committeeMaxTermLength = r.readLong()
            val govActionLifetime = r.readLong()
            val govActionDeposit = r.readLong()
            val dRepDeposit = r.readLong()
            val dRepActivity = r.readLong()

            val (refScriptNum, refScriptDenom) =
                if size > 30 then
                    val tag = r.read[Dom.Element]()
                    parseTaggedRational(tag)
                else (0L, 1L)

            ConwayProtocolParams(
              minFeeA = minFeeA,
              minFeeB = minFeeB,
              maxBBSize = maxBBSize,
              maxTxSize = maxTxSize,
              maxBHSize = maxBHSize,
              keyDeposit = keyDeposit,
              poolDeposit = poolDeposit,
              eMax = eMax,
              nOpt = nOpt,
              a0Numerator = a0Num,
              a0Denominator = a0Denom,
              rhoNumerator = rhoNum,
              rhoDenominator = rhoDenom,
              tauNumerator = tauNum,
              tauDenominator = tauDenom,
              protocolVersionMajor = pvMajor,
              protocolVersionMinor = pvMinor,
              minPoolCost = minPoolCost,
              coinsPerUTxOByte = coinsPerUTxOByte,
              costModelsRaw = costModels,
              pricesMemNumerator = memPriceNum,
              pricesMemDenominator = memPriceDenom,
              pricesStepNumerator = stepPriceNum,
              pricesStepDenominator = stepPriceDenom,
              maxTxExMem = maxTxExMem,
              maxTxExSteps = maxTxExSteps,
              maxBlockExMem = maxBlockExMem,
              maxBlockExSteps = maxBlockExSteps,
              maxValSize = maxValSize,
              collateralPercentage = collateralPercentage,
              maxCollateralInputs = maxCollateralInputs,
              poolVotingThresholdsRaw = poolVotingThresholds,
              dRepVotingThresholdsRaw = dRepVotingThresholds,
              committeeMinSize = committeeMinSize,
              committeeMaxTermLength = committeeMaxTermLength,
              govActionLifetime = govActionLifetime,
              govActionDeposit = govActionDeposit,
              dRepDeposit = dRepDeposit,
              dRepActivity = dRepActivity,
              minFeeRefScriptCostPerByteNum = refScriptNum,
              minFeeRefScriptCostPerByteDen = refScriptDenom
            )

    private def parseTaggedRational(elem: Dom.Element): (Long, Long) =
        elem match
            case tagged: Dom.TaggedElem if tagged.tag.code == 30 =>
                tagged.value match
                    case arr: Dom.ArrayElem if arr.elems.size == 2 =>
                        val num = extractLong(arr.elems(0))
                        val denom = extractLong(arr.elems(1))
                        (num, denom)
                    case other =>
                        throw new RuntimeException(
                          s"Expected array of 2 elements in Tagged(30), got $other"
                        )
            case other =>
                throw new RuntimeException(s"Expected Tagged(30, [num, denom]), got $other")

    private def extractLong(elem: Dom.Element): Long =
        elem match
            case Dom.IntElem(v)  => v.toLong
            case Dom.LongElem(v) => v
            case other           => throw new RuntimeException(s"Expected integer, got $other")
