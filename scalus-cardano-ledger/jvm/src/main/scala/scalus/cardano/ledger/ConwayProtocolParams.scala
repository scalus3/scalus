package scalus.cardano.ledger

import io.bullet.borer.*

import java.util.concurrent.atomic.AtomicBoolean

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
  * Arrays larger than 31 elements are tolerated for forward-compatibility with future era
  * extensions: trailing entries are read and discarded with a single warn-level log.
  */
case class ConwayProtocolParams(
    minFeeA: Long,
    minFeeB: Long,
    maxBBSize: Long,
    maxTxSize: Long,
    maxBHSize: Long,
    keyDeposit: Long,
    poolDeposit: Long,
    eMax: Long,
    nOpt: Long,
    a0: NonNegativeInterval,
    rho: UnitInterval,
    tau: UnitInterval,
    protocolVersion: ProtocolVersion,
    minPoolCost: Long,
    coinsPerUTxOByte: Long,
    costModels: CostModels,
    prices: ExUnitPrices,
    maxTxExUnits: ExUnits,
    maxBlockExUnits: ExUnits,
    maxValSize: Long,
    collateralPercentage: Long,
    maxCollateralInputs: Long,
    poolVotingThresholds: PoolVotingThresholds,
    dRepVotingThresholds: DRepVotingThresholds,
    committeeMinSize: Long,
    committeeMaxTermLength: Long,
    govActionLifetime: Long,
    govActionDeposit: Long,
    dRepDeposit: Long,
    dRepActivity: Long,
    minFeeRefScriptCostPerByte: NonNegativeInterval
):
    /** Convert to scalus's [[ProtocolParams]] type. All fields come from the decoded CBOR; no
      * external baseline is consulted.
      */
    def toProtocolParams: ProtocolParams =
        ProtocolParams(
          collateralPercentage = collateralPercentage,
          committeeMaxTermLength = committeeMaxTermLength,
          committeeMinSize = committeeMinSize,
          costModels = costModels,
          dRepActivity = dRepActivity,
          dRepDeposit = dRepDeposit,
          dRepVotingThresholds = dRepVotingThresholds,
          executionUnitPrices = prices,
          govActionDeposit = govActionDeposit,
          govActionLifetime = govActionLifetime,
          maxBlockBodySize = maxBBSize,
          maxBlockExecutionUnits = maxBlockExUnits,
          maxBlockHeaderSize = maxBHSize,
          maxCollateralInputs = maxCollateralInputs,
          maxTxExecutionUnits = maxTxExUnits,
          maxTxSize = maxTxSize,
          maxValueSize = maxValSize,
          minFeeRefScriptCostPerByte = minFeeRefScriptCostPerByte.floor,
          minPoolCost = minPoolCost,
          monetaryExpansion = rho.toDouble,
          poolPledgeInfluence = a0.toDouble,
          poolRetireMaxEpoch = eMax,
          poolVotingThresholds = poolVotingThresholds,
          protocolVersion = protocolVersion,
          stakeAddressDeposit = keyDeposit,
          stakePoolDeposit = poolDeposit,
          stakePoolTargetNum = nOpt,
          treasuryCut = tau.toDouble,
          txFeeFixed = minFeeB,
          txFeePerByte = minFeeA,
          utxoCostPerByte = coinsPerUTxOByte
        )

object ConwayProtocolParams:
    private val ConwayPParamsArity = 31
    private val log = scribe.Logger[ConwayProtocolParams.type]
    private val warnedOnExtras = new AtomicBoolean(false)

    def fromCbor(cbor: Array[Byte]): ConwayProtocolParams =
        Cbor.decode(cbor).to[ConwayProtocolParams].value

    given Decoder[ConwayProtocolParams] with
        def read(r: Reader): ConwayProtocolParams =
            val size = r.readArrayHeader()
            require(
              size >= ConwayPParamsArity,
              s"Expected at least $ConwayPParamsArity elements, got $size"
            )

            val minFeeA = r.readLong()
            val minFeeB = r.readLong()
            val maxBBSize = r.readLong()
            val maxTxSize = r.readLong()
            val maxBHSize = r.readLong()
            val keyDeposit = r.readLong()
            val poolDeposit = r.readLong()
            val eMax = r.readLong()
            val nOpt = r.readLong()

            val a0 = r.read[NonNegativeInterval]()
            val rho = r.read[UnitInterval]()
            val tau = r.read[UnitInterval]()
            val protocolVersion = r.read[ProtocolVersion]()

            val minPoolCost = r.readLong()
            val coinsPerUTxOByte = r.readLong()

            val costModels = r.read[CostModels]()
            val prices = r.read[ExUnitPrices]()
            val maxTxExUnits = r.read[ExUnits]()
            val maxBlockExUnits = r.read[ExUnits]()

            val maxValSize = r.readLong()
            val collateralPercentage = r.readLong()
            val maxCollateralInputs = r.readLong()

            val poolVotingThresholds = r.read[PoolVotingThresholds]()
            val dRepVotingThresholds = r.read[DRepVotingThresholds]()

            val committeeMinSize = r.readLong()
            val committeeMaxTermLength = r.readLong()
            val govActionLifetime = r.readLong()
            val govActionDeposit = r.readLong()
            val dRepDeposit = r.readLong()
            val dRepActivity = r.readLong()

            val minFeeRefScriptCostPerByte = r.read[NonNegativeInterval]()

            val extras = size - ConwayPParamsArity
            if extras > 0 then
                if warnedOnExtras.compareAndSet(false, true) then
                    log.warn(
                      s"Conway PParams CBOR has $size fields; expected $ConwayPParamsArity. " +
                          s"Skipping $extras unknown trailing field(s) — schema may have advanced."
                    )
                (0 until extras.toInt).foreach(_ => r.skipElement())

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
              a0 = a0,
              rho = rho,
              tau = tau,
              protocolVersion = protocolVersion,
              minPoolCost = minPoolCost,
              coinsPerUTxOByte = coinsPerUTxOByte,
              costModels = costModels,
              prices = prices,
              maxTxExUnits = maxTxExUnits,
              maxBlockExUnits = maxBlockExUnits,
              maxValSize = maxValSize,
              collateralPercentage = collateralPercentage,
              maxCollateralInputs = maxCollateralInputs,
              poolVotingThresholds = poolVotingThresholds,
              dRepVotingThresholds = dRepVotingThresholds,
              committeeMinSize = committeeMinSize,
              committeeMaxTermLength = committeeMaxTermLength,
              govActionLifetime = govActionLifetime,
              govActionDeposit = govActionDeposit,
              dRepDeposit = dRepDeposit,
              dRepActivity = dRepActivity,
              minFeeRefScriptCostPerByte = minFeeRefScriptCostPerByte
            )
