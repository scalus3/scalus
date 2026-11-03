package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

class ConwayProtocolParamsTest extends AnyFunSuite {

    private val base = CardanoInfo.mainnet.protocolParams

    /** The CBOR-decoded params with the given wire terms of a0, rho and tau. */
    private def decoded(
        a0: NonNegativeInterval,
        rho: UnitInterval,
        tau: UnitInterval
    ): ConwayProtocolParams =
        ConwayProtocolParams(
          minFeeA = base.txFeePerByte,
          minFeeB = base.txFeeFixed,
          maxBBSize = base.maxBlockBodySize,
          maxTxSize = base.maxTxSize,
          maxBHSize = base.maxBlockHeaderSize,
          keyDeposit = base.stakeAddressDeposit,
          poolDeposit = base.stakePoolDeposit,
          eMax = base.poolRetireMaxEpoch,
          nOpt = base.stakePoolTargetNum,
          a0 = a0,
          rho = rho,
          tau = tau,
          protocolVersion = base.protocolVersion,
          minPoolCost = base.minPoolCost,
          coinsPerUTxOByte = base.utxoCostPerByte,
          costModels = base.costModels,
          prices = base.executionUnitPrices,
          maxTxExUnits = base.maxTxExecutionUnits,
          maxBlockExUnits = base.maxBlockExecutionUnits,
          maxValSize = base.maxValueSize,
          collateralPercentage = base.collateralPercentage,
          maxCollateralInputs = base.maxCollateralInputs,
          poolVotingThresholds = base.poolVotingThresholds,
          dRepVotingThresholds = base.dRepVotingThresholds,
          committeeMinSize = base.committeeMinSize,
          committeeMaxTermLength = base.committeeMaxTermLength,
          govActionLifetime = base.govActionLifetime,
          govActionDeposit = base.govActionDeposit,
          dRepDeposit = base.dRepDeposit,
          dRepActivity = base.dRepActivity,
          minFeeRefScriptCostPerByte = NonNegativeInterval(base.minFeeRefScriptCostPerByte)
        )

    test("a0, rho and tau are in lowest terms, as the JSON readers give them") {
        // spec [DM-20a], [DM-21a]; the ledger reduces on decode with `%`
        // (cardano-ledger-core BaseTypes.hs:340)
        val params =
            decoded(
              NonNegativeInterval(6, 20),
              UnitInterval(6, 2000),
              UnitInterval(4, 20)
            ).toProtocolParams
        // NonNegativeInterval equality ignores the terms, so read them
        assert(params.poolPledgeInfluence.numerator == 3L)
        assert(params.poolPledgeInfluence.denominator == 10L)
        assert(params.monetaryExpansion == UnitInterval(3, 1000))
        assert(params.treasuryCut == UnitInterval(1, 5))
    }
}
