package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

/** The 1.4 Double form of rho, a0 and tau builds the same ProtocolParams as the exact form. */
class ProtocolParamsDoubleOverloadsTest extends AnyFunSuite {

    // mainnet: rho 3/1000, a0 3/10, tau 1/5
    private val p = CardanoInfo.mainnet.protocolParams

    test("the Double constructor converts rho, a0 and tau to the exact decimal fractions") {
        val built = new ProtocolParams(
          p.collateralPercentage,
          p.committeeMaxTermLength,
          p.committeeMinSize,
          p.costModels,
          p.dRepActivity,
          p.dRepDeposit,
          p.dRepVotingThresholds,
          p.executionUnitPrices,
          p.govActionDeposit,
          p.govActionLifetime,
          p.maxBlockBodySize,
          p.maxBlockExecutionUnits,
          p.maxBlockHeaderSize,
          p.maxCollateralInputs,
          p.maxTxExecutionUnits,
          p.maxTxSize,
          p.maxValueSize,
          p.minFeeRefScriptCostPerByte,
          p.minPoolCost,
          0.003,
          0.3,
          p.poolRetireMaxEpoch,
          p.poolVotingThresholds,
          p.protocolVersion,
          p.stakeAddressDeposit,
          p.stakePoolDeposit,
          p.stakePoolTargetNum,
          0.2,
          p.txFeeFixed,
          p.txFeePerByte,
          p.utxoCostPerByte
        )
        assert(built == p)
        assert(built.monetaryExpansion == UnitInterval(3, 1000))
    }

    test("the Double apply builds the same value as the exact apply") {
        val built = ProtocolParams(
          collateralPercentage = p.collateralPercentage,
          committeeMaxTermLength = p.committeeMaxTermLength,
          committeeMinSize = p.committeeMinSize,
          costModels = p.costModels,
          dRepActivity = p.dRepActivity,
          dRepDeposit = p.dRepDeposit,
          dRepVotingThresholds = p.dRepVotingThresholds,
          executionUnitPrices = p.executionUnitPrices,
          govActionDeposit = p.govActionDeposit,
          govActionLifetime = p.govActionLifetime,
          maxBlockBodySize = p.maxBlockBodySize,
          maxBlockExecutionUnits = p.maxBlockExecutionUnits,
          maxBlockHeaderSize = p.maxBlockHeaderSize,
          maxCollateralInputs = p.maxCollateralInputs,
          maxTxExecutionUnits = p.maxTxExecutionUnits,
          maxTxSize = p.maxTxSize,
          maxValueSize = p.maxValueSize,
          minFeeRefScriptCostPerByte = p.minFeeRefScriptCostPerByte,
          minPoolCost = p.minPoolCost,
          monetaryExpansion = 0.003,
          poolPledgeInfluence = 0.3,
          poolRetireMaxEpoch = p.poolRetireMaxEpoch,
          poolVotingThresholds = p.poolVotingThresholds,
          protocolVersion = p.protocolVersion,
          stakeAddressDeposit = p.stakeAddressDeposit,
          stakePoolDeposit = p.stakePoolDeposit,
          stakePoolTargetNum = p.stakePoolTargetNum,
          treasuryCut = 0.2,
          txFeeFixed = p.txFeeFixed,
          txFeePerByte = p.txFeePerByte,
          utxoCostPerByte = p.utxoCostPerByte
        )
        assert(built == p)
    }

    test("the Double copy builds the same value as the exact copy") {
        val other = p.copy(
          monetaryExpansion = UnitInterval(1, 100),
          poolPledgeInfluence = NonNegativeInterval(1, 10),
          treasuryCut = UnitInterval(1, 10)
        )
        val back = other.copy(
          p.collateralPercentage,
          p.committeeMaxTermLength,
          p.committeeMinSize,
          p.costModels,
          p.dRepActivity,
          p.dRepDeposit,
          p.dRepVotingThresholds,
          p.executionUnitPrices,
          p.govActionDeposit,
          p.govActionLifetime,
          p.maxBlockBodySize,
          p.maxBlockExecutionUnits,
          p.maxBlockHeaderSize,
          p.maxCollateralInputs,
          p.maxTxExecutionUnits,
          p.maxTxSize,
          p.maxValueSize,
          p.minFeeRefScriptCostPerByte,
          p.minPoolCost,
          0.003,
          0.3,
          p.poolRetireMaxEpoch,
          p.poolVotingThresholds,
          p.protocolVersion,
          p.stakeAddressDeposit,
          p.stakePoolDeposit,
          p.stakePoolTargetNum,
          0.2,
          p.txFeeFixed,
          p.txFeePerByte,
          p.utxoCostPerByte
        )
        assert(back == p)
        assert(p.copy(txFeeFixed = 1).txFeeFixed == 1)
    }

    test("a Double rho above 1 fails and names the field") {
        val e = intercept[IllegalArgumentException](
          p.copy(
            p.collateralPercentage,
            p.committeeMaxTermLength,
            p.committeeMinSize,
            p.costModels,
            p.dRepActivity,
            p.dRepDeposit,
            p.dRepVotingThresholds,
            p.executionUnitPrices,
            p.govActionDeposit,
            p.govActionLifetime,
            p.maxBlockBodySize,
            p.maxBlockExecutionUnits,
            p.maxBlockHeaderSize,
            p.maxCollateralInputs,
            p.maxTxExecutionUnits,
            p.maxTxSize,
            p.maxValueSize,
            p.minFeeRefScriptCostPerByte,
            p.minPoolCost,
            1.5,
            0.3,
            p.poolRetireMaxEpoch,
            p.poolVotingThresholds,
            p.protocolVersion,
            p.stakeAddressDeposit,
            p.stakePoolDeposit,
            p.stakePoolTargetNum,
            0.2,
            p.txFeeFixed,
            p.txFeePerByte,
            p.utxoCostPerByte
          )
        )
        assert(e.getMessage.contains("monetaryExpansion"))
    }
}
