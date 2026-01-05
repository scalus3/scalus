package scalus.cardano.ledger

import scala.quoted.*

/** ToExpr instances for ProtocolParams and its nested types.
  *
  * These instances enable compile-time generation of ProtocolParams from JSON, avoiding the need to
  * embed JSON strings in the compiled artifacts.
  */
object ProtocolParamsToExpr {

    given ToExpr[ProtocolVersion] with
        def apply(pv: ProtocolVersion)(using Quotes): Expr[ProtocolVersion] =
            '{ ProtocolVersion(${ Expr(pv.major) }, ${ Expr(pv.minor) }) }

    given ToExpr[ExUnits] with
        def apply(eu: ExUnits)(using Quotes): Expr[ExUnits] =
            '{ ExUnits(${ Expr(eu.memory) }, ${ Expr(eu.steps) }) }

    given ToExpr[UnitInterval] with
        def apply(ui: UnitInterval)(using Quotes): Expr[UnitInterval] =
            '{ UnitInterval(${ Expr(ui.numerator) }, ${ Expr(ui.denominator) }) }

    given ToExpr[NonNegativeInterval] with
        def apply(nni: NonNegativeInterval)(using Quotes): Expr[NonNegativeInterval] =
            '{ NonNegativeInterval(${ Expr(nni.numerator) }, ${ Expr(nni.denominator) }) }

    given ToExpr[ExUnitPrices] with
        def apply(eup: ExUnitPrices)(using Quotes): Expr[ExUnitPrices] =
            '{
                ExUnitPrices(
                  ${ Expr(eup.priceMemory) },
                  ${ Expr(eup.priceSteps) }
                )
            }

    given ToExpr[PoolVotingThresholds] with
        def apply(pvt: PoolVotingThresholds)(using Quotes): Expr[PoolVotingThresholds] =
            '{
                PoolVotingThresholds(
                  ${ Expr(pvt.motionNoConfidence) },
                  ${ Expr(pvt.committeeNormal) },
                  ${ Expr(pvt.committeeNoConfidence) },
                  ${ Expr(pvt.hardForkInitiation) },
                  ${ Expr(pvt.ppSecurityGroup) }
                )
            }

    given ToExpr[DRepVotingThresholds] with
        def apply(dvt: DRepVotingThresholds)(using Quotes): Expr[DRepVotingThresholds] =
            '{
                DRepVotingThresholds(
                  ${ Expr(dvt.motionNoConfidence) },
                  ${ Expr(dvt.committeeNormal) },
                  ${ Expr(dvt.committeeNoConfidence) },
                  ${ Expr(dvt.updateToConstitution) },
                  ${ Expr(dvt.hardForkInitiation) },
                  ${ Expr(dvt.ppNetworkGroup) },
                  ${ Expr(dvt.ppEconomicGroup) },
                  ${ Expr(dvt.ppTechnicalGroup) },
                  ${ Expr(dvt.ppGovGroup) },
                  ${ Expr(dvt.treasuryWithdrawal) }
                )
            }

    given ToExpr[CostModels] with
        def apply(cm: CostModels)(using Quotes): Expr[CostModels] =
            val modelsExpr = Expr.ofList(cm.models.toList.map { case (langId, costs) =>
                val costsExpr = Expr.ofList(costs.toList.map(Expr(_)))
                '{ (${ Expr(langId) }, ${ costsExpr }.toIndexedSeq) }
            })
            '{ CostModels(${ modelsExpr }.toMap) }

    given ToExpr[ProtocolParams] with
        def apply(pp: ProtocolParams)(using Quotes): Expr[ProtocolParams] =
            '{
                ProtocolParams(
                  collateralPercentage = ${ Expr(pp.collateralPercentage) },
                  committeeMaxTermLength = ${ Expr(pp.committeeMaxTermLength) },
                  committeeMinSize = ${ Expr(pp.committeeMinSize) },
                  costModels = ${ Expr(pp.costModels) },
                  dRepActivity = ${ Expr(pp.dRepActivity) },
                  dRepDeposit = ${ Expr(pp.dRepDeposit) },
                  dRepVotingThresholds = ${ Expr(pp.dRepVotingThresholds) },
                  executionUnitPrices = ${ Expr(pp.executionUnitPrices) },
                  govActionDeposit = ${ Expr(pp.govActionDeposit) },
                  govActionLifetime = ${ Expr(pp.govActionLifetime) },
                  maxBlockBodySize = ${ Expr(pp.maxBlockBodySize) },
                  maxBlockExecutionUnits = ${ Expr(pp.maxBlockExecutionUnits) },
                  maxBlockHeaderSize = ${ Expr(pp.maxBlockHeaderSize) },
                  maxCollateralInputs = ${ Expr(pp.maxCollateralInputs) },
                  maxTxExecutionUnits = ${ Expr(pp.maxTxExecutionUnits) },
                  maxTxSize = ${ Expr(pp.maxTxSize) },
                  maxValueSize = ${ Expr(pp.maxValueSize) },
                  minFeeRefScriptCostPerByte = ${ Expr(pp.minFeeRefScriptCostPerByte) },
                  minPoolCost = ${ Expr(pp.minPoolCost) },
                  monetaryExpansion = ${ Expr(pp.monetaryExpansion) },
                  poolPledgeInfluence = ${ Expr(pp.poolPledgeInfluence) },
                  poolRetireMaxEpoch = ${ Expr(pp.poolRetireMaxEpoch) },
                  poolVotingThresholds = ${ Expr(pp.poolVotingThresholds) },
                  protocolVersion = ${ Expr(pp.protocolVersion) },
                  stakeAddressDeposit = ${ Expr(pp.stakeAddressDeposit) },
                  stakePoolDeposit = ${ Expr(pp.stakePoolDeposit) },
                  stakePoolTargetNum = ${ Expr(pp.stakePoolTargetNum) },
                  treasuryCut = ${ Expr(pp.treasuryCut) },
                  txFeeFixed = ${ Expr(pp.txFeeFixed) },
                  txFeePerByte = ${ Expr(pp.txFeePerByte) },
                  utxoCostPerByte = ${ Expr(pp.utxoCostPerByte) }
                )
            }
}
