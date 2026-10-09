package scalus.examples.midgard

import scalus.cardano.onchain.plutus.prelude.List
import scalus.cardano.onchain.plutus.v3.{PolicyId, PosixTime, PubKeyHash, TxOutRef}
import scalus.compiler.Compile
import scalus.uplc.builtin.{ByteString, Data}

/** Datum of the pooled DA committee bond. Port of `midgard/da_bond_pool.DaBondPoolDatum`. */
enum DaBondPoolDatum derives Data.FromData, Data.ToData:
    case Bonded
    case Withdrawing(unlockAt: PosixTime)

enum DaBondPoolMintRedeemer derives Data.FromData, Data.ToData:
    case InitPool(outputIndex: BigInt)

enum DaBondPoolSpendRedeemer derives Data.FromData, Data.ToData:
    case TopUp(outputIndex: BigInt)
    case Slash(
        hubOracleRefInputIndex: BigInt,
        stateQueueMintRedeemerIndex: BigInt,
        correctionLockInputIndex: BigInt,
        outputIndex: BigInt
    )
    case BeginWithdraw(daParamsRefInputIndex: BigInt, outputIndex: BigInt)
    case CancelWithdraw(daParamsRefInputIndex: BigInt, outputIndex: BigInt)
    case CompleteWithdraw(amount: BigInt, daParamsRefInputIndex: BigInt, outputIndex: BigInt)

/** Port of `midgard/availability_challenge.ResponseGeometryV1`. */
case class ResponseGeometryV1(
    chunkByteLength: BigInt,
    trancheByteLength: BigInt,
    maxTrancheCount: BigInt
) derives Data.FromData,
      Data.ToData

/** Port of `midgard/availability_challenge.ParametersV1`. The pool reads three fields. */
case class ParametersV1(
    responseGeometry: ResponseGeometryV1,
    daBondLovelace: BigInt,
    challengerBondLovelace: BigInt,
    maxOpenFeeLovelace: BigInt,
    maxPublicationFeeLovelace: BigInt,
    maxSettlementFeeLovelace: BigInt,
    maxCloseFeeLovelace: BigInt,
    maxTimeoutFeeLovelace: BigInt,
    daSlashPenaltyLovelace: BigInt,
    daBondMinTopUpLovelace: BigInt,
    daBondPoolFloorLovelace: BigInt,
    challengeRecordLovelace: BigInt
) derives Data.FromData,
      Data.ToData

/** Port of `midgard/da_attestation_types.DaParamsDatum`. */
case class DaParamsDatum(
    committee: ByteString,
    committeeSignersHash: ByteString,
    daThreshold: BigInt,
    owners: List[PubKeyHash],
    updateThreshold: BigInt
) derives Data.FromData,
      Data.ToData

/** The four validator parameters, grouped off-chain only. Both the Aiken and the Scalus script take
  * them as four separate `Data` arguments; see `DaBondPoolContract.applyParams`.
  */
case class DaBondPoolParams(
    initRef: TxOutRef,
    hubOraclePolicyId: PolicyId,
    daParamsPolicyId: PolicyId,
    parameters: ParametersV1
)

@Compile
object DaBondPoolConstants {
    val poolAssetName: ByteString = ByteString.fromString("MIDGARD_DA_BOND_POOL")
    val hubOracleAssetName: ByteString = ByteString.fromString("MIDGARD_HUB_ORACLE")
    val correctionLockAssetName: ByteString = ByteString.fromString("MIDGARD_CORRECTION_LOCK")
    val daParamsAssetName: ByteString = ByteString.fromString("MIDGARD_DA_PARAMS")

    /** Constructor index of state-queue `RemoveUnavailableBlockAfterTimeout`. */
    val removeUnavailableBlockAfterTimeoutConstr: BigInt = 5

    /** `env/default.ak` `da_bond_withdraw_delay_ms_v1`. */
    val daBondWithdrawDelayMs: BigInt = 778_080_000

    /** `env/default.ak` `max_validity_range_length`. */
    val maxValidityRangeLength: BigInt = 480_000
}
