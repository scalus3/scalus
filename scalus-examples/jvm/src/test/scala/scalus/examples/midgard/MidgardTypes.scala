package scalus.examples.midgard

import scalus.cardano.onchain.plutus.v3.{Address, PolicyId, TxOutRef}
import scalus.uplc.builtin.{ByteString, Data}

// Midgard types the da-bond-pool fixtures build, ported from `onchain/aiken/lib/midgard`. The
// validator reads them as raw `Data`; these exist so the fixtures encode them by name.

/** `hub_oracle.Datum`. */
case class HubOracleDatum(
    registeredOperators: PolicyId,
    activeOperators: PolicyId,
    retiredOperators: PolicyId,
    scheduler: PolicyId,
    stateQueue: PolicyId,
    fraudProofCatalogue: PolicyId,
    fraudProof: PolicyId,
    deposit: PolicyId,
    withdrawal: PolicyId,
    txOrder: PolicyId,
    settlement: PolicyId,
    payout: PolicyId,
    registeredOperatorsAddr: Address,
    activeOperatorsAddr: Address,
    retiredOperatorsAddr: Address,
    schedulerAddr: Address,
    stateQueueAddr: Address,
    fraudProofCatalogueAddr: Address,
    fraudProofAddr: Address,
    depositAddr: Address,
    withdrawalAddr: Address,
    txOrderAddr: Address,
    settlementAddr: Address,
    reserveAddr: Address,
    payoutAddr: Address,
    reserveObserver: ByteString
) derives Data.ToData

/** `correction_lock.CorrectionIdentity`. */
enum CorrectionIdentity derives Data.ToData:
    case FraudProof(fraudProofAssetName: ByteString)
    case AttestationTimeout
    case AvailabilityChallenge(challengeAssetName: ByteString)

/** `correction_lock.Datum`. */
enum CorrectionLockDatum derives Data.ToData:
    case Idle
    case Locked(targetHeaderHash: ByteString, correctionIdentity: CorrectionIdentity)

/** `state_queue.UnattestedTimeoutRemovalApproach`. */
enum UnattestedTimeoutRemovalApproach derives Data.ToData:
    case PruneUnattestedBlockDescendant(
        predecessorRefInputIndex: BigInt,
        timedOutNodeInputOutref: TxOutRef,
        timedOutNodeOutputIndex: BigInt
    )
    case RemoveLastUnattestedBlock(predecessorInputOutref: TxOutRef, predecessorOutputIndex: BigInt)

/** `state_queue.AttestationTimeoutRemovalApproach`. */
enum AttestationTimeoutRemovalApproach derives Data.ToData:
    case PruneTimedOutBlockDescendant(
        confirmedStateRefInputIndex: BigInt,
        timedOutNodeInputOutref: TxOutRef,
        timedOutNodeOutputIndex: BigInt
    )
    case RemoveTimedOutHead(confirmedStateInputOutref: TxOutRef, confirmedStateOutputIndex: BigInt)

/** `state_queue.MintRedeemer`, up to constructor 5. The pool reads only the constructor index, so
  * constructors 0 to 3 omit their fields and the constructors after 5 are left out.
  */
enum StateQueueMintRedeemer derives Data.ToData:
    case InitV1
    case Deinit
    case CommitBlockHeader
    case RemoveFraudulentBlockHeader
    case RemoveUnattestedBlockAfterTimeout(
        yieldToRefInputIndex: BigInt,
        timedOutHeaderHash: ByteString,
        removalApproach: UnattestedTimeoutRemovalApproach
    )
    case RemoveUnavailableBlockAfterTimeout(
        yieldToRefInputIndex: BigInt,
        unavailableHeaderHash: ByteString,
        challengeAssetName: ByteString,
        removalApproach: AttestationTimeoutRemovalApproach
    )
