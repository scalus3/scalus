package scalus.cardano.ledger

import io.bullet.borer.Dom.Element
import scalus.cardano.address.{Address, Network}

// TODO: maybe replace on enum
sealed abstract class TransactionException(message: String, cause: Throwable)
    extends RuntimeException(message, cause) {
    def this(message: String) = this(message, null)
}

object TransactionException {
    // It's Shelley.InputSetEmptyUTxO in cardano-ledger
    final case class EmptyInputsException(transactionId: TransactionHash)
        extends TransactionException(s"Empty transaction inputs for transactionId $transactionId")

    // It's BabbageNonDisjointRefInputs in cardano-ledger
    final case class NonDisjointInputsAndReferenceInputsException(
        transactionId: TransactionHash,
        intersection: Set[TransactionInput]
    ) extends TransactionException(
          s"Inputs intersects with reference inputs for transactionId $transactionId, intersection: $intersection"
        )

    // It's Shelley.BadInputsUTxO in cardano-ledger
    final case class BadAllInputsUTxOException(
        transactionId: TransactionHash,
        missingInputs: Set[TransactionInput],
        missingCollateralInputs: Set[TransactionInput],
        missingReferenceInputs: Set[TransactionInput]
    ) extends TransactionException(
          s"Missing inputs, collateral inputs or reference inputs in UTxO state for transactionId $transactionId, missing inputs: $missingInputs, missing collateral inputs: $missingCollateralInputs, missing reference inputs: $missingReferenceInputs"
        )

    final case class BadInputsUTxOException(
        transactionId: TransactionHash
    ) extends TransactionException(
          s"Missing inputs in UTxO state for transactionId $transactionId"
        )

    final case class BadCollateralInputsUTxOException(
        transactionId: TransactionHash
    ) extends TransactionException(
          s"Missing collateral inputs in UTxO state for transactionId $transactionId"
        )

    final case class BadReferenceInputsUTxOException(
        transactionId: TransactionHash
    ) extends TransactionException(
          s"Missing reference inputs in UTxO state for transactionId $transactionId"
        )

    // It's Shelley.InvalidWitnessesUTXOW in cardano-ledger
    final case class InvalidSignaturesInWitnessesException(
        transactionId: TransactionHash,
        invalidVkeyWitnesses: Set[VKeyWitness],
        invalidBootstrapWitnesses: Set[BootstrapWitness]
    ) extends TransactionException(
          s"Invalid verified signatures in witnesses for transactionId $transactionId, invalid vkey witnesses: $invalidVkeyWitnesses, invalid bootstrap witnesses: $invalidBootstrapWitnesses"
        )

    // It's Shelley.MissingVKeyWitnessesUTXOW in cardano-ledger
    final case class MissingKeyHashesException(
        transactionId: TransactionHash,
        missingInputsKeyHashes: Set[AddrKeyHash | StakeKeyHash],
        missingCollateralInputsKeyHashes: Set[AddrKeyHash | StakeKeyHash],
        missingVotingProceduresKeyHashes: Set[AddrKeyHash],
        missingWithdrawalsKeyHashes: Set[AddrKeyHash | StakeKeyHash],
        missingCertificatesKeyHashes: Set[AddrKeyHash | PoolKeyHash],
        missingRequiredSignersKeyHashes: Set[AddrKeyHash]
    ) extends TransactionException(
          s"Missing key hashes for transactionId $transactionId, missing inputs key hashes: $missingInputsKeyHashes, missing collateral inputs key hashes: $missingCollateralInputsKeyHashes, missing voting procedures key hashes: $missingVotingProceduresKeyHashes, missing withdrawals key hashes: $missingWithdrawalsKeyHashes, missing certificates key hashes: $missingCertificatesKeyHashes, missing required signers key hashes: $missingRequiredSignersKeyHashes"
        )

    // It's Shelley.MissingScriptWitnessesUTXOW and Shelley.ExtraneousScriptWitnessesUTXOW in cardano-ledger
    final case class MissingOrExtraScriptHashesException(
        transactionId: TransactionHash,
        missingInputsScriptHashes: Set[ScriptHash],
        missingMintScriptHashes: Set[ScriptHash],
        missingVotingProceduresScriptHashes: Set[ScriptHash],
        missingWithdrawalsScriptHashes: Set[ScriptHash],
        missingProposalProceduresScriptHashes: Set[ScriptHash],
        missingCertificatesScriptHashes: Set[ScriptHash],
        extraScriptHashes: Set[ScriptHash]
    ) extends TransactionException(
          s"Missing or extra script hashes for transactionId $transactionId, missing inputs script hashes: $missingInputsScriptHashes, missing mint script hashes: $missingMintScriptHashes, missing voting procedures script hashes: $missingVotingProceduresScriptHashes, missing withdrawals script hashes: $missingWithdrawalsScriptHashes, missing proposal procedures script hashes: $missingProposalProceduresScriptHashes, missing certificates script hashes: $missingCertificatesScriptHashes, extra script hashes: $extraScriptHashes"
        )

    // It's Shelley.ScriptWitnessNotValidatingUTXOW in cardano-ledger
    final case class NativeScriptsException(
        transactionId: TransactionHash,
        invalidWitnessesNativeScripts: Set[ScriptHash],
        invalidProvidedReferenceNativeScripts: Set[ScriptHash],
    ) extends TransactionException(
          s"Invalid native scripts for transactionId $transactionId, invalid witnesses native scripts: $invalidWitnessesNativeScripts, invalid provided reference native scripts: $invalidProvidedReferenceNativeScripts"
        )

    // It's Shelley.MaxTxSizeUTxO in cardano-ledger
    final case class InvalidTransactionSizeException(
        transactionId: TransactionHash,
        transactionSize: Int,
        maxTransactionSize: Long
    ) extends TransactionException(
          s"Transaction size $transactionSize exceeds maximum allowed size $maxTransactionSize for transactionId $transactionId"
        )

    // It's BabbageOutputTooSmallUTxO in cardano-ledger
    final case class OutputsHaveNotEnoughCoinsException(
        transactionId: TransactionHash,
        invalidOutputs: Seq[(TransactionOutput, Coin)],
        invalidCollateralOutput: Option[(TransactionOutput, Coin)]
    ) extends TransactionException(
          s"Transaction outputs are too small for transactionId $transactionId, invalid outputs: $invalidOutputs, invalid collateral output: $invalidCollateralOutput"
        )

    // It's Alonzo.OutputTooBigUTxO in cardano-ledger
    final case class OutputsHaveTooBigValueStorageSizeException(
        transactionId: TransactionHash,
        maxValueSize: Long,
        invalidOutputs: Seq[(TransactionOutput, Int)],
        invalidCollateralOutput: Option[(TransactionOutput, Int)]
    ) extends TransactionException(
          s"Transaction outputs exceed maximum value storage size $maxValueSize for transactionId $transactionId, invalid outputs: $invalidOutputs, invalid collateral output: $invalidCollateralOutput"
        )

    // It's Allegra.OutsideValidityIntervalUTxO in cardano-ledger
    final case class OutsideValidityIntervalException(
        transactionId: TransactionHash,
        validityInterval: ValidityInterval,
        slot: SlotNo
    ) extends TransactionException(
          s"Transaction $transactionId is outside the validity interval $validityInterval for slot $slot"
        )

    // It's Shelley.ValueNotConservedUTxO in cardano-ledger
    final case class ValueNotConservedUTxOException(
        transactionId: TransactionHash,
        consumed: Value,
        produced: Value
    ) extends TransactionException(
          s"Value not conserved for transactionId $transactionId, consumed: $consumed, produced: $produced"
        )

    // It's Babbage.FeeTooSmallUTxO in cardano-ledger
    final case class FeeTooSmallException(
        transactionId: TransactionHash,
        transactionFee: Coin,
        minTransactionFee: Coin
    ) extends TransactionException(
          s"Transaction fee is too small for transactionId $transactionId, transaction fee: $transactionFee, minimum transaction fee: $minTransactionFee"
        )

    // It's Alonzo.ScriptsNotPaidUTxO in cardano-ledger
    final case class CollateralsConsistNotOnlyVKeyAddressException(
        transactionId: TransactionHash,
        invalidCollaterals: Set[(TransactionInput, TransactionOutput)]
    ) extends TransactionException(
          s"Collaterals consist not only VKey addresses for transactionId $transactionId, invalid collaterals: $invalidCollaterals"
        )

    // It's Babbage.CollateralContainsNonADA in cardano-ledger
    final case class CollateralsContainNotOnlyADAException(
        transactionId: TransactionHash,
        invalidCollaterals: Set[(TransactionInput, TransactionOutput)],
        collateralReturnOutput: Option[TransactionOutput]
    ) extends TransactionException(
          s"Collaterals contain non-ADA assets for transactionId $transactionId, invalid collaterals: $invalidCollaterals, collateral return output: $collateralReturnOutput"
        )

    // It's Alonzo.InsufficientCollateral in cardano-ledger
    final case class InsufficientTotalSumOfCollateralCoinsException(
        transactionId: TransactionHash,
        totalSumOfCollateralCoins: Coin,
        collateralReturnOutput: Option[TransactionOutput],
        transactionFee: Coin,
        collateralPercentage: Long
    ) extends TransactionException(
          s"Insufficient total sum of collateral coins for transactionId $transactionId, total sum of collateral coins: $totalSumOfCollateralCoins, collateral return output: $collateralReturnOutput, transaction fee: $transactionFee, collateral percentage: $collateralPercentage"
        )

    // It's Babbage.IncorrectTotalCollateralField in cardano-ledger
    final case class IncorrectTotalCollateralException(
        transactionId: TransactionHash,
        totalSumOfCollateralCoins: Coin,
        totalCollateral: Option[Coin]
    ) extends TransactionException(
          s"Incorrect total collateral for transactionId $transactionId, total sum of collateral coins: $totalSumOfCollateralCoins, total collateral: $totalCollateral"
        )

    // It's Alonzo.ExUnitsTooBigUTxO in cardano-ledger
    final case class ExUnitsExceedMaxException(
        transactionId: TransactionHash,
        actualTxExecutionUnits: ExUnits,
        maxTxExecutionUnits: ExUnits
    ) extends TransactionException(
          s"Execution units for transaction $transactionId exceed the maximum. Actual: $actualTxExecutionUnits, maximum: $maxTxExecutionUnits"
        )

    // It's Babbage.NoCollateralInputs in cardano-ledger
    final case class NoCollateralInputsException(transactionId: TransactionHash)
        extends TransactionException(s"No collateral inputs for transactionId $transactionId")

    // It's Alonzo.TooManyCollateralInputs in cardano-ledger
    final case class TooManyCollateralInputsException(
        transactionId: TransactionHash,
        actual: Int,
        expected: Long
    ) extends TransactionException(
          s"Too many collateral inputs for transactionId $transactionId. Actual: $actual, expected at most: $expected"
        )

    // It's Alonzo.PPViewHashesDontMatch in cardano-ledger
    final case class InvalidScriptDataHashException(
        transactionId: TransactionHash,
        actual: Option[ScriptDataHash],
        expected: Option[ScriptDataHash]
    ) extends TransactionException(
          s"Invalid script data hash for transactionId $transactionId. Actual: $actual, expected: $expected"
        )

    // It's Babbage.MalformedScriptWitnesses and Babbage.MalformedReferenceScripts in cardano-ledger
    final case class IllFormedScriptsException(
        transactionId: TransactionHash,
        invalidWitnessesScripts: Set[ScriptHash],
        InvalidScriptsFromAllOutputs: Set[ScriptHash]
    ) extends TransactionException(
          s"Ill-formed scripts for transactionId $transactionId, invalid witnesses scripts: $invalidWitnessesScripts, invalid scripts from all outputs: $InvalidScriptsFromAllOutputs"
        )

    // It's part of Shelley.validateWrongNetwork in cardano-ledger
    final case class WrongNetworkAddress(
        transactionId: TransactionHash,
        wrongNetworkAddress: Address
    ) extends TransactionException(
          s"Wrong network address $wrongNetworkAddress in transaction $transactionId"
        )

    // It's part of Shelley.validateWrongNetworkWithdrawal in cardano-ledger
    final case class WrongNetworkWithdrawal(transactionId: TransactionHash, wrongNetwork: Network)
        extends TransactionException(
          s"Wrong network withdrawal $wrongNetwork in transaction $transactionId"
        )

    // It's part of Alonzo.validateWrongNetworkInTxBody in cardano-ledger
    final case class WrongNetworkInTxBody(transactionId: TransactionHash, wrongNetworkId: Int)
        extends TransactionException(
          s"Wrong networkId $wrongNetworkId in body of transaction $transactionId"
        )

    sealed class MetadataException(message: String) extends TransactionException(message)
    object MetadataException {
        // It's Shelley.MissingTxMetadata in cardano-ledger
        final case class MissingAuxiliaryDataException(
            transactionId: TransactionHash,
            auxiliaryDataHash: AuxiliaryDataHash
        ) extends MetadataException(
              s"Missing auxiliary data for transactionId $transactionId, auxiliary data hash: $auxiliaryDataHash"
            )

        // It's Shelley.MissingTxBodyMetadataHash in cardano-ledger
        final case class MissingAuxiliaryDataHashException(
            transactionId: TransactionHash,
            auxiliaryData: AuxiliaryData
        ) extends MetadataException(
              s"Missing auxiliary data hash for transactionId $transactionId, auxiliary data: $auxiliaryData"
            )

        // It's Shelley.ConflictingMetadataHash in cardano-ledger
        final case class InvalidAuxiliaryDataHashException(
            transactionId: TransactionHash,
            actual: AuxiliaryDataHash,
            expected: AuxiliaryDataHash
        ) extends MetadataException(
              s"Invalid auxiliary data hash for transactionId $transactionId. Actual: $actual, expected: $expected"
            )

        // It's Shelley.InvalidMetadata in cardano-ledger
        final case class InvalidAuxiliaryDataException(
            transactionId: TransactionHash,
            auxiliaryData: AuxiliaryData
        ) extends MetadataException(
              s"Invalid auxiliary data for transactionId $transactionId, auxiliary data: $auxiliaryData"
            )
    }

    // It's Alonzo.ExtraRedeemers and Alonzo.MissingRedeemers in cardano-ledger
    final case class ExactSetOfRedeemersException(
        transactionId: TransactionHash,
        extraRedeemers: Set[(RedeemerTag, Int)],
        missingRedeemers: Set[(RedeemerTag, Int)]
    ) extends TransactionException(
          s"Exact set of redeemers validation failed for transactionId $transactionId, extra redeemers: $extraRedeemers, missing redeemers: $missingRedeemers"
        )

    // It's Alonzo UnspendableUTxONoDatumHash && MissingRequiredDatums && NotAllowedSupplementalDatums in cardano-ledger
    final case class DatumsException(
        transactionId: TransactionHash,
        inputsWithMissingDatums: Set[TransactionInput],
        unmatchedDatumHashes: Set[DataHash],
        notAllowedSupplementalDatumHashes: Set[DataHash]
    ) extends TransactionException(
          s"Datums validation failed for transactionId $transactionId, inputs with missing datums: $inputsWithMissingDatums, unmatched datum hashes: $unmatchedDatumHashes, not allowed supplemental datum hashes: $notAllowedSupplementalDatumHashes"
        )

    // TODO: placeholder for general exception, remove after finishing development
    final case class IllegalArgumentException(message: String) extends TransactionException(message)
}

@deprecated("Use Utxos instead", "0.12.1")
type UTxO = Map[TransactionInput, TransactionOutput]

type Utxos = Map[TransactionInput, TransactionOutput]
object Utxos:
    def empty: Utxos = Map.empty

/** Unspent Transaction Output
  *
  * @note
  *   It's common to use UTXO as a resolved transaction input-output pair, regardless whether it's
  *   spent or unspent. We use [[Utxo]] in both cases for simplicity.
  */
case class Utxo(input: TransactionInput, output: TransactionOutput) {
    def toTuple: (TransactionInput, TransactionOutput) = (input, output)
}

object Utxo {
    def apply(utxo: (TransactionInput, TransactionOutput)): Utxo =
        Utxo(utxo._1, utxo._2)
}

type GovState = Array[Element]
object GovState:
    def empty: GovState = Array.empty
type StakeMap = Map[Credential, Coin]
object StakeMap:
    def empty: StakeMap = Map.empty
case class UTxOState(
    utxo: Utxos, // UtxO entries
    deposited: Coin, // Lazy field used only for assertions
    fees: Coin, // Accumulated transaction fees
    govState: GovState, // Governance state
    stakeDistribution: StakeMap, // Stake distribution
    donation: Coin // Donation amount
)

/*
data ConwayCertState era = ConwayCertState
    { conwayCertVState :: !(VState era)
        , conwayCertPState :: !(PState era)
        , conwayCertDState :: !(DState era)
    }
 */

case class CertState(
    vstate: VotingState,
    pstate: PoolsState,
    dstate: DelegationState
)
object CertState {
    def empty: CertState = CertState(
      VotingState(Map.empty),
      PoolsState(),
      DelegationState(Map.empty, Map.empty, Map.empty, Map.empty)
    )
}

/*
 * -- | The state that tracks the voting entities (DReps and Constitutional Committee
-- members). In the formal ledger specification this type is called @GState@
data VState era = VState
  { vsDReps :: !(Map (Credential 'DRepRole) DRepState)
  , vsCommitteeState :: !(CommitteeState era)
  , vsNumDormantEpochs :: !EpochNo
  -- ^ Number of contiguous epochs in which there are exactly zero
  -- active governance proposals to vote on. It is incremented in every
  -- EPOCH rule if the number of active governance proposals to vote on
  -- continues to be zero. It is reset to zero when a new governance
  -- action is successfully proposed. We need this counter in order to
  -- bump DRep expiries through dormant periods when DReps do not have
  -- an opportunity to vote on anything.
  }*/

case class VotingState(
    dreps: Map[Credential, DRepState],
    //    vsCommitteeState: CommitteeState,
    //    vsNumDormantEpochs: EpochNo
)

/*
 * data DRepState = DRepState
  { drepExpiry :: !EpochNo
  , drepAnchor :: !(StrictMaybe Anchor)
  , drepDeposit :: !Coin
  , drepDelegs :: !(Set (Credential 'Staking))
  }
 */
type EpochNo = Long
case class DRepState(
    expiry: EpochNo,
    anchor: Option[Anchor],
    deposit: Coin,
    delegates: Set[Credential]
)

/*-- | The state used by the POOL rule, which tracks stake pool information.
data PState era = PState
  { psStakePoolParams :: !(Map (KeyHash 'StakePool) PoolParams)
  -- ^ The stake pool parameters.
  , psFutureStakePoolParams :: !(Map (KeyHash 'StakePool) PoolParams)
  -- ^ The future stake pool parameters.
  -- Changes to existing stake pool parameters are staged in order
  -- to give delegators time to react to changes.
  -- See section 11.2, "Example Illustration of the Reward Cycle",
  -- of the Shelley Ledger Specification for a sequence diagram.
  , psRetiring :: !(Map (KeyHash 'StakePool) EpochNo)
  -- ^ A map of retiring stake pools to the epoch when they retire.
  , psDeposits :: !(Map (KeyHash 'StakePool) Coin)
  -- ^ A map of the deposits for each pool
  }
 */

case class PoolsState(
//    psStakePoolParams: Map[AddrKeyHash, PoolParams],
//    psFutureStakePoolParams: Map[AddrKeyHash, PoolParams],
//    psRetiring: Map[AddrKeyHash, EpochNo],
//    psDeposits: Map[AddrKeyHash, Coin]
)

/*-- | The state used by the DELEG rule, which roughly tracks stake
-- delegation and some governance features.
data DState era = DState
  { dsUnified :: !UMap
  -- ^ Unified Reward Maps. This contains the reward map (which is the source
  -- of truth regarding the registered stake credentials, the deposit map,
  -- the delegation map, and the stake credential pointer map.
  , dsFutureGenDelegs :: !(Map FutureGenDeleg GenDelegPair)
  -- ^ Future genesis key delegations
  , dsGenDelegs :: !GenDelegs
  -- ^ Genesis key delegations
  , dsIRewards :: !InstantaneousRewards
  -- ^ Instantaneous Rewards
  }
 */

case class FutureGenDeleg(slot: Slot, genesisKeyHash: AddrKeyHash)

/** Delegation State */
case class DelegationState(
    rewards: Map[Credential, Coin], // Rewards map
    deposits: Map[Credential, Coin], // Deposits map
    stakePools: Map[Credential, PoolKeyHash], // Delegation map
    dreps: Map[Credential, DRep],
//    futureGenDelegs: Map[FutureGenDeleg, GenDelegPair],
//    genDelegs: GenDelegs,
//    instantaneousRewards: InstantaneousRewards
)
