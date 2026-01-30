package scalus.cardano.ledger

import io.bullet.borer.Dom.Element
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.utils.CollateralSufficient

// TODO: maybe replace on enum
sealed abstract class TransactionException(message: String, cause: Throwable)
    extends RuntimeException(message, cause) {
    def this(message: String) = this(message, null)
    def explain: String = message
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
        invalidOutputs: Seq[(TransactionOutput, Coin, MultiAsset)],
        invalidCollateralOutput: Option[(TransactionOutput, Coin, MultiAsset)]
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

    /** It's Babbage.FeeTooSmallUTxO && Alonzo.ScriptsNotPaidUTxO &&
      * Babbage.CollateralContainsNonADA && Alonzo.InsufficientCollateral &&
      * Babbage.IncorrectTotalCollateralField && Babbage.NoCollateralInputs in cardano-ledger
      */
    final case class FeesOkException(
        transactionId: TransactionHash,
        transactionFee: Coin,
        minRequiredFee: Coin,
        collateralPercentage: Long,
        areTotalExUnitsZero: Boolean, // 2) If the total ExUnits are 0 in both Memory and Steps, no further part needs to be checked.
        collateralReturnOutput: Option[TransactionOutput] = None,
        actualTotalSumOfCollateralCoins: Coin = Coin.zero,
        expectedTotalSumOfCollateralCoins: Option[Coin] = None,
        collateralsConsistNotVKeyAddress: Utxos = Utxos.empty,
        collateralsContainNotOnlyADA: Utxos = Utxos.empty,
        remainingAssets: MultiAsset = MultiAsset.empty,
        hasCollateralInputs: Boolean = false
    ) extends TransactionException({
            val failures = Seq(
              Option.when(transactionFee < minRequiredFee)(
                s"fee $transactionFee is less than minimum required $minRequiredFee"
              ),
              Option.when(!areTotalExUnitsZero && collateralsConsistNotVKeyAddress.nonEmpty)(
                s"collateral contains non-VKey addresses: $collateralsConsistNotVKeyAddress"
              ),
              Option.when(!areTotalExUnitsZero && remainingAssets.nonEmpty)(
                s"collateral contains non-ADA assets: $remainingAssets (UTxOs: $collateralsContainNotOnlyADA, collateral return: $collateralReturnOutput)"
              ),
              Option.when(
                !areTotalExUnitsZero && !CollateralSufficient.check(
                  actualTotalSumOfCollateralCoins,
                  transactionFee,
                  collateralPercentage
                )
              )(
                s"collateral $actualTotalSumOfCollateralCoins is insufficient (required $collateralPercentage% of fee $transactionFee)"
              ),
              Option.when(
                !areTotalExUnitsZero && expectedTotalSumOfCollateralCoins.exists(
                  actualTotalSumOfCollateralCoins != _
                )
              )(
                s"collateral $actualTotalSumOfCollateralCoins does not match expected $expectedTotalSumOfCollateralCoins"
              ),
              Option.when(!areTotalExUnitsZero && !hasCollateralInputs)(
                "no collateral inputs provided"
              )
            ).flatten

            s"Fee or collateral validation failed for transaction $transactionId: ${failures.mkString("; ")}"
        }) {
        // 1) The fee paid is >= the minimum fee
        def isTransactionFeeLessThanMinRequiredFee: Boolean = transactionFee < minRequiredFee

        // 3) The collateral consists only of VKey addresses
        def hasCollateralsConsistNotVKeyAddress: Boolean =
            !areTotalExUnitsZero && collateralsConsistNotVKeyAddress.nonEmpty

        // 4) The collateral inputs do not contain any non-ADA part
        def hasCollateralsContainNotOnlyADA: Boolean =
            !areTotalExUnitsZero && remainingAssets.nonEmpty

        // 5) The collateral is sufficient to cover the appropriate percentage of the fee marked in the transaction
        def isCollateralInsufficient: Boolean =
            !areTotalExUnitsZero && !CollateralSufficient.check(
              actualTotalSumOfCollateralCoins,
              transactionFee,
              collateralPercentage
            )

        // 6) The collateral is equivalent to total collateral asserted by the transaction
        def isCollateralNotEqualToExpected: Boolean =
            !areTotalExUnitsZero && expectedTotalSumOfCollateralCoins.exists {
                actualTotalSumOfCollateralCoins != _
            }

        // 7) There is at least one collateral input
        def areCollateralInputsMissing: Boolean = !areTotalExUnitsZero && !hasCollateralInputs
    }

    // It's Conway StakeKeyRegistered/StakeKeyNotRegistered/etc. DELEG failures in cardano-ledger
    final case class StakeCertificatesException(
        transactionId: TransactionHash,
        alreadyRegistered: Set[Credential],
        missingRegistrations: Set[Credential],
        nonZeroRewardAccounts: Map[Credential, Coin],
        invalidDeposits: Map[Credential, (Coin, Coin)],
        invalidRefunds: Map[Credential, (Coin, Coin)]
    ) extends TransactionException(
          s"""Stake certificates validation failed for transactionId $transactionId.
             |already registered credentials: $alreadyRegistered,
             |missing registrations: $missingRegistrations,
             |non-zero reward accounts: $nonZeroRewardAccounts,
             |invalid deposits (expected -> provided): $invalidDeposits,
             |invalid refunds (expected -> provided): $invalidRefunds.""".stripMargin
        )

    // It's Shelley/Conway poolRule failures (PoolDisallowed operation) in cardano-ledger
    final case class StakePoolException(
        transactionId: TransactionHash,
        notRegistered: Set[PoolKeyHash],
        rewardAccountNetworkMismatch: Set[(PoolKeyHash, Network, Network)],
        costBelowMinimum: Map[PoolKeyHash, (Coin, Coin)],
        invalidRetirementEpochs: Map[PoolKeyHash, (Long, Long, Long)]
    ) extends TransactionException(
          s"""Pool certificate validation failed for transactionId $transactionId.
             |not registered: $notRegistered,
             |reward account network mismatches (pool, rewardNetwork, expectedNetwork): $rewardAccountNetworkMismatch,
             |costs below minimum (pool -> (min, provided)): $costBelowMinimum,
             |invalid retirement epochs (pool -> (epoch, current, maxAllowed)): $invalidRetirementEpochs.""".stripMargin
        )

    // It's Alonzo.ExUnitsTooBigUTxO in cardano-ledger
    final case class ExUnitsExceedMaxException(
        transactionId: TransactionHash,
        actualTxExecutionUnits: ExUnits,
        maxTxExecutionUnits: ExUnits
    ) extends TransactionException(
          s"Execution units for transaction $transactionId exceed the maximum. Actual: $actualTxExecutionUnits, maximum: $maxTxExecutionUnits"
        )

    // It's Alonzo.TooManyCollateralInputs in cardano-ledger
    final case class TooManyCollateralInputsException(
        transactionId: TransactionHash,
        actualCollateralInputsSize: Int,
        maxCollateralInputsSize: Long
    ) extends TransactionException(
          s"Too many collateral inputs for transactionId $transactionId. Actual: $actualCollateralInputsSize, maximum: $maxCollateralInputsSize"
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
        invalidOutputAddresses: Seq[Address],
        invalidCollateralReturnAddresses: Option[Address]
    ) extends TransactionException(
          s"Wrong network address in transaction $transactionId, invalid output addresses: $invalidOutputAddresses, invalid collateral return addresses: $invalidCollateralReturnAddresses"
        )

    // It's part of Shelley.validateWrongNetworkWithdrawal in cardano-ledger
    final case class WrongNetworkWithdrawal(
        transactionId: TransactionHash,
        invalidWithdrawals: Seq[(RewardAccount, Coin)]
    ) extends TransactionException(
          s"Wrong network in withdrawals for transactionId $transactionId, invalid withdrawals: $invalidWithdrawals"
        )

    // It's part of Alonzo.validateWrongNetworkInTxBody in cardano-ledger
    final case class WrongNetworkInTxBody(
        transactionId: TransactionHash,
        actualNetworkId: Int,
        expectedNetworkId: Int
    ) extends TransactionException(
          s"Wrong network id in transaction body for transactionId $transactionId. Actual: $actualNetworkId, expected: $expectedNetworkId"
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
        inputsWithMissingDatumHashes: Set[TransactionInput],
        unmatchedDatumHashes: Set[DataHash],
        notAllowedSupplementalDatumHashes: Set[DataHash]
    ) extends TransactionException(
          s"Datums validation failed for transactionId $transactionId, inputs with missing datum hashes: $inputsWithMissingDatumHashes, unmatched datum hashes: $unmatchedDatumHashes, not allowed supplemental datum hashes: $notAllowedSupplementalDatumHashes"
        )

    final case class OutputBootAddrAttrsTooBigException(
        transactionId: TransactionHash,
        outputsWithOversizedAttrs: Seq[scalus.cardano.address.Address],
        maxAllowedSize: Int
    ) extends TransactionException(
          s"Bootstrap address attributes too big for transaction $transactionId: " +
              s"outputs with oversized attrs: $outputsWithOversizedAttrs, max allowed size: $maxAllowedSize"
        )

    /** Plutus script validation failure with execution logs.
      *
      * This exception is thrown when a Plutus script fails validation and the transaction has
      * `isValid = true`.
      */
    final case class PlutusScriptValidationException(
        transactionId: TransactionHash,
        message: String,
        logs: Seq[String]
    ) extends TransactionException(
          s"Plutus script validation failed for transactionId $transactionId: $message"
        )

    // TODO: placeholder for general exception, remove after finishing development
    final case class IllegalArgumentException(message: String) extends TransactionException(message)
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
    stakePools: Map[PoolKeyHash, Certificate.PoolRegistration] = Map.empty,
    retiring: Map[PoolKeyHash, EpochNo] = Map.empty,
    deposits: Map[PoolKeyHash, Coin] = Map.empty
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
