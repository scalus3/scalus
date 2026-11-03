package scalus.cardano.ledger

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

    // It's Conway StakeKeyRegistered/StakeKeyNotRegistered/etc. DELEG failures in cardano-ledger.
    // `delegateeStakePoolsNotRegistered` and `delegateeDRepsNotRegistered` are
    // DelegateeStakePoolNotRegisteredDELEG and DelegateeDRepNotRegisteredDELEG.
    final case class StakeCertificatesException(
        transactionId: TransactionHash,
        alreadyRegistered: Set[Credential],
        missingRegistrations: Set[Credential],
        nonZeroRewardAccounts: Map[Credential, Coin],
        invalidDeposits: Map[Credential, (Coin, Coin)],
        invalidRefunds: Map[Credential, (Coin, Coin)],
        delegateeStakePoolsNotRegistered: Set[PoolKeyHash],
        delegateeDRepsNotRegistered: Set[Credential]
    ) extends TransactionException(
          s"""Stake certificates validation failed for transactionId $transactionId.
             |already registered credentials: $alreadyRegistered,
             |missing registrations: $missingRegistrations,
             |non-zero reward accounts: $nonZeroRewardAccounts,
             |invalid deposits (expected -> provided): $invalidDeposits,
             |invalid refunds (expected -> provided): $invalidRefunds,
             |delegatee stake pools not registered: $delegateeStakePoolsNotRegistered,
             |delegatee DReps not registered: $delegateeDRepsNotRegistered.""".stripMargin
        ) {

        /** No unregistered delegatees, as the deprecated 6-argument `apply`. */
        @deprecated(
          "Pass delegateeStakePoolsNotRegistered and delegateeDRepsNotRegistered",
          "1.3.0"
        )
        def this(
            transactionId: TransactionHash,
            alreadyRegistered: Set[Credential],
            missingRegistrations: Set[Credential],
            nonZeroRewardAccounts: Map[Credential, Coin],
            invalidDeposits: Map[Credential, (Coin, Coin)],
            invalidRefunds: Map[Credential, (Coin, Coin)]
        ) = this(
          transactionId,
          alreadyRegistered,
          missingRegistrations,
          nonZeroRewardAccounts,
          invalidDeposits,
          invalidRefunds,
          Set.empty,
          Set.empty
        )
    }

    object StakeCertificatesException {
        @deprecated(
          "Pass delegateeStakePoolsNotRegistered and delegateeDRepsNotRegistered",
          "1.3.0"
        )
        def apply(
            transactionId: TransactionHash,
            alreadyRegistered: Set[Credential],
            missingRegistrations: Set[Credential],
            nonZeroRewardAccounts: Map[Credential, Coin],
            invalidDeposits: Map[Credential, (Coin, Coin)],
            invalidRefunds: Map[Credential, (Coin, Coin)]
        ): StakeCertificatesException =
            StakeCertificatesException(
              transactionId,
              alreadyRegistered,
              missingRegistrations,
              nonZeroRewardAccounts,
              invalidDeposits,
              invalidRefunds,
              Set.empty,
              Set.empty
            )
    }

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

    /** It's `PoolMedataHashTooBig` of the POOL rule in cardano-ledger
      * (eras/shelley/impl/src/Cardano/Ledger/Shelley/Rules/Pool.hs:249-254), spec [TX-2]: a pool
      * registration whose metadata hash is longer than a Blake2b-256 hash.
      *
      * @param hashSizes
      *   the size in bytes of each metadata hash that is too long, by pool
      * @param maxHashSize
      *   the longest metadata hash allowed, in bytes
      */
    final case class PoolMetadataHashTooBigException(
        transactionId: TransactionHash,
        hashSizes: Map[PoolKeyHash, Int],
        maxHashSize: Int
    ) extends TransactionException(
          s"Pool metadata hash too big for transactionId $transactionId: " +
              s"hash sizes (pool -> bytes): $hashSizes, max allowed size: $maxHashSize"
        )

    /** It's `VRFKeyHashAlreadyRegistered` of the POOL rule in cardano-ledger
      * (eras/shelley/impl/src/Cardano/Ledger/Shelley/Rules/Pool.hs:269-271, 283-286), spec [BK-5],
      * [BK-6]: at PV >= 11, a pool registration whose VRF key another pool has, spec D19.
      *
      * @param poolId
      *   the pool of the failed registration
      * @param vrfKeyHash
      *   its VRF key
      */
    final case class VrfKeyHashAlreadyRegisteredException(
        transactionId: TransactionHash,
        poolId: PoolKeyHash,
        vrfKeyHash: VrfKeyHash
    ) extends TransactionException(
          s"VRF key hash already registered for transactionId $transactionId: " +
              s"pool $poolId, VRF key hash $vrfKeyHash"
        )

    // It's Conway GOVCERT failures (ConwayGovCertPredFailure) in cardano-ledger
    final case class DRepException(
        transactionId: TransactionHash,
        alreadyRegistered: Set[Credential],
        notRegistered: Set[Credential],
        invalidDeposits: Map[Credential, (Coin, Coin)],
        invalidRefunds: Map[Credential, (Coin, Coin)]
    ) extends TransactionException(
          s"""DRep certificate validation failed for transactionId $transactionId.
             |already registered: $alreadyRegistered,
             |not registered: $notRegistered,
             |invalid deposits (expected -> provided): $invalidDeposits,
             |invalid refunds (expected -> provided): $invalidRefunds.""".stripMargin
        )

    /** It's `ConwayCommitteeHasPreviouslyResigned` of the GOVCERT rule in cardano-ledger
      * (eras/conway/impl/src/Cardano/Ledger/Conway/Rules/GovCert.hs:193-199), spec [GC-1], [GC-4]:
      * a committee certificate for a cold credential that resigned.
      */
    final case class CommitteeHasPreviouslyResignedException(
        transactionId: TransactionHash,
        coldCredential: Credential
    ) extends TransactionException(
          s"Committee member $coldCredential has previously resigned, transactionId $transactionId"
        )

    /** It's `ConwayCommitteeIsUnknown` of the GOVCERT rule in cardano-ledger
      * (eras/conway/impl/src/Cardano/Ledger/Conway/Rules/GovCert.hs:200-208), spec [GC-2], [GC-4]:
      * a committee certificate for a cold credential that is not a member of the committee and not
      * added by a pending `UpdateCommittee` proposal.
      */
    final case class CommitteeIsUnknownException(
        transactionId: TransactionHash,
        coldCredential: Credential
    ) extends TransactionException(
          s"Committee member $coldCredential is unknown, transactionId $transactionId"
        )

    // It's Conway.WithdrawalsNotInRewards in cardano-ledger
    final case class WithdrawalsNotInRewardsException(
        transactionId: TransactionHash,
        missingRewardAccounts: Map[RewardAccount, Coin],
        nonDrainingWithdrawals: Map[RewardAccount, (Coin, Coin)]
    ) extends TransactionException(
          s"""Withdrawals validation failed for transactionId $transactionId.
             |missing reward accounts: $missingRewardAccounts,
             |withdrawals that do not drain accounts (expected -> provided): $nonDrainingWithdrawals.""".stripMargin
        )

    // It's Conway.ConwayWdrlNotDelegatedToDRep in cardano-ledger
    final case class WithdrawalsNotDelegatedToDRepException(
        transactionId: TransactionHash,
        notDelegated: Set[AddrKeyHash]
    ) extends TransactionException(
          s"Withdrawals from key accounts not delegated to a DRep for transactionId $transactionId: $notDelegated"
        )

    // It's Conway.ConwayTreasuryValueMismatch in cardano-ledger
    final case class TreasuryValueMismatchException(
        transactionId: TransactionHash,
        supplied: Coin,
        expected: Coin
    ) extends TransactionException(
          s"Treasury value mismatch for transactionId $transactionId: supplied $supplied, expected $expected"
        )

    // It's Conway.ProposalDepositIncorrect of the GOV rule in cardano-ledger
    final case class ProposalDepositIncorrectException(
        transactionId: TransactionHash,
        supplied: Coin,
        expected: Coin
    ) extends TransactionException(
          s"Proposal deposit incorrect for transactionId $transactionId: supplied $supplied, expected $expected"
        )

    /** A failure of the Conway GOV rule in cardano-ledger
      * (eras/conway/impl/src/Cardano/Ledger/Conway/Rules/Gov.hs:456-643), other than
      * `ProposalDepositIncorrect`, spec [GV-19], D31. [[ruleName]] returns the ledger name of
      * `failure`.
      */
    final case class GovException(transactionId: TransactionHash, failure: GovFailure)
        extends TransactionException(
          s"GOV rule failed for transactionId $transactionId: $failure"
        )

    // It's Conway.ConwayTxRefScriptsSizeTooBig in cardano-ledger
    final case class TxRefScriptsSizeTooBigException(
        transactionId: TransactionHash,
        supplied: Int,
        expected: Int
    ) extends TransactionException(
          s"Reference scripts too big for transactionId $transactionId: supplied $supplied bytes, maximum $expected"
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
        logs: Seq[String],
        spentBudget: ExUnits,
        scriptHash: Option[ScriptHash] = None
    ) extends TransactionException(
          s"Plutus script validation failed for transactionId $transactionId: $message"
        )

    /** It's `ValidationTagMismatch (IsValid False) PassedUnexpectedly` of the UTXOS rule in
      * cardano-ledger (eras/babbage/impl/src/Cardano/Ledger/Babbage/Rules/Utxos.hs:255-260), spec
      * [FL-9]: a tx with `isValid = false` whose scripts all pass.
      */
    final case class PassedUnexpectedlyException(transactionId: TransactionHash)
        extends TransactionException(
          s"Transaction $transactionId is marked invalid, but its scripts pass"
        )

    /** It's `CollectErrors` of the UTXOS rule in cardano-ledger
      * (eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Plutus/Evaluate.hs:157-214), spec [FL-10]: the
      * Plutus scripts of the tx cannot be collected with their redeemers, cost models and contexts.
      * A phase-1 failure, whatever `isValid` says.
      *
      * @param errors
      *   in the order of `collectPlutusScriptsWithContext`: the first error last, and after it only
      *   the `NoRedeemer` errors of the later scripts
      */
    final case class CollectErrorsException(
        transactionId: TransactionHash,
        errors: Seq[CollectError]
    ) extends TransactionException(
          s"Cannot collect the Plutus scripts of transactionId $transactionId: ${errors.mkString(", ")}"
        )

    final case class IllegalArgumentException(message: String) extends TransactionException(message)

    /** Short, stable name for the rule that raised `ex` — the exception's own name without the
      * `Exception` suffix, e.g. `"ValueNotConservedUTxO"`.
      *
      * Written out as literals rather than derived from `getClass.getSimpleName`, which Scala.js
      * minification turns into a meaningless short name, or from `productPrefix`, which
      * [[MetadataException]] does not have. The match has no wildcard on purpose: adding a
      * `TransactionException` without naming it here makes this match non-exhaustive, and the
      * compiler says so, which is what stops the next rule from silently arriving as an anonymous
      * rejection. (A warning, not an error - this build does not turn warnings into errors.)
      *
      * Used by [[scalus.cardano.node.SubmitError.fromException]] to name rejections that do not map
      * onto one of its classified cases.
      */
    def ruleName(ex: TransactionException): String = ex match
        case _: EmptyInputsException => "EmptyInputs"
        case _: NonDisjointInputsAndReferenceInputsException =>
            "NonDisjointInputsAndReferenceInputs"
        case _: BadAllInputsUTxOException                  => "BadAllInputsUTxO"
        case _: BadInputsUTxOException                     => "BadInputsUTxO"
        case _: BadCollateralInputsUTxOException           => "BadCollateralInputsUTxO"
        case _: BadReferenceInputsUTxOException            => "BadReferenceInputsUTxO"
        case _: InvalidSignaturesInWitnessesException      => "InvalidSignaturesInWitnesses"
        case _: MissingKeyHashesException                  => "MissingKeyHashes"
        case _: MissingOrExtraScriptHashesException        => "MissingOrExtraScriptHashes"
        case _: NativeScriptsException                     => "NativeScripts"
        case _: InvalidTransactionSizeException            => "InvalidTransactionSize"
        case _: OutputsHaveNotEnoughCoinsException         => "OutputsHaveNotEnoughCoins"
        case _: OutputsHaveTooBigValueStorageSizeException => "OutputsHaveTooBigValueStorageSize"
        case _: OutsideValidityIntervalException           => "OutsideValidityInterval"
        case _: ValueNotConservedUTxOException             => "ValueNotConservedUTxO"
        case _: FeesOkException                            => "FeesOk"
        case _: StakeCertificatesException                 => "StakeCertificates"
        case _: StakePoolException                         => "StakePool"
        case _: PoolMetadataHashTooBigException            => "PoolMetadataHashTooBig"
        case _: VrfKeyHashAlreadyRegisteredException       => "VRFKeyHashAlreadyRegistered"
        case _: DRepException                              => "DRep"
        case _: CommitteeHasPreviouslyResignedException    => "ConwayCommitteeHasPreviouslyResigned"
        case _: CommitteeIsUnknownException                => "ConwayCommitteeIsUnknown"
        case _: WithdrawalsNotInRewardsException           => "WithdrawalsNotInRewards"
        case _: TreasuryValueMismatchException             => "TreasuryValueMismatch"
        case _: TxRefScriptsSizeTooBigException            => "TxRefScriptsSizeTooBig"
        case _: ProposalDepositIncorrectException          => "ProposalDepositIncorrect"
        case e: GovException                               => e.failure.productPrefix
        case _: WithdrawalsNotDelegatedToDRepException     => "WithdrawalsNotDelegatedToDRep"
        case _: ExUnitsExceedMaxException                  => "ExUnitsExceedMax"
        case _: TooManyCollateralInputsException           => "TooManyCollateralInputs"
        case _: InvalidScriptDataHashException             => "InvalidScriptDataHash"
        case _: IllFormedScriptsException                  => "IllFormedScripts"
        case _: WrongNetworkAddress                        => "WrongNetworkAddress"
        case _: WrongNetworkWithdrawal                     => "WrongNetworkWithdrawal"
        case _: WrongNetworkInTxBody                       => "WrongNetworkInTxBody"
        case _: MetadataException.MissingAuxiliaryDataException     => "MissingAuxiliaryData"
        case _: MetadataException.MissingAuxiliaryDataHashException => "MissingAuxiliaryDataHash"
        case _: MetadataException.InvalidAuxiliaryDataHashException => "InvalidAuxiliaryDataHash"
        case _: MetadataException.InvalidAuxiliaryDataException     => "InvalidAuxiliaryData"
        // `MetadataException` is a `sealed class`, not a trait, so it can be raised on its own.
        case _: MetadataException                  => "Metadata"
        case _: ExactSetOfRedeemersException       => "ExactSetOfRedeemers"
        case _: DatumsException                    => "Datums"
        case _: OutputBootAddrAttrsTooBigException => "OutputBootAddrAttrsTooBig"
        case _: PlutusScriptValidationException    => "PlutusScriptValidation"
        case _: PassedUnexpectedlyException        => "PassedUnexpectedly"
        case _: CollectErrorsException             => "CollectErrors"
        case _: IllegalArgumentException           => "IllegalArgument"

    /** The cardano-ledger predicate-failure constructors that `ex` stands for, at the level that
      * the Imp tests' `injectFailure` takes, spec [FL-6]: one name per constructor that the fields
      * of `ex` show, sorted by [[ledgerOrder]]. An exception with no ledger counterpart gives its
      * [[ruleName]].
      *
      * Unlike [[ruleName]], which names the Scalus rule and stays stable for
      * [[scalus.cardano.node.SubmitError.rule]], these are the names a Haskell ledger test asserts
      * on, for example `FeeTooSmallUTxO` or `CollectErrors`.
      */
    def ledgerNames(ex: TransactionException): Seq[String] = {
        def when(pairs: (Boolean, String)*): Seq[String] = pairs.collect { case (true, n) => n }
        val names = ex match
            case _: EmptyInputsException => Seq("InputSetEmptyUTxO")
            case _: NonDisjointInputsAndReferenceInputsException =>
                Seq("BabbageNonDisjointRefInputs")
            // The ledger has one BadInputsUTxO for all inputs (B/Rules/Utxo.hs:397)
            case _: BadAllInputsUTxOException | _: BadInputsUTxOException |
                _: BadCollateralInputsUTxOException | _: BadReferenceInputsUTxOException =>
                Seq("BadInputsUTxO")
            case _: InvalidSignaturesInWitnessesException => Seq("InvalidWitnessesUTXOW")
            case _: MissingKeyHashesException             => Seq("MissingVKeyWitnessesUTXOW")
            case e: MissingOrExtraScriptHashesException =>
                val missing = Seq(
                  e.missingInputsScriptHashes,
                  e.missingMintScriptHashes,
                  e.missingVotingProceduresScriptHashes,
                  e.missingWithdrawalsScriptHashes,
                  e.missingProposalProceduresScriptHashes,
                  e.missingCertificatesScriptHashes
                ).exists(_.nonEmpty)
                when(
                  e.extraScriptHashes.nonEmpty -> "ExtraneousScriptWitnessesUTXOW",
                  missing -> "MissingScriptWitnessesUTXOW"
                )
            case _: NativeScriptsException             => Seq("ScriptWitnessNotValidatingUTXOW")
            case _: InvalidTransactionSizeException    => Seq("MaxTxSizeUTxO")
            case _: OutputsHaveNotEnoughCoinsException => Seq("BabbageOutputTooSmallUTxO")
            case _: OutputsHaveTooBigValueStorageSizeException => Seq("OutputTooBigUTxO")
            case _: OutsideValidityIntervalException           => Seq("OutsideValidityIntervalUTxO")
            case _: ValueNotConservedUTxOException             => Seq("ValueNotConservedUTxO")
            case e: FeesOkException =>
                when(
                  e.isTransactionFeeLessThanMinRequiredFee -> "FeeTooSmallUTxO",
                  e.hasCollateralsConsistNotVKeyAddress -> "ScriptsNotPaidUTxO",
                  e.hasCollateralsContainNotOnlyADA -> "CollateralContainsNonADA",
                  e.isCollateralInsufficient -> "InsufficientCollateral",
                  e.isCollateralNotEqualToExpected -> "IncorrectTotalCollateralField",
                  e.areCollateralInputsMissing -> "NoCollateralInputs"
                )
            case e: StakeCertificatesException =>
                when(
                  e.invalidDeposits.nonEmpty -> "DepositIncorrectDELEG",
                  e.alreadyRegistered.nonEmpty -> "StakeKeyRegisteredDELEG",
                  e.invalidRefunds.nonEmpty -> "RefundIncorrectDELEG",
                  e.nonZeroRewardAccounts.nonEmpty -> "StakeKeyHasNonZeroAccountBalanceDELEG",
                  e.delegateeStakePoolsNotRegistered.nonEmpty -> "DelegateeStakePoolNotRegisteredDELEG",
                  e.delegateeDRepsNotRegistered.nonEmpty -> "DelegateeDRepNotRegisteredDELEG",
                  e.missingRegistrations.nonEmpty -> "StakeKeyNotRegisteredDELEG"
                )
            case e: StakePoolException =>
                when(
                  e.rewardAccountNetworkMismatch.nonEmpty -> "WrongNetworkPOOL",
                  e.costBelowMinimum.nonEmpty -> "StakePoolCostTooLowPOOL",
                  e.notRegistered.nonEmpty -> "StakePoolNotRegisteredOnKeyPOOL",
                  e.invalidRetirementEpochs.nonEmpty -> "StakePoolRetirementWrongEpochPOOL"
                )
            // the ledger's spelling (S/Rules/Pool.hs:249-254)
            case _: PoolMetadataHashTooBigException      => Seq("PoolMedataHashTooBig")
            case _: VrfKeyHashAlreadyRegisteredException => Seq("VRFKeyHashAlreadyRegistered")
            case e: DRepException =>
                when(
                  e.alreadyRegistered.nonEmpty -> "ConwayDRepAlreadyRegistered",
                  e.invalidDeposits.nonEmpty -> "ConwayDRepIncorrectDeposit",
                  e.notRegistered.nonEmpty -> "ConwayDRepNotRegistered",
                  e.invalidRefunds.nonEmpty -> "ConwayDRepIncorrectRefund"
                )
            case _: CommitteeHasPreviouslyResignedException =>
                Seq("ConwayCommitteeHasPreviouslyResigned")
            case _: CommitteeIsUnknownException => Seq("ConwayCommitteeIsUnknown")
            case e: WithdrawalsNotInRewardsException =>
                when(
                  e.missingRewardAccounts.nonEmpty -> "ConwayWithdrawalsMissingAccounts",
                  e.nonDrainingWithdrawals.nonEmpty -> "ConwayIncompleteWithdrawals"
                )
            case _: TreasuryValueMismatchException         => Seq("ConwayTreasuryValueMismatch")
            case _: TxRefScriptsSizeTooBigException        => Seq("ConwayTxRefScriptsSizeTooBig")
            case _: ProposalDepositIncorrectException      => Seq("ProposalDepositIncorrect")
            case e: GovException                           => Seq(e.failure.productPrefix)
            case _: WithdrawalsNotDelegatedToDRepException => Seq("ConwayWdrlNotDelegatedToDRep")
            case _: ExUnitsExceedMaxException              => Seq("ExUnitsTooBigUTxO")
            case _: TooManyCollateralInputsException       => Seq("TooManyCollateralInputs")
            // PPViewHashesDontMatch before PV 11 (A/Rules/Utxow.hs:300-302)
            case _: InvalidScriptDataHashException => Seq("ScriptIntegrityHashMismatch")
            case e: IllFormedScriptsException =>
                when(
                  e.invalidWitnessesScripts.nonEmpty -> "MalformedScriptWitnesses",
                  e.InvalidScriptsFromAllOutputs.nonEmpty -> "MalformedReferenceScripts"
                )
            case _: WrongNetworkAddress                             => Seq("WrongNetwork")
            case _: WrongNetworkWithdrawal                          => Seq("WrongNetworkWithdrawal")
            case _: WrongNetworkInTxBody                            => Seq("WrongNetworkInTxBody")
            case _: MetadataException.MissingAuxiliaryDataException => Seq("MissingTxMetadata")
            case _: MetadataException.MissingAuxiliaryDataHashException =>
                Seq("MissingTxBodyMetadataHash")
            case _: MetadataException.InvalidAuxiliaryDataHashException =>
                Seq("ConflictingMetadataHash")
            case _: MetadataException.InvalidAuxiliaryDataException => Seq("InvalidMetadata")
            case _: MetadataException                               => Seq.empty
            case e: ExactSetOfRedeemersException =>
                when(
                  e.extraRedeemers.nonEmpty -> "ExtraRedeemers",
                  e.missingRedeemers.nonEmpty -> "MissingRedeemers"
                )
            case e: DatumsException =>
                when(
                  e.inputsWithMissingDatumHashes.nonEmpty -> "UnspendableUTxONoDatumHash",
                  e.unmatchedDatumHashes.nonEmpty -> "MissingRequiredDatums",
                  e.notAllowedSupplementalDatumHashes.nonEmpty -> "NotAllowedSupplementalDatums"
                )
            // Conway keeps the Shelley constructor (C/Rules/Utxo.hs:122, 432)
            case _: OutputBootAddrAttrsTooBigException => Seq("OutputBootAddrAttrsTooBig")
            case _: PlutusScriptValidationException | _: PassedUnexpectedlyException =>
                Seq("ValidationTagMismatch")
            case _: CollectErrorsException   => Seq("CollectErrors")
            case _: IllegalArgumentException => Seq.empty
        if names.isEmpty then Seq(ruleName(ex)) else names.sortBy(ledgerOrder)
    }

    /** The position of the ledger constructor `name` in [[failureOrder]]; a name with no ledger
      * counterpart comes after all of them, spec [FL-7a].
      */
    private[ledger] def ledgerOrder(name: String): Int =
        failureIndex.getOrElse(name, failureOrder.size)

    /** The order of the failures of a Conway tx with at most one certificate in the list of the
      * ledger's interpreter, spec [FL-7], [FL-7a], D104. A rule's list is the reverse of the order
      * in which its own predicates and its sub-rules fail, and each sub-rule adds its own list,
      * reversed again (libs/small-steps/src/Control/State/Transition/Extended.hs:708, 721).
      */
    private[ledger] val failureOrder: IndexedSeq[String] = IndexedSeq(
      // (1) UTXOW, in source order (eras/babbage/impl/src/Cardano/Ledger/Babbage/Rules/
      // Utxow.hs:346-388); babbageMissingScripts checks the extraneous scripts first (:219-222)
      "ScriptWitnessNotValidatingUTXOW",
      "ExtraneousScriptWitnessesUTXOW",
      "MissingScriptWitnessesUTXOW",
      "UnspendableUTxONoDatumHash",
      "MissingRequiredDatums",
      "NotAllowedSupplementalDatums",
      "ExtraRedeemers",
      "MissingRedeemers",
      "InvalidWitnessesUTXOW",
      "MissingVKeyWitnessesUTXOW",
      "MissingTxMetadata",
      "MissingTxBodyMetadataHash",
      "ConflictingMetadataHash",
      "InvalidMetadata",
      "MalformedScriptWitnesses",
      "MalformedReferenceScripts",
      "ScriptIntegrityHashMismatch",
      // (2) UTXOS (B/Rules/Utxos.hs:184-195, 255-266)
      "ValidationTagMismatch",
      "CollectErrors",
      // (3) UTXO, in reverse source order (B/Rules/Utxo.hs:378-434), feesOK (:213-261) included
      "TooManyCollateralInputs",
      "ExUnitsTooBigUTxO",
      "MaxTxSizeUTxO",
      "WrongNetworkInTxBody",
      "WrongNetworkWithdrawal",
      "WrongNetwork",
      "OutputBootAddrAttrsTooBig",
      "OutputTooBigUTxO",
      "BabbageOutputTooSmallUTxO",
      "ValueNotConservedUTxO",
      "BadInputsUTxO",
      "NoCollateralInputs",
      "IncorrectTotalCollateralField",
      "InsufficientCollateral",
      "CollateralContainsNonADA",
      "ScriptsNotPaidUTxO",
      "FeeTooSmallUTxO",
      "InputSetEmptyUTxO",
      "OutsideForecast",
      "OutsideValidityIntervalUTxO",
      "BabbageNonDisjointRefInputs",
      // (4) GOV, in source order (eras/conway/impl/src/Cardano/Ledger/Conway/Rules/Gov.hs:489-620)
      "UnelectedCommitteeVoters",
      "ProposalCantFollow",
      "MalformedProposal",
      "ProposalReturnAccountDoesNotExist",
      "TreasuryWithdrawalReturnAccountsDoNotExist",
      "ProposalDepositIncorrect",
      "ProposalProcedureNetworkIdMismatch",
      "TreasuryWithdrawalsNetworkIdMismatch",
      "InvalidGuardrailsScriptHash",
      "ZeroTreasuryWithdrawals",
      "ConflictingCommitteeUpdate",
      "ExpirationEpochTooSmall",
      "InvalidPrevGovActionId",
      "VotersDoNotExist",
      "GovActionsDoNotExist",
      "VotingOnExpiredGovAction",
      "DisallowedVoters",
      // (5) the certificate, in source order: DELEG (C/Rules/Deleg.hs:195-290; one order that each
      // certificate kind keeps), POOL (eras/shelley/impl/src/Cardano/Ledger/Shelley/Rules/
      // Pool.hs:235-326), GOVCERT (C/Rules/GovCert.hs:193-275)
      "DepositIncorrectDELEG",
      "StakeKeyRegisteredDELEG",
      "RefundIncorrectDELEG",
      "StakeKeyHasNonZeroAccountBalanceDELEG",
      "DelegateeStakePoolNotRegisteredDELEG",
      "DelegateeDRepNotRegisteredDELEG",
      "StakeKeyNotRegisteredDELEG",
      "WrongNetworkPOOL",
      "PoolMedataHashTooBig",
      "StakePoolCostTooLowPOOL",
      "VRFKeyHashAlreadyRegistered",
      "StakePoolNotRegisteredOnKeyPOOL",
      "StakePoolRetirementWrongEpochPOOL",
      "ConwayCommitteeHasPreviouslyResigned",
      "ConwayCommitteeIsUnknown",
      "ConwayDRepAlreadyRegistered",
      "ConwayDRepIncorrectDeposit",
      "ConwayDRepNotRegistered",
      "ConwayDRepIncorrectRefund",
      // (6) LEDGER, in reverse source order (C/Rules/Ledger.hs:394-416,
      // eras/shelley/impl/src/Cardano/Ledger/Shelley/Rules/Ledger.hs:361-369)
      "ConwayIncompleteWithdrawals",
      "ConwayWithdrawalsMissingAccounts",
      "ConwayWdrlNotDelegatedToDRep",
      "ConwayTxRefScriptsSizeTooBig",
      "ConwayTreasuryValueMismatch"
    )

    private val failureIndex: Map[String, Int] = failureOrder.zipWithIndex.toMap
}

/** A collect error of `collectPlutusScriptsWithContext` in cardano-ledger
  * (eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Plutus/Evaluate.hs:185-188), spec [FL-10]. Conway
  * never raises `NoWitness`: a needed script that the tx does not provide is dropped (`mapMaybe`,
  * :178-179).
  */
enum CollectError {

    /** No redeemer for the script of the purpose `tag` at `index`. */
    case NoRedeemer(tag: RedeemerTag, index: Int)

    /** The pparams have no cost model for `language`. */
    case NoCostModel(language: Language)

    /** The script context cannot be built, for example `ReferenceInputsNotDisjointFromInputs` of
      * spec [TX-1]; `message` says why.
      */
    case BadTranslation(message: String)
}

type StakeMap = Map[Credential, Coin]
object StakeMap:
    def empty: StakeMap = Map.empty
case class UTxOState(
    utxo: Utxos, // UtxO entries
    deposited: Coin, // The deposits the ledger holds, `utxosDeposited`, spec [BK-1]
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
    vstate: VotingState = VotingState.empty,
    pstate: PoolsState = PoolsState.empty,
    dstate: DelegationState = DelegationState()
)
object CertState {
    def empty: CertState = CertState(
      VotingState.empty,
      PoolsState.empty,
      DelegationState()
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

/** The voting state, the Haskell `VState`.
  *
  * @param numDormantEpochs
  *   the number of epochs in a row with no active proposal, `vsNumDormantEpochs` of the ledger,
  *   spec [DM-3]. A tx with a proposal adds it to the DRep expiries and resets it
  *   (eras/conway/impl/src/Cardano/Ledger/Conway/Rules/Certs.hs:313-333), spec [DR-3].
  * @param committeeState
  *   the authorization of each committee cold credential that has one, `vsCommitteeState` of the
  *   ledger, spec [DM-1]. It is the third field, not the second as in Haskell, so that the product
  *   accessors of the earlier fields keep their types (D29).
  */
case class VotingState(
    dreps: Map[Credential, DRepState],
    numDormantEpochs: EpochNo,
    committeeState: Map[Credential, CommitteeAuthorization]
) {

    /** The 1.4 constructor, with no dormant counter and no committee state, spec D17. */
    @deprecated(
      "use the constructor with numDormantEpochs and committeeState, or VotingState.empty",
      "1.4.1"
    )
    def this(dreps: Map[Credential, DRepState] = Map.empty) = this(dreps, 0L, Map.empty)
}

object VotingState {

    /** No DReps, no dormant epochs and no committee authorization. */
    val empty: VotingState = VotingState(Map.empty, 0L, Map.empty)

    /** The 1.4 factory, with no dormant counter and no committee state, spec D17. */
    @deprecated(
      "use the factory with numDormantEpochs and committeeState, or VotingState.empty",
      "1.4.1"
    )
    def apply(dreps: Map[Credential, DRepState] = Map.empty): VotingState =
        VotingState(dreps, 0L, Map.empty)
}

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

/** The pool state of the POOL rule, the Haskell `PState`.
  *
  * @param vrfKeyHashes
  *   the number of pools per VRF key, `psVRFKeyHashes` of the ledger, spec [DM-4]. POOL keeps it at
  *   PV >= 11 only (eras/shelley/impl/src/Cardano/Ledger/Shelley/Rules/Pool.hs:266-310).
  */
case class PoolsState(
    stakePools: Map[PoolKeyHash, Certificate.PoolRegistration],
    futureStakePoolParams: Map[PoolKeyHash, Certificate.PoolRegistration],
    retiring: Map[PoolKeyHash, EpochNo],
    deposits: Map[PoolKeyHash, Coin],
    vrfKeyHashes: Map[VrfKeyHash, Long]
) {

    /** The 1.4 constructor, with no VRF map, spec D17. */
    @deprecated("use the constructor with vrfKeyHashes, or PoolsState.empty", "1.4.1")
    def this(
        stakePools: Map[PoolKeyHash, Certificate.PoolRegistration] = Map.empty,
        futureStakePoolParams: Map[PoolKeyHash, Certificate.PoolRegistration] = Map.empty,
        retiring: Map[PoolKeyHash, EpochNo] = Map.empty,
        deposits: Map[PoolKeyHash, Coin] = Map.empty
    ) = this(stakePools, futureStakePoolParams, retiring, deposits, Map.empty)
}

object PoolsState {

    /** No pools. */
    val empty: PoolsState = PoolsState(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

    /** The 1.4 factory, with no VRF map, spec D17. */
    @deprecated("use the factory with vrfKeyHashes, or PoolsState.empty", "1.4.1")
    def apply(
        stakePools: Map[PoolKeyHash, Certificate.PoolRegistration] = Map.empty,
        futureStakePoolParams: Map[PoolKeyHash, Certificate.PoolRegistration] = Map.empty,
        retiring: Map[PoolKeyHash, EpochNo] = Map.empty,
        deposits: Map[PoolKeyHash, Coin] = Map.empty
    ): PoolsState = PoolsState(stakePools, futureStakePoolParams, retiring, deposits, Map.empty)
}

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

/** Delegation state: the `dsAccounts` part of the Haskell `DState`.
  *
  * @param accounts
  *   one record per registered stake account. An account is registered if and only if this map
  *   contains its credential.
  */
case class DelegationState(
    accounts: Map[Credential, ConwayAccountState]
//    futureGenDelegs: Map[FutureGenDeleg, GenDelegPair],
//    genDelegs: GenDelegs,
//    instantaneousRewards: InstantaneousRewards
) {

    /** Builds the accounts from the 4 maps of the old shape, as the deprecated 4-map `apply`. */
    @deprecated("use DelegationState(accounts) instead", "1.3.0")
    def this(
        rewards: Map[Credential, Coin],
        deposits: Map[Credential, Coin],
        stakePools: Map[Credential, PoolKeyHash],
        dreps: Map[Credential, DRep]
    ) = this(DelegationState.accountsOf(rewards, deposits, stakePools, dreps))

    /** The reward balance of each registered account. */
    @deprecated("use accounts instead", "1.3.0")
    def rewards: Map[Credential, Coin] = accounts.view.mapValues(_.balance).toMap

    /** The deposit of each registered account. */
    @deprecated("use accounts instead", "1.3.0")
    def deposits: Map[Credential, Coin] = accounts.view.mapValues(_.deposit).toMap

    /** The pool each account delegates its stake to, for the accounts that delegate. */
    @deprecated("use accounts instead", "1.3.0")
    def stakePools: Map[Credential, PoolKeyHash] =
        accounts.flatMap((cred, account) => account.stakePoolDelegation.map(cred -> _))

    /** The DRep each account delegates its vote to, for the accounts that delegate. */
    @deprecated("use accounts instead", "1.3.0")
    def dreps: Map[Credential, DRep] =
        accounts.flatMap((cred, account) => account.dRepDelegation.map(cred -> _))
}

object DelegationState {

    /** No registered accounts. */
    val empty: DelegationState = DelegationState(Map.empty[Credential, ConwayAccountState])

    /** No registered accounts. */
    def apply(): DelegationState = empty

    /** Builds the accounts from the 4 maps of the old shape. A credential in `rewards` or in
      * `deposits` becomes an account; a missing balance or deposit is zero. A delegation of a
      * credential in neither map is dropped.
      */
    // Defaults only to keep 1.3 Scala call sites compiling; Java and JS callers never had them.
    @deprecated("use DelegationState(accounts) instead", "1.3.0")
    def apply(
        rewards: Map[Credential, Coin] = Map.empty,
        deposits: Map[Credential, Coin] = Map.empty,
        stakePools: Map[Credential, PoolKeyHash] = Map.empty,
        dreps: Map[Credential, DRep] = Map.empty
    ): DelegationState = DelegationState(accountsOf(rewards, deposits, stakePools, dreps))

    private def accountsOf(
        rewards: Map[Credential, Coin],
        deposits: Map[Credential, Coin],
        stakePools: Map[Credential, PoolKeyHash],
        dreps: Map[Credential, DRep]
    ): Map[Credential, ConwayAccountState] =
        (rewards.keySet ++ deposits.keySet).iterator.map { cred =>
            cred -> ConwayAccountState(
              balance = rewards.getOrElse(cred, Coin.zero),
              deposit = deposits.getOrElse(cred, Coin.zero),
              stakePoolDelegation = stakePools.get(cred),
              dRepDelegation = dreps.get(cred)
            )
        }.toMap
}
