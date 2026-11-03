package scalus.cardano.ledger

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.TransactionException.*

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

/** The cardano-ledger constructor names of every [[TransactionException]] case, spec [FL-6], and
  * their order, spec [FL-7].
  */
class LedgerNamesTest extends AnyFunSuite, ArbitraryInstances {

    private def any[A: Arbitrary]: A = arbitrary[A].sample.get

    private val txId = any[TransactionHash]
    private val input = any[TransactionInput]
    private val cred = any[Credential]
    private val pool = any[PoolKeyHash]
    private val script = any[ScriptHash]
    private val dataHash = any[DataHash]
    private val account = any[RewardAccount]

    private def feesOk(
        fee: Long = 10,
        nonVKey: Utxos = Utxos.empty,
        nonAda: MultiAsset = MultiAsset.empty,
        collateral: Long = 100,
        expected: Option[Coin] = None,
        hasCollateral: Boolean = true
    ) = FeesOkException(
      txId,
      transactionFee = Coin(10),
      minRequiredFee = Coin(fee),
      collateralPercentage = 150,
      areTotalExUnitsZero = false,
      actualTotalSumOfCollateralCoins = Coin(collateral),
      expectedTotalSumOfCollateralCoins = expected,
      collateralsConsistNotVKeyAddress = nonVKey,
      remainingAssets = nonAda,
      hasCollateralInputs = hasCollateral
    )

    private val utxo: Utxos = Map(input -> any[TransactionOutput])
    private val asset = MultiAsset.asset(script, AssetName.empty, 1)

    private val cases: Seq[(TransactionException, Seq[String])] = Seq(
      EmptyInputsException(txId) -> Seq("InputSetEmptyUTxO"),
      NonDisjointInputsAndReferenceInputsException(txId, Set(input)) ->
          Seq("BabbageNonDisjointRefInputs"),
      BadAllInputsUTxOException(txId, Set(input), Set.empty, Set.empty) -> Seq("BadInputsUTxO"),
      BadInputsUTxOException(txId) -> Seq("BadInputsUTxO"),
      BadCollateralInputsUTxOException(txId) -> Seq("BadInputsUTxO"),
      BadReferenceInputsUTxOException(txId) -> Seq("BadInputsUTxO"),
      InvalidSignaturesInWitnessesException(txId, Set(any[VKeyWitness]), Set.empty) ->
          Seq("InvalidWitnessesUTXOW"),
      MissingKeyHashesException(
        txId,
        Set.empty,
        Set.empty,
        Set.empty,
        Set.empty,
        Set.empty,
        Set.empty
      ) ->
          Seq("MissingVKeyWitnessesUTXOW"),
      MissingOrExtraScriptHashesException(
        txId,
        Set(script),
        Set.empty,
        Set.empty,
        Set.empty,
        Set.empty,
        Set.empty,
        Set(script)
      ) -> Seq("ExtraneousScriptWitnessesUTXOW", "MissingScriptWitnessesUTXOW"),
      MissingOrExtraScriptHashesException(
        txId,
        Set.empty,
        Set.empty,
        Set.empty,
        Set.empty,
        Set.empty,
        Set(script),
        Set.empty
      ) -> Seq("MissingScriptWitnessesUTXOW"),
      NativeScriptsException(txId, Set(script), Set.empty) ->
          Seq("ScriptWitnessNotValidatingUTXOW"),
      InvalidTransactionSizeException(txId, 20, 10) -> Seq("MaxTxSizeUTxO"),
      OutputsHaveNotEnoughCoinsException(txId, Seq.empty, None) ->
          Seq("BabbageOutputTooSmallUTxO"),
      OutputsHaveTooBigValueStorageSizeException(txId, 10, Seq.empty, None) ->
          Seq("OutputTooBigUTxO"),
      OutsideValidityIntervalException(txId, ValidityInterval(None, Some(1)), 2) ->
          Seq("OutsideValidityIntervalUTxO"),
      ValueNotConservedUTxOException(txId, Value.zero, Value.lovelace(1)) ->
          Seq("ValueNotConservedUTxO"),
      feesOk(fee = 20) -> Seq("FeeTooSmallUTxO"),
      // every collateral check fails; the list is the reverse of feesOK's, spec [FL-7] (3)
      feesOk(fee = 20, utxo, asset, 1, Some(Coin(2)), hasCollateral = false) -> Seq(
        "NoCollateralInputs",
        "IncorrectTotalCollateralField",
        "InsufficientCollateral",
        "CollateralContainsNonADA",
        "ScriptsNotPaidUTxO",
        "FeeTooSmallUTxO"
      ),
      StakeCertificatesException(
        txId,
        Set(cred),
        Set(cred),
        Map(cred -> Coin(1)),
        Map(cred -> (Coin(1), Coin(2))),
        Map(cred -> (Coin(1), Coin(2))),
        Set(pool),
        Set(cred)
      ) -> Seq(
        "DepositIncorrectDELEG",
        "StakeKeyRegisteredDELEG",
        "RefundIncorrectDELEG",
        "StakeKeyHasNonZeroAccountBalanceDELEG",
        "DelegateeStakePoolNotRegisteredDELEG",
        "DelegateeDRepNotRegisteredDELEG",
        "StakeKeyNotRegisteredDELEG"
      ),
      StakePoolException(
        txId,
        Set(pool),
        Set((pool, Network.Mainnet, Network.Testnet)),
        Map(pool -> (Coin(1), Coin(0))),
        Map(pool -> (1L, 2L, 3L))
      ) -> Seq(
        "WrongNetworkPOOL",
        "StakePoolCostTooLowPOOL",
        "StakePoolNotRegisteredOnKeyPOOL",
        "StakePoolRetirementWrongEpochPOOL"
      ),
      PoolMetadataHashTooBigException(txId, Map(pool -> 33), 32) -> Seq("PoolMedataHashTooBig"),
      VrfKeyHashAlreadyRegisteredException(txId, pool, any[VrfKeyHash]) ->
          Seq("VRFKeyHashAlreadyRegistered"),
      DRepException(
        txId,
        Set(cred),
        Set(cred),
        Map(cred -> (Coin(1), Coin(2))),
        Map(cred -> (Coin(1), Coin(2)))
      ) -> Seq(
        "ConwayDRepAlreadyRegistered",
        "ConwayDRepIncorrectDeposit",
        "ConwayDRepNotRegistered",
        "ConwayDRepIncorrectRefund"
      ),
      CommitteeHasPreviouslyResignedException(txId, cred) ->
          Seq("ConwayCommitteeHasPreviouslyResigned"),
      CommitteeIsUnknownException(txId, cred) -> Seq("ConwayCommitteeIsUnknown"),
      WithdrawalsNotInRewardsException(
        txId,
        Map(account -> Coin(1)),
        Map(account -> (Coin(1), Coin(2)))
      ) -> Seq("ConwayIncompleteWithdrawals", "ConwayWithdrawalsMissingAccounts"),
      WithdrawalsNotDelegatedToDRepException(txId, Set(any[AddrKeyHash])) ->
          Seq("ConwayWdrlNotDelegatedToDRep"),
      TreasuryValueMismatchException(txId, Coin(1), Coin(2)) -> Seq("ConwayTreasuryValueMismatch"),
      ProposalDepositIncorrectException(txId, Coin(1), Coin(2)) -> Seq("ProposalDepositIncorrect"),
      GovException(txId, GovFailure.VotersDoNotExist(Nil)) -> Seq("VotersDoNotExist"),
      TxRefScriptsSizeTooBigException(txId, 2, 1) -> Seq("ConwayTxRefScriptsSizeTooBig"),
      ExUnitsExceedMaxException(txId, ExUnits(2, 2), ExUnits(1, 1)) -> Seq("ExUnitsTooBigUTxO"),
      TooManyCollateralInputsException(txId, 4, 3) -> Seq("TooManyCollateralInputs"),
      InvalidScriptDataHashException(txId, None, Some(any[ScriptDataHash])) ->
          Seq("ScriptIntegrityHashMismatch"),
      IllFormedScriptsException(txId, Set(script), Set(script)) ->
          Seq("MalformedScriptWitnesses", "MalformedReferenceScripts"),
      WrongNetworkAddress(txId, Seq(any[Address]), None) -> Seq("WrongNetwork"),
      WrongNetworkWithdrawal(txId, Seq(account -> Coin(1))) -> Seq("WrongNetworkWithdrawal"),
      WrongNetworkInTxBody(txId, 0, 1) -> Seq("WrongNetworkInTxBody"),
      MetadataException.MissingAuxiliaryDataException(txId, any[AuxiliaryDataHash]) ->
          Seq("MissingTxMetadata"),
      MetadataException.MissingAuxiliaryDataHashException(txId, any[AuxiliaryData]) ->
          Seq("MissingTxBodyMetadataHash"),
      MetadataException.InvalidAuxiliaryDataHashException(
        txId,
        any[AuxiliaryDataHash],
        any[AuxiliaryDataHash]
      ) -> Seq("ConflictingMetadataHash"),
      MetadataException.InvalidAuxiliaryDataException(txId, any[AuxiliaryData]) ->
          Seq("InvalidMetadata"),
      // no ledger counterpart: the rule name, spec [FL-6]
      new MetadataException("bare") -> Seq("Metadata"),
      ExactSetOfRedeemersException(txId, Set((RedeemerTag.Mint, 0)), Set((RedeemerTag.Spend, 0))) ->
          Seq("ExtraRedeemers", "MissingRedeemers"),
      DatumsException(txId, Set(input), Set(dataHash), Set(dataHash)) -> Seq(
        "UnspendableUTxONoDatumHash",
        "MissingRequiredDatums",
        "NotAllowedSupplementalDatums"
      ),
      OutputBootAddrAttrsTooBigException(txId, Seq.empty, 64) -> Seq("OutputBootAddrAttrsTooBig"),
      PlutusScriptValidationException(txId, "failed", Seq.empty, ExUnits.zero) ->
          Seq("ValidationTagMismatch"),
      PassedUnexpectedlyException(txId) -> Seq("ValidationTagMismatch"),
      CollectErrorsException(txId, Seq(CollectError.NoCostModel(Language.PlutusV3))) ->
          Seq("CollectErrors"),
      IllegalArgumentException("unknown") -> Seq("IllegalArgument")
    )

    test("ledgerNames gives the ledger constructors of every exception case, spec [FL-6]") {
        for (ex, expected) <- cases do
            assert(TransactionException.ledgerNames(ex) == expected, ruleName(ex))
    }

    test("every ledger name has a place in the ledger order, spec [FL-6], [FL-7]") {
        // ledgerOrder puts an unknown name last, so a misspelled name would sort without a failure
        val unordered = cases.flatMap(_._2).filterNot(TransactionException.failureOrder.contains)
        // the rule names of the exceptions with no ledger counterpart
        assert(unordered.toSet == Set("Metadata", "IllegalArgument"), unordered)
        // GovException names its GovFailure case
        val govFailures = summon[Mirror.SumOf[GovFailure]]
        val govNames =
            constValueTuple[govFailures.MirroredElemLabels].productIterator.map(_.toString).toList
        assert(govNames.size == 16)
        for name <- govNames do assert(TransactionException.failureOrder.contains(name), name)
    }

    test("ruleName of the new exceptions, spec [FL-9], [FL-10]") {
        assert(ruleName(PassedUnexpectedlyException(txId)) == "PassedUnexpectedly")
        assert(ruleName(CollectErrorsException(txId, Seq.empty)) == "CollectErrors")
    }

    test("ledgerNames sorts by the order of the ledger's interpreter, spec [FL-7]") {
        // (1) UTXOW before (2) UTXOS before (3) UTXO before (4) GOV before (5) CERTS before (6)
        // LEDGER, one name of each group
        val groups = Seq(
          "MissingRedeemers",
          "CollectErrors",
          "BadInputsUTxO",
          "VotersDoNotExist",
          "StakeKeyNotRegisteredDELEG",
          "ConwayWdrlNotDelegatedToDRep"
        )
        assert(groups.sortBy(TransactionException.ledgerOrder) == groups)
        // (3) and (6) are reversed: BadInputsUTxO is checked before ValueNotConservedUTxO
        // (B/Rules/Utxo.hs:397, 400) and comes after it
        assert(
          Seq("BadInputsUTxO", "ValueNotConservedUTxO").sortBy(TransactionException.ledgerOrder) ==
              Seq("ValueNotConservedUTxO", "BadInputsUTxO")
        )
        // a name with no ledger counterpart comes last
        assert(
          TransactionException.ledgerOrder("IllegalArgument") >
              TransactionException.ledgerOrder("ConwayTreasuryValueMismatch")
        )
    }

}
