package scalus.cardano.ledger.rules

import scala.collection.immutable.{ListSet, SortedSet}

/** Explicit list of all default validators for ledger rules validation.
  */
object DefaultValidators {

    /** All built-in validators for Cardano ledger rules. */
    val all: Set[STS.Validator] = SortedSet[STS.Validator](
      AllInputsMustBeInUtxoValidator,
      EmptyInputsValidator,
      ExactSetOfRedeemersValidator,
      ExUnitsTooBigValidator,
      FeesOkValidator,
      InputsAndReferenceInputsDisjointValidator,
      MetadataValidator,
      MissingKeyHashesValidator,
      MissingOrExtraScriptHashesValidator,
      MissingRequiredDatumsValidator,
      NativeScriptsValidator,
      OutsideForecastValidator,
      OutsideValidityIntervalValidator,
      OutputBootAddrAttrsSizeValidator,
      OutputsHaveNotEnoughCoinsValidator,
      OutputsHaveTooBigValueStorageSizeValidator,
      ProtocolParamsViewHashesMatchValidator,
      ScriptsWellFormedValidator,
      TooManyCollateralInputsValidator,
      TransactionSizeValidator,
      TreasuryValueMismatchValidator,
      ValueNotConservedUTxOValidator,
      VerifiedSignaturesInWitnessesValidator,
      StakeCertificatesValidator,
      StakePoolCertificatesValidator,
      CertsValidator,
      WrongNetworkInTxBodyValidator,
      WrongNetworkValidator,
      WrongNetworkWithdrawalValidator
    )(using Ordering.by(_.name))
}

/** Explicit list of all default mutators for ledger rules validation. */
object DefaultMutators {

    /** All built-in mutators for Cardano ledger rules, in the order of the Conway LEDGER rule:
      * withdrawals and certificates, then scripts and UTxO. A `ListSet` iterates in this order,
      * spec [SC-3a]. [[CertsMutator]] applies the certificates in one ordered pass, spec [SC-22].
      */
    val all: Set[STS.Mutator] = ListSet[STS.Mutator](
      CertsMutator,
      PlutusScriptsTransactionMutator
    )
}
