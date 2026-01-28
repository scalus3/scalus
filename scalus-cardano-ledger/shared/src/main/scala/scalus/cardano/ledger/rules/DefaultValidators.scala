package scalus.cardano.ledger.rules

/** Explicit list of all default validators for ledger rules validation.
  */
object DefaultValidators {

    /** All built-in validators for Cardano ledger rules. */
    val all: Set[STS.Validator] = Set(
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
      ValueNotConservedUTxOValidator,
      VerifiedSignaturesInWitnessesValidator,
      WrongNetworkInTxBodyValidator,
      WrongNetworkValidator,
      WrongNetworkWithdrawalValidator
    )
}

/** Explicit list of all default mutators for ledger rules validation. */
object DefaultMutators {

    /** All built-in mutators for Cardano ledger rules. */
    val all: Set[STS.Mutator] = Set(
      PlutusScriptsTransactionMutator
    )
}
