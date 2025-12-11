package scalus.cardano.txbuilder

import cats.*
import cats.implicits.*
import monocle.syntax.all.*
import monocle.{Focus, Lens}
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.address
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.GovAction.*
import scalus.cardano.ledger.TransactionWitnessSet.given
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.cardano.txbuilder.Datum.DatumValue
import scalus.cardano.txbuilder.StepError.*
import scalus.cardano.txbuilder.TransactionBuilder.*
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint as _, *}
import scalus.|>

import scala.collection.immutable.SortedMap

private class TransactionStepsProcessor(private var _ctx: Context) {

    /** Transaction builder monad. Retains context at point of failure, if there's any.
      */
    type Result[A] =
        Either[StepError | RedeemerIndexingInternalError, A]

    // Helpers to cut down on type signature noise
    private val unit: Result[Unit] = Right(())

    private def ctx: Context = _ctx

    private def modify0(f: Context => Context): Unit =
        _ctx = f(ctx)

    def applySteps(steps: Seq[TransactionBuilderStep]): (Context, Result[Unit]) = {
        val result = for {
            _ <- processSteps(steps)
            res <- TransactionConversion
                .fromEditableTransactionSafe(
                  EditableTransaction(
                    transaction = ctx.transaction,
                    redeemers = ctx.redeemers.toVector
                  )
                )
                .left
                .map(detachedRedeemer => RedeemerIndexingInternalError(detachedRedeemer, steps))
            // Replace the transaction in the context, keeping the rest
            _ = modify0(Focus[Context](_.transaction).replace(res))

            // Replace delayed redeemers if any exist
            _ <-
                if ctx.delayedRedeemerSpecs.nonEmpty then
                    replaceDelayedRedeemers(
                      ctx.redeemers,
                      ctx.delayedRedeemerSpecs,
                      ctx.transaction
                    ).map { updatedRedeemers =>
                        modify0(_.replaceRedeemers(updatedRedeemers))
                    }
                else unit
        } yield ()
        _ctx -> result
    }

    private def processSteps(steps: Seq[TransactionBuilderStep]): Result[Unit] =
        steps.traverse_(processStep)

    // Tries to add the output to the resolved utxo set, throwing an error if
    // the input is already mapped to another output
    private def addResolvedUtxo(utxo: Utxo, step: TransactionBuilderStep): Result[Unit] = {
        val mbNewUtxos = ctx.resolvedUtxos.addUtxo(utxo)
        mbNewUtxos match {
            case None =>
                Left(
                  ResolvedUtxosIncoherence(
                    input = utxo.input,
                    existingOutput = ctx.resolvedUtxos.utxos(utxo.input),
                    incoherentOutput = utxo.output,
                    step = step
                  )
                )
            case Some(utxos) =>
                modify0(_.copy(resolvedUtxos = utxos))
                unit
        }
    }

    private def processStep(step: TransactionBuilderStep): Result[Unit] = step match {
        case spend: Spend => useSpend(spend)

        case delayedSpend: SpendWithDelayedRedeemer =>
            useSpendWithDelayedRedeemer(delayedSpend)

        case send: Send =>
            useSend(send)

        case mint: TransactionBuilderStep.Mint =>
            useMint(mint)

        case referenceOutput: ReferenceOutput =>
            useReferenceOutput(referenceOutput)

        case fee: Fee =>
            useFee(fee)

        case validityStartSlot: ValidityStartSlot =>
            useValidityStartSlot(validityStartSlot)

        case validityEndSlot: ValidityEndSlot =>
            useValidityEndSlot(validityEndSlot)

        case addCollateral: AddCollateral =>
            useAddCollateral(addCollateral)

        case modifyAuxiliaryData: ModifyAuxiliaryData =>
            Right(useModifyAuxiliaryData(modifyAuxiliaryData))

        case issueCertificate: IssueCertificate =>
            useIssueCertificate(issueCertificate)

        case withdrawRewards: WithdrawRewards =>
            useWithdrawRewards(withdrawRewards)

        case submitProposal: SubmitProposal =>
            useSubmitProposal(submitProposal)

        case submitVotingProcedure: SubmitVotingProcedure =>
            useSubmitVotingProcedure(submitVotingProcedure)
    }

    // -------------------------------------------------------------------------
    // Spend step
    // -------------------------------------------------------------------------

    /** Tries to modify the transaction to make it consume a given output and add the requisite
      * signature(s) to the Context's _.expectedSigners. Uses witness to try to satisfy spending
      * requirements.
      */
    private def useSpend(spend: Spend): Result[Unit] = {
        val utxo = spend.utxo
        val witness = spend.witness

        // Extract the key hash, erroring if not a Shelley PKH address
        def getPaymentVerificationKeyHash(address: Address): Result[AddrKeyHash] =
            address match {
                case sa: ShelleyAddress =>
                    sa.payment match {
                        case kh: ShelleyPaymentPart.Key => Right(kh.hash)
                        case _: ShelleyPaymentPart.Script =>
                            Left(
                              WrongOutputType(WitnessKind.KeyBased, utxo, spend)
                            )
                    }
                case _ => Left(WrongOutputType(WitnessKind.KeyBased, utxo, spend))
            }

        def getPaymentScriptHash(address: Address): Result[ScriptHash] =
            address match {
                case sa: ShelleyAddress =>
                    sa.payment match {
                        case s: ShelleyPaymentPart.Script => Right(s.hash)
                        case _: ShelleyPaymentPart.Key =>
                            Left(WrongOutputType(WitnessKind.ScriptBased, utxo, spend))
                    }
                case _ =>
                    Left(WrongOutputType(WitnessKind.ScriptBased, utxo, spend))
            }

        for {
            _ <- assertNetworkId(utxo.output.address, spend)
            _ <- assertInputDoesNotAlreadyExist(utxo.input, spend)

            // Add input
            _ = modify0(
              unsafeCtxBodyL
                  .refocus(_.inputs)
                  .modify(inputs => TaggedSortedSet.from(appendDistinct(utxo.input, inputs.toSeq)))
            )
            // Add utxo to resolvedUtxos
            _ <- addResolvedUtxo(utxo, spend)
            // Handle the witness
            _ <- witness match {
                // Case 1: Key-locked input
                case _: PubKeyWitness.type =>
                    for {
                        // Extract the key hash, erroring if not a Shelley PKH address
                        keyHash <- getPaymentVerificationKeyHash(utxo.output.address)
                    } yield usePubKeyWitness(ExpectedSigner(keyHash))
                // Case 2: Native script-locked input
                // Ensure the hash matches the witness, handle the output components,
                // defer to witness handling
                case native: NativeScriptWitness =>
                    for {
                        scriptHash <- getPaymentScriptHash(utxo.output.address)
                        _ <- assertScriptHashMatchesSource(
                          scriptHash,
                          native.scriptSource,
                          spend
                        )
                    } yield useNativeScript(native.scriptSource, native.additionalSigners)

                // Case 3: Plutus script-locked input
                // Ensure the hash matches the witness, handle the output components,
                // defer to witness handling
                case plutus: ThreeArgumentPlutusScriptWitness =>
                    for {
                        scriptHash <- getPaymentScriptHash(utxo.output.address)
                        _ <- assertScriptHashMatchesSource(
                          scriptHash,
                          plutus.scriptSource,
                          spend
                        )
                        _ = usePlutusScript(plutus.scriptSource, plutus.additionalSigners)

                        detachedRedeemer = DetachedRedeemer(
                          plutus.redeemer,
                          RedeemerPurpose.ForSpend(utxo.input)
                        )
                        _ = modify0(ctx =>
                            ctx.focus(_.redeemers)
                                .modify(r => appendDistinct(detachedRedeemer, r))
                        )
                        _ <- useDatum(utxo, plutus.datum, spend)
                    } yield ()
            }
        } yield ()
    }

    private def useDatum(
        // TODO: this is used for errors only, I think utxoId should be sufficient
        utxo: Utxo,
        datum: Datum,
        step: TransactionBuilderStep
    ): Result[Unit] =
        for {
            _ <- utxo.output.datumOption match {
                case None =>
                    Left(DatumIsMissing(utxo, step))
                case Some(DatumOption.Inline(_)) =>
                    datum match {
                        case Datum.DatumInlined => unit
                        case Datum.DatumValue(_) =>
                            Left(DatumValueForUtxoWithInlineDatum(utxo, datum, step))
                    }
                case Some(DatumOption.Hash(datumHash)) =>
                    datum match {
                        case Datum.DatumInlined =>
                            Left(DatumWitnessNotProvided(utxo, step))
                        case Datum.DatumValue(providedDatum) =>
                            val computedHash: DataHash =
                                DataHash.fromByteString(providedDatum.dataHash)

                            if datumHash == computedHash then
                                modify0(
                                  unsafeCtxWitnessL
                                      .refocus(_.plutusData)
                                      .modify(plutusData =>
                                          KeepRaw(
                                            TaggedSortedMap(
                                              plutusData.value.toSortedMap
                                                  .updated(datumHash, KeepRaw(providedDatum))
                                            )
                                          )
                                      )
                                )
                                unit
                            else Left(IncorrectDatumHash(utxo, providedDatum, datumHash, step))
                    }
            }
        } yield ()

    // -------------------------------------------------------------------------
    // Send step
    // -------------------------------------------------------------------------

    private def useSend(send: Send): Result[Unit] =
        for {
            _ <- assertNetworkId(send.output.address, send)
            _ = modify0(
              unsafeCtxBodyL
                  .refocus(_.outputs)
                  // Intentionally not using pushUnique: we can create multiple outputs of the same shape
                  .modify(outputs => outputs :+ Sized(send.output))
            )
        } yield ()

    // -------------------------------------------------------------------------
    // MintAsset step
    // -------------------------------------------------------------------------

    private def useMint(mint: TransactionBuilderStep.Mint): Result[Unit] = {
        val scriptHash = mint.scriptHash
        val assetName = mint.assetName
        val amount = mint.amount
        val witness = mint.witness

        for {
            // Not allowed to mint 0
            _ <-
                if amount == 0
                then Left(CannotMintZero(scriptHash, assetName, mint))
                else unit

            // Since we allow monoidal mints, only the final redeemer is kept. We have to remove the old redeemer
            // before adding the new one, as well as if the monoidal sum of the amounts of this mint
            // and the existing mint cause the policyId entry to be removed from the mint map.
            _ = modify0(
              Focus[Context](_.redeemers).modify(
                _.filter(detachedRedeemer =>
                    detachedRedeemer.purpose match {
                        case RedeemerPurpose.ForMint(hash) => hash != scriptHash
                        case _                             => true
                    }
                )
              )
            )

            // Common witness handling
            _ <- useNonSpendingWitness(
              Operation.Minting(scriptHash),
              Credential.ScriptHash(scriptHash),
              witness,
              mint
            )

            // This is the tricky part. We handle `Mint` steps monoidally, so we can end up with
            // reciprocal mints/burns (i.e., +5, -5) that can do one of 3 main things:
            // 1.) If a mint for the given policyId does not already exist, it creates a new entry.
            // 2.) If a mint for the given policyId already exists, but no entry for the given asset name exists,
            //     it creates it.
            // 3.) If an entry exists for both the policyId and the assetname, it adds the amount in the step to
            //     the existing amount in the map.
            //
            //
            // In addition
            //  - Case (1) can:
            //    - a.) turn a `None : Option[Mint]` into a `Some`.
            //    - b.) add the policyId to an existing non-empty map
            //  - Case (3) can either:
            //    - a.) remove a policyId in the map entirely (if the sum of the amounts are 0 are there are no other
            //          assets in the map)
            //    - b.) Turn a `Some(mint) : Option[Mint]` into a `None`, if (a) is true and there were, in addition,
            //          no other policies left in the map.
            //
            // When case (3a) is true, the redeemer corresponding to the policyId must also be removed from the
            // detached redeemers set in the context.
            currentMint = ctx |> unsafeCtxBodyL.refocus(_.mint).get
            thisMint = MultiAsset.asset(scriptHash, assetName, amount)
            replaceMint = (newMint: Option[Mint]) =>
                modify0(unsafeCtxBodyL.refocus(_.mint).replace(newMint))

            _ <- currentMint match {
                // In this case the mint map was originally completely empty.
                case None =>
                    // Above, we check that the amount != 0. Thus:
                    // Case (1, a) -- create an entirely new, non-empty map
                    replaceMint(Some(Mint(thisMint)))
                    unit

                // If this is "Some", we know we have a non-empty mint map -- at least one policyId with at least 1
                // asset.
                case Some(existing: Mint) =>
                    existing.assets.get(scriptHash) match {
                        // No current entry for the script hash; this means that "currentAmount" would be 0
                        // (by invariants of the Mint type) and thus newAmount != 0. So we can add it to the existing map.
                        case None =>
                            // Case (1, b)
                            replaceMint(
                              Some(
                                Mint(
                                  MultiAsset(
                                    existing.assets
                                        .updated(scriptHash, SortedMap(assetName -> amount))
                                  )
                                )
                              )
                            )
                            unit

                        // There is a current entry for the script hash; thus, we need to look at the inner
                        // map to decide what to do.
                        case Some(innerMap) =>
                            innerMap.get(assetName) match {
                                // No current entry for the asset name, but there must be at least one other
                                // asset name associated with the script hash (by the invariants of the Mint type).
                                // Thus, currentAmount == 0, amount != 0 => newAmount != 0
                                case None => {
                                    // Case 2: add a new asset name to an existing policy map
                                    val newInnerMap = innerMap.updated(assetName, amount)
                                    val newOuterMap =
                                        existing.assets.updated(scriptHash, newInnerMap)
                                    replaceMint(Some(Mint(MultiAsset(newOuterMap))))
                                    unit
                                }
                                case Some(currentAmount) =>
                                    val newAmount: Long = currentAmount + amount
                                    val newInnerMap: SortedMap[AssetName, Long] =
                                        if newAmount == 0
                                        then innerMap.removed(assetName)
                                        else innerMap.updated(assetName, newAmount)

                                    if newInnerMap.isEmpty
                                    // The new inner map is empty -- this means that we must remove the policyId
                                    // from the outer map, and the corresponding redeemer from the Context
                                    then {
                                        val newOuterMap = existing.assets.removed(scriptHash)
                                        val removeRedeemer: Unit = modify0(
                                          Focus[Context](_.redeemers).modify(
                                            _.filter(detachedRedeemer =>
                                                detachedRedeemer.purpose match {
                                                    case RedeemerPurpose.ForMint(hash) =>
                                                        hash != scriptHash
                                                    case _ => true
                                                }
                                            )
                                          )
                                        )

                                        if newOuterMap.isEmpty
                                        then {
                                            // The new outerMap is empty. Thus, we have to set the TxBody Mint field to None
                                            // and remove the redeemer from the context
                                            replaceMint(None)
                                            unit
                                        } else {
                                            // The new outer map is NOT empty. Thus we must only replace the current
                                            // outer map with OUR outer map, and remove our redeemer
                                            replaceMint(
                                              Some(Mint(MultiAsset(newOuterMap)))
                                            )
                                            unit
                                        }
                                    }
                                    // In this case, the new inner map is NOT empty. Thus, we only must replace
                                    // the outer map with the updated inner map.
                                    else {
                                        val newOuterMap =
                                            existing.assets.updated(scriptHash, newInnerMap)
                                        replaceMint(Some(Mint(MultiAsset(newOuterMap))))
                                        unit
                                    }
                            }
                    }
            }
        } yield ()
    }

    private def useSpendWithDelayedRedeemer(
        delayedSpend: SpendWithDelayedRedeemer
    ): Result[Unit] = {
        val utxo = delayedSpend.utxo
        val validator = delayedSpend.validator
        val datum = delayedSpend.datum

        val dummyRedeemerData = Data.I(0)

        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = ScriptSource.PlutusScriptValue(validator),
          redeemer = dummyRedeemerData,
          datum = datum.map(Datum.DatumValue.apply).getOrElse(Datum.DatumInlined),
          additionalSigners = Set.empty
        )

        val spec = DelayedRedeemerSpec(
          utxo = utxo,
          redeemerBuilder = delayedSpend.redeemerBuilder,
          validator = validator,
          datum = datum,
          step = delayedSpend
        )

        for _ <- useSpend(Spend(utxo, witness))
        yield modify0(_.addDelayedRedeemer(spec))
    }

    // -------------------------------------------------------------------------
    // ReferenceOutput step
    // -------------------------------------------------------------------------

    private def useReferenceOutput(referenceOutput: ReferenceOutput): Result[Unit] =
        for {
            _ <- assertNetworkId(referenceOutput.utxo.output.address, referenceOutput)
            _ <- assertInputDoesNotAlreadyExist(referenceOutput.utxo.input, referenceOutput)

            _ = modify0(
              // Add the referenced utxo id to the tx body
              unsafeCtxBodyL
                  .refocus(_.referenceInputs)
                  .modify(inputs =>
                      TaggedSortedSet.from(
                        appendDistinct(referenceOutput.utxo.input, inputs.toSeq)
                      )
                  )
            )

            _ <- addResolvedUtxo(referenceOutput.utxo, referenceOutput)
        } yield ()

    // -------------------------------------------------------------------------
    // Fee step
    // -------------------------------------------------------------------------

    private def useFee(step: Fee): Result[Unit] = {
        val currentFee = ctx.transaction.body.value.fee.value
        currentFee match {
            case 0 =>
                modify0(unsafeCtxBodyL.refocus(_.fee).replace(step.fee))
                unit
            case nonZero => Left(FeeAlreadySet(nonZero, step))
        }
    }

    // -------------------------------------------------------------------------
    // ValidityStartSlot step
    // -------------------------------------------------------------------------

    private def useValidityStartSlot(step: ValidityStartSlot): Result[Unit] = {
        val currentValidityStartSlot = ctx.transaction.body.value.validityStartSlot
        currentValidityStartSlot match {
            case Some(existingSlot) =>
                Left(ValidityStartSlotAlreadySet(existingSlot, step))
            case None =>
                modify0(unsafeCtxBodyL.refocus(_.validityStartSlot).replace(Some(step.slot)))
                unit
        }
    }

    // -------------------------------------------------------------------------
    // ValidityEndSlot step
    // -------------------------------------------------------------------------

    private def useValidityEndSlot(step: ValidityEndSlot): Result[Unit] = {
        val currentValidityEndSlot = ctx.transaction.body.value.ttl
        currentValidityEndSlot match {
            case Some(existingSlot) =>
                Left(ValidityEndSlotAlreadySet(existingSlot, step))
            case None =>
                modify0(unsafeCtxBodyL.refocus(_.ttl).replace(Some(step.slot)))
                unit
        }
    }

    // -------------------------------------------------------------------------
    // AddCollateral step
    // -------------------------------------------------------------------------

    private def useAddCollateral(addCollateral: AddCollateral): Result[Unit] =
        for
            _ <- assertNetworkId(addCollateral.utxo.output.address, addCollateral)
            _ <- assertAdaOnlyPubkeyUtxo(addCollateral.utxo, addCollateral)
            _ <- addResolvedUtxo(addCollateral.utxo, addCollateral)
        yield modify0(
          // Add the collateral utxo to the tx body
          unsafeCtxBodyL
              .refocus(_.collateralInputs)
              .modify(inputs =>
                  TaggedSortedSet.from(
                    appendDistinct(addCollateral.utxo.input, inputs.toSeq)
                  )
              )
        )

    /** Ensure that the output is a pubkey output containing only ada. */
    private def assertAdaOnlyPubkeyUtxo(utxo: Utxo, step: TransactionBuilderStep): Result[Unit] =
        for {
            _ <-
                if !utxo.output.value.assets.isEmpty
                then Left(CollateralWithTokens(utxo, step))
                else unit
            addr: ShelleyAddress <- utxo.output.address match {
                case sa: ShelleyAddress  => Right(sa)
                case by: ByronAddress    => Left(ByronAddressesNotSupported(by, step))
                case stake: StakeAddress => Left(CollateralNotPubKey(utxo, step))
            }
            _ <- addr.payment match {
                case ShelleyPaymentPart.Key(_: AddrKeyHash) => unit
                case _                                      => Left(CollateralNotPubKey(utxo, step))
            }
        } yield ()

    // -------------------------------------------------------------------------
    // ModifyAuxiliaryData step
    // -------------------------------------------------------------------------

    private def useModifyAuxiliaryData(modifyAuxiliaryData: ModifyAuxiliaryData): Unit = {
        val oldData = ctx.transaction.auxiliaryData
        val newData = modifyAuxiliaryData.f(oldData.map(_.value)).map(KeepRaw(_))

        modify0(
          Focus[Context](_.transaction)
              .refocus(_.auxiliaryData)
              // Fixed for Scalus 0.12.1+ - auxiliaryData is now wrapped in KeepRaw
              .replace(newData)
        )

        val newHash = newData
            .map(someData => platform.blake2b_256(ByteString.unsafeFromArray(someData.raw)))
            .map(AuxiliaryDataHash.fromByteString)

        modify0(unsafeCtxBodyL.refocus(_.auxiliaryDataHash).replace(newHash))
    }

    // -------------------------------------------------------------------------
    // IssueCertificate step
    // -------------------------------------------------------------------------

    private def useIssueCertificate(issueCertificate: IssueCertificate): Result[Unit] =
        modify0(
          unsafeCtxBodyL
              .refocus(_.certificates)
              .modify(certificates =>
                  TaggedOrderedStrictSet.from(
                    appendDistinct(issueCertificate.cert, certificates.toSeq)
                  )
              )
        )
        useCertificateWitness(
          issueCertificate.cert,
          issueCertificate.witness,
          issueCertificate
        )

    def useCertificateWitness(
        cert: Certificate,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness,
        step: TransactionBuilderStep
    ): Result[Unit] = cert match {
        // FIXME: verify
        case Certificate.UnregCert(credential, _) =>
            for {
                _ <- (credential, witness) match {
                    // Credential.KeyHash
                    case (Credential.KeyHash(_), PubKeyWitness) => unit
                    case (Credential.KeyHash(_), witness: TwoArgumentPlutusScriptWitness) =>
                        Left(
                          UnneededDeregisterWitness(
                            StakeCredential(credential),
                            witness,
                            step
                          )
                        )
                    case (Credential.KeyHash(_), witness: NativeScriptWitness) =>
                        Left(
                          UnneededDeregisterWitness(
                            StakeCredential(credential),
                            witness,
                            step
                          )
                        )
                    // Credential.ScriptHash
                    case (Credential.ScriptHash(_), PubKeyWitness) =>
                        Left(
                          WrongCredentialType(
                            Operation.CertificateOperation(cert),
                            WitnessKind.KeyBased,
                            credential,
                            step
                          )
                        )
                    case (
                          Credential.ScriptHash(scriptHash),
                          witness: TwoArgumentPlutusScriptWitness
                        ) =>
                        assertScriptHashMatchesSource(scriptHash, witness.scriptSource, step)
                    case (Credential.ScriptHash(scriptHash), witness: NativeScriptWitness) =>
                        assertScriptHashMatchesSource(scriptHash, witness.scriptSource, step)
                }
                _ <- useNonSpendingWitness(
                  Operation.CertificateOperation(cert),
                  credential,
                  witness,
                  step
                )
            } yield ()
        case Certificate.StakeDelegation(credential, _) =>
            useNonSpendingWitness(
              Operation.CertificateOperation(cert),
              credential,
              witness,
              step
            )
        // FIXME: verify
        case Certificate.RegCert(_, _)                               => unit
        case Certificate.PoolRegistration(_, _, _, _, _, _, _, _, _) => unit
        case Certificate.PoolRetirement(_, _)                        => unit
        case Certificate.VoteDelegCert(credential, _) =>
            useNonSpendingWitness(
              Operation.CertificateOperation(cert),
              credential,
              witness,
              step
            )
        case Certificate.StakeVoteDelegCert(credential, _, _) =>
            useNonSpendingWitness(
              Operation.CertificateOperation(cert),
              credential,
              witness,
              step
            )
        case Certificate.StakeRegDelegCert(credential, _, _) =>
            useNonSpendingWitness(
              Operation.CertificateOperation(cert),
              credential,
              witness,
              step
            )
        case Certificate.VoteRegDelegCert(credential, _, _) =>
            useNonSpendingWitness(
              Operation.CertificateOperation(cert),
              credential,
              witness,
              step
            )
        case Certificate.StakeVoteRegDelegCert(credential, _, _, _) =>
            useNonSpendingWitness(
              Operation.CertificateOperation(cert),
              credential,
              witness,
              step
            )
        case Certificate.AuthCommitteeHotCert(_, _)    => unit // not supported
        case Certificate.ResignCommitteeColdCert(_, _) => unit // not supported
        case Certificate.RegDRepCert(credential, _, _) =>
            useNonSpendingWitness(
              Operation.CertificateOperation(cert),
              credential,
              witness,
              step
            )
        case Certificate.UnregDRepCert(credential, _) =>
            useNonSpendingWitness(
              Operation.CertificateOperation(cert),
              credential,
              witness,
              step
            )
        case Certificate.UpdateDRepCert(credential, _) =>
            useNonSpendingWitness(
              Operation.CertificateOperation(cert),
              credential,
              witness,
              step
            )
    }

    // -------------------------------------------------------------------------
    // WithdrawRewards step
    // -------------------------------------------------------------------------

    private def useWithdrawRewards(withdrawRewards: WithdrawRewards): Result[Unit] = {
        val rewardAccount = withdrawRewards.stakeCredential.credential match {
            case Credential.KeyHash(keyHash) =>
                // Convert AddrKeyHash to StakeKeyHash - they're likely the same underlying type?
                val stakeKeyHash = keyHash.asInstanceOf[StakeKeyHash]
                val stakeAddress =
                    StakeAddress(ctx.network, StakePayload.Stake(stakeKeyHash))
                RewardAccount(stakeAddress)
            case Credential.ScriptHash(scriptHash) =>
                val stakeAddress =
                    StakeAddress(ctx.network, StakePayload.Script(scriptHash))
                RewardAccount(stakeAddress)
        }

        modify0(
          unsafeCtxBodyL
              .refocus(_.withdrawals)
              .modify(withdrawals => {
                  val currentWithdrawals =
                      withdrawals.map(_.withdrawals).getOrElse(Map.empty)
                  Some(
                    Withdrawals(
                      SortedMap
                          .from(
                            currentWithdrawals + (rewardAccount -> withdrawRewards.amount)
                          )
                    )
                  )
              })
        )

        useNonSpendingWitness(
          Operation.Withdraw(rewardAccount.address),
          withdrawRewards.stakeCredential.credential,
          withdrawRewards.witness,
          withdrawRewards
        )
    }

    // -------------------------------------------------------------------------
    // SubmitProposal step
    // -------------------------------------------------------------------------

    private def useSubmitProposal(submitProposal: SubmitProposal): Result[Unit] =
        modify0(
          unsafeCtxBodyL
              .refocus(_.proposalProcedures)
              .modify(proposals =>
                  TaggedOrderedSet.from(
                    appendDistinct(submitProposal.proposal, proposals.toSeq)
                  )
              )
        )

        def getPolicyHash(govAction: GovAction): Option[ScriptHash] = govAction match {
            case GovAction.ParameterChange(_, _, policyHash)  => policyHash
            case GovAction.TreasuryWithdrawals(_, policyHash) => policyHash
            case _                                            => None
        }

        getPolicyHash(submitProposal.proposal.govAction) match {
            case None => unit
            case Some(policyHash) =>
                useNonSpendingWitness(
                  Operation.Proposing(submitProposal.proposal),
                  Credential.ScriptHash(policyHash),
                  submitProposal.witness,
                  submitProposal
                )
        }

    // -------------------------------------------------------------------------
    // SubmitVotingProcedure step
    // -------------------------------------------------------------------------

    private def useSubmitVotingProcedure(
        submitVotingProcedure: SubmitVotingProcedure
    ): Result[Unit] =
        modify0(
          unsafeCtxBodyL
              .refocus(_.votingProcedures)
              .modify(procedures => {
                  val currentProcedures = procedures
                      .map(_.procedures)
                      .getOrElse(
                        SortedMap.empty[Voter, SortedMap[GovActionId, VotingProcedure]]
                      )
                  Some(
                    VotingProcedures(
                      currentProcedures + (submitVotingProcedure.voter -> SortedMap
                          .from(submitVotingProcedure.votes))
                    )
                  )
              })
        )

        for {
            cred <- submitVotingProcedure.voter match {
                case Voter.StakingPoolKey(poolKeyHash) =>
                    val credential = Credential.KeyHash(poolKeyHash)
                    submitVotingProcedure.witness match {
                        case _: PubKeyWitness.type => Right(credential)
                        case witness: TwoArgumentPlutusScriptWitness =>
                            Left(
                              UnneededSpoVoteWitness(
                                credential,
                                witness,
                                submitVotingProcedure
                              )
                            )
                        case witness: NativeScriptWitness =>
                            Left(
                              UnneededSpoVoteWitness(
                                credential,
                                witness,
                                submitVotingProcedure
                              )
                            )
                    }
                case Voter.ConstitutionalCommitteeHotKey(credential) =>
                    Right(Credential.KeyHash(credential))
                case Voter.ConstitutionalCommitteeHotScript(scriptHash) =>
                    Right(Credential.ScriptHash(scriptHash))
                case Voter.DRepKey(credential) =>
                    Right(Credential.KeyHash(credential))
                case Voter.DRepScript(scriptHash) =>
                    Right(Credential.ScriptHash(scriptHash))
            }
            _ <- useNonSpendingWitness(
              Operation.Voting(submitVotingProcedure.voter),
              cred,
              submitVotingProcedure.witness,
              submitVotingProcedure
            )
        } yield ()

    // -------------------------------------------------------------------------
    // Common functions - using non-spending witness
    // -------------------------------------------------------------------------

    def useNonSpendingWitness(
        credAction: Operation,
        cred: Credential,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness,
        step: TransactionBuilderStep
    ): Result[Unit] =
        for {
            _ <- witness match {
                // Pubkey credential witness: add to expected signers
                case PubKeyWitness =>
                    for {
                        _ <- assertCredentialMatchesWitness(
                          credAction,
                          PubKeyWitness,
                          cred,
                          step
                        )
                        // Add key hash to expected signers
                        _ <- cred match {
                            case Credential.KeyHash(keyHash) =>
                                Right(usePubKeyWitness(ExpectedSigner(keyHash)))
                            case _ =>
                                Left(
                                  WrongCredentialType(
                                    credAction,
                                    WitnessKind.KeyBased,
                                    cred,
                                    step
                                  )
                                )
                        }
                    } yield ()
                case witness: NativeScriptWitness =>
                    for {
                        _ <- assertCredentialMatchesWitness(
                          credAction,
                          witness,
                          cred,
                          step
                        )
                    } yield useNativeScript(witness.scriptSource, witness.additionalSigners)
                case witness: TwoArgumentPlutusScriptWitness =>
                    for {
                        _ <- assertCredentialMatchesWitness(
                          credAction,
                          witness,
                          cred,
                          step
                        )
                    } yield {
                        usePlutusScript(witness.scriptSource, witness.additionalSigners)
                        val detachedRedeemer = DetachedRedeemer(
                          datum = witness.redeemer,
                          purpose = credAction match {
                              case Operation.Withdraw(stakeAddress) =>
                                  RedeemerPurpose.ForReward(RewardAccount(stakeAddress))
                              case Operation.CertificateOperation(cert) =>
                                  RedeemerPurpose.ForCert(cert)
                              case Operation.Minting(scriptHash) =>
                                  RedeemerPurpose.ForMint(scriptHash)
                              case Operation.Voting(voter) =>
                                  RedeemerPurpose.ForVote(voter)
                              case Operation.Proposing(proposal) =>
                                  RedeemerPurpose.ForPropose(proposal)
                          }
                        )
                        modify0(ctx =>
                            ctx.focus(_.redeemers)
                                .modify(redeemers => appendDistinct(detachedRedeemer, redeemers))
                        )
                    }
            }
        } yield ()

    def assertCredentialMatchesWitness[
        A <: PubKeyWitness.type | NativeScriptWitness | TwoArgumentPlutusScriptWitness
    ](
        action: Operation,
        witness: A,
        cred: Credential,
        step: TransactionBuilderStep
    )(using hwk: HasWitnessKind[A]): Result[Unit] = {

        val wrongCredErr = WrongCredentialType(action, hwk.witnessKind, cred, step)

        witness match {
            case PubKeyWitness =>
                cred.keyHashOption.toRight(wrongCredErr).void

            case witness: NativeScriptWitness =>
                for {
                    scriptHash <- cred.scriptHashOption.toRight(wrongCredErr)
                    _ <- assertScriptHashMatchesSource(scriptHash, witness.scriptSource, step)
                } yield ()

            case witness: TwoArgumentPlutusScriptWitness =>
                for {
                    scriptHash <- cred.scriptHashOption.toRight(wrongCredErr)
                    _ <- assertScriptHashMatchesSource(scriptHash, witness.scriptSource, step)
                } yield ()
        }
    }

    // -------------------------------------------------------------------------
    // Common functions for spending/non-spending witnesses
    // -------------------------------------------------------------------------

    /** Assert that the given script hash either matches the script provided directly, or is
      * otherwise already attached to the transaction as a CIP-33 script or as a pre-existing
      * witness.
      *
      * @param neededScriptHash
      *   The script hash we are expecting to find
      * @param scriptSource
      *   Where we should look for the script
      * @return
      */
    private def assertScriptHashMatchesSource(
        neededScriptHash: ScriptHash,
        scriptSource: ScriptSource[Script],
        step: TransactionBuilderStep
    ): Result[Unit] =
        scriptSource match {
            case ScriptSource.NativeScriptValue(script) =>
                assertScriptHashMatchesScript(neededScriptHash, script, step)
            case ScriptSource.NativeScriptAttached =>
                assertAttachedScriptExists(neededScriptHash, step)
            case ScriptSource.PlutusScriptValue(script) =>
                assertScriptHashMatchesScript(neededScriptHash, script, step)
            case ScriptSource.PlutusScriptAttached =>
                assertAttachedScriptExists(neededScriptHash, step)
        }

    private def assertScriptHashMatchesScript(
        scriptHash: ScriptHash,
        script: Script,
        step: TransactionBuilderStep
    ): Result[Unit] = {
        if scriptHash != script.scriptHash then Left(IncorrectScriptHash(script, scriptHash, step))
        else unit

    }

    /** Given a script hash, check the context to ensure that a script matching the given script
      * hash is attached to the transaction either as a CIP-33 ref script or in the witness set
      */
    private def assertAttachedScriptExists(
        scriptHash: ScriptHash,
        step: TransactionBuilderStep
    ): Result[Unit] =
        for {
            resolvedScripts <- AllResolvedScripts
                .allResolvedScripts(
                  ctx.transaction,
                  ctx.resolvedUtxos.utxos
                )
                .left
                .map(_ => ScriptResolutionError(step))
            _ <-
                if resolvedScripts.map(_.scriptHash).contains(scriptHash)
                then unit
                else Left(AttachedScriptNotFound(scriptHash, step))
        } yield ()

    // -------------------------------------------------------------------------
    // ScriptSource
    // -------------------------------------------------------------------------

    private def usePubKeyWitness(expectedSigner: ExpectedSigner): Unit =
        modify0(Focus[Context](_.expectedSigners).modify(_ + expectedSigner))

    private def useNativeScript(
        nativeScript: ScriptSource[Script.Native],
        additionalSigners: Set[ExpectedSigner]
    ): Unit = {
        // Regardless of how the witness is passed, add the additional signers
        modify0(Focus[Context](_.expectedSigners).modify(_ ++ additionalSigners))

        nativeScript match {
            case ScriptSource.NativeScriptValue(ns) =>
                modify0(
                  // Add the native script to the witness set
                  unsafeCtxWitnessL
                      .refocus(_.nativeScripts)
                      .modify(s => TaggedSortedMap.from(ns +: s.toMap.values.toSeq))
                )
            // Script should already be attached, see [[assertAttachedScriptExists]]
            case ScriptSource.NativeScriptAttached => ()
        }
    }

    /** Returns Left if the input already exists in txBody.inputs or txBody.refInputs */
    private def assertInputDoesNotAlreadyExist(
        input: TransactionInput,
        step: TransactionBuilderStep,
    ): Result[Unit] =
        if (ctx.transaction.body.value.inputs.toSet ++ ctx.transaction.body.value.referenceInputs.toSet)
                .contains(input)
        then Left(InputAlreadyExists(input, step))
        else unit

    private def usePlutusScript(
        plutusScript: ScriptSource[PlutusScript],
        additionalSigners: Set[ExpectedSigner]
    ): Unit = {
        // Add script's additional signers to txBody.requiredSigners
        modify0(
          (Focus[Context](_.transaction) >>> txBodyL)
              .refocus(_.requiredSigners)
              .modify((s: TaggedSortedSet[AddrKeyHash]) =>
                  TaggedSortedSet.from(s.toSet ++ additionalSigners.map(_.hash))
              )
        )

        // Add to expected signers
        modify0(Focus[Context](_.expectedSigners).modify(_ ++ additionalSigners))

        plutusScript match {
            case ScriptSource.PlutusScriptValue(ps: PlutusScript) =>
                // Add the script value to the appropriate field
                val f = ps match {
                    case v1: Script.PlutusV1 =>
                        unsafeCtxWitnessL
                            .refocus(_.plutusV1Scripts)
                            .modify(s =>
                                TaggedSortedStrictMap(s.toSortedMap.updated(v1.scriptHash, v1))
                            )
                    case v2: Script.PlutusV2 =>
                        unsafeCtxWitnessL
                            .refocus(_.plutusV2Scripts)
                            .modify(s =>
                                TaggedSortedStrictMap(s.toSortedMap.updated(v2.scriptHash, v2))
                            )
                    case v3: Script.PlutusV3 =>
                        unsafeCtxWitnessL
                            .refocus(_.plutusV3Scripts)
                            .modify(s =>
                                TaggedSortedStrictMap(s.toSortedMap.updated(v3.scriptHash, v3))
                            )
                }
                modify0(f)
            // Script should already be attached, see [[assertAttachedScriptExists]]
            case ScriptSource.PlutusScriptAttached => ()
        }
    }

    // -------------------------------------------------------------------------
    // Common assertions
    // -------------------------------------------------------------------------

    /** Ensure that the network id of the address matches the network id of the builder context.
      */
    private def assertNetworkId(addr: Address, step: TransactionBuilderStep): Result[Unit] =
        for {
            addrNetwork <- addr.getNetwork.toRight(ByronAddressesNotSupported(addr, step))
            _ <-
                if ctx.network != addrNetwork
                then Left(WrongNetworkId(addr, step))
                else unit
        } yield ()
}
