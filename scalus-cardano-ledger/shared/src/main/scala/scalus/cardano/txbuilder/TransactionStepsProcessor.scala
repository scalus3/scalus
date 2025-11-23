package scalus.cardano.txbuilder

import cats.*
import cats.implicits.*
import monocle.syntax.all.*
import monocle.{Focus, Lens}
import scalus.builtin.Builtins.{blake2b_224, serialiseData}
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
import scalus.|>

import scala.collection.immutable.SortedMap

private class TransactionStepsProcessor(private var _ctx: Context) {

    /** Transaction builder monad. Retains context at point of failure, if there's any.
      */
    type Result[A] =
        Either[StepError | RedeemerIndexingInternalError, A]

    // Helpers to cut down on type signature noise
    private def pure0[A](value: A): Result[A] = Right(value)

    private def liftF0[A](
        either: Either[StepError | RedeemerIndexingInternalError, A]
    ): Result[A] = either

    def ctx: Context = _ctx

    private def modify0(f: Context => Context): Result[Unit] =
        _ctx = f(ctx)
        pure0(())

    private def get0: Result[Context] =
        pure0(ctx)

    def applySteps(steps: Seq[TransactionBuilderStep]): Result[Unit] = {
        for {
            _ <- processSteps(steps)
            ctx0 <- get0
            res <- liftF0(
              TransactionConversion
                  .fromEditableTransactionSafe(
                    EditableTransaction(
                      transaction = ctx0.transaction,
                      redeemers = ctx0.redeemers.toVector
                    )
                  )
                  .left
                  .map(detachedRedeemer => RedeemerIndexingInternalError(detachedRedeemer, steps))
            )
            // Replace the transaction in the context, keeping the rest
            _ <- modify0(Focus[Context](_.transaction).replace(res))

            // Replace delayed redeemers if any exist
            _ <-
                if ctx0.delayedRedeemerSpecs.nonEmpty then {
                    for {
                        ctx1 <- get0
                        updatedRedeemers <- liftF0(
                          replaceDelayedRedeemers(
                            ctx1.redeemers,
                            ctx1.delayedRedeemerSpecs,
                            ctx1.transaction
                          )
                        )
                        _ <- modify0(_.replaceRedeemers(updatedRedeemers))
                    } yield ()
                } else {
                    pure0(())
                }
        } yield ()
    }

    private def processSteps(steps: Seq[TransactionBuilderStep]): Result[Unit] =
        steps.traverse_(processStep)

    // Tries to add the output to the resolved utxo set, throwing an error if
    // the input is already mapped to another output
    private def addResolvedUtxo(
        utxo: Utxo,
        step: TransactionBuilderStep
    ): Result[Unit] =
        for {
            ctx <- get0
            mbNewUtxos = ctx.resolvedUtxos.addUtxo(utxo)
            _ <- mbNewUtxos match {
                case None =>
                    liftF0(
                      Left(
                        ResolvedUtxosIncoherence(
                          input = utxo.input,
                          existingOutput = ctx.resolvedUtxos.utxos(utxo.input),
                          incoherentOutput = utxo.output,
                          step = step
                        )
                      )
                    )
                case Some(utxos) => modify0(Focus[Context](_.resolvedUtxos).replace(utxos))
            }
        } yield ()

    private def processStep(step: TransactionBuilderStep): Result[Unit] = step match {

        case spend: TransactionBuilderStep.Spend =>
            useSpend(spend)

        case delayedSpend: TransactionBuilderStep.SpendWithDelayedRedeemer =>
            useSpendWithDelayedRedeemer(delayedSpend)

        case send: TransactionBuilderStep.Send =>
            useSend(send)

        case mint: TransactionBuilderStep.Mint =>
            useMint(mint)

        case referenceOutput: TransactionBuilderStep.ReferenceOutput =>
            useReferenceOutput(referenceOutput)

        case fee: TransactionBuilderStep.Fee =>
            useFee(fee)

        case validityStartSlot: TransactionBuilderStep.ValidityStartSlot =>
            useValidityStartSlot(validityStartSlot)

        case validityEndSlot: TransactionBuilderStep.ValidityEndSlot =>
            useValidityEndSlot(validityEndSlot)

        case addCollateral: TransactionBuilderStep.AddCollateral =>
            useAddCollateral(addCollateral)

        case modifyAuxiliaryData: TransactionBuilderStep.ModifyAuxiliaryData =>
            useModifyAuxiliaryData(modifyAuxiliaryData)

        case issueCertificate: TransactionBuilderStep.IssueCertificate =>
            useIssueCertificate(issueCertificate)

        case withdrawRewards: TransactionBuilderStep.WithdrawRewards =>
            useWithdrawRewards(withdrawRewards)

        case submitProposal: TransactionBuilderStep.SubmitProposal =>
            useSubmitProposal(submitProposal)

        case submitVotingProcedure: TransactionBuilderStep.SubmitVotingProcedure =>
            useSubmitVotingProcedure(submitVotingProcedure)
    }

    // -------------------------------------------------------------------------
    // Spend step
    // -------------------------------------------------------------------------

    /** Tries to modify the transaction to make it consume a given output and add the requisite
      * signature(s) to the Context's _.expectedSigners. Uses witness to try to satisfy spending
      * requirements.
      */
    private def useSpend(
        spend: TransactionBuilderStep.Spend
    ): Result[Unit] = {

        val utxo = spend.utxo
        val witness = spend.witness

        // Extract the key hash, erroring if not a Shelley PKH address
        def getPaymentVerificationKeyHash(address: Address): Result[AddrKeyHash] =
            liftF0(address match {
                case sa: ShelleyAddress =>
                    sa.payment match {
                        case kh: ShelleyPaymentPart.Key => Right(kh.hash)
                        case _: ShelleyPaymentPart.Script =>
                            Left(
                              WrongOutputType(WitnessKind.KeyBased, utxo, spend)
                            )
                    }
                case _ => Left(WrongOutputType(WitnessKind.KeyBased, utxo, spend))
            })

        def getPaymentScriptHash(address: Address): Result[ScriptHash] =
            liftF0(address match {
                case sa: ShelleyAddress =>
                    sa.payment match {
                        case s: ShelleyPaymentPart.Script => Right(s.hash)
                        case _: ShelleyPaymentPart.Key =>
                            Left(
                              WrongOutputType(WitnessKind.ScriptBased, utxo, spend)
                            )

                    }
                case _ =>
                    Left(WrongOutputType(WitnessKind.ScriptBased, utxo, spend))
            })

        for {
            _ <- assertNetworkId(utxo.output.address, spend)
            _ <- assertInputDoesNotAlreadyExist(utxo.input, spend)

            // Add input
            _ <- modify0(
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
                        _ <- usePubKeyWitness(ExpectedSigner(keyHash))
                    } yield ()
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
                        _ <- useNativeScript(native.scriptSource, native.additionalSigners)
                    } yield ()

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
                        _ <- usePlutusScript(plutus.scriptSource, plutus.additionalSigners)

                        detachedRedeemer = DetachedRedeemer(
                          plutus.redeemer,
                          RedeemerPurpose.ForSpend(utxo.input)
                        )
                        _ <- modify0(ctx =>
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
                    liftF0(
                      Left(DatumIsMissing(utxo, step))
                    )
                case Some(DatumOption.Inline(_)) =>
                    datum match {
                        case Datum.DatumInlined => pure0(())
                        case Datum.DatumValue(_) =>
                            liftF0(
                              Left(DatumValueForUtxoWithInlineDatum(utxo, datum, step))
                            )
                    }
                case Some(DatumOption.Hash(datumHash)) =>
                    datum match {
                        case Datum.DatumInlined =>
                            liftF0(Left(DatumWitnessNotProvided(utxo, step)))
                        case Datum.DatumValue(providedDatum) =>
                            // TODO: is that correct? Upstream Data.dataHash extension?
                            val computedHash: DataHash =
                                DataHash.fromByteString(
                                  blake2b_224(serialiseData(providedDatum))
                                )

                            if datumHash == computedHash then {
                                modify0(
                                  unsafeCtxWitnessL
                                      .refocus(_.plutusData)
                                      .modify(plutusData =>
                                          KeepRaw.apply(
                                            TaggedSortedMap.from(
                                              appendDistinct(
                                                KeepRaw.apply(providedDatum),
                                                plutusData.value.toMap.values.toSeq
                                              )
                                            )
                                          )
                                      )
                                )
                            } else {
                                liftF0(
                                  Left(
                                    IncorrectDatumHash(utxo, providedDatum, datumHash, step)
                                  )
                                )
                            }
                    }
            }
        } yield ()

    // -------------------------------------------------------------------------
    // Send step
    // -------------------------------------------------------------------------

    private def useSend(send: TransactionBuilderStep.Send): Result[Unit] =
        for {
            _ <- assertNetworkId(send.output.address, send)
            _ <- modify0(
              unsafeCtxBodyL
                  .refocus(_.outputs)
                  // Intentionally not using pushUnique: we can create multiple outputs of the same shape
                  .modify(outputs => outputs :+ Sized(send.output))
            )
        } yield ()

    // -------------------------------------------------------------------------
    // MintAsset step
    // -------------------------------------------------------------------------

    private def useMint(
        mint: TransactionBuilderStep.Mint
    ): Result[Unit] = {
        val scriptHash = mint.scriptHash
        val assetName = mint.assetName
        val amount = mint.amount
        val witness = mint.witness

        for {
            // Not allowed to mint 0
            _ <-
                if amount == 0
                then liftF0(Left(CannotMintZero(scriptHash, assetName, mint)))
                else pure0(())

            // Since we allow monoidal mints, only the final redeemer is kept. We have to remove the old redeemer
            // before adding the new one, as well as if the monoidal sum of the amounts of this mint
            // and the existing mint cause the policyId entry to be removed from the mint map.
            removeRedeemer: Result[Unit] =
                modify0(
                  Focus[Context](_.redeemers).modify(
                    _.filter(detachedRedeemer =>
                        detachedRedeemer.purpose match {
                            case RedeemerPurpose.ForMint(hash) => hash != scriptHash
                            case _                             => true
                        }
                    )
                  )
                )

            _ <- removeRedeemer

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
            ctx <- get0
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
                                        val removeRedeemer = modify0(
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
                                            for {
                                                _ <- replaceMint(None)
                                                _ <- removeRedeemer

                                            } yield ()
                                        } else
                                            for {
                                                // The new outer map is NOT empty. Thus we must only replace the current
                                                // outer map with OUR outer map, and remove our redeemer
                                                _ <- replaceMint(
                                                  Some(Mint(MultiAsset(newOuterMap)))
                                                )
                                                _ <- removeRedeemer
                                            } yield ()
                                    }
                                    // In this case, the new inner map is NOT empty. Thus, we only must replace
                                    // the outer map with the updated inner map.
                                    else {
                                        val newOuterMap =
                                            existing.assets.updated(scriptHash, newInnerMap)
                                        replaceMint(Some(Mint(MultiAsset(newOuterMap))))
                                    }
                            }
                    }
            }
        } yield ()
    }

    private def useSpendWithDelayedRedeemer(
        delayedSpend: TransactionBuilderStep.SpendWithDelayedRedeemer
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

        for {
            _ <- useSpend(TransactionBuilderStep.Spend(utxo, witness))
            _ <- modify0(_.addDelayedRedeemer(spec))
        } yield ()
    }

    // -------------------------------------------------------------------------
    // ReferenceOutput step
    // -------------------------------------------------------------------------

    private def useReferenceOutput(
        referenceOutput: TransactionBuilderStep.ReferenceOutput
    ): Result[Unit] =
        for {
            _ <- assertNetworkId(referenceOutput.utxo.output.address, referenceOutput)
            _ <- assertInputDoesNotAlreadyExist(referenceOutput.utxo.input, referenceOutput)

            _ <- modify0(
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

    private def useFee(step: TransactionBuilderStep.Fee): Result[Unit] = for {
        ctx <- get0
        currentFee = ctx.transaction.body.value.fee.value
        _ <- currentFee match {
            case 0 =>
                modify0(
                  unsafeCtxBodyL
                      .refocus(_.fee)
                      .replace(step.fee)
                )
            case nonZero => liftF0(Left(FeeAlreadySet(nonZero, step)))
        }
    } yield ()

    // -------------------------------------------------------------------------
    // ValidityStartSlot step
    // -------------------------------------------------------------------------

    private def useValidityStartSlot(
        step: TransactionBuilderStep.ValidityStartSlot
    ): Result[Unit] =
        for {
            ctx <- get0
            currentValidityStartSlot = ctx.transaction.body.value.validityStartSlot
            _ <- currentValidityStartSlot match {
                case Some(existingSlot) =>
                    liftF0(Left(ValidityStartSlotAlreadySet(existingSlot, step)))
                case None =>
                    modify0(
                      unsafeCtxBodyL
                          .refocus(_.validityStartSlot)
                          .replace(Some(step.slot))
                    )
            }
        } yield ()

    // -------------------------------------------------------------------------
    // ValidityEndSlot step
    // -------------------------------------------------------------------------

    private def useValidityEndSlot(
        step: TransactionBuilderStep.ValidityEndSlot
    ): Result[Unit] =
        for {
            ctx <- get0
            currentValidityEndSlot = ctx.transaction.body.value.ttl
            _ <- currentValidityEndSlot match {
                case Some(existingSlot) =>
                    liftF0(Left(ValidityEndSlotAlreadySet(existingSlot, step)))
                case None =>
                    modify0(
                      unsafeCtxBodyL
                          .refocus(_.ttl)
                          .replace(Some(step.slot))
                    )
            }
        } yield ()

    // -------------------------------------------------------------------------
    // AddCollateral step
    // -------------------------------------------------------------------------

    private def useAddCollateral(
        addCollateral: TransactionBuilderStep.AddCollateral
    ): Result[Unit] =
        for {
            _ <- assertNetworkId(addCollateral.utxo.output.address, addCollateral)
            _ <- assertAdaOnlyPubkeyUtxo(addCollateral.utxo, addCollateral)
            _ <- addResolvedUtxo(addCollateral.utxo, addCollateral)
            _ <- modify0(
              // Add the collateral utxo to the tx body
              unsafeCtxBodyL
                  .refocus(_.collateralInputs)
                  .modify(inputs =>
                      TaggedSortedSet.from(
                        appendDistinct(addCollateral.utxo.input, inputs.toSeq)
                      )
                  )
            )
        } yield ()

    /** Ensure that the output is a pubkey output containing only ada. */
    private def assertAdaOnlyPubkeyUtxo(
        utxo: Utxo,
        step: TransactionBuilderStep
    ): Result[Unit] =
        for {
            _ <-
                if !utxo.output.value.assets.isEmpty
                then
                    liftF0(
                      Left(CollateralWithTokens(utxo, step))
                    )
                else pure0(())
            addr: ShelleyAddress <- utxo.output.address match {
                case sa: ShelleyAddress  => pure0(sa)
                case by: ByronAddress    => liftF0(Left(ByronAddressesNotSupported(by, step)))
                case stake: StakeAddress => liftF0(Left(CollateralNotPubKey(utxo, step)))
            }
            _ <- addr.payment match {
                case ShelleyPaymentPart.Key(_: AddrKeyHash) => pure0(())
                case _ => liftF0(Left(CollateralNotPubKey(utxo, step)))
            }
        } yield ()

    // -------------------------------------------------------------------------
    // ModifyAuxiliaryData step
    // -------------------------------------------------------------------------

    private def useModifyAuxiliaryData(
        modifyAuxiliaryData: TransactionBuilderStep.ModifyAuxiliaryData
    ): Result[Unit] = {
        for {
            ctx <- get0
            oldData = ctx.transaction.auxiliaryData

            newData = modifyAuxiliaryData.f(oldData.map(_.value)).map(KeepRaw(_))
            _ <- modify0(
              Focus[Context](_.transaction)
                  .refocus(_.auxiliaryData)
                  // Fixed for Scalus 0.12.1+ - auxiliaryData is now wrapped in KeepRaw
                  .replace(newData)
            )

            newHash = newData
                .map(someData => platform.blake2b_256(ByteString.unsafeFromArray(someData.raw)))
                .map(AuxiliaryDataHash.fromByteString)
            _ <- modify0(unsafeCtxBodyL.refocus(_.auxiliaryDataHash).replace(newHash))
        } yield ()
    }

    // -------------------------------------------------------------------------
    // IssueCertificate step
    // -------------------------------------------------------------------------

    private def useIssueCertificate(
        issueCertificate: TransactionBuilderStep.IssueCertificate
    ): Result[Unit] =
        for {
            _ <- modify0(
              unsafeCtxBodyL
                  .refocus(_.certificates)
                  .modify(certificates =>
                      TaggedOrderedStrictSet.from(
                        appendDistinct(issueCertificate.cert, certificates.toSeq)
                      )
                  )
            )
            _ <- useCertificateWitness(
              issueCertificate.cert,
              issueCertificate.witness,
              issueCertificate
            )
        } yield ()

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
                    case (Credential.KeyHash(_), PubKeyWitness) => pure0(())
                    case (Credential.KeyHash(_), witness: TwoArgumentPlutusScriptWitness) =>
                        liftF0(
                          Left(
                            UnneededDeregisterWitness(
                              StakeCredential(credential),
                              witness,
                              step
                            )
                          )
                        )
                    case (Credential.KeyHash(_), witness: NativeScriptWitness) =>
                        liftF0(
                          Left(
                            UnneededDeregisterWitness(
                              StakeCredential(credential),
                              witness,
                              step
                            )
                          )
                        )
                    // Credential.ScriptHash
                    case (Credential.ScriptHash(_), PubKeyWitness) =>
                        liftF0(
                          Left(
                            WrongCredentialType(
                              Operation.CertificateOperation(cert),
                              WitnessKind.KeyBased,
                              credential,
                              step
                            )
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
        case Certificate.RegCert(_, _) =>
            pure0(())
        case Certificate.PoolRegistration(_, _, _, _, _, _, _, _, _) =>
            pure0(())
        case Certificate.PoolRetirement(_, _) =>
            pure0(())
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
        case Certificate.AuthCommitteeHotCert(_, _)    => pure0(()) // not supported
        case Certificate.ResignCommitteeColdCert(_, _) => pure0(()) // not supported
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

    private def useWithdrawRewards(
        withdrawRewards: TransactionBuilderStep.WithdrawRewards
    ): Result[Unit] =
        for {
            ctx <- get0

            rewardAccount = withdrawRewards.stakeCredential.credential match {
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

            _ <- modify0(
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

            _ <- useNonSpendingWitness(
              Operation.Withdraw(rewardAccount.address),
              withdrawRewards.stakeCredential.credential,
              withdrawRewards.witness,
              withdrawRewards
            )
        } yield ()

    // -------------------------------------------------------------------------
    // SubmitProposal step
    // -------------------------------------------------------------------------

    private def useSubmitProposal(
        submitProposal: TransactionBuilderStep.SubmitProposal
    ): Result[Unit] =
        for {
            _ <- modify0(
              unsafeCtxBodyL
                  .refocus(_.proposalProcedures)
                  .modify(proposals =>
                      TaggedOrderedSet.from(
                        appendDistinct(submitProposal.proposal, proposals.toSeq)
                      )
                  )
            )
            _ <- {
                def getPolicyHash(govAction: GovAction): Option[ScriptHash] = govAction match {
                    case GovAction.ParameterChange(_, _, policyHash)  => policyHash
                    case GovAction.TreasuryWithdrawals(_, policyHash) => policyHash
                    case _                                            => None
                }

                getPolicyHash(submitProposal.proposal.govAction) match {
                    case None =>
                        pure0(())
                    case Some(policyHash) =>
                        useNonSpendingWitness(
                          Operation.Proposing(submitProposal.proposal),
                          Credential.ScriptHash(policyHash),
                          submitProposal.witness,
                          submitProposal
                        )
                }
            }
        } yield ()

    // -------------------------------------------------------------------------
    // SubmitVotingProcedure step
    // -------------------------------------------------------------------------

    private def useSubmitVotingProcedure(
        submitVotingProcedure: TransactionBuilderStep.SubmitVotingProcedure
    ): Result[Unit] =
        for {
            _ <- modify0(
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
            _ <- for {
                cred <- submitVotingProcedure.voter match {
                    case Voter.StakingPoolKey(poolKeyHash) =>
                        val credential = Credential.KeyHash(poolKeyHash)
                        submitVotingProcedure.witness match {
                            case _: PubKeyWitness.type => pure0(credential)
                            case witness: TwoArgumentPlutusScriptWitness =>
                                liftF0(
                                  Left(
                                    UnneededSpoVoteWitness(
                                      credential,
                                      witness,
                                      submitVotingProcedure
                                    )
                                  )
                                )
                            case witness: NativeScriptWitness =>
                                liftF0(
                                  Left(
                                    UnneededSpoVoteWitness(
                                      credential,
                                      witness,
                                      submitVotingProcedure
                                    )
                                  )
                                )
                        }
                    case Voter.ConstitutionalCommitteeHotKey(credential) =>
                        pure0(
                          Credential.KeyHash(credential)
                        )
                    case Voter.ConstitutionalCommitteeHotScript(scriptHash) =>
                        pure0(
                          Credential.ScriptHash(scriptHash)
                        )
                    case Voter.DRepKey(credential) =>
                        pure0(
                          Credential.KeyHash(credential)
                        )
                    case Voter.DRepScript(scriptHash) =>
                        pure0(
                          Credential.ScriptHash(scriptHash)
                        )
                }
                _ <- useNonSpendingWitness(
                  Operation.Voting(submitVotingProcedure.voter),
                  cred,
                  submitVotingProcedure.witness,
                  submitVotingProcedure
                )
            } yield ()
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
                                usePubKeyWitness(ExpectedSigner(keyHash))
                            case _ =>
                                liftF0(
                                  Left(
                                    WrongCredentialType(
                                      credAction,
                                      WitnessKind.KeyBased,
                                      cred,
                                      step
                                    )
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
                        _ <- useNativeScript(witness.scriptSource, witness.additionalSigners)
                    } yield ()
                case witness: TwoArgumentPlutusScriptWitness =>
                    for {
                        _ <- assertCredentialMatchesWitness(
                          credAction,
                          witness,
                          cred,
                          step
                        )
                        _ <- usePlutusScript(witness.scriptSource, witness.additionalSigners)
                        _ <- {
                            val detachedRedeemer = DetachedRedeemer(
                              datum = witness.redeemer,
                              purpose = credAction match {
                                  case Operation.Withdraw(stakeAddress) =>
                                      RedeemerPurpose.ForReward(
                                        RewardAccount(stakeAddress)
                                      )
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
                                    .modify(redeemers =>
                                        appendDistinct(detachedRedeemer, redeemers)
                                    )
                            )
                        }
                    } yield ()
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

        val result: Result[Unit] = witness match {
            case PubKeyWitness =>
                cred.keyHashOption match {
                    case Some(_) => pure0(())
                    case None    => liftF0(Left(wrongCredErr))
                }

            case witness: NativeScriptWitness =>
                for {
                    scriptHash <- cred.scriptHashOption match {
                        case Some(hash) => pure0(hash)
                        case None       => liftF0(Left(wrongCredErr))
                    }
                    _ <- assertScriptHashMatchesSource(scriptHash, witness.scriptSource, step)
                } yield ()

            case witness: TwoArgumentPlutusScriptWitness =>
                for {
                    scriptHash <- cred.scriptHashOption match {
                        case Some(hash) => pure0(hash)
                        case None       => liftF0(Left(wrongCredErr))
                    }
                    _ <- assertScriptHashMatchesSource(scriptHash, witness.scriptSource, step)
                } yield ()
        }
        result
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
        if scriptHash != script.scriptHash then {
            liftF0(
              Left(IncorrectScriptHash(script, scriptHash, step))
            )
        } else {
            pure0(())
        }
    }

    /** Given a script hash, check the context to ensure that a script matching the given script
      * hash is attached to the transaction either as a CIP-33 ref script or in the witness set
      */
    private def assertAttachedScriptExists(
        scriptHash: ScriptHash,
        step: TransactionBuilderStep
    ): Result[Unit] =
        for {
            ctx <- get0
            resolvedScripts <- liftF0(
              AllResolvedScripts
                  .allResolvedScripts(
                    ctx.transaction,
                    ctx.resolvedUtxos.utxos
                  )
                  .left
                  .map(_ => ScriptResolutionError(step))
            )
            _ <-
                if resolvedScripts.map(_.scriptHash).contains(scriptHash)
                then pure0(())
                else
                    liftF0(
                      Left(
                        AttachedScriptNotFound(scriptHash, step)
                      )
                    )
        } yield ()

    // -------------------------------------------------------------------------
    // ScriptSource
    // -------------------------------------------------------------------------

    private def usePubKeyWitness(expectedSigner: ExpectedSigner): Result[Unit] =
        modify0(Focus[Context](_.expectedSigners).modify(_ + expectedSigner))

    private def useNativeScript(
        nativeScript: ScriptSource[Script.Native],
        additionalSigners: Set[ExpectedSigner]
    ): Result[Unit] = {
        for {
            // Regardless of how the witness is passed, add the additional signers
            _ <- modify0(
              Focus[Context](_.expectedSigners).modify(_ ++ additionalSigners)
            )

            _ <- nativeScript match {
                case ScriptSource.NativeScriptValue(ns) =>
                    modify0(
                      // Add the native script to the witness set
                      unsafeCtxWitnessL
                          .refocus(_.nativeScripts)
                          .modify(s =>
                              TaggedSortedMap.from(appendDistinct(ns, s.toMap.values.toSeq))
                          )
                    )
                // Script should already be attached, see [[assertAttachedScriptExists]]
                case ScriptSource.NativeScriptAttached => pure0(())
            }
        } yield ()
    }

    /** Returns Left if the input already exists in txBody.inputs or txBody.refInputs */
    private def assertInputDoesNotAlreadyExist(
        input: TransactionInput,
        step: TransactionBuilderStep,
    ): Result[Unit] =
        for {
            state <- get0
            _ <-
                if (state.transaction.body.value.inputs.toSet ++ state.transaction.body.value.referenceInputs.toSet)
                        .contains(input)
                then liftF0(Left(InputAlreadyExists(input, step)))
                else pure0(())
        } yield ()

    private def usePlutusScript(
        plutusScript: ScriptSource[PlutusScript],
        additionalSigners: Set[ExpectedSigner]
    ): Result[Unit] =
        for {
            // Add script's additional signers to txBody.requiredSigners
            _ <- modify0(
              (Focus[Context](_.transaction) >>> txBodyL)
                  .refocus(_.requiredSigners)
                  .modify((s: TaggedSortedSet[AddrKeyHash]) =>
                      TaggedSortedSet.from(
                        s.toSet ++ additionalSigners.map(_.hash)
                      )
                  )
            )

            // Add to expected signers
            _ <- modify0(
              Focus[Context](_.expectedSigners).modify(_ ++ additionalSigners)
            )

            _ <- plutusScript match {
                case ScriptSource.PlutusScriptValue(ps: PlutusScript) =>
                    // Add the script value to the appropriate field
                    ps match {
                        case v1: Script.PlutusV1 =>
                            modify0(
                              unsafeCtxWitnessL
                                  .refocus(_.plutusV1Scripts)
                                  .modify(s =>
                                      TaggedSortedStrictMap.from(
                                        appendDistinct(v1, s.toMap.values.toSeq)
                                      )
                                  )
                            )
                        case v2: Script.PlutusV2 =>
                            modify0(
                              unsafeCtxWitnessL
                                  .refocus(_.plutusV2Scripts)
                                  .modify(s =>
                                      TaggedSortedStrictMap.from(
                                        appendDistinct(v2, s.toMap.values.toSeq)
                                      )
                                  )
                            )
                        case v3: Script.PlutusV3 =>
                            modify0(
                              unsafeCtxWitnessL
                                  .refocus(_.plutusV3Scripts)
                                  .modify(s =>
                                      TaggedSortedStrictMap.from(
                                        appendDistinct(v3, s.toMap.values.toSeq)
                                      )
                                  )
                            )
                    }
                // Script should already be attached, see [[assertAttachedScriptExists]]
                case ScriptSource.PlutusScriptAttached => pure0(())
            }
        } yield ()

    // -------------------------------------------------------------------------
    // Common assertions
    // -------------------------------------------------------------------------

    /** Ensure that the network id of the address matches the network id of the builder context.
      */
    private def assertNetworkId(addr: Address, step: TransactionBuilderStep): Result[Unit] =
        for {
            context: Context <- get0
            addrNetwork <- addr.getNetwork match
                case Some(network) => pure0(network)
                case None =>
                    liftF0(
                      Left(ByronAddressesNotSupported(addr, step))
                    )
            _ <-
                if context.network != addrNetwork
                then
                    liftF0(
                      Left(WrongNetworkId(addr, step))
                    )
                else pure0(())
        } yield ()
}
