package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.{AllNeededScriptHashes, AllResolvedScripts}

// It's babbageMissingScripts in cardano-ledger
object MissingOrExtraScriptHashesValidator extends STS.Validator {
    override final type Error = TransactionException.BadInputsUTxOException |
        TransactionException.BadReferenceInputsUTxOException |
        TransactionException.MissingOrExtraScriptHashesException

    override def validate(context: Context, state: State, event: Event): Result = {
        for
            scriptHashesValidator <- ScriptHashesValidator(event, state.utxos)
            _ <- scriptHashesValidator.validate()
        yield ()
    }

    private case class ScriptHashesValidator private (
        transactionId: TransactionHash,
        allWitnessesScriptHashes: Set[ScriptHash],
        allNeededInputsScriptHashesNoRefs: Set[ScriptHash],
        allNeededMintScriptHashesNoRefs: Set[ScriptHash],
        allNeededVotingProceduresScriptHashesNoRefs: Set[ScriptHash],
        allNeededWithdrawalsScriptHashesNoRefs: Set[ScriptHash],
        allNeededProposalProceduresScriptHashesNoRefs: Set[ScriptHash],
        allNeededCertificatesScriptHashesNoRefs: Set[ScriptHash]
    ) {
        def validate(): Either[
          TransactionException.MissingOrExtraScriptHashesException,
          Unit
        ] = {
            val missingInputsScriptHashes =
                allNeededInputsScriptHashesNoRefs.diff(allWitnessesScriptHashes)
            val missingMintScriptHashes =
                allNeededMintScriptHashesNoRefs.diff(allWitnessesScriptHashes)
            val missingVotingProceduresScriptHashes =
                allNeededVotingProceduresScriptHashesNoRefs.diff(allWitnessesScriptHashes)
            val missingWithdrawalsScriptHashes =
                allNeededWithdrawalsScriptHashesNoRefs.diff(allWitnessesScriptHashes)
            val missingProposalProceduresScriptHashes =
                allNeededProposalProceduresScriptHashesNoRefs.diff(allWitnessesScriptHashes)
            val missingCertificatesScriptHashes =
                allNeededCertificatesScriptHashesNoRefs.diff(allWitnessesScriptHashes)

            val allNeededScriptHashesNoRefs = (
              allNeededInputsScriptHashesNoRefs.view ++
                  allNeededMintScriptHashesNoRefs.view ++
                  allNeededVotingProceduresScriptHashesNoRefs.view ++
                  allNeededWithdrawalsScriptHashesNoRefs.view ++
                  allNeededProposalProceduresScriptHashesNoRefs.view ++
                  allNeededCertificatesScriptHashesNoRefs.view
            ).toSet

            val extraScriptHashes = allWitnessesScriptHashes.diff(allNeededScriptHashesNoRefs)

            if missingInputsScriptHashes.nonEmpty ||
                missingMintScriptHashes.nonEmpty ||
                missingVotingProceduresScriptHashes.nonEmpty ||
                missingWithdrawalsScriptHashes.nonEmpty ||
                missingProposalProceduresScriptHashes.nonEmpty ||
                missingCertificatesScriptHashes.nonEmpty ||
                extraScriptHashes.nonEmpty
            then
                Left(
                  TransactionException.MissingOrExtraScriptHashesException(
                    transactionId,
                    missingInputsScriptHashes,
                    missingMintScriptHashes,
                    missingVotingProceduresScriptHashes,
                    missingWithdrawalsScriptHashes,
                    missingProposalProceduresScriptHashes,
                    missingCertificatesScriptHashes,
                    extraScriptHashes
                  )
                )
            else Right(())
        }
    }

    private object ScriptHashesValidator {
        def apply(
            event: Event,
            utxo: Utxos
        ): Either[
          TransactionException.BadInputsUTxOException |
              TransactionException.BadReferenceInputsUTxOException,
          ScriptHashesValidator
        ] = {
            for
                allProvidedReferenceScriptHashes <- AllResolvedScripts
                    .allProvidedReferenceScriptHashes(event, utxo)

                allNeededInputsScriptHashes <- AllNeededScriptHashes.allNeededInputsScriptHashes(
                  event,
                  utxo
                )

                allWitnessesScriptHashes = AllResolvedScripts.allWitnessesScriptHashes(event)

                allNeededInputsScriptHashesNoRefs = allNeededInputsScriptHashes.diff(
                  allProvidedReferenceScriptHashes
                )

                allNeededMintScriptHashesNoRefs = AllNeededScriptHashes
                    .allNeededMintScriptHashes(event)
                    .diff(allProvidedReferenceScriptHashes)

                allNeededVotingProceduresScriptHashesNoRefs = AllNeededScriptHashes
                    .allNeededVotingProceduresScriptHashesView(event)
                    .filterNot(allProvidedReferenceScriptHashes.contains)
                    .toSet

                allNeededWithdrawalsScriptHashesNoRefs = AllNeededScriptHashes
                    .allNeededWithdrawalsScriptHashesView(event)
                    .filterNot(allProvidedReferenceScriptHashes.contains)
                    .toSet

                allNeededProposalProceduresScriptHashesNoRefs = AllNeededScriptHashes
                    .allNeededProposalProceduresScriptHashesView(event)
                    .filterNot(allProvidedReferenceScriptHashes.contains)
                    .toSet

                allNeededCertificatesScriptHashesNoRefs = AllNeededScriptHashes
                    .allNeededCertificatesScriptHashesView(event)
                    .filterNot(allProvidedReferenceScriptHashes.contains)
                    .toSet
            yield ScriptHashesValidator(
              event.id,
              allWitnessesScriptHashes,
              allNeededInputsScriptHashesNoRefs,
              allNeededMintScriptHashesNoRefs,
              allNeededVotingProceduresScriptHashesNoRefs,
              allNeededWithdrawalsScriptHashesNoRefs,
              allNeededProposalProceduresScriptHashesNoRefs,
              allNeededCertificatesScriptHashesNoRefs
            )
        }
    }
}
