package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.{AllNeededKeyHashes, AllWitnessesKeyHashes}

// It's Shelley.validateNeededWitnesses in cardano-ledger
object MissingKeyHashesValidator extends STS.Validator {
    override final type Error = TransactionException.BadInputsUTxOException |
        TransactionException.BadCollateralInputsUTxOException |
        TransactionException.MissingKeyHashesException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val utxo = state.utxos

        val allWitnessesKeyHashes = AllWitnessesKeyHashes.allWitnessesKeyHashes(event)

        val missingInputsKeyHashes =
            findMissingInputsKeyHashes(event, allWitnessesKeyHashes, utxo) match
                case Right(foundMissingInputsKeyHashes) => foundMissingInputsKeyHashes
                case Left(exception)                    => return failure(exception)

        val missingCollateralInputsKeyHashes =
            findMissingCollateralInputsKeyHashes(event, allWitnessesKeyHashes, utxo) match
                case Right(foundMissingCollateralInputsKeyHashes) =>
                    foundMissingCollateralInputsKeyHashes
                case Left(exception) => return failure(exception)

        val missingVotingProceduresKeyHashes =
            findMissingVotingProceduresKeyHashes(event, allWitnessesKeyHashes)

        val missingWithdrawalsKeyHashes =
            findMissingWithdrawalsKeyHashes(event, allWitnessesKeyHashes)

        val missingCertificatesKeyHashes =
            findMissingCertificatesKeyHashes(event, allWitnessesKeyHashes)

        val missingRequiredSignersKeyHashes =
            findMissingRequiredSignersKeyHashes(event, allWitnessesKeyHashes)

        if missingInputsKeyHashes.nonEmpty ||
            missingCollateralInputsKeyHashes.nonEmpty ||
            missingVotingProceduresKeyHashes.nonEmpty ||
            missingWithdrawalsKeyHashes.nonEmpty ||
            missingCertificatesKeyHashes.nonEmpty ||
            missingRequiredSignersKeyHashes.nonEmpty
        then
            return failure(
              TransactionException.MissingKeyHashesException(
                transactionId,
                missingInputsKeyHashes,
                missingCollateralInputsKeyHashes,
                missingVotingProceduresKeyHashes,
                missingWithdrawalsKeyHashes,
                missingCertificatesKeyHashes,
                missingRequiredSignersKeyHashes
              )
            )

        success
    }

    private def findMissingInputsKeyHashes(
        event: Event,
        allWitnessesKeyHashes: Set[AddrKeyHash],
        utxo: Utxos
    ): Either[TransactionException.BadInputsUTxOException, Set[AddrKeyHash | StakeKeyHash]] = {
        AllNeededKeyHashes.allNeededInputsKeyHashes(event, utxo).map {
            _.filterNot(keyHash => allWitnessesKeyHashes.exists(_ == keyHash))
        }
    }

    private def findMissingCollateralInputsKeyHashes(
        event: Event,
        allWitnessesKeyHashes: Set[AddrKeyHash],
        utxo: Utxos
    ): Either[TransactionException.BadCollateralInputsUTxOException, Set[
      AddrKeyHash | StakeKeyHash
    ]] = {
        AllNeededKeyHashes.allNeededCollateralInputsKeyHashes(event, utxo).map {
            _.filterNot(keyHash => allWitnessesKeyHashes.exists(_ == keyHash))
        }
    }

    private def findMissingVotingProceduresKeyHashes(
        event: Event,
        allWitnessesKeyHashes: Set[AddrKeyHash],
    ): Set[AddrKeyHash] = {
        AllNeededKeyHashes
            .allNeededVotingProceduresKeyHashesView(event)
            .filterNot(allWitnessesKeyHashes.contains)
            .toSet
    }

    private def findMissingWithdrawalsKeyHashes(
        event: Event,
        allWitnessesKeyHashes: Set[AddrKeyHash]
    ): Set[AddrKeyHash | StakeKeyHash] = {
        AllNeededKeyHashes
            .allNeededWithdrawalsKeyHashesView(event)
            .filterNot(keyHash => allWitnessesKeyHashes.exists(_ == keyHash))
            .toSet
    }

    private def findMissingCertificatesKeyHashes(
        event: Event,
        allWitnessesKeyHashes: Set[AddrKeyHash]
    ): Set[AddrKeyHash | PoolKeyHash] = {
        AllNeededKeyHashes
            .allNeededCertificatesKeyHashesView(event)
            .filterNot(keyHash => allWitnessesKeyHashes.exists(_ == keyHash))
            .toSet
    }

    private def findMissingRequiredSignersKeyHashes(
        event: Event,
        allWitnessesKeyHashes: Set[AddrKeyHash]
    ): Set[AddrKeyHash] = {
        AllNeededKeyHashes
            .allNeededRequiredSignersKeyHashes(event)
            .diff(allWitnessesKeyHashes)
    }
}
