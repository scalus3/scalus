package scalus.cardano.ledger
package utils

object MissingKeyHashes {
    def validateAllMissingKeyHashes(
        transaction: Transaction,
        allWitnessesKeyHashes: Set[AddrKeyHash],
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadCollateralInputsUTxOException |
          TransactionException.MissingKeyHashesException,
      Unit
    ] = {
        val transactionId = transaction.id

        val missingInputsKeyHashes =
            findMissingInputsKeyHashes(transaction, allWitnessesKeyHashes, utxos) match
                case Right(foundMissingInputsKeyHashes) => foundMissingInputsKeyHashes
                case Left(exception)                    => return Left(exception)

        val missingCollateralInputsKeyHashes =
            findMissingCollateralInputsKeyHashes(transaction, allWitnessesKeyHashes, utxos) match
                case Right(foundMissingCollateralInputsKeyHashes) =>
                    foundMissingCollateralInputsKeyHashes
                case Left(exception) => return Left(exception)

        val missingVotingProceduresKeyHashes =
            findMissingVotingProceduresKeyHashes(transaction, allWitnessesKeyHashes)

        val missingWithdrawalsKeyHashes =
            findMissingWithdrawalsKeyHashes(transaction, allWitnessesKeyHashes)

        val missingCertificatesKeyHashes =
            findMissingCertificatesKeyHashes(transaction, allWitnessesKeyHashes)

        val missingRequiredSignersKeyHashes =
            findMissingRequiredSignersKeyHashes(transaction, allWitnessesKeyHashes)

        if missingInputsKeyHashes.nonEmpty ||
            missingCollateralInputsKeyHashes.nonEmpty ||
            missingVotingProceduresKeyHashes.nonEmpty ||
            missingWithdrawalsKeyHashes.nonEmpty ||
            missingCertificatesKeyHashes.nonEmpty ||
            missingRequiredSignersKeyHashes.nonEmpty
        then
            return Left(
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

        Right(())
    }

    def findAllMissingKeyHashes(
        transaction: Transaction,
        allWitnessesKeyHashes: Set[AddrKeyHash],
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadCollateralInputsUTxOException,
      Set[AddrKeyHash | StakeKeyHash | PoolKeyHash]
    ] = {
        for {
            missingInputsKeyHashes <- findMissingInputsKeyHashes(
              transaction,
              allWitnessesKeyHashes,
              utxos
            )
            missingCollateralInputsKeyHashes <- findMissingCollateralInputsKeyHashes(
              transaction,
              allWitnessesKeyHashes,
              utxos
            )
        } yield missingInputsKeyHashes ++
            missingCollateralInputsKeyHashes ++
            findMissingVotingProceduresKeyHashes(transaction, allWitnessesKeyHashes) ++
            findMissingWithdrawalsKeyHashes(transaction, allWitnessesKeyHashes) ++
            findMissingCertificatesKeyHashes(transaction, allWitnessesKeyHashes) ++
            findMissingRequiredSignersKeyHashes(transaction, allWitnessesKeyHashes)
    }

    def findMissingInputsKeyHashes(
        transaction: Transaction,
        allWitnessesKeyHashes: Set[AddrKeyHash],
        utxos: Utxos
    ): Either[TransactionException.BadInputsUTxOException, Set[AddrKeyHash | StakeKeyHash]] = {
        AllNeededKeyHashes.allNeededInputsKeyHashes(transaction, utxos).map {
            _.filterNot(keyHash => allWitnessesKeyHashes.exists(_ == keyHash))
        }
    }

    def findMissingCollateralInputsKeyHashes(
        transaction: Transaction,
        allWitnessesKeyHashes: Set[AddrKeyHash],
        utxos: Utxos
    ): Either[TransactionException.BadCollateralInputsUTxOException, Set[
      AddrKeyHash | StakeKeyHash
    ]] = {
        AllNeededKeyHashes.allNeededCollateralInputsKeyHashes(transaction, utxos).map {
            _.filterNot(keyHash => allWitnessesKeyHashes.exists(_ == keyHash))
        }
    }

    def findMissingVotingProceduresKeyHashes(
        transaction: Transaction,
        allWitnessesKeyHashes: Set[AddrKeyHash],
    ): Set[AddrKeyHash] = {
        AllNeededKeyHashes
            .allNeededVotingProceduresKeyHashesView(transaction)
            .filterNot(allWitnessesKeyHashes.contains)
            .toSet
    }

    def findMissingWithdrawalsKeyHashes(
        transaction: Transaction,
        allWitnessesKeyHashes: Set[AddrKeyHash]
    ): Set[AddrKeyHash | StakeKeyHash] = {
        AllNeededKeyHashes
            .allNeededWithdrawalsKeyHashesView(transaction)
            .filterNot(keyHash => allWitnessesKeyHashes.exists(_ == keyHash))
            .toSet
    }

    def findMissingCertificatesKeyHashes(
        transaction: Transaction,
        allWitnessesKeyHashes: Set[AddrKeyHash]
    ): Set[AddrKeyHash | PoolKeyHash] = {
        AllNeededKeyHashes
            .allNeededCertificatesKeyHashesView(transaction)
            .filterNot(keyHash => allWitnessesKeyHashes.exists(_ == keyHash))
            .toSet
    }

    def findMissingRequiredSignersKeyHashes(
        transaction: Transaction,
        allWitnessesKeyHashes: Set[AddrKeyHash]
    ): Set[AddrKeyHash] = {
        AllNeededKeyHashes
            .allNeededRequiredSignersKeyHashes(transaction)
            .diff(allWitnessesKeyHashes)
    }
}
