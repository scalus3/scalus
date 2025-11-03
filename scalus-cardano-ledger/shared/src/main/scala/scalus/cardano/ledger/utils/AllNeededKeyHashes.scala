package scalus.cardano.ledger
package utils

import scala.collection.View
import scala.util.boundary
import scala.util.boundary.break

object AllNeededKeyHashes {
    def allNeededKeyHashes(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadCollateralInputsUTxOException,
      Set[AddrKeyHash | StakeKeyHash | PoolKeyHash]
    ] = {
        allNeededKeyHashesView(transaction, utxo).map(_.toSet)
    }

    def allNeededKeyHashesView(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadCollateralInputsUTxOException,
      View[AddrKeyHash | StakeKeyHash | PoolKeyHash]
    ] = {
        for
            allNeededInputsKeyHashes <- allNeededInputsKeyHashes(transaction, utxo)
            allNeededCollateralInputsKeyHashes <- allNeededCollateralInputsKeyHashes(
              transaction,
              utxo
            )
        yield
            allNeededInputsKeyHashes.view ++
                allNeededCollateralInputsKeyHashes.view
            allNeededVotingProceduresKeyHashesView(transaction) ++
                allNeededWithdrawalsKeyHashesView(transaction) ++
                allNeededCertificatesKeyHashesView(transaction) ++
                allNeededRequiredSignersKeyHashes(transaction).view
    }

    def allNeededInputsKeyHashes(
        transaction: Transaction,
        utxo: Utxos
    ): Either[TransactionException.BadInputsUTxOException, Set[AddrKeyHash | StakeKeyHash]] = {
        neededInputsKeyHashes(
          transaction.id,
          transaction.body.value.inputs.toSet,
          utxo,
          TransactionException.BadInputsUTxOException(_)
        )
    }

    def allNeededCollateralInputsKeyHashes(
        transaction: Transaction,
        utxo: Utxos
    ): Either[TransactionException.BadCollateralInputsUTxOException, Set[
      AddrKeyHash | StakeKeyHash
    ]] = {
        neededInputsKeyHashes(
          transaction.id,
          transaction.body.value.collateralInputs.toSet,
          utxo,
          TransactionException.BadCollateralInputsUTxOException(_)
        )
    }

    def allNeededVotingProceduresKeyHashes(
        transaction: Transaction
    ): Set[AddrKeyHash] = {
        allNeededVotingProceduresKeyHashesView(transaction).toSet
    }

    def allNeededVotingProceduresKeyHashesView(
        transaction: Transaction
    ): View[AddrKeyHash] = {
        val votingProcedures =
            transaction.body.value.votingProcedures.map(_.procedures).getOrElse(Map.empty)

        for
            voter <- votingProcedures.keySet.view
            keyHash <- voter.keyHashOption
        yield keyHash
    }

    def allNeededWithdrawalsKeyHashes(
        transaction: Transaction
    ): Set[AddrKeyHash | StakeKeyHash] = {
        allNeededWithdrawalsKeyHashesView(transaction).toSet
    }

    def allNeededWithdrawalsKeyHashesView(
        transaction: Transaction
    ): View[AddrKeyHash | StakeKeyHash] = {
        val withdrawals = transaction.body.value.withdrawals.map(_.withdrawals).getOrElse(Map.empty)

        for
            rewardAccount <- withdrawals.keySet.view
            keyHash <- rewardAccount.keyHashOption
        yield keyHash
    }

    def allNeededCertificatesKeyHashes(
        transaction: Transaction
    ): Set[AddrKeyHash | PoolKeyHash] = {
        allNeededCertificatesKeyHashesView(transaction).toSet
    }

    def allNeededCertificatesKeyHashesView(
        transaction: Transaction
    ): View[AddrKeyHash | PoolKeyHash] = {
        val certificates = transaction.body.value.certificates
        for
            certificate <- certificates.toSeq.view
            keyHashes <- certificate.keyHashes
        yield keyHashes
    }

    def allNeededRequiredSignersKeyHashes(
        transaction: Transaction
    ): Set[AddrKeyHash] = transaction.body.value.requiredSigners.toSet

    private def neededInputsKeyHashes[
        ExceptionT <: TransactionException.BadInputsUTxOException |
            TransactionException.BadCollateralInputsUTxOException
    ](
        transactionId: TransactionHash,
        inputs: Set[TransactionInput],
        utxo: Utxos,
        missingUTxOException: TransactionHash => ExceptionT
    ): Either[ExceptionT, Set[AddrKeyHash | StakeKeyHash]] = boundary {
        val result = for
            input <- inputs
            keyHash <- utxo.get(input) match
                case Some(output) => output.address.keyHashOption
                // This check allows to be an order independent in the sequence of validation rules
                case None => break(Left(missingUTxOException(transactionId)))
        yield keyHash
        Right(result)
    }
}
