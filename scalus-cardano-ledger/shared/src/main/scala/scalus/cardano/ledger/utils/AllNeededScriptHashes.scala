package scalus.cardano.ledger
package utils

import scala.collection.View
import scala.util.boundary
import scala.util.boundary.break

object AllNeededScriptHashes {
    def allNeededScriptHashes(
        transaction: Transaction,
        utxos: Utxos
    ): Either[TransactionException.BadInputsUTxOException, Set[ScriptHash]] = {
        allNeededScriptHashesView(transaction, utxos).map(_.toSet)
    }

    def allNeededScriptHashesView(
        transaction: Transaction,
        utxos: Utxos
    ): Either[TransactionException.BadInputsUTxOException, View[ScriptHash]] = {
        for allNeededInputsScriptHashes <- allNeededInputsScriptHashes(transaction, utxos)
        yield allNeededInputsScriptHashes.view ++
            allNeededMintScriptHashesView(transaction) ++
            allNeededVotingProceduresScriptHashesView(transaction) ++
            allNeededWithdrawalsScriptHashesView(transaction) ++
            allNeededProposalProceduresScriptHashesView(transaction) ++
            allNeededCertificatesScriptHashesView(transaction)
    }

    def allNeededInputsScriptHashes(
        transaction: Transaction,
        utxos: Utxos,
    ): Either[TransactionException.BadInputsUTxOException, Set[ScriptHash]] = {
        allNeededInputsScriptIndexHashesAndOutputs(transaction, utxos).map {
            indexedHashesAndOutputs =>
                indexedHashesAndOutputs.map { case (_, scriptHash, _) => scriptHash }
        }
    }

    def allNeededInputsScriptIndexHashesAndOutputs(
        transaction: Transaction,
        utxos: Utxos,
    ): Either[TransactionException.BadInputsUTxOException, Set[
      (Int, ScriptHash, TransactionOutput)
    ]] = boundary {
        val transactionId = transaction.id
        val inputs = transaction.body.value.inputs.toSet.view

        val result = for
            (input, index) <- inputs.zipWithIndex
            output <- utxos.get(input) match
                case someOutput @ Some(_) => someOutput
                // This check allows to be an order independent in the sequence of validation rules
                case None =>
                    break(Left(TransactionException.BadInputsUTxOException(transactionId)))
            scriptHash <- output.address.scriptHashOption
        yield (index, scriptHash, output)
        Right(result.toSet)
    }

    def allNeededMintScriptHashes(
        transaction: Transaction
    ): Set[ScriptHash] = {
        allNeededMintScriptHashesView(transaction).toSet
    }

    def allNeededMintScriptHashesView(
        transaction: Transaction
    ): View[ScriptHash] = {
        allNeededMintScriptIndexHashesView(transaction).map(_._2)
    }

    def allNeededMintScriptIndexHashes(
        transaction: Transaction
    ): Set[(Int, ScriptHash)] = {
        allNeededMintScriptIndexHashesView(transaction).toSet
    }

    def allNeededMintScriptIndexHashesView(
        transaction: Transaction
    ): View[(Int, ScriptHash)] = {
        transaction.body.value.mint
            .getOrElse(MultiAsset.empty)
            .assets
            .keySet
            .view
            .zipWithIndex
            .map { case (policyId, index) =>
                (index, policyId)
            }
    }

    def allNeededVotingProceduresScriptHashes(
        transaction: Transaction
    ): Set[ScriptHash] = {
        allNeededVotingProceduresScriptHashesView(transaction).toSet
    }

    def allNeededVotingProceduresScriptHashesView(
        transaction: Transaction
    ): View[ScriptHash] = {
        allNeededVotingProceduresScriptIndexHashesView(transaction).map(_._2)
    }

    def allNeededVotingProceduresScriptIndexHashes(
        transaction: Transaction
    ): Set[(Int, ScriptHash)] = {
        allNeededVotingProceduresScriptIndexHashesView(transaction).toSet
    }

    def allNeededVotingProceduresScriptIndexHashesView(
        transaction: Transaction
    ): View[(Int, ScriptHash)] = {
        val voters = transaction.body.value.votingProcedures
            .map(_.procedures)
            .getOrElse(Map.empty)
            .keySet
            .view

        for
            (voter, index) <- voters.zipWithIndex
            scriptHash <- voter.scriptHashOption
        yield (index, scriptHash)
    }

    def allNeededWithdrawalsScriptHashes(
        transaction: Transaction
    ): Set[ScriptHash] = {
        allNeededWithdrawalsScriptHashesView(transaction).toSet
    }

    def allNeededWithdrawalsScriptHashesView(
        transaction: Transaction
    ): View[ScriptHash] = {
        allNeededWithdrawalsScriptIndexHashesView(transaction).map(_._2)
    }

    def allNeededWithdrawalsScriptIndexHashes(
        transaction: Transaction
    ): Set[(Int, ScriptHash)] = {
        allNeededWithdrawalsScriptIndexHashesView(transaction).toSet
    }

    def allNeededWithdrawalsScriptIndexHashesView(
        transaction: Transaction
    ): View[(Int, ScriptHash)] = {
        val rewardAccounts = transaction.body.value.withdrawals
            .map(_.withdrawals)
            .getOrElse(Map.empty)
            .keySet
            .view

        for
            (rewardAccount, index) <- rewardAccounts.zipWithIndex
            scriptHash <- rewardAccount.scriptHashOption
        yield (index, scriptHash)
    }

    def allNeededProposalProceduresScriptHashes(
        transaction: Transaction
    ): Set[ScriptHash] = {
        allNeededProposalProceduresScriptHashesView(transaction).toSet
    }

    def allNeededProposalProceduresScriptHashesView(
        transaction: Transaction
    ): View[ScriptHash] = {
        allNeededProposalProceduresScriptIndexHashesView(transaction).map(_._2)
    }

    def allNeededProposalProceduresScriptIndexHashes(
        transaction: Transaction
    ): Set[(Int, ScriptHash)] = {
        allNeededProposalProceduresScriptIndexHashesView(transaction).toSet
    }

    def allNeededProposalProceduresScriptIndexHashesView(
        transaction: Transaction
    ): View[(Int, ScriptHash)] = {
        val govActions = transaction.body.value.proposalProcedures.toSeq.view
            .map(_.govAction)

        for
            (govAction, index) <- govActions.zipWithIndex
            scriptHash <- govAction.scriptHashOption
        yield (index, scriptHash)
    }

    def allNeededCertificatesScriptHashes(
        transaction: Transaction
    ): Set[ScriptHash] = {
        allNeededCertificatesScriptHashesView(transaction).toSet
    }

    def allNeededCertificatesScriptHashesView(
        transaction: Transaction
    ): View[ScriptHash] = {
        allNeededCertificatesScriptIndexHashesView(transaction).map(_._2)
    }

    def allNeededCertificatesScriptIndexHashes(
        transaction: Transaction
    ): Set[(Int, ScriptHash)] = {
        allNeededCertificatesScriptIndexHashesView(transaction).toSet
    }

    def allNeededCertificatesScriptIndexHashesView(
        transaction: Transaction
    ): View[(Int, ScriptHash)] = {
        val certificates = transaction.body.value.certificates.toSeq.view

        for
            (certificate, index) <- certificates.zipWithIndex
            scriptHash <- certificate.scriptHashOption
        yield (index, scriptHash)
    }
}
