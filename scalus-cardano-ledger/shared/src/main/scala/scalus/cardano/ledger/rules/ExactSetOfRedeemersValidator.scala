package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.{AllNeededScriptHashes, AllResolvedScripts}

// It's part of Babbage.hasExactSetOfRedeemers in cardano-ledger
object ExactSetOfRedeemersValidator extends STS.Validator {
    override final type Error = TransactionException.BadInputsUTxOException |
        TransactionException.BadReferenceInputsUTxOException |
        TransactionException.ExactSetOfRedeemersException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transaction = event
        val utxo = state.utxos

        for {
            neededScriptHashes <- AllNeededScriptHashes.allNeededScriptHashes(transaction, utxo)
            resolvedPlutusHashes <- AllResolvedScripts.allResolvedPlutusScriptHashes(
              transaction,
              utxo
            )
            neededPlutusScriptHashes = neededScriptHashes.intersect(resolvedPlutusHashes)
            neededRedeemers = buildNeededRedeemers(transaction, utxo, neededPlutusScriptHashes)
            actualRedeemers = transaction.witnessSet.redeemers
                .map { _.value.toMap.keySet }
                .getOrElse(Set.empty)
            extraRedeemers = actualRedeemers -- neededRedeemers
            missingRedeemers = neededRedeemers -- actualRedeemers
            _ <-
                if extraRedeemers.nonEmpty || missingRedeemers.nonEmpty then
                    failure(
                      TransactionException.ExactSetOfRedeemersException(
                        transaction.id,
                        extraRedeemers,
                        missingRedeemers
                      )
                    )
                else success
        } yield ()
    }

    private def buildNeededRedeemers(
        transaction: Transaction,
        utxo: Utxos,
        neededPlutusScriptHashes: Set[ScriptHash]
    ): Set[(RedeemerTag, Int)] = {
        val spendRedeemers = AllNeededScriptHashes
            .allNeededInputsScriptIndexHashesAndOutputs(transaction, utxo)
            .getOrElse(Set.empty)
            .view
            .filter { case (_, scriptHash, _) => neededPlutusScriptHashes.contains(scriptHash) }
            .map { case (index, _, _) => (RedeemerTag.Spend, index) }

        val mintRedeemers = AllNeededScriptHashes
            .allNeededMintScriptIndexHashes(transaction)
            .view
            .filter { case (_, scriptHash) => neededPlutusScriptHashes.contains(scriptHash) }
            .map { case (index, _) => (RedeemerTag.Mint, index) }

        val certRedeemers = AllNeededScriptHashes
            .allNeededCertificatesScriptIndexHashes(transaction)
            .view
            .filter { case (_, scriptHash) => neededPlutusScriptHashes.contains(scriptHash) }
            .map { case (index, _) => (RedeemerTag.Cert, index) }

        val rewardRedeemers = AllNeededScriptHashes
            .allNeededWithdrawalsScriptIndexHashes(transaction)
            .view
            .filter { case (_, scriptHash) => neededPlutusScriptHashes.contains(scriptHash) }
            .map { case (index, _) => (RedeemerTag.Reward, index) }

        val votingRedeemers = AllNeededScriptHashes
            .allNeededVotingProceduresScriptIndexHashes(transaction)
            .view
            .filter { case (_, scriptHash) => neededPlutusScriptHashes.contains(scriptHash) }
            .map { case (index, _) => (RedeemerTag.Voting, index) }

        val proposingRedeemers = AllNeededScriptHashes
            .allNeededProposalProceduresScriptIndexHashes(transaction)
            .view
            .filter { case (_, scriptHash) => neededPlutusScriptHashes.contains(scriptHash) }
            .map { case (index, _) => (RedeemerTag.Proposing, index) }

        (
          spendRedeemers ++ mintRedeemers ++ certRedeemers ++ rewardRedeemers ++ votingRedeemers ++ proposingRedeemers
        ).toSet
    }
}
