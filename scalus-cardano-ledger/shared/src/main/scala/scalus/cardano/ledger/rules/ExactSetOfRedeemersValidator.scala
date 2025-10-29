package scalus.cardano.ledger.rules

import scalus.cardano.ledger.{RedeemerTag, ScriptHash, Transaction, TransactionException, Utxos}
import scalus.cardano.ledger.utils.{AllNeededScriptHashes, AllResolvedScripts}

// It's part of Babbage.hasExactSetOfRedeemers in cardano-ledger
object ExactSetOfRedeemersValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transaction = event
        val utxo = state.utxos

        val result = for {
            neededScriptHashes <- AllNeededScriptHashes.allNeededScriptHashes(transaction, utxo)
            resolvedPlutusHashes <- AllResolvedScripts.allResolvedPlutusScriptHashes(
              transaction,
              utxo
            )
            plutusScriptHashes = neededScriptHashes.intersect(resolvedPlutusHashes)
            neededRedeemers = buildNeededRedeemers(transaction, utxo, plutusScriptHashes)
            actualRedeemers = transaction.witnessSet.redeemers match {
                case Some(redeemers) => redeemers.value.toMap.keySet
                case None            => Set.empty[(RedeemerTag, Int)]
            }
        } yield {
            val extraRedeemers = actualRedeemers -- neededRedeemers
            val missingRedeemers = neededRedeemers -- actualRedeemers
            if extraRedeemers.nonEmpty || missingRedeemers.nonEmpty then {
                failure(
                  TransactionException.ExactSetOfRedeemersException(
                    transaction.id,
                    extraRedeemers,
                    missingRedeemers
                  )
                )
            } else {
                success
            }
        }
        result.flatten
    }

    private def buildNeededRedeemers(
        transaction: Transaction,
        utxo: Utxos,
        plutusScriptHashes: Set[ScriptHash]
    ): Set[(RedeemerTag, Int)] = {
        val spendRedeemers = AllNeededScriptHashes
            .allNeededInputsScriptIndexHashesAndOutputs(transaction, utxo)
            .getOrElse(Set.empty)
            .filter { case (_, scriptHash, _) => plutusScriptHashes.contains(scriptHash) }
            .map { case (index, _, _) => (RedeemerTag.Spend, index) }

        val mintRedeemers = AllNeededScriptHashes
            .allNeededMintScriptIndexHashes(transaction)
            .filter { case (_, scriptHash) => plutusScriptHashes.contains(scriptHash) }
            .map { case (index, _) => (RedeemerTag.Mint, index) }

        val certRedeemers = AllNeededScriptHashes
            .allNeededCertificatesScriptIndexHashes(transaction)
            .filter { case (_, scriptHash) => plutusScriptHashes.contains(scriptHash) }
            .map { case (index, _) => (RedeemerTag.Cert, index) }

        val rewardRedeemers = AllNeededScriptHashes
            .allNeededWithdrawalsScriptIndexHashes(transaction)
            .filter { case (_, scriptHash) => plutusScriptHashes.contains(scriptHash) }
            .map { case (index, _) => (RedeemerTag.Reward, index) }

        val votingRedeemers = AllNeededScriptHashes
            .allNeededVotingProceduresScriptIndexHashes(transaction)
            .filter { case (_, scriptHash) => plutusScriptHashes.contains(scriptHash) }
            .map { case (index, _) => (RedeemerTag.Voting, index) }

        val proposingRedeemers = AllNeededScriptHashes
            .allNeededProposalProceduresScriptIndexHashes(transaction)
            .filter { case (_, scriptHash) => plutusScriptHashes.contains(scriptHash) }
            .map { case (index, _) => (RedeemerTag.Proposing, index) }

        spendRedeemers ++ mintRedeemers ++ certRedeemers ++ rewardRedeemers ++ votingRedeemers ++ proposingRedeemers
    }
}
