package scalus.cardano.ledger
package rules

import scala.collection.immutable.SortedMap

// It's the Conway CERTS rule predicate checks in cardano-ledger
object CertsValidator extends STS.Validator {
    override final type Error = TransactionException

    private case class ValidationState(
        rewards: Map[Credential, Coin],
        missingRewardAccounts: Map[RewardAccount, Coin] = Map.empty,
        nonDrainingWithdrawals: Map[RewardAccount, (Coin, Coin)] = Map.empty
    ) {
        def hasErrors: Boolean =
            missingRewardAccounts.nonEmpty || nonDrainingWithdrawals.nonEmpty

        def validateWithdrawal(withdrawal: (RewardAccount, Coin)): ValidationState = {
            val (rewardAccount, amount) = withdrawal
            val credential = rewardAccount.address.credential
            rewards.get(credential) match
                case None =>
                    copy(missingRewardAccounts =
                        missingRewardAccounts.updated(rewardAccount, amount)
                    )
                case Some(expected) if expected != amount =>
                    copy(nonDrainingWithdrawals =
                        nonDrainingWithdrawals.updated(rewardAccount, expected -> amount)
                    )
                case _ => this
        }
    }

    override def validate(context: Context, state: State, event: Event): Result = {
        val withdrawals: SortedMap[RewardAccount, Coin] =
            event.body.value.withdrawals.getOrElse(Withdrawals.empty).withdrawals

        val initialState = ValidationState(state.certState.dstate.rewards)
        val finalState = withdrawals.foldLeft(initialState)(_ validateWithdrawal _)

        if finalState.hasErrors then
            failure(
              TransactionException.WithdrawalsNotInRewardsException(
                event.id,
                finalState.missingRewardAccounts,
                finalState.nonDrainingWithdrawals
              )
            )
        else success
    }

    private[rules] def applyWithdrawals(
        rewards: Map[Credential, Coin],
        withdrawals: SortedMap[RewardAccount, Coin]
    ): Map[Credential, Coin] =
        withdrawals.foldLeft(rewards) { case (acc, (rewardAccount, amount)) =>
            val credential = rewardAccount.address.credential
            acc.get(credential) match
                case Some(expected) if expected == amount => acc - credential
                case _                                    => acc
        }
}
