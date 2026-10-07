package scalus.cardano.ledger
package rules

import scala.collection.immutable.SortedMap

// It's the Conway CERTS rule predicate checks in cardano-ledger
object CertsValidator extends STS.Validator {
    override final type Error = TransactionException

    private case class ValidationState(
        accounts: Map[Credential, ConwayAccountState],
        missingRewardAccounts: Map[RewardAccount, Coin] = Map.empty,
        nonDrainingWithdrawals: Map[RewardAccount, (Coin, Coin)] = Map.empty
    ) {
        def hasErrors: Boolean =
            missingRewardAccounts.nonEmpty || nonDrainingWithdrawals.nonEmpty

        infix def validateWithdrawal(withdrawal: (RewardAccount, Coin)): ValidationState = {
            val (rewardAccount, amount) = withdrawal
            val credential = rewardAccount.address.credential
            accounts.get(credential).map(_.balance) match
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

    /** A phase-2-invalid tx skips this check: Conway LEDGER checks withdrawals only when `isValid`
      * is true, spec [SC-3f].
      */
    override def validate(context: Context, state: State, event: Event): Result = {
        if !event.isValid then success
        else
            val withdrawals: SortedMap[RewardAccount, Coin] =
                event.body.value.withdrawals.getOrElse(Withdrawals.empty).withdrawals

            val initialState = ValidationState(state.certState.dstate.accounts)
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

    /** Subtracts each withdrawal from its account balance. The account stays registered, spec
      * [SC-2], [SC-3]. A withdrawal that names no account, or exceeds the balance, changes nothing:
      * [[validate]] rejects it.
      */
    private[rules] def applyWithdrawals(
        accounts: Map[Credential, ConwayAccountState],
        withdrawals: SortedMap[RewardAccount, Coin]
    ): Map[Credential, ConwayAccountState] =
        withdrawals.foldLeft(accounts) { case (acc, (rewardAccount, amount)) =>
            val credential = rewardAccount.address.credential
            acc.get(credential) match
                case Some(account) if amount <= account.balance =>
                    acc.updated(credential, account.copy(balance = account.balance - amount))
                case _ => acc
        }
}
