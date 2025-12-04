package scalus.cardano.ledger
package rules

import scalus.cardano.address.Network
import scala.collection.immutable.SortedMap

// It's part of Shelley.validateWrongNetworkWithdrawal in cardano-ledger
object WrongNetworkWithdrawalValidator extends STS.Validator {
    override final type Error = TransactionException.WrongNetworkWithdrawal

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val withdrawals = event.body.value.withdrawals.getOrElse(Withdrawals.empty).withdrawals
        val expectedNetwork = context.env.network

        val invalidWithdrawals = findInvalidWithdrawals(withdrawals, expectedNetwork)

        if invalidWithdrawals.nonEmpty then
            failure(
              TransactionException.WrongNetworkWithdrawal(
                transactionId,
                invalidWithdrawals
              )
            )
        else success
    }

    private def findInvalidWithdrawals(
        withdrawals: SortedMap[RewardAccount, Coin],
        expectedNetwork: Network
    ): IndexedSeq[(RewardAccount, Coin)] = {
        (
          for
              withdrawal @ (rewardAccount, _) <- withdrawals.view
              if rewardAccount.address.network != expectedNetwork
          yield withdrawal
        ).toIndexedSeq
    }
}
