package scalus.cardano.ledger
package rules

import scala.collection.immutable.SortedMap

// It's the Conway CERTS state transition in cardano-ledger
object CertsMutator extends STS.Mutator {
    override final type Error = TransactionException

    override def transit(context: Context, state: State, event: Event): Result =
        CertsValidator.validate(context, state, event) match
            case Left(err) => failure(err)
            case Right(_) =>
                val withdrawals: SortedMap[RewardAccount, Coin] =
                    event.body.value.withdrawals.getOrElse(Withdrawals.empty).withdrawals
                val rewardsAfterWithdrawals =
                    CertsValidator.applyWithdrawals(
                      state.certState.dstate.rewards,
                      withdrawals
                    )
                val updatedDState =
                    state.certState.dstate.copy(rewards = rewardsAfterWithdrawals)
                val updatedCertState = state.certState.copy(dstate = updatedDState)
                val updatedState = state.copy(certState = updatedCertState)
                success(updatedState)
}
