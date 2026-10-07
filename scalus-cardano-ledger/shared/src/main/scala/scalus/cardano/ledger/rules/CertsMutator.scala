package scalus.cardano.ledger
package rules

import scala.collection.immutable.SortedMap

// It's the withdrawal part of the Conway CERTS state transition in cardano-ledger
object CertsMutator extends STS.Mutator {
    override final type Error = TransactionException

    /** Subtracts the withdrawals from the accounts. A phase-2-failed tx applies none, spec [SC-3b].
      */
    override def transit(context: Context, state: State, event: Event): Result = {
        if !event.isValid then success(state)
        else
            CertsValidator.validate(context, state, event) match
                case Left(err) => failure(err)
                case Right(_) =>
                    val withdrawals: SortedMap[RewardAccount, Coin] =
                        event.body.value.withdrawals.getOrElse(Withdrawals.empty).withdrawals
                    val accounts =
                        CertsValidator.applyWithdrawals(
                          state.certState.dstate.accounts,
                          withdrawals
                        )
                    val updatedCertState =
                        state.certState.copy(dstate = DelegationState(accounts))
                    success(state.copy(certState = updatedCertState))
    }
}
