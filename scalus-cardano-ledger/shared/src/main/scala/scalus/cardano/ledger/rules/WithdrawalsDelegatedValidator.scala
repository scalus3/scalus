package scalus.cardano.ledger
package rules

// It's validateWithdrawalsDelegated of the Conway LEDGER rule in cardano-ledger
object WithdrawalsDelegatedValidator extends STS.Validator {
    override final type Error = TransactionException.WithdrawalsNotDelegatedToDRepException

    /** Rejects a tx that withdraws from a key-hash account with no DRep delegation, spec [SC-23].
      *
      * A validator sees the state before the tx's certificates, as Haskell does, so a vote
      * delegation in the same tx does not count. Script-hash accounts are exempt. Conway LEDGER
      * skips the check in the bootstrap phase (protocol version 9) and when `isValid` is false.
      */
    override def validate(context: Context, state: State, event: Event): Result =
        if !event.isValid || context.env.params.protocolVersion.major == 9 then success
        else
            val accounts = state.certState.dstate.accounts
            val notDelegated = event.body.value.withdrawals.toSet
                .flatMap(_.withdrawals.keySet)
                .flatMap(_.address.credential.keyHashOption)
                .filter(keyHash =>
                    accounts.get(Credential.KeyHash(keyHash)).flatMap(_.dRepDelegation).isEmpty
                )
            if notDelegated.isEmpty then success
            else
                failure(
                  TransactionException.WithdrawalsNotDelegatedToDRepException(
                    event.id,
                    notDelegated
                  )
                )
}
