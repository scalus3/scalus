package scalus.cardano.ledger
package rules

// It's validateTreasuryValue of the Conway LEDGER rule in cardano-ledger
object TreasuryValueMismatchValidator extends STS.Validator {
    override final type Error = TransactionException.TreasuryValueMismatchException

    /** Rejects a tx whose `currentTreasuryValue` differs from `env.treasury`, spec [SC-7]. Conway
      * LEDGER runs this check only when `isValid` is true, spec [SC-7e].
      */
    override def validate(context: Context, state: State, event: Event): Result =
        event.body.value.currentTreasuryValue match
            case Some(supplied) if event.isValid && supplied != context.env.treasury =>
                failure(
                  TransactionException.TreasuryValueMismatchException(
                    event.id,
                    supplied = supplied,
                    expected = context.env.treasury
                  )
                )
            case _ => success
}
