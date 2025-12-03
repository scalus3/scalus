package scalus.cardano.ledger
package rules

// It's Allegra.validateOutsideValidityIntervalUTxO in cardano-ledger
object OutsideValidityIntervalValidator extends STS.Validator {
    override final type Error = TransactionException.OutsideValidityIntervalException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val validityInterval = event.validityInterval
        val slot = context.env.slot

        if !checkInterval(validityInterval, slot) then
            failure(
              TransactionException.OutsideValidityIntervalException(
                transactionId,
                validityInterval,
                slot
              )
            )
        else success
    }

    // Test if a slot is in the Validity interval. Recall that a ValidityInterval
    // is a half Open interval, that is why we use (slot < invalidHereafter)
    private def checkInterval(
        validityInterval: ValidityInterval,
        slot: SlotNo
    ): Boolean = {
        validityInterval match
            case ValidityInterval(None, None)                   => true
            case ValidityInterval(None, Some(invalidHereafter)) => slot < invalidHereafter
            case ValidityInterval(Some(invalidBefore), None)    => slot >= invalidBefore
            case ValidityInterval(Some(invalidBefore), Some(invalidHereafter)) =>
                slot >= invalidBefore && slot < invalidHereafter
    }
}
