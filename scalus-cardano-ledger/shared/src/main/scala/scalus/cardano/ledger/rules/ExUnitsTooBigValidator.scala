package scalus.cardano.ledger
package rules

// Checks that the total execution units for a transaction don't exceed the protocol-defined maximum.
// It's Alonzo.validateExUnitsTooBigUTxO in cardano-ledger
object ExUnitsTooBigValidator extends STS.Validator {
    override type Error = TransactionException.ExUnitsExceedMaxException

    override def validate(context: Context, state: State, event: Event): Result = {
        val maxTxExecutionUnits = context.env.params.maxTxExecutionUnits

        val actualTxExecutionUnits = event.witnessSet.redeemers
            .map(_.value.totalExUnits)
            .getOrElse(ExUnits.zero)

        // Memory and steps are checked independently, like pointWiseExUnits (<=) in cardano-ledger
        if actualTxExecutionUnits.exceeds(maxTxExecutionUnits) then
            failure(
              TransactionException.ExUnitsExceedMaxException(
                event.id,
                actualTxExecutionUnits,
                maxTxExecutionUnits
              )
            )
        else success
    }
}
