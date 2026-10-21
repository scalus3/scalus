package scalus.cardano.ledger
package rules

// It's Shelley.validateMaxTxSizeUTxO in cardano-ledger
object TransactionSizeValidator extends STS.Validator {
    override final type Error = TransactionException.InvalidTransactionSizeException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        // The ledger's `sizeTxF`: the encoding without the isValid flag, `toCBORForSizeComputation`
        val transactionSize = event.toCborForFeeCalculation.length
        val maxTransactionSize = context.env.params.maxTxSize

        if transactionSize > maxTransactionSize then
            failure(
              TransactionException.InvalidTransactionSizeException(
                transactionId,
                transactionSize,
                maxTransactionSize
              )
            )
        else success
    }
}
