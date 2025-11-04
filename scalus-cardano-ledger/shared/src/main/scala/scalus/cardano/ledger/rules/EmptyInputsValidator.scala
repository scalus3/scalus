package scalus.cardano.ledger
package rules

// txins txb ≠ ∅
// It's Shelley.validateInputSetEmptyUTxO in cardano-ledger
object EmptyInputsValidator extends STS.Validator {
    override final type Error = TransactionException.EmptyInputsException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val inputs = event.body.value.inputs

        if inputs.toSet.isEmpty then
            return failure(TransactionException.EmptyInputsException(transactionId))

        success
    }
}
