package scalus.cardano.ledger
package rules

// ‖collateral tx‖  ≤  maxCollInputs
// Alonzo.validateTooManyCollateralInputs
object TooManyCollateralInputsValidator extends STS.Validator {
    override type Error = TransactionException.TooManyCollateralInputsException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val actualCollateralInputsSize = event.body.value.collateralInputs.toSet.size
        val maxCollateralInputsSize = context.env.params.maxCollateralInputs

        if actualCollateralInputsSize > maxCollateralInputsSize then
            failure(
              TransactionException.TooManyCollateralInputsException(
                transactionId,
                actualCollateralInputsSize,
                maxCollateralInputsSize
              )
            )
        else success
    }
}
