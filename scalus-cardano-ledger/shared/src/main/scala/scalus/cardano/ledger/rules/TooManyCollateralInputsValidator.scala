package scalus.cardano.ledger.rules
import scalus.cardano.ledger.TransactionException

// ‖collateral tx‖  ≤  maxCollInputs
// Alonzo.validateTooManyCollateralInputs
object TooManyCollateralInputsValidator extends STS.Validator {

    override type Error = TransactionException.TooManyCollateralInputsException

    override def validate(
        context: Context,
        state: State,
        tx: Event
    ): TooManyCollateralInputsValidator.Result = {
        val maxColl = context.env.params.maxCollateralInputs
        val numColl = tx.body.value.collateralInputs.toSet.size

        if numColl > maxColl then {
            failure(
              TransactionException.TooManyCollateralInputsException(
                tx.id,
                numColl,
                maxColl
              )
            )
        } else success
    }
}
