package scalus.cardano.ledger
package rules

// It's part of Alonzo.validateWrongNetworkInTxBody in cardano-ledger
object WrongNetworkInTxBodyValidator extends STS.Validator {
    override final type Error = TransactionException.WrongNetworkInTxBody

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val expectedNetworkId = context.env.network.networkId.toInt
        val actualNetworkId = event.body.value.networkId
            .getOrElse(expectedNetworkId) // Default to expected if not present

        if actualNetworkId != expectedNetworkId then
            failure(
              TransactionException
                  .WrongNetworkInTxBody(transactionId, actualNetworkId, expectedNetworkId)
            )
        else success
    }
}
