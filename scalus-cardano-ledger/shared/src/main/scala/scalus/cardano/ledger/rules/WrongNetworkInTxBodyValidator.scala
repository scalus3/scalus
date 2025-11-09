package scalus.cardano.ledger.rules

import scalus.cardano.ledger.TransactionException.WrongNetworkInTxBody

// It's part of Alonzo.validateWrongNetworkInTxBody in cardano-ledger
object WrongNetworkInTxBodyValidator extends STS.Validator {
    override final type Error = WrongNetworkInTxBody
    override def validate(context: Context, state: State, event: Event): Result = {
        event.body.value.networkId.fold(success)(networkId =>
            if networkId != context.env.network.networkId then
                failure(WrongNetworkInTxBody(event.id, networkId))
            else success
        )
    }
}
