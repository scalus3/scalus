package scalus.cardano.ledger
package rules

// It's ppViewHashesMatch in cardano-ledger
object ProtocolParamsViewHashesMatchValidator extends STS.Validator {
    override final type Error = TransactionException.BadInputsUTxOException |
        TransactionException.BadReferenceInputsUTxOException |
        TransactionException.InvalidScriptDataHashException

    override def validate(context: Context, state: State, event: Event): Result = {
        val utxo = state.utxos
        val protocolParams = context.env.params
        val expectedScriptDataHash = event.body.value.scriptDataHash

        for
            actualScriptDataHash <- ScriptDataHashGenerator.computeScriptDataHash(
              event,
              utxo,
              protocolParams
            )

            _ <-
                if actualScriptDataHash == expectedScriptDataHash then success
                else
                    failure(
                      TransactionException.InvalidScriptDataHashException(
                        event.id,
                        actualScriptDataHash,
                        expectedScriptDataHash
                      )
                    )
        yield ()
    }
}
