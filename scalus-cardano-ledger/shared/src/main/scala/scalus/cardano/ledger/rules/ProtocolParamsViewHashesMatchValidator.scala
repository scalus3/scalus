package scalus.cardano.ledger
package rules

// It's ppViewHashesMatch in cardano-ledger
//
// Validates that the script data hash in the transaction body matches the computed hash from:
//   hash = Blake2b-256(redeemers || datums || cost_models)
// where cost_models are filtered by the intersection of needed scripts and provided scripts.
// This matches the Haskell cardano-ledger implementation.
object ProtocolParamsViewHashesMatchValidator extends STS.Validator {
    override final type Error = TransactionException.BadInputsUTxOException |
        TransactionException.BadReferenceInputsUTxOException |
        TransactionException.InvalidScriptDataHashException

    override def validate(context: Context, state: State, event: Event): Result = {
        val utxo = state.utxos
        val protocolParams = context.env.params
        val actualScriptDataHash = event.body.value.scriptDataHash

        for
            expectedScriptDataHash <- ScriptDataHashGenerator.computeScriptDataHash(
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
