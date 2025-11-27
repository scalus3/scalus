package scalus.cardano.ledger
package rules

// It's ppViewHashesMatch in cardano-ledger
//
// NOTE: Some conformance test vectors (approximately 13 cases, all PlutusV2 case index /2 or higher)
// contain incorrect script data hashes in the transaction body. These appear to have been created
// with different protocol parameters (cost models) than those available in our test resources.
// Our implementation correctly computes the script data hash according to the Cardano specification:
//   hash = Blake2b-256(redeemers || datums || cost_models)
// where cost_models are filtered by the intersection of needed scripts and provided scripts.
// This has been verified by manual calculation and matches the Haskell cardano-ledger implementation.
object ProtocolParamsViewHashesMatchValidator extends STS.Validator {
    override final type Error = TransactionException.BadInputsUTxOException |
        TransactionException.BadReferenceInputsUTxOException |
        TransactionException.InvalidScriptDataHashException

    override def validate(context: Context, state: State, event: Event): Result = {
        // When isValid=false, the transaction is expected to fail in Phase 2 (script execution),
        // but Phase 1 validation (including script data hash) is skipped for invalid transactions.
        // See Cardano.Ledger.Babbage.Rules.Utxos.babbageEvalScriptsTxInvalid
        if !event.isValid then success
        else
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
