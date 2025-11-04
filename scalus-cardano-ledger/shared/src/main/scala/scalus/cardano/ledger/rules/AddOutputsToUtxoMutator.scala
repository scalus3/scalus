package scalus.cardano.ledger
package rules

// It's part of Shelley.updateUTxOState in cardano-ledger
object AddOutputsToUtxoMutator extends STS.Mutator {
    override final type Error = Nothing

    override def transit(context: Context, state: State, event: Event): Result = {
        val addedUtxo: Utxos = event.body.value.outputs.view.zipWithIndex.map {
            case (Sized(output, _), index) =>
                TransactionInput(event.id, index) -> output
        }.toMap

        success(state.copy(utxos = state.utxos ++ addedUtxo))
    }
}
