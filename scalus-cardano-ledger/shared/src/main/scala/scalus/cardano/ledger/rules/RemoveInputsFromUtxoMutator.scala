package scalus.cardano.ledger
package rules

// It's part of Shelley.updateUTxOState in cardano-ledger
object RemoveInputsFromUtxoMutator extends STS.Mutator {
    override final type Error = Nothing

    override def transit(context: Context, state: State, event: Event): Result = {
        success(state.copy(utxos = state.utxos -- event.body.value.inputs.toSortedSet))
    }
}
