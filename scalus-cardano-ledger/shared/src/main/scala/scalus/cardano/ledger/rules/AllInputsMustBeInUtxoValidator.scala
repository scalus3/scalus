package scalus.cardano.ledger
package rules

// allInputs = spendInputs txb ∪ collInputs txb ∪ refInputs txb
// (spendInputs txb ∪ collInputs txb ∪ refInputs txb) ⊆ dom utxo
// It's Babbage.validateBadInputsUTxO in cardano-ledger
object AllInputsMustBeInUtxoValidator extends STS.Validator {
    override final type Error = TransactionException.BadAllInputsUTxOException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val body = event.body.value
        val utxo = state.utxos

        val missingInputs = body.inputs.toSortedSet.filterNot(utxo.contains)
        val missingCollateralInputs = body.collateralInputs.toSortedSet.filterNot(utxo.contains)
        val missingReferenceInputs = body.referenceInputs.toSortedSet.filterNot(utxo.contains)

        if missingInputs.nonEmpty || missingCollateralInputs.nonEmpty || missingReferenceInputs.nonEmpty
        then
            return failure(
              TransactionException.BadAllInputsUTxOException(
                transactionId,
                missingInputs,
                missingCollateralInputs,
                missingReferenceInputs
              )
            )

        success
    }
}
