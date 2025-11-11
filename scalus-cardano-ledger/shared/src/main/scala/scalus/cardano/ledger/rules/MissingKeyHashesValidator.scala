package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.{AllWitnessesKeyHashes, MissingKeyHashes}

// It's Shelley.validateNeededWitnesses in cardano-ledger
object MissingKeyHashesValidator extends STS.Validator {
    override final type Error = TransactionException.BadInputsUTxOException |
        TransactionException.BadCollateralInputsUTxOException |
        TransactionException.MissingKeyHashesException

    override def validate(context: Context, state: State, event: Event): Result = {
        val allWitnessesKeyHashes = AllWitnessesKeyHashes.allWitnessesKeyHashes(event)
        val utxos = state.utxos

        MissingKeyHashes.validateAllMissingKeyHashes(event, allWitnessesKeyHashes, utxos)
    }
}
