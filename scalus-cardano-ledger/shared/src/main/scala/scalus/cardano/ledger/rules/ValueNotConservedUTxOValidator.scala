package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.TxBalance

/** This is Shelley.validateValueNotConservedUTxO
  *
  * consumed pp utxo txb = produced pp poolParams txb
  */
object ValueNotConservedUTxOValidator extends STS.Validator {
    override final type Error = TransactionException.BadInputsUTxOException |
        TransactionException.ValueNotConservedUTxOException

    override def validate(context: Context, state: State, tx: Transaction): Result = {
        val transactionId = tx.id
        val params = context.env.params

        for
            consumed <- TxBalance.consumed(tx, state.certState, state.utxos, params)
            produced = TxBalance.produced(tx, params)
            _ <-
                if consumed == produced then success
                else
                    failure(
                      TransactionException
                          .ValueNotConservedUTxOException(transactionId, consumed, produced)
                    )
        yield ()
    }
}
