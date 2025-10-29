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
        val txBody = tx.body.value
        val mint = txBody.mint.getOrElse(MultiAsset.empty)

        TxBalance.consumed(tx, state.certState, state.utxos, context.env.params).flatMap {
            consumed =>
                val produced = TxBalance.produced(tx)
                if consumed == produced then success
                else
                    failure(
                      TransactionException
                          .ValueNotConservedUTxOException(transactionId, consumed, produced)
                    )
        }

    }
}
