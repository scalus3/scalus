package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.TxBalance

/** This is Shelley.validateValueNotConservedUTxO
  *
  * consumed pp utxo txb = produced pp poolParams txb
  *
  * Both sides are summed and compared exactly, as the ledger's `Integer` does: a transaction can
  * make a `Long` sum wrap around to the other side's total. The error reports amounts beyond the
  * `Long` range as `Long.MaxValue` or `Long.MinValue`.
  */
object ValueNotConservedUTxOValidator extends STS.Validator {
    override final type Error = TransactionException.BadInputsUTxOException |
        TransactionException.ValueNotConservedUTxOException

    override def validate(context: Context, state: State, tx: Transaction): Result = {
        val transactionId = tx.id
        val params = context.env.params

        for
            consumed <- TxBalance.consumedExact(tx, state.certState, state.utxos, params)
            produced = TxBalance.producedExact(tx, state.certState, params)
            _ <-
                if consumed == produced then success
                else
                    failure(
                      TransactionException.ValueNotConservedUTxOException(
                        transactionId,
                        consumed.toClampedValue,
                        produced.toClampedValue
                      )
                    )
        yield ()
    }
}
