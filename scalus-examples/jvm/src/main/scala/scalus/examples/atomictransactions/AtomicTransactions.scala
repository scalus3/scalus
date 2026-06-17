package scalus.examples.atomictransactions

import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}

/** Illustrates Cardano's native transaction atomicity (rosetta `atomic_transactions`).
  *
  * On EVM chains, performing several actions atomically requires a contract that batches sub-calls
  * and rolls back on failure. On Cardano every transaction is atomic by the ledger rules: all inputs
  * are consumed and all outputs are created in a single step, or nothing changes at all. So
  * "batching" needs no contract — it is just spending several UTxOs in one transaction.
  */
case class AtomicTransactions(env: CardanoInfo) {

    /** Build one transaction that spends every UTxO in `senderUtxos` and pays `amount` to
      * `recipient`, returning change to `changeAddress`.
      *
      * Either all of those inputs are consumed and the payment is made, or the whole transaction is
      * rejected by the ledger — there is no partial outcome. That all-or-nothing guarantee is the
      * atomicity an EVM batching contract would have to implement by hand.
      */
    def batchPay(
        senderUtxos: Utxos,
        recipient: Address,
        amount: Coin,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction =
        senderUtxos
            .foldLeft(TxBuilder(env)) { case (builder, entry) => builder.spend(Utxo(entry)) }
            .payTo(recipient, Value(amount))
            .complete(availableUtxos = senderUtxos, changeAddress)
            .sign(signer)
            .transaction
}
