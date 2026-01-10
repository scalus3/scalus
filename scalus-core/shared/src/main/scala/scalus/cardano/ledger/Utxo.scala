package scalus.cardano.ledger

/** Type alias for a map of unspent transaction outputs */
type Utxos = Map[TransactionInput, TransactionOutput]

object Utxos:
    def empty: Utxos = Map.empty
    def apply(utxos: Utxo*): Utxos = Map(utxos.map(_.toTuple)*)

    extension (utxos: Utxos) {
        def utxos: Seq[Utxo] = utxos.map(Utxo.apply).toSeq
    }

/** Unspent Transaction Output
  *
  * @note
  *   It's common to use UTXO as a resolved transaction input-output pair, regardless whether it's
  *   spent or unspent. We use [[Utxo]] in both cases for simplicity.
  */
case class Utxo(input: TransactionInput, output: TransactionOutput) {
    def toTuple: (TransactionInput, TransactionOutput) = (input, output)
}

object Utxo {
    def apply(utxo: (TransactionInput, TransactionOutput)): Utxo =
        Utxo(utxo._1, utxo._2)
}
