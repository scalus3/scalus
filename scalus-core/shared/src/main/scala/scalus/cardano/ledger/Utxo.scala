package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder}

/** Type alias for a map of unspent transaction outputs */
type Utxos = Map[TransactionInput, TransactionOutput]

object Utxos:
    def empty: Utxos = Map.empty
    @scala.annotation.varargs
    def apply(utxos: Utxo*): Utxos = Map(utxos.map(_.toTuple)*)

    extension (utxos: Utxos) {
        def utxos: Seq[Utxo] = utxos.map(Utxo.apply).toSeq
    }

    /** Reads a UTxO set as the ledger encodes it: a CBOR map from input to output. */
    val mapDecoder: Decoder[Utxos] = summon[Decoder[Map[TransactionInput, TransactionOutput]]]

    /** Reads a UTxO set as a CBOR array of `[input, output]` pairs, each a CIP-30
      * `transaction_unspent_output`. A later pair with the same input replaces an earlier one.
      */
    val pairsDecoder: Decoder[Utxos] =
        summon[Decoder[Vector[Utxo]]].map(_.iterator.map(_.toTuple).toMap)

    /** Reads a UTxO set in either form: a CBOR map as [[mapDecoder]], an array as [[pairsDecoder]].
      */
    val mapOrPairsDecoder: Decoder[Utxos] = Decoder { r =>
        if r.hasMapHeader || r.hasMapStart then mapDecoder.read(r) else pairsDecoder.read(r)
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

    /** `[input, output]`, CIP-30's `transaction_unspent_output`. */
    given Encoder[Utxo] =
        summon[Encoder[(TransactionInput, TransactionOutput)]].contramap(_.toTuple)

    /** `[input, output]`, CIP-30's `transaction_unspent_output`. */
    given Decoder[Utxo] = summon[Decoder[(TransactionInput, TransactionOutput)]].map(Utxo(_))
}
