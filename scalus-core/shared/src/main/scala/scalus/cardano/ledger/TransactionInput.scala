package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.builtin.ByteString
import scalus.builtin.ByteString.given

/** Represents an input to a transaction
  *
  * A transaction input is a reference to an output from a previous transaction, identified by the
  * transaction ID and the index of the output.
  */
final case class TransactionInput(
    /** The ID of the transaction that produced the output we're spending */
    transactionId: TransactionHash,

    /** The index of the output in the transaction we're spending */
    index: Int
) derives Codec:
    require(index >= 0, s"Invalid index of TransactionInput, expected: >= 0, actual: $index")

object TransactionInput {
    given Ordering[TransactionInput] with
        def compare(x: TransactionInput, y: TransactionInput): Int =
            summon[Ordering[ByteString]].compare(x.transactionId, y.transactionId) match
                case 0 => x.index.compareTo(y.index)
                case c => c
}

/** Alias for [[TransactionInput]] */
type Input = TransactionInput

/** Alias for [[TransactionInput]] */
val Input = TransactionInput
