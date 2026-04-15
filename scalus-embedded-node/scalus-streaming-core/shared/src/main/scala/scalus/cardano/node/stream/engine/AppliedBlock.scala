package scalus.cardano.node.stream.engine

import scalus.cardano.ledger.{TransactionHash, TransactionInput, TransactionOutput}
import scalus.cardano.node.stream.{ChainPoint, ChainTip}

/** Engine-internal view of a block that has been parsed enough to be applied to the UTxO set.
  * Decouples the engine from block CBOR and witness-set details we don't care about for indexing.
  *
  * `spent` and `created` are pre-extracted from the transactions to keep the engine's inner loops
  * free of per-tx destructuring.
  */
final case class AppliedBlock(
    tip: ChainTip,
    transactions: Seq[AppliedTransaction]
) {
    def point: ChainPoint = tip.point

    lazy val spent: Set[TransactionInput] =
        transactions.iterator.flatMap(_.inputs).toSet

    lazy val created: Map[TransactionInput, TransactionOutput] =
        transactions.iterator.flatMap(_.outputsById).toMap

    lazy val transactionIds: Set[TransactionHash] =
        transactions.iterator.map(_.id).toSet
}

final case class AppliedTransaction(
    id: TransactionHash,
    inputs: Set[TransactionInput],
    outputs: IndexedSeq[TransactionOutput]
) {
    def outputsById: Iterator[(TransactionInput, TransactionOutput)] =
        outputs.iterator.zipWithIndex.map((o, i) => TransactionInput(id, i) -> o)
}
