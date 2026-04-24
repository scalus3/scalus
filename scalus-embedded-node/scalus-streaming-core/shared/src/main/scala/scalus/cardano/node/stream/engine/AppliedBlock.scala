package scalus.cardano.node.stream.engine

import scalus.cardano.ledger.{Block, KeepRaw, OriginalCborByteArray, TransactionHash, TransactionInput, TransactionOutput}
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

object AppliedBlock {

    /** Project a decoded [[Block]] (with its original CBOR bytes retained) into the engine's
      * [[AppliedBlock]] shape. The caller owns the `blockRaw.raw` bytes and supplies the tip
      * separately — for network paths that's the peer's reported tip, for snapshot-restore paths
      * it's typically reconstructed from the immutable index.
      *
      * Rebinds `OriginalCborByteArray` from `blockRaw.raw` so each transaction's `KeepRaw` parts
      * resolve their backing slice correctly — same discipline as
      * `scalus.cardano.network.ChainApplier.toAppliedBlock`, kept in one place so both the N2N
      * applier and the Mithril snapshot restorer produce identical results.
      */
    def fromRaw(tip: ChainTip, blockRaw: KeepRaw[Block]): AppliedBlock = {
        given OriginalCborByteArray = OriginalCborByteArray(blockRaw.raw)
        val txs = blockRaw.value.transactions.map { tx =>
            AppliedTransaction(
              id = tx.id,
              inputs = tx.body.value.inputs.toSet,
              outputs = tx.body.value.outputs.map(_.value).toIndexedSeq
            )
        }
        AppliedBlock(tip, txs)
    }
}

final case class AppliedTransaction(
    id: TransactionHash,
    inputs: Set[TransactionInput],
    outputs: IndexedSeq[TransactionOutput]
) {
    def outputsById: Iterator[(TransactionInput, TransactionOutput)] =
        outputs.iterator.zipWithIndex.map((o, i) => TransactionInput(id, i) -> o)
}
