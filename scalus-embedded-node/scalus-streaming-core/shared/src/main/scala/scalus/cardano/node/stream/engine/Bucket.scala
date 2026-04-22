package scalus.cardano.node.stream.engine

import scalus.cardano.ledger.{TransactionHash, TransactionInput, TransactionOutput, Utxos}
import scalus.cardano.node.stream.ChainPoint

import scala.collection.mutable

/** Engine-level storage for a single [[UtxoKey]]: the set of UTxOs currently matching the key, plus
  * a reverse-delta log aligned with the rollback buffer so the bucket can be restored on rollback.
  *
  * One bucket per distinct key; shared by every subscription whose query decomposes to include that
  * key. `refCount` tracks how many subscriptions are watching — when it drops to zero the engine
  * removes the bucket.
  *
  * Not thread-safe — the engine's single-threaded serial executor guards all mutation.
  */
final class Bucket(val key: UtxoKey) {

    private[engine] val current = mutable.Map.empty[TransactionInput, TransactionOutput]
    private[engine] val history = mutable.ArrayDeque.empty[Bucket.BlockDelta]
    private[engine] var refCount: Int = 0

    /** Seed the bucket from a point-in-time UTxO snapshot. Filters against the key before
      * inserting, so callers can safely pass a wider set (e.g. the backup's answer for a multi-key
      * query).
      */
    def seed(utxos: Utxos): Utxos = {
        val accepted = mutable.Map.empty[TransactionInput, TransactionOutput]
        utxos.foreach { (i, o) =>
            if UtxoKey.matches(key, i, o) then {
                current.update(i, o)
                accepted.update(i, o)
            }
        }
        accepted.toMap
    }

    /** Apply a block and record the reverse-delta. Returns per-tx records of which UTxOs in this
      * bucket were created/spent so the engine can drive subscription fan-out without rescanning
      * the block.
      */
    def applyForward(block: AppliedBlock): Bucket.Delta = {
        val addedBuf = mutable.ArrayBuffer.empty[Bucket.CreatedRec]
        val removedBuf = mutable.ArrayBuffer.empty[Bucket.SpentRec]
        block.transactions.foreach { tx =>
            tx.inputs.foreach { input =>
                current.remove(input).foreach { out =>
                    removedBuf += Bucket.SpentRec(input, out, tx.id)
                }
            }
            tx.outputs.iterator.zipWithIndex.foreach { (out, idx) =>
                val input = TransactionInput(tx.id, idx)
                if UtxoKey.matches(key, input, out) then {
                    current.update(input, out)
                    addedBuf += Bucket.CreatedRec(input, out, tx.id)
                }
            }
        }
        val added = addedBuf.toSeq
        val removed = removedBuf.toSeq
        history += Bucket.BlockDelta(block.point, added, removed)
        Bucket.Delta(block.point, added, removed)
    }

    /** Undo the most recently applied block's effect on this bucket. Returns the reverse-delta so
      * the engine can emit tracking events (typically a single `RolledBack(to)` per subscription,
      * computed from the highest reverted `ChainPoint`).
      */
    def applyBackward(): Option[Bucket.Delta] = {
        if history.isEmpty then None
        else {
            val delta = history.removeLast()
            delta.added.foreach(rec => current.remove(rec.input))
            delta.removed.foreach(rec => current.update(rec.input, rec.output))
            Some(delta)
        }
    }

    /** Discard history entries up to and including the named point — called when a block ages out
      * of the rollback window and its reverse-delta is no longer needed.
      */
    def forgetUpTo(point: ChainPoint): Unit = {
        while history.nonEmpty && history.head.point != point do history.removeHead()
        if history.nonEmpty && history.head.point == point then history.removeHead()
    }

    /** Look up this bucket's reverse-delta for the given block point. O(1) when the history is
      * empty (the common case for a freshly-acquired bucket), otherwise O(n) over the rollback
      * window — callers are expected to be on the replay / snapshot path, not the live hot path.
      */
    def deltaAt(point: ChainPoint): Option[Bucket.Delta] =
        if history.isEmpty then None
        else history.find(_.point == point)

    def snapshot: Utxos = current.toMap

    def size: Int = current.size
}

object Bucket {

    final case class CreatedRec(
        input: TransactionInput,
        output: TransactionOutput,
        producedBy: TransactionHash
    )

    final case class SpentRec(
        input: TransactionInput,
        output: TransactionOutput,
        spentBy: TransactionHash
    )

    /** Reverse-delta / per-block diff. */
    final case class Delta(
        point: ChainPoint,
        added: Seq[CreatedRec],
        removed: Seq[SpentRec]
    )

    private[engine] type BlockDelta = Delta
    private[engine] val BlockDelta = Delta
}
