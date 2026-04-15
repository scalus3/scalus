package scalus.cardano.node.stream.engine

import scalus.cardano.node.stream.{ChainPoint, ChainTip}

import scala.collection.mutable

/** Bounded FIFO of blocks considered volatile — i.e., within `securityParam` of the current tip and
  * therefore still subject to rollback.
  *
  * Not thread-safe. The engine owns a single-threaded `ExecutionContext` that serialises all
  * mutating calls; snapshot readers go through published `AtomicReference`s on the engine, not
  * through this object directly.
  */
final class RollbackBuffer(val securityParam: Int) {

    private val blocks = mutable.ArrayDeque.empty[AppliedBlock]

    /** Append a block to the volatile tail. If that pushes the buffer past `securityParam`, the
      * oldest block(s) become immutable and are returned so the caller can flush them to durable
      * storage (no-op for the in-memory profile).
      */
    def applyForward(block: AppliedBlock): Seq[AppliedBlock] = {
        blocks += block
        val evicted = mutable.ArrayBuffer.empty[AppliedBlock]
        while blocks.size > securityParam do evicted += blocks.removeHead()
        evicted.toSeq
    }

    /** Pop blocks from the tail until the tip matches `to`.
      *
      * Returns [[RollbackOutcome.Reverted]] with the reverted blocks (most recent first) when `to`
      * was found in the buffer OR when `to == ChainPoint.origin` and the buffer is fully drained.
      * Returns [[RollbackOutcome.PastHorizon]] when `to` was not found, leaving the buffer empty —
      * callers (the engine) must treat this as fatal and fail every subscription with a
      * resync-required error, because the reverse-deltas for blocks older than the immutable tip
      * are no longer available.
      */
    def rollbackTo(to: ChainPoint): RollbackBuffer.RollbackOutcome = {
        val reverted = mutable.ArrayBuffer.empty[AppliedBlock]
        while blocks.nonEmpty && blocks.last.point != to do reverted += blocks.removeLast()
        if blocks.nonEmpty || to == ChainPoint.origin then
            RollbackBuffer.RollbackOutcome.Reverted(reverted.toSeq)
        else RollbackBuffer.RollbackOutcome.PastHorizon(reverted.toSeq)
    }

    def tip: Option[ChainTip] = blocks.lastOption.map(_.tip)

    def volatileTail: Seq[AppliedBlock] = blocks.toSeq

    def size: Int = blocks.size

    def isEmpty: Boolean = blocks.isEmpty
}

object RollbackBuffer {

    /** What `rollbackTo` produced. */
    enum RollbackOutcome {

        /** `to` was found (or buffer emptied to origin). `reverted` holds the popped blocks in
          * reverse chronological order (most recent first) so engines can undo per-block deltas in
          * order.
          */
        case Reverted(reverted: Seq[AppliedBlock])

        /** `to` was not found in the buffer and the buffer has been fully drained; reverse-delta
          * state for any older block is lost. The engine should surface a resync-required fault to
          * subscribers.
          */
        case PastHorizon(drained: Seq[AppliedBlock])
    }
}
