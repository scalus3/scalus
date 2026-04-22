package scalus.cardano.node.stream.engine.replay

import scalus.cardano.node.stream.ChainPoint
import scalus.cardano.node.stream.engine.AppliedBlock

/** [[ReplaySource]] backed by a point-in-time copy of the engine's in-memory volatile tail.
  *
  * The caller (the engine) hands over an already-materialised `Seq[AppliedBlock]` snapshot taken on
  * the worker thread, avoiding any cross-thread read of the live rollback buffer. Iteration is a
  * pure filter on that snapshot.
  *
  * Coverage rule: `from == ChainPoint.origin` emits every block in the snapshot whose point is
  * `<= to`. Otherwise `from` must equal one of the blocks in the snapshot; anything older fails
  * with [[ReplayError.ReplaySourceExhausted]] — reverse-delta history for blocks outside the
  * buffer's window is not available in memory.
  */
final class RollbackBufferReplaySource(snapshot: Seq[AppliedBlock]) extends ReplaySource {

    def iterate(
        from: ChainPoint,
        to: ChainPoint
    ): Either[ReplayError.ReplaySourceExhausted, Iterator[AppliedBlock]] = {
        if from == to then Right(Iterator.empty)
        else if from == ChainPoint.origin then
            Right(snapshot.iterator.takeWhile(blk => blk.point.slot <= to.slot))
        else
            val idx = snapshot.indexWhere(_.point == from)
            if idx < 0 then Left(ReplayError.ReplaySourceExhausted(from))
            else Right(snapshot.iterator.drop(idx + 1).takeWhile(blk => blk.point.slot <= to.slot))
    }
}
