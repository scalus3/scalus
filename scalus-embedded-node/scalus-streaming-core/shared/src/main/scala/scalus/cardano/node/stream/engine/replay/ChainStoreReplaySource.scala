package scalus.cardano.node.stream.engine.replay

import scalus.cardano.node.stream.ChainPoint
import scalus.cardano.node.stream.engine.{AppliedBlock, ChainStore}

/** [[ReplaySource]] that delegates to a pluggable [[ChainStore]]. Used by the engine as a fallback
  * when the in-memory rollback buffer doesn't cover the checkpoint.
  *
  * M7 ships the source shape; concrete `ChainStore` backends land with M9.
  */
final class ChainStoreReplaySource(store: ChainStore) extends ReplaySource {

    def iterate(
        from: ChainPoint,
        to: ChainPoint
    ): Either[ReplayError.ReplaySourceExhausted, Iterator[AppliedBlock]] =
        if from == to then Right(Iterator.empty)
        else store.blocksBetween(from, to)
}
