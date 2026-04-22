package scalus.cardano.node.stream.engine

import scalus.cardano.node.stream.ChainPoint
import scalus.cardano.node.stream.engine.replay.ReplayError

/** Pluggable durable block-history source.
  *
  * An application that wants checkpoint replay from arbitrarily old points — or
  * [[scalus.cardano.node.stream.StorageProfile.Heavy]] mode in the future — provides a `ChainStore`
  * implementation via [[scalus.cardano.node.stream.StreamProviderConfig.chainStore]].
  *
  * M7 defines only the trait shape so replay can fall through to a store when the rollback buffer
  * doesn't cover the checkpoint. Concrete backends (SQLite, RocksDB, file-backed) land with M9;
  * until then users pass `None` and the engine fails uncovered checkpoints with
  * [[replay.ReplayError.ReplaySourceExhausted]].
  *
  * Implementations must:
  *
  *   - Return an iterator whose block `tip.point`s are strictly greater than `from` and less than
  *     or equal to `to`. Same semantics as [[replay.ReplaySource.iterate]].
  *   - Be thread-safe for read; the engine calls `blocksBetween` from its worker thread.
  */
trait ChainStore {

    /** Blocks strictly after `from` and up to and including `to`. Returns
      * `Left(ReplaySourceExhausted(point))` if the store does not cover `from` (e.g. an old
      * checkpoint pruned from the store's horizon).
      */
    def blocksBetween(
        from: ChainPoint,
        to: ChainPoint
    ): Either[ReplayError.ReplaySourceExhausted, Iterator[AppliedBlock]]
}
