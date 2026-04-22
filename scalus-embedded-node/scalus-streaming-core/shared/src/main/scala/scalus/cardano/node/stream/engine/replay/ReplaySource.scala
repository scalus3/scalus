package scalus.cardano.node.stream.engine.replay

import scalus.cardano.node.stream.ChainPoint
import scalus.cardano.node.stream.engine.AppliedBlock

/** Source of historical blocks for a [[scalus.cardano.node.stream.engine.Engine]] replay.
  *
  * Given a checkpoint `from` (exclusive) and a target `to` (inclusive), the source produces the
  * ordered sequence of [[AppliedBlock]]s that lie strictly after `from` and up to and including
  * `to`. The engine replays each block through a per-subscription dry-run of `onRollForward`
  * without mutating global engine state.
  *
  * Synchronous vs asynchronous sources:
  *
  *   - Plain [[ReplaySource]]s (rollback buffer, ChainStore) answer synchronously from an
  *     already-materialised in-memory or on-disk store. The engine replays the whole window inside
  *     one worker task; live events queue behind it naturally and ordering needs no buffering.
  *   - [[AsyncReplaySource]]s (peer replay) need network I/O to satisfy the query. The engine runs
  *     a two-phase flow: prefetch off the worker, apply on the worker. During prefetch the
  *     subscription is in the registry in `ReplayPending` state so live events are captured in a
  *     per-subscription buffer; that buffer drains into the mailbox right before the state flips
  *     back to `Live`.
  *
  * Three implementations ship with M7:
  *
  *   - [[RollbackBufferReplaySource]] — reads from the engine's in-memory volatile tail.
  *   - [[ChainStoreReplaySource]] — delegates to a pluggable ChainStore; concrete stores land with
  *     M9 so the source itself is a no-op until then.
  *   - `PeerReplaySource` (M7 Phase 2b, in `scalus-cardano-network`) — opens a short-lived separate
  *     N2N connection, runs ChainSync + BlockFetch over the replay window, closes the connection,
  *     returns the prefetched blocks. Does NOT share the live applier's mux: Ouroboros N2N allows
  *     exactly one ChainSync state machine per connection.
  */
trait ReplaySource {

    /** Produce the ordered block iterator for a replay window.
      *
      * Returns `Left(ReplaySourceExhausted)` when `from` is not covered by the source. Otherwise
      * the iterator yields blocks whose `tip.point` is strictly greater than `from` and less than
      * or equal to `to`.
      *
      * `from == to` produces an empty iterator. `from` equal to an existing block's point means
      * that block is NOT emitted (the checkpoint is the last point the app claims to have
      * processed).
      *
      * [[AsyncReplaySource]] implementations must return `Left(ReplaySourceExhausted)` here — they
      * answer via [[AsyncReplaySource.prefetch]] instead. The engine tries `iterate` first on every
      * source and only falls through to `prefetch` when no sync source covers the window.
      */
    def iterate(
        from: ChainPoint,
        to: ChainPoint
    ): Either[ReplayError.ReplaySourceExhausted, Iterator[AppliedBlock]]
}
