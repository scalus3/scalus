package scalus.cardano.node.stream.engine

import scalus.cardano.ledger.{TransactionInput, TransactionOutput, Utxos}
import scalus.cardano.node.UtxoQuery
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.cardano.node.stream.engine.replay.ReplayError

/** Pluggable durable block-history store.
  *
  * An application that wants checkpoint replay from arbitrarily old points — or
  * [[scalus.cardano.node.stream.StorageProfile.Heavy]] mode (M9.P3) — provides a `ChainStore`
  * implementation via [[scalus.cardano.node.stream.StreamProviderConfig.chainStore]].
  *
  * The trait covers both the read path (historical blocks for [[replay.ChainStoreReplaySource]],
  * used by M7 checkpoint replay) and the write path needed to keep the store populated from the
  * live chain-sync loop. When a `ChainStore` is configured, the engine calls [[appendBlock]] on
  * every `onRollForward` and [[rollbackTo]] on every `onRollBackward`; the engine worker is the
  * only writer.
  *
  * Implementations are not required to be thread-safe — the engine serialises every access through
  * its single worker thread.
  *
  * See `docs/local/claude/indexer/chain-store-m9.md` for the full design.
  */
trait ChainStore {

    /** Blocks strictly after `from` and up to and including `to`. Returns
      * `Left(ReplaySourceExhausted(point))` if the store does not cover `from` (e.g. `from`
      * pre-dates the store's horizon, or the chain forked off and `from` is on a dead branch).
      */
    def blocksBetween(
        from: ChainPoint,
        to: ChainPoint
    ): Either[ReplayError.ReplaySourceExhausted, Iterator[AppliedBlock]]

    /** Persist `block` at its point. Idempotent on duplicate `(slot, hash)` — a restart that
      * re-applies already-seen blocks must not double-count. The engine calls this after updating
      * its own state in `onRollForward`, so errors here do not affect live fan-out.
      */
    def appendBlock(block: AppliedBlock): Unit

    /** Forget every stored block whose point is strictly after `to`. No-op if `to` equals (or is
      * ahead of) the current tip. Called from `onRollBackward`.
      */
    def rollbackTo(to: ChainPoint): Unit

    /** Current tip of the store, or `None` if empty. Consulted at engine start for warm-resume
      * diagnostics; the engine does NOT feed it back into its own tip cell (the engine's tip comes
      * from `EnginePersistenceStore` in M6 and live chain-sync).
      */
    def tip: Option[ChainTip]

    /** Drop blocks older than `horizon`. Default is a no-op — stores retain everything unless the
      * caller explicitly bounds disk. Apps that want a sliding-window store override this and wire
      * it into their own housekeeping loop.
      */
    def pruneBefore(horizon: ChainPoint): Unit = ()

    /** Release any resources (file handles, native allocations). Idempotent. The provider calls
      * `close()` from its `preClose` teardown hook, after the chain-sync loop has stopped.
      */
    def close(): Unit
}

/** Optional mixin for [[ChainStore]] backends that also maintain a queryable UTxO set — the thing
  * [[scalus.cardano.node.stream.StorageProfile.Heavy]] depends on.
  *
  * Not every ChainStore needs this. A block-history-only store (M7 replay via a file archive, a
  * minimal SQL dump) legitimately doesn't, and the engine falls through to the configured backup as
  * before. A store that does implement this trait must keep the UTxO set consistent with the block
  * history across `appendBlock` / `rollbackTo` — the engine treats the two as co-authoritative for
  * Heavy mode.
  */
trait ChainStoreUtxoSet { self: ChainStore =>

    /** Query UTxOs matching `q` from the local store. `None` means "not answerable locally" — the
      * caller (usually [[Engine.findUtxosLocal]]) falls through to the configured backup.
      * `Some(utxos)` is authoritative, even if empty.
      */
    def findUtxosFromStore(q: UtxoQuery): Option[Utxos]

    /** Bulk-replace the UTxO set with `utxos`, anchoring it at `tip`. Consumed by M10's
      * `ChainStoreRestorer` when the store bootstraps from a snapshot; also available to apps that
      * manage their own restore pipeline.
      *
      * **Not required to be atomic.** A mainnet-sized UTxO set can't fit in a single write batch on
      * every backend, so implementations stream the restore in chunks. A crash mid-restore leaves
      * the store in a partial state; callers must treat it as discardable-and-re-runnable (wipe and
      * re-invoke `restoreUtxoSet`). Implementations MUST write `tip` last, so a partially-restored
      * store has no tip and is observably invalid — the provider's cold-start guard (no persisted
      * tip) triggers a fresh bootstrap on the next run.
      */
    def restoreUtxoSet(
        tip: ChainTip,
        utxos: Iterator[(TransactionInput, TransactionOutput)]
    ): Unit
}

object ChainStore {

    /** No-op read-only store — the M7 placeholder. Every `blocksBetween` returns `Left(exhausted)`;
      * writes are discarded. Useful when the config asks for a ChainStore wire but the backend
      * isn't ready yet.
      */
    val noop: ChainStore = new ChainStore {
        def blocksBetween(
            from: ChainPoint,
            to: ChainPoint
        ): Either[ReplayError.ReplaySourceExhausted, Iterator[AppliedBlock]] =
            Left(ReplayError.ReplaySourceExhausted(from))

        def appendBlock(block: AppliedBlock): Unit = ()
        def rollbackTo(to: ChainPoint): Unit = ()
        def tip: Option[ChainTip] = None
        def close(): Unit = ()
    }
}
