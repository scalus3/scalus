package scalus.cardano.node.stream.engine.persistence

import scalus.cardano.ledger.{TransactionHash, TransactionInput, TransactionOutput}
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.cardano.node.stream.engine.{Bucket, UtxoKey}

/** Append-only log entry describing one mutation applied to the engine's state.
  *
  * Records are produced on the engine's worker thread after the in-memory mutation succeeds and
  * before the mutator's `Future` completes, so the live applier's backpressure extends to
  * persistence (see [[EnginePersistenceStore.appendSync]]).
  *
  * Each variant maps 1:1 to a single engine entry-point. Replaying the full journal in order
  * reproduces the engine state that produced it, up to the last fully-written record.
  */
sealed trait JournalRecord

object JournalRecord {

    /** Produced by [[scalus.cardano.node.stream.engine.Engine.onRollForward]] — the per-bucket
      * deltas are pre-computed as a side-effect of live fan-out so the journal writer does not
      * repeat work the engine already did.
      */
    final case class Forward(
        tip: ChainTip,
        txIds: Set[TransactionHash],
        bucketDeltas: Map[UtxoKey, BucketDelta]
    ) extends JournalRecord

    /** Produced by [[scalus.cardano.node.stream.engine.Engine.onRollBackward]] on the `Reverted`
      * outcome. The reader pops from the rebuilt rollback buffer until tip == to.
      */
    final case class Backward(to: ChainPoint) extends JournalRecord

    /** Produced by [[scalus.cardano.node.stream.engine.Engine.notifySubmit]]. Append-only until an
      * explicit [[OwnForgotten]].
      */
    final case class OwnSubmitted(hash: TransactionHash) extends JournalRecord

    /** Produced by [[scalus.cardano.node.stream.engine.Engine]]'s `clearOwnSubmission` path. Rare
      * in practice.
      */
    final case class OwnForgotten(hash: TransactionHash) extends JournalRecord

    // Note: a `ParamsChanged` variant is intentionally absent in M6 — the engine never
    // mutates `paramsRef` from the worker in this milestone (no on-chain parameter tracking
    // yet, and the `backup.fetchLatestParams` path is synchronous one-shot). The ProtocolParams
    // cell resets to `cardanoInfo.protocolParams` on warm restart, which is the correct M6
    // answer. See `docs/local/claude/indexer/engine-persistence-minimal.md` and M12b for the
    // advanced-persistence follow-up.
}

/** Serialisable form of [[Bucket.Delta]] — mirrors its shape but keeps the persistence package free
  * of live engine internals so codecs can evolve independently.
  */
final case class BucketDelta(
    added: Seq[Bucket.CreatedRec],
    removed: Seq[Bucket.SpentRec]
)

/** Shape-equivalent of the per-block summary the rollback buffer needs: tip + tx ids + per-bucket
  * deltas. Enough to drive `forgetBlock` / `rollbackTo` / `applyBackward` correctly without
  * carrying full transaction bodies (outputs and inputs live in [[BucketDelta]] already).
  */
final case class AppliedBlockSummary(
    tip: ChainTip,
    txIds: Set[TransactionHash],
    bucketDeltas: Map[UtxoKey, BucketDelta]
)

/** Frozen snapshot of a bucket: its key and the live `TransactionInput → TransactionOutput` map.
  * History (reverse-delta log) is *not* persisted here — it is rebuilt from the snapshot's
  * [[EngineSnapshotFile.volatileTail]] during load.
  */
final case class BucketState(
    key: UtxoKey,
    current: Map[TransactionInput, TransactionOutput]
)
