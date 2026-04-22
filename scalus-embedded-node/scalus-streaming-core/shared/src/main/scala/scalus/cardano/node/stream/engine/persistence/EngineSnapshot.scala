package scalus.cardano.node.stream.engine.persistence

import scalus.cardano.ledger.TransactionHash
import scalus.cardano.node.stream.ChainTip
import scalus.cardano.node.stream.engine.UtxoKey

/** Persisted engine state — the pair of (compacted snapshot, records appended since) that
  * [[EnginePersistenceStore.load]] returns.
  *
  * `snapshot` is `None` only on the very first run for a given `appId`. The `journal` is the
  * append-only records written after the snapshot was sealed; rebuild replays them in order against
  * the reconstructed snapshot state.
  */
final case class PersistedEngineState(
    snapshot: Option[EngineSnapshotFile],
    journal: Seq[JournalRecord]
)

/** The compacted on-disk snapshot.
  *
  * Cross-check fields (`appId`, `networkMagic`) exist to catch the "edited config without wiping
  * state" footgun early. Mismatches surface as [[EnginePersistenceError.Mismatched]] from `load`.
  *
  * `volatileTail` holds up to `securityParam` per-block summaries so the rollback buffer is
  * faithfully reconstructed — not just the tip.
  */
final case class EngineSnapshotFile(
    schemaVersion: Int,
    appId: String,
    networkMagic: Long,
    tip: Option[ChainTip],
    ownSubmissions: Set[TransactionHash],
    volatileTail: Seq[AppliedBlockSummary],
    buckets: Map[UtxoKey, BucketState]
)

object EngineSnapshotFile {

    /** Bumped on incompatible on-disk format changes. Snapshots older than this value fail to load
      * with [[EnginePersistenceError.SchemaMismatch]]; users reset + cold-start.
      */
    val CurrentSchemaVersion: Int = 1
}
