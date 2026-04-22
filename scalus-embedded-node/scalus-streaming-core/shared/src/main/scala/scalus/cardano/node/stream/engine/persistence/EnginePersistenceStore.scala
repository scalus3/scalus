package scalus.cardano.node.stream.engine.persistence

import scala.collection.mutable
import scala.concurrent.Future

/** Durable engine-state persistence surface.
  *
  * The engine's worker thread is the only caller of [[appendSync]]; implementations may buffer.
  * [[load]], [[flush]], [[compact]], [[close]] are coarse-grained and return `Future[_]` because
  * they may touch the filesystem; none of them are called on the engine's hot path.
  *
  * See `docs/local/claude/indexer/engine-persistence-minimal.md` for the design and M6 guarantees.
  */
trait EnginePersistenceStore {

    /** Load the persisted state, if any. Returns `None` when no prior state exists for the backing
      * resource (first run). Returns a failed Future with [[EnginePersistenceError]] on schema /
      * mismatch / I/O failure.
      */
    def load(): Future[Option[PersistedEngineState]]

    /** Append a journal record synchronously from the engine's worker thread. Buffering is an
      * implementation detail; [[flush]] is the only point where durability is guaranteed.
      * Implementations should not throw — transient I/O failures are logged and surfaced via the
      * next [[flush]] / [[compact]] future.
      */
    def appendSync(record: JournalRecord): Unit

    /** Flush any buffered journal data and fsync. Called by the engine on clean shutdown and by the
      * background ticker.
      */
    def flush(): Future[Unit]

    /** Rewrite the snapshot from `snap` and truncate the journal. Atomic at the filesystem level —
      * an interrupted compaction leaves the previous snapshot intact.
      */
    def compact(snap: EngineSnapshotFile): Future[Unit]

    /** Release any file handles and process-level locks. */
    def close(): Future[Unit]
}

object EnginePersistenceStore {

    /** Discards everything. Accepting this is accepting Cold restart semantics (see the *Restart
      * semantics* section of the indexer-node doc).
      */
    val noop: EnginePersistenceStore = new NoopStore

    /** Ephemeral in-memory store; primarily for tests that want to exercise the round-trip without
      * touching the filesystem. Each instance is independent — two engines backed by distinct
      * instances do not share state.
      */
    def inMemory(): EnginePersistenceStore = new InMemoryStore

    private final class NoopStore extends EnginePersistenceStore {
        def load(): Future[Option[PersistedEngineState]] = Future.successful(None)
        def appendSync(record: JournalRecord): Unit = ()
        def flush(): Future[Unit] = Future.unit
        def compact(snap: EngineSnapshotFile): Future[Unit] = Future.unit
        def close(): Future[Unit] = Future.unit
    }

    private final class InMemoryStore extends EnginePersistenceStore {
        private var sealedSnapshot: Option[EngineSnapshotFile] = None
        private val buffer = mutable.ArrayBuffer.empty[JournalRecord]
        private var closed = false

        def load(): Future[Option[PersistedEngineState]] = synchronized {
            if sealedSnapshot.isEmpty && buffer.isEmpty then Future.successful(None)
            else Future.successful(Some(PersistedEngineState(sealedSnapshot, buffer.toSeq)))
        }

        def appendSync(record: JournalRecord): Unit = synchronized {
            if !closed then buffer += record
        }

        def flush(): Future[Unit] = Future.unit

        def compact(snap: EngineSnapshotFile): Future[Unit] = synchronized {
            sealedSnapshot = Some(snap)
            buffer.clear()
            Future.unit
        }

        def close(): Future[Unit] = synchronized {
            closed = true
            Future.unit
        }
    }
}
