package scalus.cardano.node.stream.engine.persistence

/** Typed failures surfaced through [[EnginePersistenceStore.load]] (and, rarely,
  * [[EnginePersistenceStore.compact]] / [[EnginePersistenceStore.flush]] futures).
  *
  * Callers that want to ride through a corrupted-state scenario can react to the variant they
  * recognise — the canonical response is "log, delete the persistence files via `reset`,
  * cold-start."
  */
sealed abstract class EnginePersistenceError(msg: String) extends RuntimeException(msg)

object EnginePersistenceError {

    /** The on-disk snapshot was written by a newer (or older incompatible) version of the library.
      * M6 does not ship a migration path — caller must reset or abort.
      */
    final case class SchemaMismatch(found: Int, expected: Int)
        extends EnginePersistenceError(
          s"persistence schema version mismatch: found=$found, expected=$expected"
        )

    /** The persisted state does not match the current configuration (appId renamed, network magic
      * changed). Loading would corrupt state; caller resets or aborts.
      */
    final case class Mismatched(detail: String)
        extends EnginePersistenceError(s"persistence / config mismatch: $detail")

    /** Underlying I/O failure. */
    final case class Io(cause: Throwable)
        extends EnginePersistenceError(s"persistence I/O failure: ${cause.getMessage}") {
        initCause(cause)
    }

    /** Unrecoverable CBOR decoding failure at `at` bytes into the file. The partial-tail recovery
      * path (warn + truncate) handles the common case where the last record is half-written; this
      * variant fires when an earlier byte is corrupt and recovery can't reasonably continue.
      */
    final case class Corrupt(at: Long, cause: Throwable)
        extends EnginePersistenceError(s"persistence corruption at byte $at: ${cause.getMessage}") {
        initCause(cause)
    }

    /** Another process already holds the persistence lock for this `appId`. Two Scalus apps with
      * the same `appId` on the same host would otherwise share (and corrupt) state.
      */
    final case class Locked(path: String)
        extends EnginePersistenceError(s"persistence already locked by another process: $path")
}
