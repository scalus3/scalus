package scalus.cardano.node.stream.engine.snapshot

/** Typed failures raised by the snapshot I/O layer. All fatal at provider construction time —
  * snapshot restore runs before any engine subscribers exist, so there is no fan-out to disturb.
  */
sealed abstract class SnapshotError(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause)

object SnapshotError {

    /** Snapshot's recorded schema version doesn't match what this library understands. */
    final case class SnapshotSchemaMismatch(expected: Int, actual: Int)
        extends SnapshotError(
          s"snapshot schema version $actual does not match expected $expected"
        )

    /** Snapshot's header says one thing and the body does another (wrong block count, wrong tip,
      * wrong utxo count, sha256 mismatch, bad footer sentinel, premature EOF, …).
      */
    final case class SnapshotCorrupted(detail: String, cause: Throwable = null)
        extends SnapshotError(s"snapshot corrupted: $detail", cause)

    /** Config error at provider-construction time — e.g. `bootstrap` set without `chainStore`. */
    final case class SnapshotConfigError(detail: String) extends SnapshotError(detail)
}
