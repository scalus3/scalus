package scalus.cardano.node.stream.engine.replay

import scalus.cardano.node.stream.{ChainPoint, ChainTip}

/** Typed failures raised on a per-subscription replay. All are fatal to the owning subscription but
  * never to the engine — live subscribers and the global chain-sync loop are unaffected.
  */
sealed abstract class ReplayError(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause)

object ReplayError {

    /** The replay source cannot cover `point`. Common causes: `point` is older than the engine's
      * rollback-buffer horizon and no ChainStore/peer source is configured, or a configured peer
      * source returned `NoIntersection`.
      *
      * Recovery for the caller: wipe its own checkpoint state and re-subscribe from `StartFrom.Tip`
      * (accepting a fresh seed-from-backup), or bring up a ChainStore / Mithril snapshot and retry.
      */
    final case class ReplaySourceExhausted(point: ChainPoint)
        extends ReplayError(s"no replay source can reach checkpoint $point")

    /** The checkpoint is strictly after the engine's current tip. This can only happen when the
      * app's own checkpoint state is newer than the engine's warm-restored state — typically
      * because the engine's persistence file was wiped independently of the app's DB.
      */
    final case class ReplayCheckpointAhead(point: ChainPoint, currentTip: ChainTip)
        extends ReplayError(
          s"checkpoint $point is ahead of engine tip ${currentTip.point} (engine persistence out of sync with caller's checkpoint)"
        )

    /** The replay source encountered an unrecoverable error mid-iteration — transport failure on a
      * peer replay, disk I/O error on a ChainStore replay, etc. The underlying cause is preserved.
      */
    final case class ReplayInterrupted(detail: String, underlying: Throwable)
        extends ReplayError(s"replay interrupted: $detail", underlying)
}
