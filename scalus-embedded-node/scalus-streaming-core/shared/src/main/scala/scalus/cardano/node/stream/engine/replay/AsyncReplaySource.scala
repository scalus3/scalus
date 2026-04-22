package scalus.cardano.node.stream.engine.replay

import scalus.cardano.node.stream.ChainPoint
import scalus.cardano.node.stream.engine.AppliedBlock

import scala.concurrent.{ExecutionContext, Future}

/** A [[ReplaySource]] whose coverage check requires asynchronous I/O. The engine drives it through
  * a two-phase flow: [[prefetch]] runs off the worker thread, the returned blocks are then applied
  * to the subscription's mailbox inside a single worker task.
  *
  * The engine calls [[iterate]] first on every source — async sources must return
  * `Left(ReplaySourceExhausted)` from it so the fallback cascade reaches [[prefetch]] only if no
  * synchronous source covered the window. This keeps the "rollback buffer before peer replay"
  * precedence cheap: a covered window pays no network cost.
  *
  * Contract for [[prefetch]]:
  *
  *   - `from == to` must complete with `Right(Seq.empty)` without opening a connection.
  *   - `Left(ReplaySourceExhausted(from))` signals the implementation cannot reach `from`; the
  *     engine tries the next async source (or fails the subscription).
  *   - `Left(ReplayInterrupted(...))` signals an unrecoverable transport / protocol fault mid
  *     fetch; the engine fails the subscription with this error and does NOT cascade.
  *   - A failed Future is treated as `Left(ReplayInterrupted("prefetch", cause))`.
  *   - On success the returned sequence contains blocks whose `point.slot` is strictly greater than
  *     `from.slot` and less than or equal to `to.slot`, in chain order.
  */
trait AsyncReplaySource extends ReplaySource {

    /** Fetch the replay window into memory. See trait docs for the coverage / ordering contract. */
    def prefetch(
        from: ChainPoint,
        to: ChainPoint
    )(using ExecutionContext): Future[Either[ReplayError, Seq[AppliedBlock]]]

    /** Async sources are not directly iterable; force the engine to route through [[prefetch]]. */
    final override def iterate(
        from: ChainPoint,
        to: ChainPoint
    ): Either[ReplayError.ReplaySourceExhausted, Iterator[AppliedBlock]] =
        Left(ReplayError.ReplaySourceExhausted(from))
}
