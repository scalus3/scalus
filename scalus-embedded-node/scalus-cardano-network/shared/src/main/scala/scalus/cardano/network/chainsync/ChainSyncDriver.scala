package scalus.cardano.network.chainsync

import scalus.cardano.infra.CancelToken
import scalus.cardano.network.chainsync.ChainSyncMessage.*
import scalus.cardano.network.ChainSyncError
import scalus.cardano.network.infra.{CborMessageStream, MiniProtocolBytes, MiniProtocolId}
import scalus.uplc.builtin.ByteString

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/** One event the driver observes on the chain-sync stream. Wire shapes are preserved: `Forward`
  * carries the raw era-tagged header bytes for decoding by [[scalus.cardano.network.BlockEnvelope]]
  * at the applier layer; `Backward` carries the wire-level [[Point]] which the applier converts to
  * [[scalus.cardano.node.stream.ChainPoint]] before calling `Engine.onRollBackward`.
  */
sealed trait ChainSyncEvent
object ChainSyncEvent {
    final case class Forward(era: Int, headerBytes: ByteString, tip: Tip) extends ChainSyncEvent
    final case class Backward(to: Point, tip: Tip) extends ChainSyncEvent
}

/** Initiator-side driver for the ChainSync mini-protocol (id 2).
  *
  * State machine (client agency on Idle; server agency on AwaitReply / Intersect):
  * {{{
  *   findIntersect(points) ──► send MsgFindIntersect ──► wait MsgIntersectFound / NotFound
  *   next()                ──► send MsgRequestNext   ──► wait MsgRollForward / Backward
  *                                                       (MsgAwaitReply is a transient
  *                                                        server-pause, we keep awaiting)
  *   close()               ──► send MsgDone (best effort)
  * }}}
  *
  * Error policy: any wire-level surprise (unexpected message shape in a given state, decode
  * failure) or transport-level cancellation propagates the failure to the caller's `Future`. The
  * driver does NOT itself fire `connectionRoot.cancel(...)` — that decision belongs to the chain
  * applier, which has the full context (e.g. "decode error → root cancel, but unknown-era might be
  * tolerable in a future M").
  *
  * Single-consumer contract: at most one `findIntersect` / `next` / `close` call may be in flight
  * on the same driver instance. Inherited from [[CborMessageStream]]. Violating this races on the
  * mux state and produces undefined behaviour — including the private `closed` flag, which is
  * deliberately a plain `var` (not `@volatile`) to make the contract loud at the JMM level rather
  * than paper it over.
  */
final class ChainSyncDriver(
    handle: MiniProtocolBytes,
    cancelToken: CancelToken,
    logger: scribe.Logger = ChainSyncDriver.defaultLogger
)(using ExecutionContext) {

    private val stream =
        new CborMessageStream[ChainSyncMessage](MiniProtocolId.ChainSync, handle)

    private var closed: Boolean = false

    /** Negotiate a starting point. Sends `MsgFindIntersect(points)` and interprets the reply:
      *   - `MsgIntersectFound(point, tip)` → `Future.successful((point, tip))`
      *   - `MsgIntersectNotFound(tip)` → `Future.failed(ChainSyncError.NoIntersection(tip))`
      *   - peer EOF or any other message → typed failure
      *
      * `points` may be empty — the peer will respond with `MsgIntersectNotFound` carrying its own
      * tip, which is how [[IntersectSeeker]] discovers the peer's tip in its two-step dance for
      * `StartFrom.Tip`.
      */
    def findIntersect(points: Seq[Point]): Future[(Point, Tip)] = {
        if closed then return Future.failed(new IllegalStateException("driver closed"))
        stream
            .send(MsgFindIntersect(points.toList), cancelToken)
            .flatMap(_ => stream.receive(cancelToken))
            .flatMap {
                case Some(MsgIntersectFound(p, t)) =>
                    Future.successful((p, t))
                case Some(MsgIntersectNotFound(t)) =>
                    Future.failed(ChainSyncError.NoIntersection(t))
                case Some(other) =>
                    Future.failed(
                      unexpectedMessage("awaiting intersect reply", other)
                    )
                case None =>
                    Future.failed(unexpectedEof("awaiting intersect reply"))
            }
            .recoverWith {
                case NonFatal(t)
                    if !t.isInstanceOf[ChainSyncError] &&
                        !t.isInstanceOf[IllegalStateException] =>
                    Future.failed(wrapIfDecode(t, "findIntersect"))
            }
    }

    /** Request and return the next chain event. [[ChainSyncMessage.MsgAwaitReply]] is handled
      * transparently — the driver keeps waiting on the same stream until a real event arrives.
      *
      * Returns `Future(None)` when the peer sends `MsgDone`; any further calls after that will
      * fail.
      */
    def next(): Future[Option[ChainSyncEvent]] = {
        if closed then return Future.failed(new IllegalStateException("driver closed"))
        stream
            .send(MsgRequestNext, cancelToken)
            .flatMap(_ => awaitActualEvent())
            .recoverWith {
                case NonFatal(t)
                    if !t.isInstanceOf[ChainSyncError] &&
                        !t.isInstanceOf[IllegalStateException] =>
                    Future.failed(wrapIfDecode(t, "next"))
            }
    }

    private def awaitActualEvent(): Future[Option[ChainSyncEvent]] =
        stream.receive(cancelToken).flatMap {
            case Some(MsgRollForward(era, bytes, tip)) =>
                Future.successful(Some(ChainSyncEvent.Forward(era, bytes, tip)))
            case Some(MsgRollBackward(to, tip)) =>
                Future.successful(Some(ChainSyncEvent.Backward(to, tip)))
            case Some(MsgAwaitReply) =>
                // Transient server-side parked state: the next real message on this stream
                // will be a RollForward / RollBackward once the peer has something to say.
                logger.debug("peer parked us with MsgAwaitReply; continuing to wait")
                awaitActualEvent()
            case Some(MsgDone) =>
                closed = true
                Future.successful(None)
            case Some(other) =>
                Future.failed(unexpectedMessage("awaiting next event", other))
            case None =>
                Future.failed(unexpectedEof("awaiting next event"))
        }

    /** Send `MsgDone` (best-effort) and mark the driver closed. Subsequent calls to
      * [[findIntersect]] / [[next]] fail with `IllegalStateException`.
      */
    def close(): Future[Unit] = {
        if closed then return Future.unit
        closed = true
        stream.send(MsgDone, cancelToken).recover { case NonFatal(t) =>
            logger.debug(s"MsgDone best-effort send failed: $t")
            ()
        }
    }

    private def unexpectedMessage(where: String, msg: ChainSyncMessage): Throwable =
        ChainSyncError.Decode(
          s"$where: unexpected peer message ${msg.getClass.getSimpleName}",
          cause = null
        )

    private def unexpectedEof(where: String): Throwable =
        ChainSyncError.Decode(s"$where: peer EOF", cause = null)

    /** CBOR-level decode failures from `CborMessageStream` surface as `FrameDecodeException`; wrap
      * in our typed `ChainSyncError.Decode` so callers pattern-match one hierarchy.
      */
    private def wrapIfDecode(t: Throwable, where: String): Throwable = t match {
        case fde: scalus.cardano.network.infra.FrameDecodeException =>
            ChainSyncError.Decode(where, fde)
        case other => other
    }
}

object ChainSyncDriver {
    private[chainsync] val defaultLogger: scribe.Logger =
        scribe.Logger("scalus.cardano.network.chainsync")
}
