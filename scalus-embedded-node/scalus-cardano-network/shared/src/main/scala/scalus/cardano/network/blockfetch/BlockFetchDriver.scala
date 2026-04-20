package scalus.cardano.network.blockfetch

import scalus.cardano.infra.CancelToken
import scalus.cardano.network.blockfetch.BlockFetchMessage.*
import scalus.cardano.network.chainsync.Point
import scalus.cardano.network.ChainSyncError
import scalus.cardano.network.infra.{CborMessageStream, MiniProtocolBytes, MiniProtocolId}
import scalus.uplc.builtin.ByteString

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/** One block delivered by the peer. `era` is the HardFork era tag carried by the inner tag24
  * payload of `MsgBlock` (`[era_u16, block_cbor]`); `blockBytes` is the era-specific block CBOR
  * already unwrapped. Applier is expected to cross-check `era` against the era of the ChainSync
  * header that triggered the fetch and invoke [[scalus.cardano.network.BlockEnvelope.decodeBlock]].
  */
final case class FetchedBlock(era: Int, blockBytes: ByteString)

/** Initiator-side driver for the BlockFetch mini-protocol (id 3).
  *
  * State machine:
  * {{{
  *   fetchOne(point) ──► send MsgRequestRange(point, point)
  *                    ├► MsgStartBatch → MsgBlock(bytes) → MsgBatchDone → (block)
  *                    └► MsgNoBlocks                                     → NoBlocks
  *   close()         ──► send MsgClientDone (best effort)
  * }}}
  *
  * One in-flight range at a time — M5 scope. Higher pipeline depth is a performance knob that
  * the chain applier doesn't need for a live-follow workload; revisit in M7's cold-start replay
  * path.
  *
  * Single-consumer contract: at most one `fetchOne` / `close` call may be in flight on the same
  * driver instance (inherited from [[CborMessageStream]]). Violating this races on the mux state
  * and on the private `closed` flag, which is a plain `var` — the contract is enforced by the
  * caller, not the driver.
  */
final class BlockFetchDriver(
    handle: MiniProtocolBytes,
    cancelToken: CancelToken,
    logger: scribe.Logger = BlockFetchDriver.defaultLogger
)(using ExecutionContext) {

    private val stream =
        new CborMessageStream[BlockFetchMessage](MiniProtocolId.BlockFetch, handle)

    // Single-consumer contract — see class docstring. Plain var, not @volatile.
    private var closed: Boolean = false

    /** Fetch a single block identified by `point`. Encoded as a degenerate range `[point, point]`
      * — the peer responds with a one-element batch on success.
      *
      * Returns:
      *   - `Future.successful(Right(FetchedBlock))` — peer delivered the block.
      *   - `Future.successful(Left(ChainSyncError.MissingBlock))` — peer returned
      *     `MsgNoBlocks` (a chain-sync / block-fetch race: the peer rolled back between
      *     advertising the header and our fetch request). The applier decides whether to
      *     continue or escalate.
      *   - `Future.failed(...)` — wire-level failure (decode error, unexpected message,
      *     premature EOF, transport cancel). The applier treats these as fatal.
      */
    def fetchOne(point: Point): Future[Either[ChainSyncError.MissingBlock, FetchedBlock]] = {
        if closed then return Future.failed(new IllegalStateException("driver closed"))
        stream
            .send(MsgRequestRange(point, point), cancelToken)
            .flatMap(_ => awaitBatchStart(point))
            .recoverWith {
                case NonFatal(t)
                    if !t.isInstanceOf[ChainSyncError] &&
                        !t.isInstanceOf[IllegalStateException] =>
                    Future.failed(wrapIfDecode(t, "fetchOne"))
            }
    }

    private def awaitBatchStart(
        point: Point
    ): Future[Either[ChainSyncError.MissingBlock, FetchedBlock]] =
        stream.receive(cancelToken).flatMap {
            case Some(MsgStartBatch) => awaitBlock(point)
            case Some(MsgNoBlocks) =>
                Future.successful(Left(ChainSyncError.MissingBlock(Point.toChainPoint(point))))
            case Some(other) =>
                Future.failed(unexpectedMessage("awaiting batch start", other))
            case None =>
                Future.failed(unexpectedEof("awaiting batch start"))
        }

    private def awaitBlock(
        point: Point
    ): Future[Either[ChainSyncError.MissingBlock, FetchedBlock]] =
        stream.receive(cancelToken).flatMap {
            case Some(MsgBlock(era, bytes)) =>
                // Peer must still send MsgBatchDone next; consume and discard before returning
                // so the stream is back in Idle.
                stream.receive(cancelToken).flatMap {
                    case Some(MsgBatchDone) =>
                        Future.successful(Right(FetchedBlock(era, bytes)))
                    case Some(other) =>
                        // Multi-block batches are legal per the CDDL but we only request one
                        // block in M5 — unexpected.
                        Future.failed(unexpectedMessage("awaiting batch done", other))
                    case None =>
                        Future.failed(unexpectedEof("awaiting batch done"))
                }
            case Some(other) =>
                Future.failed(unexpectedMessage("awaiting block in batch", other))
            case None =>
                Future.failed(unexpectedEof("awaiting block in batch"))
        }

    /** Send `MsgClientDone` best-effort and mark the driver closed. Further calls to
      * [[fetchOne]] fail with `IllegalStateException`.
      */
    def close(): Future[Unit] = {
        if closed then return Future.unit
        closed = true
        stream.send(MsgClientDone, cancelToken).recover { case NonFatal(t) =>
            logger.debug(s"MsgClientDone best-effort send failed: $t")
            ()
        }
    }

    private def unexpectedMessage(where: String, msg: BlockFetchMessage): Throwable =
        ChainSyncError.Decode(
          s"$where: unexpected peer message ${msg.getClass.getSimpleName}",
          cause = null
        )

    private def unexpectedEof(where: String): Throwable =
        ChainSyncError.Decode(s"$where: peer EOF", cause = null)

    private def wrapIfDecode(t: Throwable, where: String): Throwable = t match {
        case fde: scalus.cardano.network.infra.FrameDecodeException =>
            ChainSyncError.Decode(where, fde)
        case other => other
    }
}

object BlockFetchDriver {
    private[blockfetch] val defaultLogger: scribe.Logger =
        scribe.Logger("scalus.cardano.network.blockfetch")
}
