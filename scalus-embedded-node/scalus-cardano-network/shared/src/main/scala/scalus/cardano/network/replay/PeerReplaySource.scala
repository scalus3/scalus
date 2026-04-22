package scalus.cardano.network.replay

import cps.*
import cps.monads.FutureAsyncMonad
import scalus.cardano.network.blockfetch.{BlockFetchDriver, FetchedBlock}
import scalus.cardano.network.chainsync.{ChainSyncDriver, ChainSyncEvent, IntersectSeeker, Point}
import scalus.cardano.network.infra.MiniProtocolId
import scalus.cardano.network.{BlockEnvelope, ChainApplier, ChainSyncError, Era, NodeToNodeConnection}
import scalus.cardano.node.stream.engine.AppliedBlock
import scalus.cardano.node.stream.engine.replay.{AsyncReplaySource, ReplayError}
import scalus.cardano.node.stream.{ChainPoint, ChainTip, StartFrom}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/** [[AsyncReplaySource]] that prefetches a replay window via a short-lived separate N2N connection.
  * See `docs/local/claude/indexer/checkpoint-restart-m7.md` § *Phase 2b peer replay*.
  *
  * Lifecycle per prefetch:
  *
  *   1. Ask the [[PeerReplayConnectionFactory]] for a fresh handshaken connection.
  *   2. Run `IntersectSeeker.seek(StartFrom.At(from))`. `NoIntersection` is the peer's way of
  *      saying "I cannot reach your checkpoint" — we convert it to
  *      [[ReplayError.ReplaySourceExhausted]] so the engine cascades to the next source.
  *   3. Loop: pull `ChainSync.next()` events, decode headers, fetch blocks via `BlockFetch`,
  *      accumulate [[AppliedBlock]]s until `chainPoint.slot >= to.slot`.
  *   4. Close the transient connection, regardless of outcome.
  *
  * The initial `MsgRollBackward` the peer always sends after a successful `MsgFindIntersect` is
  * absorbed silently — per the Ouroboros convention (see `ChainApplier.run`'s comment on the same
  * point). A second `Backward` during the pull loop is a real rollback mid-replay and produces
  * [[ReplayError.ReplayInterrupted]]; callers re-checkpoint and retry.
  *
  * Why a separate connection rather than a second ChainSync channel on the live applier's mux:
  * Ouroboros N2N specifies exactly one ChainSync state machine per connection (mini-protocol id 2
  * is singular). Two concurrent `FindIntersect` / `RequestNext` sequences over one mux violate the
  * peer's state machine. The cost is one TCP handshake per replay — negligible against the
  * BlockFetch cost for a meaningful window.
  */
final class PeerReplaySource(
    factory: PeerReplayConnectionFactory,
    logger: scribe.Logger = PeerReplaySource.defaultLogger
) extends AsyncReplaySource {

    def prefetch(
        from: ChainPoint,
        to: ChainPoint
    )(using ExecutionContext): Future[Either[ReplayError, Seq[AppliedBlock]]] = {
        if from == to then Future.successful(Right(Seq.empty))
        else
            factory.open().transformWith {
                case scala.util.Failure(t) =>
                    Future.successful(Left(ReplayError.ReplayInterrupted("open connection", t)))
                case scala.util.Success(conn) =>
                    runPrefetch(conn, from, to)
            }
    }

    /** Drive one prefetch against an open connection; close the connection on any exit path.
      *
      * Drivers are wired straight to `conn.rootToken`; there is no intermediate cancel scope
      * because the connection is transient — its lifetime IS the replay's lifetime.
      */
    private def runPrefetch(
        conn: NodeToNodeConnection,
        from: ChainPoint,
        to: ChainPoint
    )(using ExecutionContext): Future[Either[ReplayError, Seq[AppliedBlock]]] = {
        val chainSync = new ChainSyncDriver(
          conn.channel(MiniProtocolId.ChainSync),
          conn.rootToken,
          logger
        )
        val blockFetch = new BlockFetchDriver(
          conn.channel(MiniProtocolId.BlockFetch),
          conn.rootToken,
          logger
        )

        pullBlocks(chainSync, blockFetch, from, to).transformWith { result =>
            // Driver close() sends MsgDone best-effort; transport faults during teardown are
            // logged at debug by the drivers themselves. We do not await them before closing
            // the connection because the connection close cancels the mux regardless.
            chainSync.close()
            blockFetch.close()
            conn.close().transformWith(_ => Future.fromTry(result))
        }
    }

    /** The block-collection loop. Uses typed exceptions inside `async[Future]` for early exits and
      * recovers them at the outer level into the `Either` return shape.
      */
    private def pullBlocks(
        chainSync: ChainSyncDriver,
        blockFetch: BlockFetchDriver,
        from: ChainPoint,
        to: ChainPoint
    )(using ExecutionContext): Future[Either[ReplayError, Seq[AppliedBlock]]] = {
        val collected: Future[Seq[AppliedBlock]] = async[Future] {
            try {
                await(IntersectSeeker.seek(chainSync, StartFrom.At(from)))
            } catch {
                case _: ChainSyncError.NoIntersection =>
                    throw ReplayError.ReplaySourceExhausted(from)
            }

            val buf = ArrayBuffer.empty[AppliedBlock]
            var done = false
            var initialAckSeen = false

            while !done do {
                await(chainSync.next()) match {
                    case None =>
                        // Peer sent MsgDone. Acceptable only if we reached `to`.
                        if buf.lastOption.forall(_.point.slot < to.slot) then
                            throw ReplayError.ReplayInterrupted(
                              s"peer sent MsgDone before reaching $to (last seen: ${buf.lastOption.map(_.point)})",
                              new RuntimeException("premature MsgDone")
                            )
                        done = true

                    case Some(ChainSyncEvent.Backward(p, _)) =>
                        // Ouroboros convention: the very first response after a successful
                        // MsgFindIntersect is always MsgRollBackward(intersectPoint, tip). That's
                        // the peer's ack of our resume point, not an actual rewind — drop it.
                        if !initialAckSeen then initialAckSeen = true
                        else
                            throw ReplayError.ReplayInterrupted(
                              s"peer rolled back mid-replay to ${Point.toChainPoint(p)}",
                              new RuntimeException("rollback during replay")
                            )

                    case Some(ChainSyncEvent.Forward(era, headerBytes, _)) =>
                        val headerRaw = BlockEnvelope.decodeHeader(
                          Era.fromWire(era),
                          headerBytes
                        ) match {
                            case Left(err) =>
                                throw ReplayError.ReplayInterrupted("decode header", err)
                            case Right(h) => h
                        }
                        val chainPoint = ChainApplier.pointOf(headerBytes, headerRaw.value)
                        val chainTip = ChainTip(chainPoint, headerRaw.value.blockNumber)
                        val wirePoint =
                            Point.BlockPoint(chainPoint.slot, chainPoint.blockHash)

                        await(blockFetch.fetchOne(wirePoint)) match {
                            case Left(_) =>
                                throw ReplayError.ReplayInterrupted(
                                  s"peer missing block at $chainPoint mid-replay",
                                  new RuntimeException("MsgNoBlocks")
                                )
                            case Right(FetchedBlock(blockEra, blockBytes)) =>
                                ChainApplier.decodeFetchedBlock(
                                  era,
                                  chainTip,
                                  blockEra,
                                  blockBytes,
                                  logger
                                ) match {
                                    case Left(err) =>
                                        throw ReplayError.ReplayInterrupted("decode block", err)
                                    case Right(applied) =>
                                        buf += applied
                                        if chainPoint.slot >= to.slot then done = true
                                }
                        }
                }
            }

            buf.toSeq
        }

        collected.map(blocks => Right(blocks): Either[ReplayError, Seq[AppliedBlock]]).recover {
            case e: ReplayError => Left(e)
            case NonFatal(t)    => Left(ReplayError.ReplayInterrupted("replay driver", t))
        }
    }
}

object PeerReplaySource {
    val defaultLogger: scribe.Logger =
        scribe.Logger("scalus.cardano.network.replay.PeerReplaySource")
}
