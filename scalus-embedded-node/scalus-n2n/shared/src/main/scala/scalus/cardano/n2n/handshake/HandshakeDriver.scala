package scalus.cardano.n2n.handshake

import scalus.cardano.infra.{CancelScope, CancelledException}
import scalus.cardano.n2n.handshake.HandshakeMessage.*
import scalus.cardano.n2n.{CborMessageStream, FrameDecodeException, MiniProtocolBytes, MiniProtocolId, NetworkMagic}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/** Outcome of a successful handshake: the version the peer accepted and the version-data they sent
  * back (needed by later callers that want to read e.g. `perasSupport`).
  */
final case class NegotiatedVersion(version: Int, data: NodeToNodeVersionData)

/** Initiator-side driver for the N2N handshake mini-protocol (id 0).
  *
  * Flow: propose → await → one of:
  *   - [[MsgAcceptVersion]] → [[NegotiatedVersion]]
  *   - [[MsgRefuse]] → fails with a typed [[HandshakeError]]
  *   - [[MsgQueryReply]] → fails with [[HandshakeError.UnexpectedMessage]] (we always send
  *     `query = false`, so receiving a query reply is a protocol violation)
  *   - [[MsgProposeVersions]] → fails with [[HandshakeError.UnexpectedMessage]] (we're initiator;
  *     the peer shouldn't propose back)
  *   - timeout → fails with [[HandshakeError.HandshakeTimeoutException]]
  *   - decode error → fails with [[HandshakeError.DecodeError]]
  *
  * Timeout policy: a 30-second deadline (default) is scheduled on the supplied [[CancelScope]]'s
  * timer. If the deadline fires first, a linked `timeoutSource` is cancelled, which propagates into
  * [[MiniProtocolBytes.receive]] as a `CancelledException`; the driver translates that into
  * `HandshakeTimeoutException`. If the reply arrives first, the scheduler handle is cancelled so
  * the timer doesn't keep a reference alive.
  *
  * Scope isolation: the timeout cancel does NOT fire the supplied scope itself — only the
  * per-handshake linked source. A caller wrapping the driver in a larger `connectionRoot` scope can
  * decide (typically in M8's `NodeToNodeClient`) whether handshake timeout is connection-fatal.
  *
  * See `docs/local/claude/indexer/n2n-transport.md` § *Handshake* for the wire format and error
  * model.
  */
final class HandshakeDriver(
    logger: scribe.Logger = scribe.Logger[HandshakeDriver]
) {

    def run(
        handle: MiniProtocolBytes,
        magic: NetworkMagic,
        scope: CancelScope,
        timeout: FiniteDuration = 30.seconds
    )(using ExecutionContext): Future[NegotiatedVersion] = {
        val table = defaultProposal(magic)
        val stream = new CborMessageStream[HandshakeMessage](MiniProtocolId.Handshake, handle)

        val deadline = scope.linked()
        val scheduled =
            scope.schedule(timeout, deadline, new HandshakeError.HandshakeTimeoutException)

        val sent = stream.send(MsgProposeVersions(table), scope.token)
        val exchange = sent.flatMap { _ => stream.receive(deadline.token) }

        exchange
            .transform { result =>
                // Always cancel the timer on exchange completion — success AND failure — so we
                // don't leak a scheduled fire into the timer's pending queue.
                scheduled.cancel()
                result.map(interpret(table, _))
            }
            .recoverWith {
                case _: CancelledException if deadline.token.isCancelled =>
                    Future.failed(new HandshakeError.HandshakeTimeoutException)
                case fde: FrameDecodeException =>
                    Future.failed(new HandshakeError.DecodeError(fde.getMessage, fde))
                case NonFatal(t) =>
                    Future.failed(t)
            }
    }

    /** Default version table proposed to the peer: v14 and v16 only (the doc explicitly skips v15
      * as a transient version). Values follow the doc's "Version-data values for M4" table —
      * `initiatorOnly=true`, `peerSharing=0`, `query=false`, `perasSupport=false` for v16.
      */
    private def defaultProposal(magic: NetworkMagic): VersionTable = VersionTable(
      VersionNumber.V14 -> NodeToNodeVersionData.V14(
        networkMagic = magic,
        initiatorOnlyDiffusionMode = true,
        peerSharing = 0,
        query = false
      ),
      VersionNumber.V16 -> NodeToNodeVersionData.V16(
        networkMagic = magic,
        initiatorOnlyDiffusionMode = true,
        peerSharing = 0,
        query = false,
        perasSupport = false
      )
    )

    private def interpret(
        proposed: VersionTable,
        received: Option[HandshakeMessage]
    ): NegotiatedVersion = received match {
        case None =>
            throw new HandshakeError.DecodeError("EOF before handshake reply", cause = null)
        case Some(MsgAcceptVersion(version, data)) =>
            logger.info(s"handshake accepted at v$version")
            NegotiatedVersion(version, data)
        case Some(MsgRefuse(reason)) =>
            logger.warn(s"handshake refused: $reason")
            reason match {
                case RefuseReason.VersionMismatch(peer) =>
                    throw new HandshakeError.VersionMismatch(proposed.keySet, peer)
                case RefuseReason.HandshakeDecodeError(version, message) =>
                    throw new HandshakeError.Refused(version, s"peer decode error: $message")
                case RefuseReason.Refused(version, message) =>
                    throw new HandshakeError.Refused(version, message)
            }
        case Some(MsgQueryReply(_)) =>
            throw new HandshakeError.UnexpectedMessage("MsgQueryReply")
        case Some(MsgProposeVersions(_)) =>
            throw new HandshakeError.UnexpectedMessage("MsgProposeVersions")
    }
}
