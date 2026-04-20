package scalus.cardano.network.handshake

import scalus.cardano.infra.{CancelSource, CancelledException, Timer}
import scalus.cardano.network.handshake.HandshakeMessage.*
import scalus.cardano.network.{CborMessageStream, FrameDecodeException, MiniProtocolBytes, MiniProtocolId, NetworkMagic}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/** Outcome of a successful handshake: the version the peer accepted and the version-data they sent
  * back (needed by later callers that want to read e.g. `perasSupport`).
  */
final case class NegotiatedVersion(version: Int, data: NodeToNodeVersionData)

/** Initiator-side driver for the N2N handshake mini-protocol (id 0). One-shot: call [[run]] once
  * per connection, receive a `Future[NegotiatedVersion]`, discard.
  *
  * Flow: propose → await → one of:
  *   - [[MsgAcceptVersion]] → [[NegotiatedVersion]]
  *   - [[MsgRefuse]] → fails with a typed [[HandshakeError]]
  *   - [[MsgQueryReply]] → fails with [[HandshakeError.UnexpectedMessage]] (we always send
  *     `query = false`, so receiving a query reply is a protocol violation)
  *   - [[MsgProposeVersions]] → fails with [[HandshakeError.UnexpectedMessage]] (we're initiator;
  *     the peer shouldn't propose back)
  *   - timeout → fails with [[HandshakeError.Timeout]]
  *   - decode error → fails with [[HandshakeError.DecodeError]]
  *
  * Timeout policy: a 30-second deadline (default) is scheduled on the supplied `timer`. On expiry
  * the timer cancels `cancelScope` with a [[HandshakeError.Timeout]] as the cause; every in-flight
  * operation on the stream observes `cancelScope.token` and fails. If the reply arrives first, the
  * scheduled handle is cancelled so the timer doesn't fire.
  *
  * Scope isolation: `cancelScope` is typically a linked child of the connection root, so firing it
  * does NOT take down the root. The caller composing this driver into [[NodeToNodeClient]] decides
  * whether handshake timeout escalates further.
  *
  * See `docs/local/claude/indexer/n2n-transport.md` § *Handshake* for the wire format and error
  * model.
  */
object HandshakeDriver {

    /** Default logger used when the caller doesn't pass one — one lookup per module, not per
      * `run()` invocation.
      */
    private val defaultLogger: scribe.Logger = scribe.Logger("scalus.cardano.network.handshake")

    def run(
        handle: MiniProtocolBytes,
        magic: NetworkMagic,
        cancelScope: CancelSource,
        timer: Timer,
        timeout: FiniteDuration = 30.seconds,
        logger: scribe.Logger = defaultLogger
    )(using ExecutionContext): Future[NegotiatedVersion] = {
        val table = defaultProposal(magic)
        val stream = new CborMessageStream[HandshakeMessage](MiniProtocolId.Handshake, handle)

        val scheduled = timer.schedule(timeout) {
            cancelScope.cancel(new HandshakeError.Timeout)
        }

        val exchange = stream
            .send(MsgProposeVersions(table), cancelScope.token)
            .flatMap(_ => stream.receive(cancelScope.token))

        exchange
            .transform { result =>
                // Cancel the timer on completion (success or failure) so no stray scheduled
                // fire lingers in the timer's pending queue.
                scheduled.cancel()
                result.map(interpret(table, _, logger))
            }
            .recoverWith {
                case e: CancelledException if cancelScope.token.isCancelled =>
                    // Our scope fired — re-raise whatever cause is stored (the timer sets
                    // HandshakeError.Timeout; outer callers may set their own).
                    Future.failed(cancelScope.token.cause.getOrElse(e))
                case e: CancelledException =>
                    // CancelledException not attributable to our scope (e.g. the mux's route
                    // cancelled independently). Propagate as-is; don't falsely synthesise a
                    // HandshakeError.Timeout.
                    Future.failed(e)
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
        received: Option[HandshakeMessage],
        logger: scribe.Logger
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
        case Some(m @ (MsgQueryReply(_) | MsgProposeVersions(_))) =>
            throw new HandshakeError.UnexpectedMessage(m)
    }
}
