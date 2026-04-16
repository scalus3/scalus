package scalus.cardano.n2n

import cps.*
import cps.monads.FutureAsyncMonad
import scalus.cardano.node.stream.DeltaBufferPolicy
import scalus.cardano.node.stream.engine.Mailbox
import scalus.uplc.builtin.ByteString

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/** Connection-fatal wire-protocol violation: peer sent an SDU whose routing is unambiguously wrong
  * (unknown protocol number, frame for a route we already closed, etc.).
  */
final class UnexpectedFrameException(message: String) extends RuntimeException(message)

/** Multiplexer configuration. */
final case class MuxConfig(
    sduMaxPayload: Int = Sdu.MaxPayloadSize,
    mailboxCapacity: DeltaBufferPolicy = DeltaBufferPolicy.Bounded(256)
)

/** Per-(protocol, direction) routing state. `Live` is the steady state — frames flow into the
  * mailbox. `Draining` means we've told the state machine to wind the protocol down: inbound frames
  * are still pulled off the wire to avoid stalling the mux reader but are silently discarded.
  * `Closed` means the peer has sent its MsgDone; any subsequent frame for this route is a peer
  * misbehaviour and fires the connection root.
  */
enum RouteState {
    case Live, Draining, Closed
}

/** Mux-facing ops handed to mini-protocol state machines. State machines never reach into the
  * concrete [[Multiplexer]]; they see this narrow interface, which keeps transport internals out of
  * protocol-layer code and lets tests supply a fake mux trivially.
  *
  * M4 exposes the graceful-wind-down methods on this interface; none of M4's own state machines
  * (handshake, keep-alive) drive routes into `Draining` — the first real user is M5's chain-sync
  * unsubscribe.
  */
trait RoutingOps {

    /** Mark the route as `Draining` — inbound frames will be pulled and discarded until either the
      * peer's MsgDone is observed (caller then invokes [[finishClose]]) or the connection root
      * cancels.
      */
    def beginDraining(proto: MiniProtocolId, dir: Direction): Unit

    /** Signal that the peer's MsgDone has been observed on the wire. Transitions the route from
      * `Draining` (or `Live`) to `Closed` and releases its mailbox; further inbound frames for this
      * route fire the connection root.
      */
    def finishClose(proto: MiniProtocolId, dir: Direction): Unit

    /** Fire the connection root with a typed cause — used on unrecoverable protocol-level failures
      * (decode error, state-machine invariant violation).
      */
    def escalateRoot(cause: Throwable): Unit
}

/** The Multiplexer demultiplexes inbound SDUs into per-(proto, Responder) byte streams and
  * serialises outbound writes through a single FIFO queue. One reader future runs for the lifetime
  * of the connection; any wire-level fatal fires the connection-root cancel, which fans out to
  * every consumer.
  *
  * Handles are created lazily via [[channel]]. An inbound SDU for a protocol without a handle is a
  * wire-level violation (there is no mailbox to deliver to and no consumer to create one on-demand
  * safely), so the reader fires the root with [[UnexpectedFrameException]].
  *
  * The reader and writer operate concurrently on the same [[AsyncByteChannel]] but never on
  * overlapping operations — the channel's concurrency contract allows one read + one write in
  * flight, which is what we do.
  */
final class Multiplexer(
    channel: AsyncByteChannel,
    root: CancelSource,
    config: MuxConfig = MuxConfig(),
    clock: MonotonicClock = MonotonicClock.system,
    logger: scribe.Logger = scribe.Logger[Multiplexer]
)(using ec: ExecutionContext)
    extends RoutingOps {

    require(
      config.sduMaxPayload > 0 && config.sduMaxPayload <= Sdu.MaxPayloadSize,
      s"sduMaxPayload=${config.sduMaxPayload} out of (0, ${Sdu.MaxPayloadSize}]"
    )

    private val routeLock = new AnyRef
    private val routes = mutable.Map.empty[(MiniProtocolId, Direction), Route]

    // Write queue: each send chains onto `writeTail` so that `channel.write` is serialised
    // FIFO. `recoverWith` on the tail absorbs failure so one failed write doesn't break the
    // chain for downstream callers; the failure propagates to the caller via their own
    // Future and triggers root cancel through the failure observer attached per-write.
    private val writeLock = new AnyRef
    @volatile private var writeTail: Future[Unit] = Future.unit

    // Kick off the reader loop. Runs until root cancels or the channel hits EOF.
    private val readerLoop: Future[Unit] = runReader()

    // --------------------------------------------------------------------------------------
    // Public API
    // --------------------------------------------------------------------------------------

    /** Get (or create) the handle for a mini-protocol. Calls for the same protocol return the same
      * handle — idempotent. The returned handle's `scope` is linked to the connection root.
      */
    def channel(proto: MiniProtocolId): MiniProtocolBytes = routeLock.synchronized {
        val inboundKey = (proto, Direction.Responder)
        val route = routes.getOrElseUpdate(inboundKey, makeRoute(proto, Direction.Responder))
        new Handle(proto, route)
    }

    /** Await reader-loop completion. Completes normally on clean EOF or on root cancel. */
    def closed: Future[Unit] = readerLoop

    // --------------------------------------------------------------------------------------
    // RoutingOps
    // --------------------------------------------------------------------------------------

    def beginDraining(proto: MiniProtocolId, dir: Direction): Unit = routeLock.synchronized {
        routes.get((proto, dir)).foreach { r =>
            if r.state == RouteState.Live then {
                r.state = RouteState.Draining
                logger.debug(s"route ${proto}/$dir transitioned Live -> Draining")
            }
        }
    }

    def finishClose(proto: MiniProtocolId, dir: Direction): Unit = routeLock.synchronized {
        routes.get((proto, dir)).foreach { r =>
            if r.state != RouteState.Closed then {
                r.state = RouteState.Closed
                r.mailbox.close()
                r.source.cancel(CancelledException(s"$proto/$dir closed"))
                logger.debug(s"route ${proto}/$dir transitioned to Closed")
            }
        }
    }

    def escalateRoot(cause: Throwable): Unit = root.cancel(cause)

    // --------------------------------------------------------------------------------------
    // Reader loop
    // --------------------------------------------------------------------------------------

    /** Reader loop. The returned Future:
      *   - completes with `Unit` on clean EOF between SDUs (the peer closed graciously);
      *   - completes with a failure on any wire-level or framing error;
      *   - completes with `CancelledException` (or the root's stored cause, if set) when somebody
      *     else cancelled the root while we were blocked on a read.
      *
      * In every failure path we also fire `root.cancel(cause)` so scope-based consumers see the
      * event promptly; the Future-level failure is the authoritative cause for the caller that
      * awaits `mux.closed`.
      */
    private def runReader(): Future[Unit] = async[Future] {
        var running = true
        while running do {
            try {
                await(channel.readExactly(Sdu.HeaderSize, root.token)) match {
                    case None =>
                        logger.info("peer sent clean EOF; closing mux")
                        shutdownAllRoutes()
                        root.cancel(new AsyncByteChannel.ChannelClosedException("peer closed"))
                        running = false
                    case Some(headerBytes) =>
                        val header = Sdu.parseHeader(headerBytes)
                        await(channel.readExactly(header.length, root.token)) match {
                            case None =>
                                throw new AsyncByteChannel.UnexpectedEofException(header.length, 0)
                            case Some(payload) =>
                                routeInbound(header, payload)
                        }
                }
            } catch {
                case NonFatal(t) =>
                    // On a cancel-induced failure the root already carries the authoritative
                    // cause; surface that to the caller. Otherwise this is a wire-level
                    // fatal: fire root ourselves and propagate.
                    val toPropagate = t match {
                        case _: CancelledException => root.token.cause.getOrElse(t)
                        case _ =>
                            if !root.token.isCancelled then {
                                logger.error(s"reader loop failure: $t")
                                root.cancel(t)
                            }
                            t
                    }
                    throw toPropagate
                case fatal: Throwable =>
                    // InterruptedException, ControlThrowable, OOM, etc. Don't absorb them —
                    // but DO fire root so `mux.closed` awaiters aren't left hanging forever.
                    if !root.token.isCancelled then root.cancel(fatal)
                    throw fatal
            }
        }
    }

    private def routeInbound(header: Sdu.Header, payload: ByteString): Unit = {
        header.protocol match {
            case None =>
                root.cancel(
                  UnexpectedFrameException(
                    s"unknown protocol wire=${header.protocolWire} in SDU header"
                  )
                )
            case Some(proto) =>
                val key = (proto, header.direction)
                routeLock.synchronized(routes.get(key)) match {
                    case None =>
                        root.cancel(
                          UnexpectedFrameException(
                            s"inbound SDU for $proto/${header.direction} with no active route"
                          )
                        )
                    case Some(r) =>
                        r.state match {
                            case RouteState.Live =>
                                // `Mailbox.offer` synchronously sets a terminal on overflow;
                                // the state machine's next `receive()` pull will surface
                                // `ScalusBufferOverflowException`. Escalation policy
                                // (whether an overflow is connection-fatal) lives in the
                                // state machine, not the mux — it owns its protocol's
                                // semantics.
                                r.mailbox.offer(payload)
                            case RouteState.Draining =>
                                logger.trace(s"drained frame on $proto/${header.direction}")
                            case RouteState.Closed =>
                                root.cancel(
                                  UnexpectedFrameException(
                                    s"SDU for already-closed route $proto/${header.direction}"
                                  )
                                )
                        }
                }
        }
    }

    private def shutdownAllRoutes(): Unit = routeLock.synchronized {
        routes.values.foreach { r =>
            if r.state != RouteState.Closed then {
                r.state = RouteState.Closed
                r.mailbox.close()
            }
        }
    }

    // Propagate root cancel to route mailboxes and protocol scopes.
    private val rootOnCancel: Cancellable = root.token.onCancel { () =>
        val cause = root.token.cause.getOrElse(CancelledException("root cancelled"))
        routeLock.synchronized {
            routes.values.foreach { r =>
                if r.state != RouteState.Closed then {
                    r.state = RouteState.Closed
                    r.mailbox.fail(cause)
                }
            }
        }
    }

    // --------------------------------------------------------------------------------------
    // Write path
    // --------------------------------------------------------------------------------------

    private def sendChunks(
        proto: MiniProtocolId,
        dir: Direction,
        payload: ByteString,
        cancel: CancelToken
    ): Future[Unit] = {
        if root.token.isCancelled then
            return Future.failed(new AsyncByteChannel.ChannelClosedException("mux closed"))
        if cancel.isCancelled then return Future.failed(CancelledException("pre-send"))
        val arr = payload.bytes
        if arr.length == 0 then enqueueOneSdu(proto, dir, Array.emptyByteArray, cancel)
        else {
            val max = config.sduMaxPayload
            val nChunks = (arr.length + max - 1) / max
            if nChunks == 1 then enqueueOneSdu(proto, dir, arr, cancel)
            else {
                // Chain sequentially — each SDU is enqueued only after the previous completes,
                // preserving FIFO across mini-protocols sharing the write queue.
                (0 until nChunks)
                    .map { i =>
                        val from = i * max
                        val to = math.min(from + max, arr.length)
                        val chunk = java.util.Arrays.copyOfRange(arr, from, to)
                        () => enqueueOneSdu(proto, dir, chunk, cancel)
                    }
                    .foldLeft(Future.unit) { (prev, next) => prev.flatMap(_ => next()) }
            }
        }
    }

    private def enqueueOneSdu(
        proto: MiniProtocolId,
        dir: Direction,
        payload: Array[Byte],
        cancel: CancelToken
    ): Future[Unit] = {
        val sduBytes = encodeSdu(proto, dir, payload)
        enqueueWrite(sduBytes, cancel)
    }

    private def enqueueWrite(bytes: ByteString, cancel: CancelToken): Future[Unit] =
        writeLock.synchronized {
            val thisWrite = writeTail.flatMap { _ =>
                if root.token.isCancelled then
                    Future.failed(new AsyncByteChannel.ChannelClosedException("mux closed"))
                else if cancel.isCancelled then Future.failed(CancelledException("pre-write"))
                else channel.write(bytes, cancel)
            }
            writeTail = thisWrite.recover { case _ => () }
            thisWrite.failed.foreach { t =>
                if !root.token.isCancelled then {
                    logger.error(s"write failed; firing root: $t")
                    root.cancel(t)
                }
            }
            thisWrite
        }

    private def encodeSdu(
        proto: MiniProtocolId,
        dir: Direction,
        payload: Array[Byte]
    ): ByteString = {
        val ts = clock.nowMicros().toInt // low 32 bits on the wire
        val headerBytes = Sdu.encodeHeader(ts, proto, dir, payload.length)
        val out = new Array[Byte](Sdu.HeaderSize + payload.length)
        System.arraycopy(headerBytes, 0, out, 0, Sdu.HeaderSize)
        if payload.length > 0 then System.arraycopy(payload, 0, out, Sdu.HeaderSize, payload.length)
        ByteString.unsafeFromArray(out)
    }

    // --------------------------------------------------------------------------------------
    // Internal state
    // --------------------------------------------------------------------------------------

    private final class Route(
        val proto: MiniProtocolId,
        val dir: Direction,
        val mailbox: Mailbox[ByteString],
        val source: CancelSource
    ) {
        @volatile var state: RouteState = RouteState.Live
    }

    private def makeRoute(proto: MiniProtocolId, dir: Direction): Route = {
        val capacity = config.mailboxCapacity match {
            case DeltaBufferPolicy.Bounded(n) => n
            case DeltaBufferPolicy.Unbounded  => Int.MaxValue
        }
        val source = CancelSource.linkedTo(root.token)
        // Mailbox failures (overflow, producer fail) are delivered to the state-machine
        // consumer via `pull()`'s failed Future. The consumer is responsible for deciding
        // whether to escalate to root via `RoutingOps.escalateRoot(cause)`.
        val mailbox = Mailbox.delta[ByteString](maxSize = capacity)
        new Route(proto, dir, mailbox, source)
    }

    private final class Handle(proto: MiniProtocolId, route: Route) extends MiniProtocolBytes {
        def scope: CancelToken = route.source.token

        def receive(cancel: CancelToken = scope): Future[Option[ByteString]] = {
            if cancel.isCancelled then Future.failed(CancelledException("pre-receive"))
            else route.mailbox.pull()
        }

        def send(message: ByteString, cancel: CancelToken = scope): Future[Unit] =
            sendChunks(proto, Direction.Initiator, message, cancel)
    }
}
