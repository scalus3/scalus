package scalus.cardano.n2n.keepalive

import cps.*
import cps.monads.FutureAsyncMonad
import scalus.cardano.infra.{CancelSource, CancelToken, CancelledException, Timer}
import scalus.cardano.n2n.keepalive.KeepAliveMessage.*
import scalus.cardano.n2n.{CborMessageStream, MiniProtocolBytes, MiniProtocolId, MonotonicClock}

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/** Initiator-side driver for the KeepAlive mini-protocol (id 8). Runs for the lifetime of the
  * connection: every `interval` it sends `MsgKeepAlive(cookie)` and awaits the echo, publishing the
  * RTT to the supplied `rttRef`.
  *
  * Error policy: every failure is connection-fatal — the driver fires `connectionRoot.cancel(...)`
  * with a typed `KeepAliveError`:
  *   - Beat timeout (no response within `beatTimeout`) → `connectionRoot.cancel(Timeout)`
  *   - Cookie mismatch (peer echoed a different cookie) → `connectionRoot.cancel(CookieMismatch)`
  *
  * Clean shutdown: when `connectionRoot` cancels (externally or driver-initiated), the driver sends
  * `MsgDone` best-effort and exits.
  *
  * See `docs/local/claude/indexer/n2n-transport.md` § *KeepAlive* for the wire format and error
  * model.
  */
object KeepAliveDriver {

    private val defaultLogger: scribe.Logger = scribe.Logger("scalus.cardano.n2n.keepalive")

    /** Run the heartbeat loop. Returns a `Future[Unit]` that completes when the loop exits (on
      * `connectionRoot` cancel or peer `MsgDone`).
      *
      * @param handle
      *   the mux's KeepAlive channel.
      * @param connectionRoot
      *   the connection root source. Fired on cookie mismatch / beat timeout; observed for external
      *   shutdown.
      * @param timer
      *   used for per-beat deadlines and inter-beat sleeps.
      * @param rttRef
      *   updated on each successful beat with the most recent round-trip time.
      */
    def run(
        handle: MiniProtocolBytes,
        connectionRoot: CancelSource,
        timer: Timer,
        rttRef: AtomicReference[Option[FiniteDuration]] =
            new AtomicReference[Option[FiniteDuration]](None),
        interval: FiniteDuration = 30.seconds,
        beatTimeout: FiniteDuration = 20.seconds,
        clock: MonotonicClock = MonotonicClock.system,
        logger: scribe.Logger = defaultLogger
    )(using ExecutionContext): Future[Unit] = {
        val stream = new CborMessageStream[KeepAliveMessage](MiniProtocolId.KeepAlive, handle)

        // Best-effort MsgDone on root cancel. Registered BEFORE the loop so we catch both
        // external shutdown and driver-initiated root cancel.
        val doneOnShutdown = connectionRoot.token.onCancel { () =>
            stream.send(MsgDone, CancelToken.never).failed.foreach { t =>
                logger.debug(s"MsgDone best-effort send failed: $t")
            }
        }

        val loop = heartbeatLoop(
          stream,
          connectionRoot,
          timer,
          rttRef,
          interval,
          beatTimeout,
          clock,
          logger
        )

        loop.andThen { case _ => doneOnShutdown.cancel() }
    }

    private def heartbeatLoop(
        stream: CborMessageStream[KeepAliveMessage],
        connectionRoot: CancelSource,
        timer: Timer,
        rttRef: AtomicReference[Option[FiniteDuration]],
        interval: FiniteDuration,
        beatTimeout: FiniteDuration,
        clock: MonotonicClock,
        logger: scribe.Logger
    )(using ExecutionContext): Future[Unit] = async[Future] {
        var cookie: Int = 0
        var running = true
        while running && !connectionRoot.token.isCancelled do {
            val beatScope = CancelSource.linkedTo(connectionRoot.token)
            val scheduled = timer.schedule(beatTimeout) {
                beatScope.cancel(new KeepAliveError.Timeout)
            }
            val sentAt = clock.nowMicros()

            val beatOutcome: BeatOutcome =
                try {
                    await(stream.send(MsgKeepAlive(cookie), beatScope.token))
                    val reply = await(stream.receive(beatScope.token))
                    scheduled.cancel()
                    reply match {
                        case Some(MsgKeepAliveResponse(echo)) if echo == cookie =>
                            val rtt =
                                FiniteDuration(clock.nowMicros() - sentAt, TimeUnit.MICROSECONDS)
                            rttRef.set(Some(rtt))
                            logger.debug(s"beat ack cookie=$cookie rtt=$rtt")
                            BeatOutcome.Acked
                        case Some(MsgKeepAliveResponse(other)) =>
                            BeatOutcome.CookieMismatch(cookie, other)
                        case Some(MsgKeepAlive(_)) =>
                            // We're the initiator; peer sending MsgKeepAlive is a wire
                            // violation. Treat as fatal via the Failed path.
                            BeatOutcome.Failed(
                              new IllegalStateException(
                                s"peer sent MsgKeepAlive — we're the initiator, not the responder"
                              )
                            )
                        case Some(MsgDone) =>
                            logger.info("peer sent MsgDone; stopping keep-alive")
                            BeatOutcome.PeerDone
                        case None =>
                            logger.info("keep-alive stream EOF; stopping")
                            BeatOutcome.Eof
                    }
                } catch {
                    // connectionRoot cancelled from outside (external teardown or our own
                    // error-escalation path) — exit cleanly regardless of the exception shape.
                    // The cause carried by the stream's failed Future may be any Throwable
                    // (whatever was passed to the upstream cancel), not specifically
                    // CancelledException.
                    case NonFatal(_) if connectionRoot.token.isCancelled =>
                        scheduled.cancel()
                        BeatOutcome.RootCancelled
                    case _: CancelledException if beatScope.token.isCancelled =>
                        scheduled.cancel()
                        BeatOutcome.BeatTimeout
                    case NonFatal(t) =>
                        scheduled.cancel()
                        BeatOutcome.Failed(t)
                }

            beatOutcome match {
                case BeatOutcome.Acked =>
                    cookie = (cookie + 1) & 0xffff
                    try await(timer.sleep(interval, connectionRoot.token))
                    catch {
                        case _: CancelledException => running = false
                    }
                case BeatOutcome.BeatTimeout =>
                    val cause = new KeepAliveError.Timeout
                    connectionRoot.cancel(cause)
                    throw cause
                case BeatOutcome.CookieMismatch(expected, received) =>
                    val cause = new KeepAliveError.CookieMismatch(expected, received)
                    connectionRoot.cancel(cause)
                    throw cause
                case BeatOutcome.PeerDone | BeatOutcome.Eof | BeatOutcome.RootCancelled =>
                    running = false
                case BeatOutcome.Failed(t) =>
                    // Unexpected transport failure — escalate to connection root.
                    connectionRoot.cancel(t)
                    throw t
            }
        }
    }

    /** Outcome of one heartbeat exchange. Keeps the match-on-result code out of
      * `async[Future] { try { ... } }` territory where dotty-cps-async + try/catch + complex
      * returns interact poorly.
      */
    private sealed trait BeatOutcome
    private object BeatOutcome {
        case object Acked extends BeatOutcome
        case object BeatTimeout extends BeatOutcome
        final case class CookieMismatch(expected: Int, received: Int) extends BeatOutcome
        case object PeerDone extends BeatOutcome
        case object Eof extends BeatOutcome
        case object RootCancelled extends BeatOutcome
        final case class Failed(cause: Throwable) extends BeatOutcome
    }
}
