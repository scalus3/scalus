package scalus.cardano.network.keepalive

import io.bullet.borer.Cbor
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.infra.{CancelSource, CancelToken, FakeTimer}
import scalus.cardano.network.infra.MiniProtocolBytes
import scalus.cardano.network.keepalive.KeepAliveMessage.*
import scalus.uplc.builtin.ByteString

import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Future, Promise}

class KeepAliveDriverSuite extends AnyFunSuite with ScalaFutures with Eventually {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(3, Seconds), interval = Span(10, Millis))

    /** Scripted bytes handle: tests stage replies via `stage(...)` and inspect `sentOutbound` to
      * assert what the driver sent. Mirrors the HandshakeDriver test stub.
      */
    private final class ScriptedBytes extends MiniProtocolBytes {
        private val lock = new AnyRef
        private val inbound = mutable.ArrayDeque.empty[Option[ByteString]]
        private var pending: Option[Promise[Option[ByteString]]] = None
        val sentOutbound = mutable.ArrayBuffer.empty[ByteString]

        def receive(cancel: CancelToken = CancelToken.never): Future[Option[ByteString]] =
            lock.synchronized {
                if cancel.isCancelled then
                    Future.failed(cancel.cause.getOrElse(new RuntimeException("cancelled")))
                else if inbound.nonEmpty then Future.successful(inbound.removeHead())
                else {
                    val p = Promise[Option[ByteString]]()
                    pending = Some(p)
                    cancel.onCancel { () =>
                        lock.synchronized {
                            if pending.exists(_ eq p) then {
                                pending = None
                                val _ = p.tryFailure(
                                  cancel.cause.getOrElse(new RuntimeException("cancelled"))
                                )
                            }
                        }
                    }
                    p.future
                }
            }

        def send(message: ByteString, cancel: CancelToken = CancelToken.never): Future[Unit] =
            lock.synchronized {
                sentOutbound += message
                Future.unit
            }

        def stage(reply: KeepAliveMessage): Unit =
            offer(Some(ByteString.unsafeFromArray(Cbor.encode(reply).toByteArray)))

        def stageEof(): Unit = offer(None)

        private def offer(value: Option[ByteString]): Unit = lock.synchronized {
            pending match {
                case Some(p) =>
                    pending = None
                    val _ = p.trySuccess(value)
                case None => inbound.append(value)
            }
        }

        /** Read the cookie of the most recent MsgKeepAlive sent — tests ack with matching or
          * mismatched cookies.
          */
        def lastSentCookie: Int = {
            val bytes = sentOutbound.last.bytes
            Cbor.decode(bytes).to[KeepAliveMessage].value match {
                case MsgKeepAlive(c) => c
                case other           => fail(s"expected MsgKeepAlive, got $other")
            }
        }
    }

    /** Test fixture: connection root + FakeTimer + stub handle. */
    private def newFixture(): (ScriptedBytes, CancelSource, FakeTimer) = {
        val timer = new FakeTimer()
        val root = CancelSource()
        (new ScriptedBytes, root, timer)
    }

    private val interval = 30.seconds
    private val beatTimeout = 20.seconds

    // --------------------------------------------------------------------------------------
    // Happy path
    // --------------------------------------------------------------------------------------

    test("three beats succeed; RTT is published on each ack") {
        val (peer, root, timer) = newFixture()
        val rtt = new AtomicReference[Option[FiniteDuration]](None)
        val f = KeepAliveDriver.run(peer, root, timer, rtt, interval, beatTimeout)

        // Ack one beat and wait until the driver is sleeping between beats. We key on the
        // pending entry's fireAt == currentTime + interval — that's the sleep specifically,
        // distinct from the beat-timeout (fireAt = currentTime + beatTimeout) that the ack
        // cancels.
        def ackAndWaitForSleep(): Unit = {
            val c = peer.lastSentCookie
            peer.stage(MsgKeepAliveResponse(c))
            eventually {
                val expected = timer.currentTime + interval
                assert(timer.pendingFireAts.contains(expected))
            }
        }

        // Beat 1: driver has sent MsgKeepAlive(0) synchronously.
        eventually(assert(peer.sentOutbound.size == 1))
        assert(peer.lastSentCookie == 0)
        ackAndWaitForSleep()
        eventually(assert(rtt.get.isDefined))

        timer.advance(interval)
        eventually(assert(peer.sentOutbound.size == 2))
        assert(peer.lastSentCookie == 1)
        ackAndWaitForSleep()

        timer.advance(interval)
        eventually(assert(peer.sentOutbound.size == 3))
        assert(peer.lastSentCookie == 2)
        peer.stage(MsgKeepAliveResponse(peer.lastSentCookie))

        // No root cancel anywhere in the happy path.
        assert(!root.token.isCancelled)
        assert(!f.isCompleted, "loop should still be running (not cancelled)")
    }

    // --------------------------------------------------------------------------------------
    // Cookie mismatch
    // --------------------------------------------------------------------------------------

    test("cookie mismatch fires connectionRoot with KeepAliveError.CookieMismatch") {
        val (peer, root, timer) = newFixture()
        val rtt = new AtomicReference[Option[FiniteDuration]](None)
        val f = KeepAliveDriver.run(peer, root, timer, rtt, interval, beatTimeout)

        eventually { assert(peer.sentOutbound.size == 1) }
        val c = peer.lastSentCookie
        // Reply with a different cookie.
        peer.stage(MsgKeepAliveResponse(c + 7))

        eventually { assert(root.token.isCancelled) }
        val cause = root.token.cause.get
        assert(cause.isInstanceOf[KeepAliveError.CookieMismatch])
        val cm = cause.asInstanceOf[KeepAliveError.CookieMismatch]
        assert(cm.expected == c)
        assert(cm.received == c + 7)
    }

    // --------------------------------------------------------------------------------------
    // Beat timeout
    // --------------------------------------------------------------------------------------

    test("beat timeout fires connectionRoot with KeepAliveError.Timeout") {
        val (peer, root, timer) = newFixture()
        val rtt = new AtomicReference[Option[FiniteDuration]](None)
        val _ = KeepAliveDriver.run(peer, root, timer, rtt, interval, beatTimeout)

        // Driver sent beat 1; no response staged. Advance past beatTimeout.
        eventually { assert(peer.sentOutbound.size == 1) }
        timer.advance(beatTimeout + 1.second)

        eventually { assert(root.token.isCancelled) }
        val cause = root.token.cause.get
        assert(cause.isInstanceOf[KeepAliveError.Timeout])
    }

    // --------------------------------------------------------------------------------------
    // Clean shutdown paths
    // --------------------------------------------------------------------------------------

    test("peer MsgDone exits the loop cleanly without root cancel") {
        val (peer, root, timer) = newFixture()
        val rtt = new AtomicReference[Option[FiniteDuration]](None)
        val f = KeepAliveDriver.run(peer, root, timer, rtt, interval, beatTimeout)

        eventually { assert(peer.sentOutbound.size == 1) }
        peer.stage(MsgDone)

        f.futureValue // completes
        assert(!root.token.isCancelled)
    }

    test("peer EOF mid-beat exits the loop cleanly without root cancel") {
        val (peer, root, timer) = newFixture()
        val rtt = new AtomicReference[Option[FiniteDuration]](None)
        val f = KeepAliveDriver.run(peer, root, timer, rtt, interval, beatTimeout)

        eventually { assert(peer.sentOutbound.size == 1) }
        peer.stageEof()

        f.futureValue
        assert(!root.token.isCancelled)
    }

    test("keepAliveScope cancel exits the loop and triggers a best-effort MsgDone") {
        val (peer, root, timer) = newFixture()
        val rtt = new AtomicReference[Option[FiniteDuration]](None)
        val f = KeepAliveDriver.run(peer, root, timer, rtt, interval, beatTimeout)

        eventually { assert(peer.sentOutbound.size == 1) }

        // External teardown — cascade into keepAliveScope via root.
        root.cancel(new RuntimeException("user close"))

        f.futureValue
        // The scope's onCancel listener sends MsgDone best-effort; inspect the last frame.
        eventually { assert(peer.sentOutbound.size >= 2) }
        val lastBytes = peer.sentOutbound.last.bytes
        val lastMsg = Cbor.decode(lastBytes).to[KeepAliveMessage].value
        assert(lastMsg == MsgDone)
    }
}
