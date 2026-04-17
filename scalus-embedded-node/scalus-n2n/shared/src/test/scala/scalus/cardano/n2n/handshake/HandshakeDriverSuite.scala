package scalus.cardano.n2n.handshake

import io.bullet.borer.Cbor
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.infra.{CancelScope, CancelSource, CancelToken, FakeTimer}
import scalus.cardano.n2n.handshake.HandshakeMessage.*
import scalus.cardano.n2n.{MiniProtocolBytes, NetworkMagic}
import scalus.uplc.builtin.ByteString

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}

/** Drives the [[HandshakeDriver]] state machine against a scripted peer backed by a simple
  * in-memory byte-stream. The peer's reply is staged before or after the driver's `send`, and
  * timing is deterministic via [[FakeTimer]].
  */
class HandshakeDriverSuite extends AnyFunSuite with ScalaFutures {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(2, Seconds), interval = Span(10, Millis))

    // --------------------------------------------------------------------------------------
    // Test harness — a MiniProtocolBytes backed by a queue of CBOR-encoded replies.
    // --------------------------------------------------------------------------------------

    private final class ScriptedBytes extends MiniProtocolBytes {
        private val lock = new AnyRef
        private val inbound = mutable.ArrayDeque.empty[Option[ByteString]]
        private var pending: Option[Promise[Option[ByteString]]] = None
        val sentOutbound = mutable.ArrayBuffer.empty[ByteString]
        private val src = CancelSource()

        def cancelScope: CancelToken = src.token

        def receive(cancel: CancelToken = cancelScope): Future[Option[ByteString]] =
            lock.synchronized {
                if cancel.isCancelled then
                    Future.failed(cancel.cause.getOrElse(new RuntimeException))
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

        def send(message: ByteString, cancel: CancelToken = cancelScope): Future[Unit] = {
            sentOutbound += message
            Future.unit
        }

        def stage(reply: HandshakeMessage): Unit =
            offer(Some(ByteString.unsafeFromArray(Cbor.encode(reply).toByteArray)))

        def stageRawBytes(bs: ByteString): Unit = offer(Some(bs))

        def stageEof(): Unit = offer(None)

        private def offer(value: Option[ByteString]): Unit = lock.synchronized {
            pending match {
                case Some(p) =>
                    pending = None
                    val _ = p.trySuccess(value)
                case None => inbound.append(value)
            }
        }
    }

    private def newScope(timer: FakeTimer = new FakeTimer()): (CancelScope, FakeTimer) = {
        val ts = if timer ne null then timer else new FakeTimer()
        (new CancelScope(CancelSource(), ts), ts)
    }

    private val magic = NetworkMagic.Preview

    // --------------------------------------------------------------------------------------
    // Happy path
    // --------------------------------------------------------------------------------------

    test("driver sends MsgProposeVersions with v14 + v16 and returns on MsgAcceptVersion") {
        val peer = new ScriptedBytes
        val (scope, _) = newScope()
        val f = new HandshakeDriver().run(peer, magic, scope)

        // Peer accepts at v16.
        val acceptedData = NodeToNodeVersionData.V16(
          networkMagic = magic,
          initiatorOnlyDiffusionMode = false,
          peerSharing = 0,
          query = false,
          perasSupport = false
        )
        peer.stage(MsgAcceptVersion(16, acceptedData))

        val negotiated = f.futureValue
        assert(negotiated == NegotiatedVersion(16, acceptedData))

        // And the driver must have sent a well-formed MsgProposeVersions.
        assert(peer.sentOutbound.size == 1)
        val proposal =
            Cbor.decode(peer.sentOutbound.head.bytes).to[HandshakeMessage].value
        proposal match {
            case MsgProposeVersions(table) =>
                assert(table.keySet == Set(14, 16))
                assert(
                  table(14).asInstanceOf[NodeToNodeVersionData.V14].networkMagic.value ==
                      magic.value
                )
                assert(
                  table(16).asInstanceOf[NodeToNodeVersionData.V16].perasSupport == false
                )
            case other => fail(s"expected MsgProposeVersions, got $other")
        }
    }

    // --------------------------------------------------------------------------------------
    // Peer-refused paths
    // --------------------------------------------------------------------------------------

    test("MsgRefuse/VersionMismatch surfaces as HandshakeError.VersionMismatch") {
        val peer = new ScriptedBytes
        val (scope, _) = newScope()
        val f = new HandshakeDriver().run(peer, magic, scope)
        peer.stage(MsgRefuse(RefuseReason.VersionMismatch(List(11, 12, 13))))

        val ex = f.failed.futureValue
        assert(ex.isInstanceOf[HandshakeError.VersionMismatch])
        val vm = ex.asInstanceOf[HandshakeError.VersionMismatch]
        assert(vm.proposed == Set(14, 16))
        assert(vm.peerSupported == List(11, 12, 13))
    }

    test("MsgRefuse/Refused surfaces as HandshakeError.Refused") {
        val peer = new ScriptedBytes
        val (scope, _) = newScope()
        val f = new HandshakeDriver().run(peer, magic, scope)
        peer.stage(MsgRefuse(RefuseReason.Refused(16, "magic mismatch")))

        val ex = f.failed.futureValue
        assert(ex.isInstanceOf[HandshakeError.Refused])
        val r = ex.asInstanceOf[HandshakeError.Refused]
        assert(r.versionTried == 16)
        assert(r.reason == "magic mismatch")
    }

    test("MsgRefuse/HandshakeDecodeError maps to Refused with 'peer decode error' prefix") {
        val peer = new ScriptedBytes
        val (scope, _) = newScope()
        val f = new HandshakeDriver().run(peer, magic, scope)
        peer.stage(MsgRefuse(RefuseReason.HandshakeDecodeError(14, "bad cbor")))

        val ex = f.failed.futureValue
        assert(ex.isInstanceOf[HandshakeError.Refused])
        val r = ex.asInstanceOf[HandshakeError.Refused]
        assert(r.versionTried == 14)
        assert(r.reason.contains("bad cbor"))
    }

    // --------------------------------------------------------------------------------------
    // Protocol violations
    // --------------------------------------------------------------------------------------

    test("MsgQueryReply in initiator flow is treated as an unexpected message") {
        val peer = new ScriptedBytes
        val (scope, _) = newScope()
        val f = new HandshakeDriver().run(peer, magic, scope)
        peer.stage(MsgQueryReply(VersionTable()))

        val ex = f.failed.futureValue
        assert(ex.isInstanceOf[HandshakeError.UnexpectedMessage])
    }

    test("MsgProposeVersions back from peer is an unexpected message") {
        val peer = new ScriptedBytes
        val (scope, _) = newScope()
        val f = new HandshakeDriver().run(peer, magic, scope)
        peer.stage(MsgProposeVersions(VersionTable()))

        val ex = f.failed.futureValue
        assert(ex.isInstanceOf[HandshakeError.UnexpectedMessage])
    }

    test("EOF before reply surfaces as DecodeError") {
        val peer = new ScriptedBytes
        val (scope, _) = newScope()
        val f = new HandshakeDriver().run(peer, magic, scope)
        peer.stageEof()

        val ex = f.failed.futureValue
        assert(ex.isInstanceOf[HandshakeError.DecodeError])
    }

    test("garbage bytes surface as DecodeError") {
        val peer = new ScriptedBytes
        val (scope, _) = newScope()
        val f = new HandshakeDriver().run(peer, magic, scope)
        // 0xFF alone isn't decodable as a top-level item; pad to two bytes to push past
        // insufficient-input.
        peer.stageRawBytes(ByteString.unsafeFromArray(Array[Byte](0xff.toByte, 0x00.toByte)))

        val ex = f.failed.futureValue
        assert(ex.isInstanceOf[HandshakeError.DecodeError], s"got ${ex.getClass}: ${ex.getMessage}")
    }

    // --------------------------------------------------------------------------------------
    // Timeout path — driven deterministically by FakeTimer
    // --------------------------------------------------------------------------------------

    test("timeout fires HandshakeTimeoutException and leaves outer scope unaffected") {
        val peer = new ScriptedBytes
        val timer = new FakeTimer()
        val scope = new CancelScope(CancelSource(), timer)
        val f = new HandshakeDriver().run(peer, magic, scope, timeout = 30.seconds)

        // No reply staged — driver is awaiting. Advance past deadline.
        timer.advance(29.seconds)
        assert(!f.isCompleted, "driver must still be waiting before deadline")
        timer.advance(2.seconds)

        val ex = f.failed.futureValue
        assert(ex.isInstanceOf[HandshakeError.HandshakeTimeoutException])
        // Outer scope not cancelled — timeout only took down the linked deadline source.
        assert(!scope.token.isCancelled)
    }

    test("early success cancels the scheduled timeout (no stray pending entries)") {
        val peer = new ScriptedBytes
        val timer = new FakeTimer()
        val scope = new CancelScope(CancelSource(), timer)
        val f = new HandshakeDriver().run(peer, magic, scope)

        peer.stage(
          MsgAcceptVersion(
            16,
            NodeToNodeVersionData.V16(magic, true, 0, false, false)
          )
        )
        val _ = f.futureValue
        assert(timer.pendingCount == 0, "timeout scheduler should be drained on success")
    }

    test("outer scope cancel before reply propagates the outer cause") {
        val peer = new ScriptedBytes
        val (scope, _) = newScope()
        val f = new HandshakeDriver().run(peer, magic, scope)

        val outerCause = new RuntimeException("outer cancel")
        scope.source.cancel(outerCause)
        val ex = f.failed.futureValue
        // The outer cancel flows through the linked deadline token and back up as the
        // stored cause — the driver does not classify upstream cancels as timeouts.
        assert(ex eq outerCause, s"expected outer cause, got $ex")
    }
}
