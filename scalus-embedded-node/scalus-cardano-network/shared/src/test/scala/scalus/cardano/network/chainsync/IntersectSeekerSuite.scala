package scalus.cardano.network.chainsync

import io.bullet.borer.Cbor
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.infra.{CancelSource, CancelToken}
import scalus.cardano.ledger.BlockHash
import scalus.cardano.network.infra.MiniProtocolBytes
import scalus.cardano.network.chainsync.ChainSyncMessage.*
import scalus.cardano.node.stream.{ChainPoint, StartFrom}
import scalus.uplc.builtin.ByteString

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

class IntersectSeekerSuite extends AnyFunSuite with ScalaFutures {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(2, Seconds), interval = Span(10, Millis))

    // Reused scripted-peer harness shape — kept independent of ChainSyncDriverSuite so the
    // two tests can evolve separately.
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

        def send(message: ByteString, cancel: CancelToken = CancelToken.never): Future[Unit] = {
            sentOutbound += message
            Future.unit
        }

        def stage(reply: ChainSyncMessage): Unit =
            offer(Some(ByteString.unsafeFromArray(Cbor.encode(reply).toByteArray)))

        private def offer(value: Option[ByteString]): Unit = lock.synchronized {
            pending match {
                case Some(p) =>
                    pending = None
                    val _ = p.trySuccess(value)
                case None => inbound.append(value)
            }
        }
    }

    private def sampleHash(seed: Int): BlockHash =
        BlockHash.fromByteString(ByteString.fromArray(Array.fill(32)(seed.toByte)))

    private def decodeSent(peer: ScriptedBytes, idx: Int): ChainSyncMessage =
        Cbor.decode(peer.sentOutbound(idx).bytes).to[ChainSyncMessage].value

    private def newPair(): (ScriptedBytes, ChainSyncDriver) = {
        val peer = new ScriptedBytes
        (peer, new ChainSyncDriver(peer, CancelSource().token))
    }

    test("StartFrom.Origin: single FindIntersect([Origin])") {
        val (peer, driver) = newPair()
        peer.stage(MsgIntersectFound(Point.Origin, Tip.origin))

        val result = IntersectSeeker.seek(driver, StartFrom.Origin).futureValue
        assert(result == Point.Origin)
        assert(peer.sentOutbound.size == 1)
        assert(decodeSent(peer, 0) == MsgFindIntersect(List(Point.Origin)))
    }

    test("StartFrom.At(point): single FindIntersect([point])") {
        val (peer, driver) = newPair()
        val hash = sampleHash(7)
        val cp = ChainPoint(slot = 12345L, blockHash = hash)
        val wirePoint = Point.BlockPoint(12345L, hash)
        peer.stage(MsgIntersectFound(wirePoint, Tip(wirePoint, 999L)))

        val result = IntersectSeeker.seek(driver, StartFrom.At(cp)).futureValue
        assert(result == wirePoint)
        assert(decodeSent(peer, 0) == MsgFindIntersect(List(wirePoint)))
    }

    test("StartFrom.At(Origin-ChainPoint): normalises to Point.Origin candidate") {
        val (peer, driver) = newPair()
        peer.stage(MsgIntersectFound(Point.Origin, Tip.origin))

        val _ = IntersectSeeker.seek(driver, StartFrom.At(ChainPoint.origin)).futureValue
        assert(decodeSent(peer, 0) == MsgFindIntersect(List(Point.Origin)))
    }

    test("StartFrom.Tip: two-step dance (Origin → peer-tip)") {
        val (peer, driver) = newPair()
        val peerTipPoint = Point.BlockPoint(1000L, sampleHash(42))
        val peerTip = Tip(peerTipPoint, 100L)

        // Step 1: intersect at Origin; peer replies with its tip.
        peer.stage(MsgIntersectFound(Point.Origin, peerTip))
        // Step 2: driver re-intersects at peerTip.
        peer.stage(MsgIntersectFound(peerTipPoint, peerTip))

        val result = IntersectSeeker.seek(driver, StartFrom.Tip).futureValue
        assert(result == peerTipPoint)

        assert(peer.sentOutbound.size == 2)
        assert(decodeSent(peer, 0) == MsgFindIntersect(List(Point.Origin)))
        assert(decodeSent(peer, 1) == MsgFindIntersect(List(peerTipPoint)))
    }

    test("StartFrom.Tip on a freshly-started chain (peer tip is origin)") {
        val (peer, driver) = newPair()

        // Peer at origin — both intersects land on Origin.
        peer.stage(MsgIntersectFound(Point.Origin, Tip.origin))
        peer.stage(MsgIntersectFound(Point.Origin, Tip.origin))

        val result = IntersectSeeker.seek(driver, StartFrom.Tip).futureValue
        assert(result == Point.Origin)
        assert(peer.sentOutbound.size == 2)
    }

    test("StartFrom.At: NoIntersection propagates from driver") {
        val (peer, driver) = newPair()
        val peerTip = Tip(Point.BlockPoint(500L, sampleHash(5)), 50L)
        peer.stage(MsgIntersectNotFound(peerTip))

        val checkpoint = ChainPoint(slot = 1L, blockHash = sampleHash(1))
        val err = IntersectSeeker.seek(driver, StartFrom.At(checkpoint)).failed.futureValue
        err match {
            case scalus.cardano.network.ChainSyncError.NoIntersection(t) =>
                assert(t == peerTip)
            case other => fail(s"expected NoIntersection, got $other")
        }
    }
}
