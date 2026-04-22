package scalus.cardano.network.replay

import io.bullet.borer.{Cbor, Decoder}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.infra.{CancelSource, CancelToken}
import scalus.cardano.ledger.{Block, BlockHeader, KeepRaw, OriginalCborByteArray}
import scalus.cardano.network.*
import scalus.cardano.network.blockfetch.BlockFetchMessage
import scalus.cardano.network.blockfetch.BlockFetchMessage.{MsgBatchDone, MsgBlock, MsgStartBatch}
import scalus.cardano.network.chainsync.ChainSyncMessage
import scalus.cardano.network.chainsync.ChainSyncMessage.{MsgDone, MsgIntersectFound, MsgIntersectNotFound, MsgRollBackward, MsgRollForward}
import scalus.cardano.network.chainsync.{Point, Tip}
import scalus.cardano.network.handshake.{NegotiatedVersion, NodeToNodeVersionData}
import scalus.cardano.network.infra.{AsyncByteChannel, MiniProtocolBytes, MiniProtocolId}
import scalus.cardano.node.stream.ChainPoint
import scalus.cardano.node.stream.engine.replay.ReplayError
import scalus.uplc.builtin.ByteString

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}

/** Unit-test coverage for [[PeerReplaySource]]. Uses a scripted [[NodeToNodeConnection]] with
  * scripted ChainSync / BlockFetch peers, so the source's protocol interactions can be asserted
  * against recorded expectations without a real peer. Block and header fixtures are the same
  * Conway-era mainnet blocks used by [[scalus.cardano.network.ChainApplierSuite]].
  */
class PeerReplaySourceSuite extends AnyFunSuite with ScalaFutures {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(5, Seconds), interval = Span(20, Millis))

    // --------------------------------------------------------------------------------------
    // Stub plumbing — same shape as ChainApplierSuite.
    // --------------------------------------------------------------------------------------

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

        def stageChainSync(m: ChainSyncMessage): Unit =
            offer(Some(ByteString.unsafeFromArray(Cbor.encode(m).toByteArray)))

        def stageBlockFetch(m: BlockFetchMessage): Unit =
            offer(Some(ByteString.unsafeFromArray(Cbor.encode(m).toByteArray)))

        def stageEof(): Unit = offer(None)

        private def offer(v: Option[ByteString]): Unit = lock.synchronized {
            pending match {
                case Some(p) =>
                    pending = None
                    val _ = p.trySuccess(v)
                case None => inbound.append(v)
            }
        }
    }

    private final class StubConnection(
        val chainSyncPeer: ScriptedBytes,
        val blockFetchPeer: ScriptedBytes
    ) extends NodeToNodeConnection {
        val rootSource: CancelSource = CancelSource()
        private val closedPromise = Promise[Unit]()
        rootSource.token.onCancel { () =>
            val _ = closedPromise.trySuccess(())
        }

        def negotiatedVersion: NegotiatedVersion =
            NegotiatedVersion(
              16,
              NodeToNodeVersionData.V16(NetworkMagic.YaciDevnet, true, 0, false, false)
            )

        def channel(proto: MiniProtocolId): MiniProtocolBytes = proto match {
            case MiniProtocolId.ChainSync  => chainSyncPeer
            case MiniProtocolId.BlockFetch => blockFetchPeer
            case other                     => sys.error(s"test stub doesn't serve $other")
        }

        def rootToken: CancelToken = rootSource.token
        def rtt: Option[FiniteDuration] = None
        def closed: Future[Unit] = closedPromise.future
        def close(): Future[Unit] = {
            rootSource.cancel(new AsyncByteChannel.ChannelClosedException("user close"))
            closedPromise.future.recover { case _ => () }
        }
    }

    // --------------------------------------------------------------------------------------
    // Block fixtures — copied from ChainApplierSuite, same resource slicing.
    // --------------------------------------------------------------------------------------

    private final case class Fixture(
        era: Int,
        block: Block,
        header: BlockHeader,
        headerBytes: ByteString,
        blockBytes: ByteString
    )

    private def loadFixture(resource: String): Fixture = {
        val raw = getClass.getResourceAsStream(resource).readAllBytes()
        val (fileEra, blockBytes) = sliceBlockFile(raw)
        val wireEra = fileEra - 1
        val headerBytes = sliceHeader(blockBytes)
        val block = decodeBlockFromBytes(blockBytes)
        Fixture(
          era = wireEra,
          block = block,
          header = block.header,
          headerBytes = ByteString.unsafeFromArray(headerBytes),
          blockBytes = ByteString.unsafeFromArray(blockBytes)
        )
    }

    private def sliceBlockFile(raw: Array[Byte]): (Int, Array[Byte]) = {
        given OriginalCborByteArray = OriginalCborByteArray(raw)
        given Decoder[EraAndBlock] = Decoder { r =>
            val len = r.readArrayHeader().toInt
            require(len == 2, s"expected BlockFile = [era, block]; got array of $len")
            val era = r.readInt()
            val block = r.read[KeepRaw[Block]]()
            EraAndBlock(era, block)
        }
        val decoded = Cbor.decode(raw).to[EraAndBlock].value
        (decoded.era, decoded.block.raw)
    }

    private def sliceHeader(blockBytes: Array[Byte]): Array[Byte] = {
        given OriginalCborByteArray = OriginalCborByteArray(blockBytes)
        given Decoder[BlockHead] = Decoder { r =>
            r.readArrayHeader()
            val header = r.read[KeepRaw[BlockHeader]]()
            BlockHead(header)
        }
        Cbor.decode(blockBytes).withPrefixOnly.to[BlockHead].value.header.raw
    }

    private def decodeBlockFromBytes(blockBytes: Array[Byte]): Block = {
        given OriginalCborByteArray = OriginalCborByteArray(blockBytes)
        Cbor.decode(blockBytes).to[Block].value
    }

    private final case class EraAndBlock(era: Int, block: KeepRaw[Block])
    private final case class BlockHead(header: KeepRaw[BlockHeader])

    private lazy val fixture1: Fixture = loadFixture("/blocks/block-11544748.cbor")
    private lazy val fixture2: Fixture = loadFixture("/blocks/block-11544518.cbor")

    private def dummyTip: Tip = Tip.origin

    // --------------------------------------------------------------------------------------
    // Factory stubs
    // --------------------------------------------------------------------------------------

    private final class CountingFactory(openFn: () => Future[NodeToNodeConnection])
        extends PeerReplayConnectionFactory {
        val opens = new AtomicInteger(0)

        def open()(using scala.concurrent.ExecutionContext): Future[NodeToNodeConnection] = {
            val _ = opens.incrementAndGet()
            openFn()
        }
    }

    // --------------------------------------------------------------------------------------
    // Tests
    // --------------------------------------------------------------------------------------

    test("from == to returns empty without opening a connection") {
        val factory = new CountingFactory(() => sys.error("should not be called"))
        val source = new PeerReplaySource(factory)

        val p = ChainApplier.pointOf(fixture1.headerBytes, fixture1.header)
        val result = source.prefetch(p, p).futureValue
        assert(result == Right(Seq.empty))
        assert(factory.opens.get == 0)
    }

    test("factory open failure surfaces as ReplayInterrupted") {
        val factory = new CountingFactory(() => Future.failed(new RuntimeException("no route")))
        val source = new PeerReplaySource(factory)

        val p1 = ChainApplier.pointOf(fixture1.headerBytes, fixture1.header)
        val p2 = ChainApplier.pointOf(fixture2.headerBytes, fixture2.header)
        val result = source.prefetch(p1, p2).futureValue
        assert(result.isLeft)
        val err = result.left.toOption.get
        assert(err.isInstanceOf[ReplayError.ReplayInterrupted])
    }

    test("NoIntersection maps to ReplaySourceExhausted and the connection is closed") {
        val chainSync = new ScriptedBytes
        val blockFetch = new ScriptedBytes
        val conn = new StubConnection(chainSync, blockFetch)
        val factory = new CountingFactory(() => Future.successful(conn))

        chainSync.stageChainSync(MsgIntersectNotFound(dummyTip))

        val p1 = ChainApplier.pointOf(fixture1.headerBytes, fixture1.header)
        val p2 = ChainApplier.pointOf(fixture2.headerBytes, fixture2.header)

        val source = new PeerReplaySource(factory)
        val result = source.prefetch(p1, p2).futureValue
        assert(result.isLeft)
        assert(result.left.toOption.get.isInstanceOf[ReplayError.ReplaySourceExhausted])
        // The connection's root is cancelled on close(), so `closed` should be done.
        assert(conn.closed.isCompleted)
    }

    test("happy path: intersect at from, absorb initial ack, pull blocks up to `to`") {
        // fixture1's slot < fixture2's? Let's order by slot at runtime to be fixture-agnostic.
        val (firstFx, lastFx) = {
            val p1 = ChainApplier.pointOf(fixture1.headerBytes, fixture1.header)
            val p2 = ChainApplier.pointOf(fixture2.headerBytes, fixture2.header)
            if p1.slot <= p2.slot then (fixture1, fixture2) else (fixture2, fixture1)
        }
        val from = ChainPoint.origin
        val toPoint = ChainApplier.pointOf(lastFx.headerBytes, lastFx.header)
        val intersectWire = Point.Origin

        val chainSync = new ScriptedBytes
        val blockFetch = new ScriptedBytes
        val conn = new StubConnection(chainSync, blockFetch)
        val factory = new CountingFactory(() => Future.successful(conn))

        // Intersect at Origin, peer ack with RollBackward to Origin, then two forwards.
        chainSync.stageChainSync(MsgIntersectFound(intersectWire, dummyTip))
        chainSync.stageChainSync(MsgRollBackward(intersectWire, dummyTip))
        chainSync.stageChainSync(MsgRollForward(era = firstFx.era, firstFx.headerBytes, dummyTip))
        chainSync.stageChainSync(MsgRollForward(era = lastFx.era, lastFx.headerBytes, dummyTip))

        blockFetch.stageBlockFetch(MsgStartBatch)
        blockFetch.stageBlockFetch(MsgBlock(firstFx.era, firstFx.blockBytes))
        blockFetch.stageBlockFetch(MsgBatchDone)
        blockFetch.stageBlockFetch(MsgStartBatch)
        blockFetch.stageBlockFetch(MsgBlock(lastFx.era, lastFx.blockBytes))
        blockFetch.stageBlockFetch(MsgBatchDone)

        val source = new PeerReplaySource(factory)
        val result = source.prefetch(from, toPoint).futureValue
        assert(result.isRight, s"expected Right, got $result")
        val blocks = result.toOption.get
        assert(blocks.size == 2)
        assert(blocks.head.point == ChainApplier.pointOf(firstFx.headerBytes, firstFx.header))
        assert(blocks.last.point == toPoint)
        assert(factory.opens.get == 1)
        // Connection was closed after prefetch.
        assert(conn.closed.isCompleted)
    }

    test("peer rollback during replay produces ReplayInterrupted") {
        // Need the staged forward's slot to be strictly less than `to.slot` so the pull loop
        // doesn't terminate on the forward before seeing the rollback. Pick the lower-slot
        // fixture as the forward and an artificial `to` past it.
        val (forwardFx, laterFx) = {
            val p1 = ChainApplier.pointOf(fixture1.headerBytes, fixture1.header)
            val p2 = ChainApplier.pointOf(fixture2.headerBytes, fixture2.header)
            if p1.slot <= p2.slot then (fixture1, fixture2) else (fixture2, fixture1)
        }
        val laterPoint = ChainApplier.pointOf(laterFx.headerBytes, laterFx.header)

        val chainSync = new ScriptedBytes
        val blockFetch = new ScriptedBytes
        val conn = new StubConnection(chainSync, blockFetch)
        val factory = new CountingFactory(() => Future.successful(conn))

        val from = ChainPoint.origin

        // Intersect + initial ack + one forward + rollback mid-stream.
        chainSync.stageChainSync(MsgIntersectFound(Point.Origin, dummyTip))
        chainSync.stageChainSync(MsgRollBackward(Point.Origin, dummyTip))
        chainSync.stageChainSync(
          MsgRollForward(era = forwardFx.era, forwardFx.headerBytes, dummyTip)
        )
        chainSync.stageChainSync(MsgRollBackward(Point.Origin, dummyTip))

        blockFetch.stageBlockFetch(MsgStartBatch)
        blockFetch.stageBlockFetch(MsgBlock(forwardFx.era, forwardFx.blockBytes))
        blockFetch.stageBlockFetch(MsgBatchDone)

        val source = new PeerReplaySource(factory)
        val result = source.prefetch(from, laterPoint).futureValue
        assert(result.isLeft, s"expected Left, got $result")
        assert(result.left.toOption.get.isInstanceOf[ReplayError.ReplayInterrupted])
        assert(conn.closed.isCompleted)
    }

    test("peer MsgDone before reaching `to` produces ReplayInterrupted") {
        val chainSync = new ScriptedBytes
        val blockFetch = new ScriptedBytes
        val conn = new StubConnection(chainSync, blockFetch)
        val factory = new CountingFactory(() => Future.successful(conn))

        val from = ChainPoint.origin
        val p1 = ChainApplier.pointOf(fixture1.headerBytes, fixture1.header)
        val p2 = ChainApplier.pointOf(fixture2.headerBytes, fixture2.header)

        chainSync.stageChainSync(MsgIntersectFound(Point.Origin, dummyTip))
        chainSync.stageChainSync(MsgRollBackward(Point.Origin, dummyTip))
        // One forward, then MsgDone from peer before we've reached `to`.
        chainSync.stageChainSync(MsgRollForward(era = fixture1.era, fixture1.headerBytes, dummyTip))
        chainSync.stageChainSync(MsgDone)

        blockFetch.stageBlockFetch(MsgStartBatch)
        blockFetch.stageBlockFetch(MsgBlock(fixture1.era, fixture1.blockBytes))
        blockFetch.stageBlockFetch(MsgBatchDone)

        val targetSlot = math.max(p1.slot, p2.slot) + 1000L
        val source = new PeerReplaySource(factory)
        val targetPoint = ChainPoint(targetSlot, p2.blockHash)
        val result = source.prefetch(from, targetPoint).futureValue
        assert(result.isLeft)
        assert(result.left.toOption.get.isInstanceOf[ReplayError.ReplayInterrupted])
    }
}
