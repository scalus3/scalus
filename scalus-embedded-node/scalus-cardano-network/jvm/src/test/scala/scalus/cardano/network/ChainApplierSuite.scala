package scalus.cardano.network

import io.bullet.borer.{Cbor, Decoder, Reader}
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.infra.{CancelSource, CancelToken}
import scalus.cardano.ledger.{Block, BlockHeader, CardanoInfo, KeepRaw, OriginalCborByteArray}
import scalus.cardano.network.blockfetch.BlockFetchMessage
import scalus.cardano.network.blockfetch.BlockFetchMessage.{MsgBatchDone, MsgBlock, MsgNoBlocks, MsgStartBatch}
import scalus.cardano.network.chainsync.ChainSyncMessage
import scalus.cardano.network.chainsync.ChainSyncMessage.{MsgIntersectFound, MsgRollBackward, MsgRollForward}
import scalus.cardano.network.chainsync.{Point, Tip}
import scalus.cardano.network.handshake.{NegotiatedVersion, NodeToNodeVersionData}
import scalus.cardano.network.infra.{AsyncByteChannel, MiniProtocolBytes, MiniProtocolId}
import scalus.cardano.node.stream.engine.Engine
import scalus.cardano.node.stream.{ChainPoint, ChainTip, StartFrom}
import scalus.uplc.builtin.ByteString

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}

/** End-to-end loopback test for [[ChainApplier]] wiring a pair of [[ScriptedBytes]] peers to a
  * real [[Engine]]. Exercises intersect → forwards → block-fetch → engine onRollForward, plus
  * the rollback and chain-sync/block-fetch race (MsgNoBlocks) paths.
  *
  * Uses two real Cardano Conway-era block fixtures copied from
  * `bloxbean-cardano-client-lib`'s test resources — synthesising arbitrary encoded blocks is
  * fragile (the borer-derived encoder doesn't always round-trip arbitrary-generated trees
  * because raw-byte fidelity is only preserved on decode-then-encode, not generate-then-encode).
  *
  * Placed in the JVM-only test tree because it reads classpath resources.
  */
class ChainApplierSuite extends AnyFunSuite with ScalaFutures with Eventually {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(5, Seconds), interval = Span(20, Millis))

    // --------------------------------------------------------------------------------------
    // Harness
    // --------------------------------------------------------------------------------------

    /** Scripted MiniProtocolBytes — same shape as the driver-level suites. */
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

        private def offer(v: Option[ByteString]): Unit = lock.synchronized {
            pending match {
                case Some(p) =>
                    pending = None
                    val _ = p.trySuccess(v)
                case None => inbound.append(v)
            }
        }
    }

    /** NodeToNodeConnection stub serving two `ScriptedBytes` for ChainSync and BlockFetch. */
    private final class StubConnection(
        val chainSyncPeer: ScriptedBytes,
        val blockFetchPeer: ScriptedBytes
    ) extends NodeToNodeConnection {
        val rootSource: CancelSource = CancelSource()
        private val closedPromise = Promise[Unit]()
        rootSource.token.onCancel { () => val _ = closedPromise.trySuccess(()) }

        def negotiatedVersion: NegotiatedVersion =
            NegotiatedVersion(
              16,
              NodeToNodeVersionData.V16(NetworkMagic.YaciDevnet, true, 0, false, false)
            )

        def channel(proto: MiniProtocolId): MiniProtocolBytes = proto match {
            case MiniProtocolId.ChainSync  => chainSyncPeer
            case MiniProtocolId.BlockFetch => blockFetchPeer
            case other => sys.error(s"test stub doesn't serve $other")
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
    // Block fixtures — copied from bloxbean-cardano-client-lib test resources. These are real
    // Conway-era blocks pulled from Cardano mainnet; the BlockFile format is [era, block].
    // --------------------------------------------------------------------------------------

    private final case class Fixture(
        era: Int,
        block: Block,
        header: BlockHeader,
        headerBytes: ByteString,
        blockBytes: ByteString
    )

    /** Slice the inner era-specific Block and BlockHeader CBOR bytes out of a BlockFile
      * `[era, block]` without re-encoding.
      *
      * Re-encoding via `Cbor.encode(block)` is unreliable on real-chain blocks: scalus-core's
      * `KeepRaw[A]` encoder writes `raw` bytes directly to the output, which bypasses borer's
      * validator; the validator then fails on what looks like a truncated definite-length map
      * or array. Slicing the raw input instead yields the exact bytes the peer would deliver
      * on the wire, and they round-trip cleanly through `BlockEnvelope.decodeHeader` /
      * `decodeBlock`.
      */
    /** Slice the inner era-specific Block and BlockHeader CBOR bytes out of a BlockFile
      * `[era, block]` without re-encoding.
      *
      * Re-encoding via `Cbor.encode(block)` is unreliable on real-chain blocks: scalus-core's
      * `KeepRaw[A]` encoder writes `raw` bytes directly to the output, which bypasses borer's
      * validator; the validator then fails on what looks like a truncated definite-length map
      * or array. Slicing the raw input instead yields the exact bytes the peer would deliver
      * on the wire, and they round-trip cleanly through `BlockEnvelope.decodeHeader` /
      * `decodeBlock`.
      */
    private def loadFixture(resource: String): Fixture = {
        val raw = getClass.getResourceAsStream(resource).readAllBytes()
        val (fileEra, blockBytes) = sliceBlockFile(raw)
        // bloxbean's `BlockFile` on-disk format uses Cardano-spec 1-based era numbering
        // (Byron=1, Shelley=2, …, Conway=7). N2N wire uses ouroboros-consensus HardFork
        // 0-based indexing (Byron=0, …, Conway=6). Convert here so the wire messages we
        // stage match what a real peer would send.
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
            r.readArrayHeader() // Block = [header, txBodies, txWitnessSets, auxMap, invalidTxs]
            val header = r.read[KeepRaw[BlockHeader]]()
            BlockHead(header)
        }
        // `withPrefixOnly` lets borer stop after BlockHead without complaining about trailing
        // Block fields the decoder intentionally ignored.
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

    private def newEngine(): Engine =
        new Engine(CardanoInfo.preview, backup = None, securityParam = 4)

    // --------------------------------------------------------------------------------------
    // Tests
    // --------------------------------------------------------------------------------------

    test("MsgBlock codec round-trips a real Conway block") {
        // Regression guard for the codec rewrite that moved era inside the tag24 wrapper.
        // A broken inner-wrapper slice would silently produce a different decoded blockBytes
        // and break the applier's decodeBlock step downstream.
        import io.bullet.borer.Cbor
        import scalus.cardano.network.blockfetch.BlockFetchMessage
        val original = BlockFetchMessage.MsgBlock(fixture1.era, fixture1.blockBytes)
        val encoded = Cbor.encode(original: BlockFetchMessage).toByteArray
        val decoded = Cbor.decode(encoded).to[BlockFetchMessage].value
        assert(decoded == original, s"round-trip mismatch: got $decoded")
    }

    test("happy path: intersect at Origin + 2 forwards → engine tip advances") {
        val chainSync = new ScriptedBytes
        val blockFetch = new ScriptedBytes
        val conn = new StubConnection(chainSync, blockFetch)
        val engine = newEngine()

        val p1 = ChainApplier.pointOf(fixture1.headerBytes, fixture1.header)
        val p2 = ChainApplier.pointOf(fixture2.headerBytes, fixture2.header)

        chainSync.stageChainSync(MsgIntersectFound(Point.Origin, dummyTip))
        chainSync.stageChainSync(MsgRollForward(era = fixture1.era, fixture1.headerBytes, dummyTip))
        chainSync.stageChainSync(MsgRollForward(era = fixture2.era, fixture2.headerBytes, dummyTip))

        blockFetch.stageBlockFetch(MsgStartBatch)
        blockFetch.stageBlockFetch(MsgBlock(fixture1.era, fixture1.blockBytes))
        blockFetch.stageBlockFetch(MsgBatchDone)
        blockFetch.stageBlockFetch(MsgStartBatch)
        blockFetch.stageBlockFetch(MsgBlock(fixture2.era, fixture2.blockBytes))
        blockFetch.stageBlockFetch(MsgBatchDone)

        val handle = ChainApplier.spawn(conn, engine, StartFrom.Origin)

        try {
            eventually {
                // Surface applier-loop failures instead of silently hanging on an unrelated
                // tip assertion.
                handle.done.value.foreach {
                    case scala.util.Failure(t) => fail(s"applier done failed: $t", t)
                    case _                     => ()
                }
                assert(engine.currentTip.map(_.point).contains(p2))
            }
            assert(engine.currentTip == Some(ChainTip(p2, fixture2.header.blockNumber)))
        } finally {
            handle.cancel().futureValue
            engine.shutdown().futureValue
        }
    }

    test("rollback: engine tip rewinds after MsgRollBackward") {
        val chainSync = new ScriptedBytes
        val blockFetch = new ScriptedBytes
        val conn = new StubConnection(chainSync, blockFetch)
        val engine = newEngine()

        val p1 = ChainApplier.pointOf(fixture1.headerBytes, fixture1.header)

        chainSync.stageChainSync(MsgIntersectFound(Point.Origin, dummyTip))
        chainSync.stageChainSync(MsgRollForward(era = fixture1.era, fixture1.headerBytes, dummyTip))
        chainSync.stageChainSync(MsgRollForward(era = fixture2.era, fixture2.headerBytes, dummyTip))
        chainSync.stageChainSync(
          MsgRollBackward(Point.BlockPoint(p1.slot, p1.blockHash), dummyTip)
        )

        blockFetch.stageBlockFetch(MsgStartBatch)
        blockFetch.stageBlockFetch(MsgBlock(fixture1.era, fixture1.blockBytes))
        blockFetch.stageBlockFetch(MsgBatchDone)
        blockFetch.stageBlockFetch(MsgStartBatch)
        blockFetch.stageBlockFetch(MsgBlock(fixture2.era, fixture2.blockBytes))
        blockFetch.stageBlockFetch(MsgBatchDone)

        val handle = ChainApplier.spawn(conn, engine, StartFrom.Origin)

        try {
            eventually {
                assert(engine.currentTip.map(_.point).contains(p1))
            }
            assert(engine.currentTip == Some(ChainTip(p1, fixture1.header.blockNumber)))
        } finally {
            handle.cancel().futureValue
            engine.shutdown().futureValue
        }
    }

    test("missing block: MsgNoBlocks from fetch drops the header and continues") {
        val chainSync = new ScriptedBytes
        val blockFetch = new ScriptedBytes
        val conn = new StubConnection(chainSync, blockFetch)
        val engine = newEngine()

        val p2 = ChainApplier.pointOf(fixture2.headerBytes, fixture2.header)

        chainSync.stageChainSync(MsgIntersectFound(Point.Origin, dummyTip))
        chainSync.stageChainSync(MsgRollForward(era = fixture1.era, fixture1.headerBytes, dummyTip))
        chainSync.stageChainSync(MsgRollForward(era = fixture2.era, fixture2.headerBytes, dummyTip))

        // First fetch returns NoBlocks (chain-sync/block-fetch rollback race simulation).
        blockFetch.stageBlockFetch(MsgNoBlocks)
        // Second fetch succeeds normally.
        blockFetch.stageBlockFetch(MsgStartBatch)
        blockFetch.stageBlockFetch(MsgBlock(fixture2.era, fixture2.blockBytes))
        blockFetch.stageBlockFetch(MsgBatchDone)

        val handle = ChainApplier.spawn(conn, engine, StartFrom.Origin)

        try {
            eventually {
                assert(engine.currentTip.map(_.point).contains(p2))
            }
        } finally {
            handle.cancel().futureValue
            engine.shutdown().futureValue
        }
    }

    test("cancel() stops the loop and done completes") {
        val chainSync = new ScriptedBytes
        val blockFetch = new ScriptedBytes
        val conn = new StubConnection(chainSync, blockFetch)
        val engine = newEngine()

        chainSync.stageChainSync(MsgIntersectFound(Point.Origin, dummyTip))
        // Applier intersects then parks on the next MsgRequestNext reply.

        val handle = ChainApplier.spawn(conn, engine, StartFrom.Origin)
        // Let the loop enter the waiting state.
        Thread.sleep(100)

        handle.cancel().futureValue
        assert(handle.done.isCompleted)
        engine.shutdown().futureValue
    }

    test("initial RollBackward to non-Origin intersect (pre-forward) is ignored") {
        // Real ouroboros-network peers send `MsgRollBackward(intersectPoint, tip)` as the
        // first response to `MsgRequestNext` after a successful `MsgFindIntersect` at a
        // non-Origin point — it's a protocol-level confirmation of the resume position, not
        // a rewind of any actually-applied blocks. The applier must accept it without
        // asking the engine to roll back (which would fail with ResyncRequiredException
        // because the rollback buffer is empty).
        val chainSync = new ScriptedBytes
        val blockFetch = new ScriptedBytes
        val conn = new StubConnection(chainSync, blockFetch)
        val engine = newEngine()

        val p1 = ChainApplier.pointOf(fixture1.headerBytes, fixture1.header)
        val intersectPoint = Point.BlockPoint(p1.slot, p1.blockHash)

        chainSync.stageChainSync(MsgIntersectFound(intersectPoint, dummyTip))
        // Peer's first response after intersect: rollback to the intersect point itself.
        chainSync.stageChainSync(MsgRollBackward(intersectPoint, dummyTip))
        // Then a real forward — the applier should process this normally.
        chainSync.stageChainSync(MsgRollForward(era = fixture1.era, fixture1.headerBytes, dummyTip))

        blockFetch.stageBlockFetch(MsgStartBatch)
        blockFetch.stageBlockFetch(MsgBlock(fixture1.era, fixture1.blockBytes))
        blockFetch.stageBlockFetch(MsgBatchDone)

        val handle = ChainApplier.spawn(conn, engine, StartFrom.At(Point.toChainPoint(intersectPoint)))

        try {
            eventually {
                // If the rollback had been applied, `done` would fail with ResyncRequiredException.
                handle.done.value.foreach {
                    case scala.util.Failure(t) => fail(s"applier done failed unexpectedly: $t", t)
                    case _                     => ()
                }
                assert(engine.currentTip.map(_.point).contains(p1))
            }
        } finally {
            handle.cancel().futureValue
            engine.shutdown().futureValue
        }
    }

    test("peer MsgDone completes done normally") {
        val chainSync = new ScriptedBytes
        val blockFetch = new ScriptedBytes
        val conn = new StubConnection(chainSync, blockFetch)
        val engine = newEngine()

        chainSync.stageChainSync(MsgIntersectFound(Point.Origin, dummyTip))
        chainSync.stageChainSync(ChainSyncMessage.MsgDone)

        val handle = ChainApplier.spawn(conn, engine, StartFrom.Origin)
        handle.done.futureValue
        engine.shutdown().futureValue
    }
}
