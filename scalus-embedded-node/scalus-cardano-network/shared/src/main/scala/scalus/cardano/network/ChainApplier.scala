package scalus.cardano.network

import cps.*
import cps.monads.FutureAsyncMonad
import io.bullet.borer.{Cbor, Decoder}
import scalus.cardano.infra.{CancelSource, CancelledException}
import scalus.cardano.ledger.{
    Block,
    BlockHash,
    BlockHeader,
    BlockHeaderBody,
    KeepRaw,
    OriginalCborByteArray
}
import scalus.cardano.network.blockfetch.{BlockFetchDriver, FetchedBlock}
import scalus.cardano.network.chainsync.{
    ChainSyncDriver,
    ChainSyncEvent,
    IntersectSeeker,
    Point
}
import scalus.cardano.network.infra.MiniProtocolId
import scalus.cardano.node.stream.engine.{AppliedBlock, AppliedTransaction, Engine}
import scalus.cardano.node.stream.{ChainPoint, ChainTip, StartFrom}
import scalus.uplc.builtin.{platform, ByteString}

import scala.concurrent.{ExecutionContext, Future}

/** Composes [[ChainSyncDriver]] and [[BlockFetchDriver]] into the single event stream the
  * [[Engine]] expects: `onRollForward(AppliedBlock)` per block, `onRollBackward(ChainPoint)` per
  * rollback, propagating typed [[ChainSyncError]] on unrecoverable wire failures.
  *
  * Flow:
  *   1. `IntersectSeeker.seek` negotiates the start point.
  *   2. Loop: `chainSync.next()` yields an event.
  *       - `Forward(era, headerBytes, tip)` → decode header → hash header-body → fetch block
  *         via BlockFetch → decode block → build `AppliedBlock` → `engine.onRollForward`.
  *       - `Backward(to, tip)` → `engine.onRollBackward(to)`.
  *       - `MsgNoBlocks` from the fetch → treated as "rollback raced us", the next chain-sync
  *         event will be the rollback (per the M5 design doc); we log and continue.
  *   3. On a decode / wire-protocol failure: the loop's `Future` fails with the typed cause;
  *      the handle's `done` reflects it.
  *
  * Back-pressure: `engine.onRollForward` returns a `Future[Unit]` that completes when the
  * engine's worker has processed the block. The loop awaits that future before the next
  * `chainSync.next()` — so a slow subscriber stalls the sync loop instead of flooding memory.
  *
  * Cancellation: the applier owns an `applierScope` that's `linkedTo(connectionRoot)`. Either
  * endpoint firing (connection cancelling, or the caller invoking `handle.cancel()`) propagates
  * into the drivers' pending `receive` calls via their `cancelToken`, which unwinds the loop as
  * a failed `Future`. The handle's `done` then completes with that cause.
  *
  * See `docs/local/claude/indexer/cardano-network-chainsync.md` § *Chain applier*.
  */
private final class ChainApplier(
    conn: NodeToNodeConnection,
    engine: Engine,
    cancelToken: scalus.cardano.infra.CancelToken,
    logger: scribe.Logger
)(using ExecutionContext) {

    private val chainSync = new ChainSyncDriver(
      conn.channel(MiniProtocolId.ChainSync),
      cancelToken,
      logger
    )
    private val blockFetch = new BlockFetchDriver(
      conn.channel(MiniProtocolId.BlockFetch),
      cancelToken,
      logger
    )

    /** Drive the sync loop until a failure, a peer `MsgDone`, or the applier scope cancels.
      *
      * Every driver call is threaded through a single sequential `async[Future]` chain, which
      * satisfies the drivers' single-consumer contract. On loop exit (normal or error) sends
      * `MsgDone` / `MsgClientDone` best-effort so the peer can tear down the routes gracefully;
      * if `cancelToken` already fired these no-op internally.
      */
    def run(startFrom: StartFrom): Future[Unit] = async[Future] {
        try {
            val intersect = await(IntersectSeeker.seek(chainSync, startFrom))
            logger.info(s"chain-sync intersected at $intersect; starting loop")
            await(loop())
        } finally {
            // Best-effort tear-down; return values are Futures but the peer's `MsgDone` ack is
            // not something we block on. If `cancelToken` already fired these no-op internally
            // (the drivers' `close` recovers from a failed send).
            chainSync.close()
            blockFetch.close()
        }
    }

    private def loop(): Future[Unit] = async[Future] {
        var running = true
        while running do {
            await(chainSync.next()) match {
                case None =>
                    logger.info("peer sent MsgDone; chain-sync loop complete")
                    running = false
                case Some(ChainSyncEvent.Forward(era, headerBytes, tip)) =>
                    await(processForward(era, headerBytes, tip))
                case Some(ChainSyncEvent.Backward(to, _tip)) =>
                    val point = Point.toChainPoint(to)
                    logger.debug(s"RollBackward to $point")
                    await(engine.onRollBackward(point))
            }
        }
    }

    private def processForward(
        era: Int,
        headerBytes: ByteString,
        peerTip: scalus.cardano.network.chainsync.Tip
    ): Future[Unit] = async[Future] {
        val headerRaw = BlockEnvelope.decodeHeader(Era.fromWire(era), headerBytes) match {
            case Left(err) => throw err
            case Right(h)  => h
        }
        val chainPoint = ChainApplier.pointOf(headerBytes, headerRaw.value)
        val chainTip = ChainTip(chainPoint, headerRaw.value.blockNumber)
        val wirePoint = Point.BlockPoint(chainPoint.slot, chainPoint.blockHash)

        logger.debug(s"RollForward header @ $chainPoint (peer tip $peerTip)")

        await(blockFetch.fetchOne(wirePoint)) match {
            case Left(_) =>
                // Rollback raced our fetch — per the design doc, drop this header and continue;
                // the next ChainSync event will be the RollBackward.
                logger.info(
                  s"BlockFetch returned NoBlocks for $wirePoint; " +
                      s"assuming chain-sync/block-fetch rollback race, continuing"
                )
            case Right(FetchedBlock(blockEra, blockBytes)) =>
                val blockRaw = BlockEnvelope.decodeBlock(Era.fromWire(blockEra), blockBytes) match {
                    case Left(err) => throw err
                    case Right(b)  => b
                }
                val applied = ChainApplier.toAppliedBlock(chainTip, blockRaw)
                await(engine.onRollForward(applied))
        }
    }
}

/** Handle returned by [[ChainApplier.spawn]]. Lifecycle:
  *
  *   - [[done]]: completes normally on peer MsgDone or `cancel(cause)` with a
  *     [[CancelledException]]; completes with the typed cause otherwise.
  *   - [[cancel]]: stops the loop by firing the internal applier scope. Drivers' pending
  *     `receive`s fail via their `cancelToken` and the loop unwinds. Returns a future that
  *     mirrors [[done]] but swallows the failure — `cancel()` is a request to stop, callers
  *     don't need the cause back.
  */
final class ChainApplierHandle private[network] (
    applierScope: CancelSource,
    val done: Future[Unit]
)(using ExecutionContext) {

    def cancel(cause: Throwable = new CancelledException("applier cancel")): Future[Unit] = {
        if !applierScope.token.isCancelled then applierScope.cancel(cause)
        done.recover { case _ => () }
    }
}

object ChainApplier {

    private val defaultLogger: scribe.Logger =
        scribe.Logger("scalus.cardano.network.ChainApplier")

    /** Wire a live [[NodeToNodeConnection]] up to an [[Engine]] and start the sync loop. Returns
      * immediately with a handle; loop progress surfaces on `handle.done`.
      *
      * The applier owns an internal `CancelSource` linked to `conn.rootToken`. Callers should
      * not close the connection while the applier is running unless they intend to terminate
      * sync — use `handle.cancel()` to stop just the applier.
      */
    def spawn(
        conn: NodeToNodeConnection,
        engine: Engine,
        startFrom: StartFrom,
        logger: scribe.Logger = defaultLogger
    )(using ExecutionContext): ChainApplierHandle = {
        val applierScope = CancelSource.linkedTo(conn.rootToken)
        val applier = new ChainApplier(conn, engine, applierScope.token, logger)
        val done = applier.run(startFrom)
        new ChainApplierHandle(applierScope, done)
    }

    /** Construct the [[ChainPoint]] for a freshly-received header.
      *
      * The peer's Point-hash convention (see ouroboros-consensus's
      * `hashAnnotated bheaderBody` for Shelley+) is `Blake2b_256(original CBOR bytes of the
      * HeaderBody sub-field)`. We get that by re-decoding the header bytes through a local
      * struct that captures `KeepRaw[BlockHeaderBody]`; `KeepRaw`'s borer-derived decoder
      * slices the original-CBOR-byte-array to the inner sub-field's range.
      *
      * The returned `ChainPoint.blockHash` matches what the peer sends in `MsgRollBackward`,
      * which is what the engine's rollback buffer compares against (structural equality on
      * `ChainPoint`). A mismatch here silently broke rollback handling, so the double-decode
      * is worth the perf hit (~one extra parse per block).
      */
    private[network] def pointOf(headerBytes: ByteString, header: BlockHeader): ChainPoint = {
        val bodyRaw = headerBodyRaw(headerBytes)
        val hash = BlockHash.fromByteString(platform.blake2b_256(bodyRaw))
        ChainPoint(header.slot, hash)
    }

    private def headerBodyRaw(headerBytes: ByteString): ByteString = {
        given OriginalCborByteArray = OriginalCborByteArray(headerBytes.bytes)
        val fields = Cbor.decode(headerBytes.bytes).to[HeaderFields].value
        ByteString.unsafeFromArray(fields.headerBody.raw)
    }

    /** Internal helper for extracting the original CBOR byte range of the HeaderBody field
      * inside an encoded [[BlockHeader]] = `[HeaderBody, bodySignature]`. Used only for
      * computing the peer-compatible block hash.
      */
    private final case class HeaderFields(
        headerBody: KeepRaw[BlockHeaderBody],
        bodySignature: ByteString
    )

    private object HeaderFields {
        given (using OriginalCborByteArray): Decoder[HeaderFields] = Decoder { r =>
            val arrLen = r.readArrayHeader().toInt
            if arrLen != 2 then
                r.validationFailure(s"BlockHeader: expected array of 2, got $arrLen")
            val body = r.read[KeepRaw[BlockHeaderBody]]()
            val sig = ByteString.fromArray(r.readByteArray())
            HeaderFields(body, sig)
        }
    }

    /** Project a decoded block into the engine's [[AppliedBlock]] shape.
      *
      * `Block.transactions` needs the original CBOR bytes to reassemble each `Transaction` from
      * its KeepRaw parts, so we pass `blockRaw.raw` through the implicit
      * [[OriginalCborByteArray]].
      */
    private[network] def toAppliedBlock(tip: ChainTip, blockRaw: KeepRaw[Block]): AppliedBlock = {
        given OriginalCborByteArray = OriginalCborByteArray(blockRaw.raw)
        val txs = blockRaw.value.transactions.map { tx =>
            AppliedTransaction(
              id = tx.id,
              inputs = tx.body.value.inputs.toSet,
              outputs = tx.body.value.outputs.map(_.value).toIndexedSeq
            )
        }
        AppliedBlock(tip, txs)
    }
}
