package scalus.cardano.node.stream.internal

import scalus.cardano.address.Address
import scalus.cardano.infra.ResyncRequiredException
import scalus.cardano.ledger.{Transaction, TransactionHash, Utxos}
import scalus.cardano.node.UtxoSource
import scalus.cardano.node.stream.{BlockNo, ChainPoint, ScalusAsyncSource}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** A block as the follower identifies it. */
private[stream] case class BlockRef(point: ChainPoint, blockNo: BlockNo)

/** A transaction the follower observed, with its UTxO effects resolved.
  *
  * `spent` is the *resolved* consumed outputs — Blockfrost's `/txs/{hash}/utxos` returns the
  * address and value of each input, not just its reference, which is exactly what the hub's
  * contract needs and what a subscriber watching an address cannot reconstruct on its own.
  */
private[stream] case class ObservedTransaction(tx: Transaction, created: Utxos, spent: Utxos)

/** The slice of Blockfrost this follower needs.
  *
  * Narrow on purpose: it is the seam a fake is written against, so the polling logic — which is the
  * part with the interesting failure modes — is testable without HTTP, a clock or a key.
  */
private[stream] trait BlockfrostChainApi {

    /** `GET /blocks/latest`. Used once, to decide where "the tip" is at subscription time. */
    def latestBlock(): Future[BlockRef]

    /** `GET /blocks/{hash}/next` — the blocks that follow `hash`, in ascending order.
      *
      * `None` means Blockfrost does not have `hash` on its chain any more (a 404), which is how a
      * reorg announces itself here: the block we last reported has been orphaned. Detecting it
      * costs nothing beyond the poll we were making anyway, which is why this is the poll rather
      * than `/blocks/latest`.
      */
    def blocksAfter(block: BlockRef): Future[Option[Seq[BlockRef]]]

    /** `GET /addresses/{address}/transactions?from=&to=` — the address's transactions within a
      * block-height range, which is what makes an address subscription one request per block
      * instead of a scan.
      */
    def addressTransactionsIn(
        address: Address,
        from: BlockNo,
        to: BlockNo
    ): Future[Seq[TransactionHash]]

    /** The transaction body plus its resolved UTxO effects. */
    def transaction(hash: TransactionHash): Future[ObservedTransaction]
}

/** Polls Blockfrost and reports what it finds as [[ChainEvent]]s.
  *
  * ## Cost
  *
  * One `/blocks/{hash}/next` per poll, plus — per new block — one `/addresses/{a}/transactions` per
  * watched address and two requests per matching transaction. Cost therefore scales with *how many
  * addresses are watched*, not with how busy the chain is, which is the whole reason this polls per
  * address rather than fetching blocks.
  *
  * Two per transaction is the floor rather than the figure. A transaction whose outputs cannot be
  * classified from `/txs/{hash}/utxos` alone costs a third (see
  * `BlockfrostProvider.scriptPhaseFailed`), and one touching a reference script costs a further two
  * per script hash the client has not already cached.
  *
  * ## A transient failure is not survived
  *
  * There is no retry or backoff anywhere on this path: any non-2xx that is not a 404 throws, and
  * the failure travels straight to every subscriber. On a metered backend where a 429 is a normal
  * event that is a real gap — and so is the converse, a lagging replica answering 404 for a block
  * it has not indexed yet, which is reported here as a reorg. Both are known and deliberate for
  * now; see the M2 plan.
  *
  * ## Reorgs: detected, not reconciled
  *
  * When `/blocks/{hash}/next` 404s, the block we last reported is no longer on chain. This follower
  * does not walk back to the fork point — that is the rollback ring, and it is deliberately not in
  * the light driver. It fails instead, with [[ResyncRequiredException]], so subscribers learn their
  * view is untrustworthy rather than silently diverging from the chain. The provider declares
  * `rollbackHorizon = None`, which stays truthful: it never emits `RolledBack`.
  *
  * @param pollInterval
  *   how long to wait between polls. The main quota dial: every poll is at least one request
  *   whether or not the chain moved.
  * @param delay
  *   how to wait. Injected so tests can drive the loop without real time.
  */
private[stream] final class BlockfrostChainFollower(
    api: BlockfrostChainApi,
    pollInterval: FiniteDuration,
    delay: FiniteDuration => Future[Unit]
)(using ec: ExecutionContext)
    extends ChainFollower {

    /** Bounded for the same reason subscriber buffers are: an unbounded producer queue turns a
      * driver that cannot keep up into an OOM naming nothing, instead of a failure naming this
      * feed. Each entry holds a whole `AppliedBlock`, and the driver is the only consumer and pulls
      * in a loop, so reaching this bound at all means something is already wrong.
      */
    private val mailbox = Mailbox.delta[ChainEvent](BlockfrostChainFollower.eventBufferBound)

    // Guarded by `this`: mutated from the poll loop and from `watch` on the caller's thread.
    private var watched: Set[UtxoSource] = Set.empty
    private var last: Option[BlockRef] = None
    private var stopped = false

    /** Whether the poll loop is spending anything. Starts true so a follower nobody suspends
      * behaves exactly as before; the provider sets it from live demand.
      */
    private var observing = true
    private var started = false

    /** An anchor read is outstanding, so a second `subscribe` must not issue another. */
    private var anchoring = false

    /** The block whose set of probed sources was most recently decided.
      *
      * Two assignment sites, with different arguments for why each is safe:
      *
      *   - `emitBlock` updates it in the same critical section that reads `watched`, which is what
      *     lets `watch` report a position a caller can rely on: a block already being assembled has
      *     been recorded here, and every block assembled afterwards sees the new set.
      *   - `start` seeds it from the chain tip. That one rests not on the lock but on the backend's
      *     `/blocks/{hash}/next` contract — nothing at or below the starting tip is ever emitted,
      *     so nothing at or below it needs covering.
      *
      * Kept as a whole [[BlockRef]] rather than a point so the height is available for the
      * monotonicity guard, while `watch` returns the point — see [[ChainFollower.watch]] for why a
      * height alone cannot name a position across a fork.
      *
      * Deliberately ahead of `last`, which is the actual delivery cursor: sources are fixed before
      * any of the block's requests are issued, so a block can be recorded here while none of its
      * transactions have been fetched yet.
      */
    private var committed: Option[BlockRef] = None

    override def events: ScalusAsyncSource[ChainEvent] = mailbox

    override def watch(sources: Set[UtxoSource]): ChainPoint = synchronized {
        // A stopped follower will never assemble another block, so a position promising coverage
        // above it would be a lie the caller has no way to detect: it would register, and then
        // wait forever with no events, no Idle and no error.
        if stopped then
            throw new IllegalStateException(
              "this follower is closed and will produce no further blocks; a subscription " +
                  "registered against it could never be covered"
            )
        watched = sources
        committed.map(_.point).getOrElse(ChainPoint.origin)
    }

    override def stopWatching(sources: Set[UtxoSource]): Unit = synchronized {
        // A stopped follower assembles nothing, so there is nothing left to stop watching; see
        // ChainFollower.stopWatching for why this must not throw.
        if !stopped then watched = sources
    }

    /** Suspending clears the cursor as well as stopping the poll: on resume the next poll re-reads
      * the tip and continues from there. Keeping the old cursor would make a resumed follower walk
      * every block of the quiet interval and hand them to a subscription that asked for events from
      * *now* — a backlog nobody subscribed for, paid for one request at a time.
      */
    /** Fix the resume position, if there is not one already. See [[ChainFollower.anchor]]. */
    override def anchor(): Unit = {
        val needed = synchronized(!stopped && last.isEmpty && !anchoring)
        if needed then {
            synchronized { anchoring = true }
            api.latestBlock().onComplete {
                case Success(tip) =>
                    synchronized {
                        anchoring = false
                        // `start` may have won the race and set a cursor of its own; a later
                        // anchor must not drag the feed backwards over blocks already delivered.
                        if last.isEmpty then last = Some(tip)
                        if committed.forall(_.blockNo <= tip.blockNo) then committed = Some(tip)
                    }
                case Failure(_) =>
                    // Leave the cursor empty: the next poll re-reads the tip, which is the same
                    // answer one interval later. An anchor is an optimisation of position, not a
                    // liveness requirement, so it must not fail the feed.
                    synchronized { anchoring = false }
            }
        }
    }

    override def setObserving(active: Boolean): Unit = synchronized {
        if active then observing = true
        else if observing then {
            observing = false
            // Drop the cursor so a resumed follower re-reads the tip rather than replaying the
            // quiet interval. `anchor` runs on the *next* subscription and puts a position back,
            // so this discards a stale cursor without discarding a fresh commitment.
            last = None
        }
    }

    override def close(): Unit = {
        synchronized { stopped = true }
        mailbox.close()
    }

    /** Stopped, or pointless to continue.
      *
      * A bounded mailbox fails its consumer on overflow and then silently discards everything
      * offered afterwards, so without this check the loop keeps polling — one `/blocks/next` per
      * interval plus a request per watched address per block, against a metered daily quota — for a
      * feed nobody can ever read again.
      */
    private def isStopped: Boolean = synchronized(stopped) || mailbox.isClosed

    /** Start polling. The loop is a `Future` chain, not a thread, so it works on Scala.js too. */
    override def start(): Unit = {
        val begin = synchronized {
            if started || stopped then false
            else { started = true; true }
        }
        if begin then
            api.latestBlock().onComplete {
                case Success(tip) =>
                    synchronized {
                        // An anchor set when a subscription registered already names where that
                        // subscription must be observed from, and demand can arrive minutes later
                        // — long enough for several blocks, including the one carrying a
                        // transaction submitted in between. Seeding unconditionally here would
                        // drag the cursor forward over exactly those blocks and lose them.
                        if last.isEmpty then {
                            last = Some(tip)
                            // Nothing below the tip is ever emitted, so this is the first position
                            // a subscription could be observed from.
                            committed = Some(tip)
                        }
                    }
                    loop()
                case Failure(t) => mailbox.fail(t)
            }
    }

    private def loop(): Unit = if !isStopped then
        delay(pollInterval).flatMap(_ => if isStopped then Future.unit else poll()).onComplete {
            case Success(_) => loop()
            case Failure(t) => mailbox.fail(t)
        }
    else close()

    private def poll(): Future[Unit] = {
        val (active, from) = synchronized((observing, last))
        if !active then Future.unit
        else
            from match
                // No cursor: either the follower was suspended, or its initial read failed. Either
                // way the tip is where a subscription that exists now wants to start.
                case None =>
                    api.latestBlock().map { tip =>
                        synchronized {
                            last = Some(tip)
                            if committed.forall(_.blockNo <= tip.blockNo) then committed = Some(tip)
                        }
                    }
                case Some(ref) =>
                    api.blocksAfter(ref).flatMap {
                        case None =>
                            Future.failed(
                              ResyncRequiredException(
                                s"block ${ref.blockNo} is no longer on chain, so the chain forked " +
                                    "below the last reported block; this provider detects reorgs " +
                                    "but does not reconcile them, and the subscriber's view " +
                                    "cannot be trusted"
                              )
                            )
                        case Some(blocks) => sequentially(blocks)(emitBlock)
                    }
    }

    /** Blocks must be observed in order — the hub takes each applied block's height as the new tip
      * — so these cannot be run concurrently even though each is independent.
      */
    private def sequentially[A](items: Seq[A])(f: A => Future[Unit]): Future[Unit] =
        items.foldLeft(Future.unit)((acc, a) =>
            acc.flatMap(_ => if isStopped then Future.unit else f(a))
        )

    /** Like `Future.sequence`, but one request at a time.
      *
      * Deliberately not concurrent. Rate limiting is the *expected* steady state for a metered
      * backend, and firing a block's per-address and per-transaction requests all at once maximises
      * the chance of provoking the 429 this follower has no way to survive. Latency is the cheaper
      * thing to spend here.
      */
    private def sequentiallyCollect[A, B](items: Seq[A])(f: A => Future[B]): Future[Seq[B]] =
        items.foldLeft(Future.successful(Vector.empty[B]))((acc, a) =>
            acc.flatMap(bs => f(a).map(bs :+ _))
        )

    private def emitBlock(block: BlockRef): Future[Unit] = {
        // Only address sources are probed, so only address sources may be claimed. Declaring the
        // whole watched set would tell the hub this block is authoritative for, say, a FromAsset
        // subscription that was never looked up — which is precisely the silent event loss
        // BlockCoverage exists to prevent, committed by the thing that reports the coverage.
        val addresses = synchronized {
            // Fixing this block's source set and recording that it is fixed must be one step; a
            // `watch` landing between them would be told this height was already covered by the
            // new set when it was not.
            // Monotonic: a backend that serves a stale `/blocks/{hash}/next` page — a lagging
            // replica is the realistic case — must not be able to drag this backwards, or a
            // `watch` landing next would be handed a position implying coverage of blocks that
            // were already assembled with the old source set. This follower never rolls back (it
            // fails instead), so monotonicity here is unconditional; one that did would have to
            // lower it deliberately, as ChainFollower.watch describes.
            if committed.forall(_.blockNo <= block.blockNo) then committed = Some(block)
            watched.collect { case UtxoSource.FromAddress(a) => a }
        }
        val probed: Set[UtxoSource] = addresses.map(UtxoSource.FromAddress(_))
        val hashes = sequentiallyCollect(addresses.toSeq)(a =>
            api.addressTransactionsIn(a, block.blockNo, block.blockNo)
        )
            // One transaction can touch several watched addresses; the hub must see it once.
            .map(_.flatten.distinct)
        hashes
            .flatMap(hs => sequentiallyCollect(hs)(api.transaction))
            .map { observed =>
                val applied = observed.map(o => AppliedTransaction(o.tx, o.created, o.spent))
                mailbox.offer(
                  ChainEvent.RollForward(
                    AppliedBlock(
                      point = block.point,
                      blockNo = block.blockNo,
                      txs = applied,
                      block = None,
                      // Every height is reported, matches or not, and always with the full set of
                      // sources probed at it — both halves of AppliedBlock's contract.
                      coverage = BlockCoverage.Sources(probed)
                    )
                  )
                )
                synchronized { last = Some(block) }
            }
    }
}

private[stream] object BlockfrostChainFollower {

    /** Blocks the follower may run ahead of the driver by. See the mailbox's own note. */
    val eventBufferBound: Int = 256
}
