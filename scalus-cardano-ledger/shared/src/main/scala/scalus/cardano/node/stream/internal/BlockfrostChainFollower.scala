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

    private val mailbox = Mailbox.delta[ChainEvent]()

    // Guarded by `this`: mutated from the poll loop and from `watch` on the caller's thread.
    private var watched: Set[UtxoSource] = Set.empty
    private var last: Option[BlockRef] = None
    private var stopped = false

    override def events: ScalusAsyncSource[ChainEvent] = mailbox

    override def watch(sources: Set[UtxoSource]): Unit = synchronized { watched = sources }

    override def close(): Unit = {
        synchronized { stopped = true }
        mailbox.close()
    }

    private def isStopped: Boolean = synchronized(stopped)

    /** Start polling. The loop is a `Future` chain, not a thread, so it works on Scala.js too. */
    def start(): Unit = {
        api.latestBlock().onComplete {
            case Success(tip) =>
                synchronized { last = Some(tip) }
                loop()
            case Failure(t) => mailbox.fail(t)
        }
    }

    private def loop(): Unit = if !isStopped then
        delay(pollInterval).flatMap(_ => if isStopped then Future.unit else poll()).onComplete {
            case Success(_) => loop()
            case Failure(t) => mailbox.fail(t)
        }

    private def poll(): Future[Unit] = {
        val from = synchronized(last)
        from match
            case None => Future.unit
            case Some(ref) =>
                api.blocksAfter(ref).flatMap {
                    case None =>
                        Future.failed(
                          ResyncRequiredException(
                            s"block ${ref.blockNo} is no longer on chain, so the chain forked below " +
                                "the last reported block; this provider detects reorgs but does " +
                                "not reconcile them, and the subscriber's view cannot be trusted"
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

    private def emitBlock(block: BlockRef): Future[Unit] = {
        val sources = synchronized(watched)
        val addresses = sources.collect { case UtxoSource.FromAddress(a) => a }
        val hashes = Future
            .sequence(
              addresses.toSeq.map(a => api.addressTransactionsIn(a, block.blockNo, block.blockNo))
            )
            // One transaction can touch several watched addresses; the hub must see it once.
            .map(_.flatten.distinct)
        hashes
            .flatMap(hs => Future.sequence(hs.map(api.transaction)))
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
                      coverage = BlockCoverage.Sources(sources)
                    )
                  )
                )
                synchronized { last = Some(block) }
            }
    }
}
