package scalus.cardano.node.stream.internal

import scalus.cardano.infra.{ResyncRequiredException, UnsupportedSubscriptionException}
import scalus.cardano.ledger.{CardanoInfo, ProtocolParams, TransactionHash}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.node.stream.*

import scala.collection.mutable

/** Subscription registry and event fan-out, shared by every provider.
  *
  * This is the part of a streaming implementation that is the same whatever the chain data comes
  * from: keeping track of who is subscribed to what, deciding which events reach whom, gating on
  * confirmation depth, and turning a block into per-subscription deltas. What differs per backend —
  * polling REST, following a gRPC stream, driving an emulator, replaying a chain store — sits
  * *above* this, and hands it [[AppliedBlock]]s.
  *
  * ## No worker thread
  *
  * State is guarded by `this` and every operation runs on the caller's thread. That is not a
  * simplification of the threaded design, it is a different trade: registration and fan-out are
  * ordered by the caller's own program order, which makes `subscribe(q); submit(tx)` race-free
  * without any happens-before argument about a queue. It also compiles to Scala.js, where there is
  * no thread to hand work to.
  *
  * Events are handed to mailboxes *outside* the monitor. A consumer's continuation can run inline
  * on `offer`, and that continuation is allowed to cancel its subscription, which re-enters this
  * hub — holding the lock across `offer` would deadlock on exactly that path.
  *
  * ## Delivery happens outside the monitor
  *
  * Events are *buffered* into mailboxes under the lock and *delivered* after releasing it. Both
  * halves matter. Buffering under the lock is what makes a registration plus its seed one
  * indivisible step, so a subscriber can never see a live `Spent` before the `Created` it belongs
  * after. Delivering outside it is what keeps a consumer's continuation — which may cancel, and so
  * re-enter this hub — from running while the monitor is held.
  */
final class SubscriptionHub(val cardanoInfo: CardanoInfo, val capabilities: StreamCapabilities) {

    /** Depth at which a block counts as settled. Taken from the provider's own declaration rather
      * than a separate constructor parameter, so the classifier deciding whether a subscription is
      * serviceable and the hub deciding when to release it cannot disagree.
      */
    val securityParam: Int = capabilities.rollbackHorizon.getOrElse(0)

    private final class UtxoSubscription(
        val query: UtxoEventQuery,
        val opts: SubscriptionOptions,
        val mailbox: Mailbox[UtxoEvent]
    ) {
        var lastEmitted: BlockNo = 0L
    }

    private final class TxSubscription(
        val query: TransactionQuery,
        val opts: SubscriptionOptions,
        val mailbox: Mailbox[TransactionEvent]
    ) {
        var lastEmitted: BlockNo = 0L
    }

    private final class BlockSubscription(
        val query: BlockQuery,
        val opts: SubscriptionOptions,
        val mailbox: Mailbox[BlockEvent]
    ) {
        var lastEmitted: BlockNo = 0L
    }

    private var nextId: Long = 0L
    private var closed: Boolean = false
    private var tip: ChainTip = ChainTip.origin
    private var params: ProtocolParams = cardanoInfo.protocolParams

    private val utxoSubs = mutable.LinkedHashMap.empty[Long, UtxoSubscription]
    private val txSubs = mutable.LinkedHashMap.empty[Long, TxSubscription]
    private val blockSubs = mutable.LinkedHashMap.empty[Long, BlockSubscription]
    private val tipSubs = mutable.LinkedHashMap.empty[Long, Mailbox[ChainTip]]
    private val paramSubs = mutable.LinkedHashMap.empty[Long, Mailbox[ProtocolParams]]
    private val statusSubs =
        mutable.LinkedHashMap.empty[TransactionHash, mutable.LinkedHashMap[Long, Mailbox[
          TransactionStatus
        ]]]
    private val statuses = mutable.Map.empty[TransactionHash, TransactionStatus]

    /** Blocks not yet released to every subscription, newest last. Bounded by [[securityParam]],
      * which is also the rollback horizon: a reorg deeper than this cannot be reconciled from here.
      */
    private val recent = mutable.ArrayDeque.empty[AppliedBlock]

    def currentTip: ChainTip = synchronized(tip)

    def latestParams: ProtocolParams = synchronized(params)

    def nextSubscriptionId(): Long = synchronized { nextId += 1; nextId }

    /** Throw unless this provider can serve the request — synchronously, before anything is
      * registered, so the exception arrives at the call that caused it.
      */
    def require(request: SubscriptionRequest): Unit = {
        if synchronized(closed) then
            throw new IllegalStateException("provider is closed; it serves no new subscriptions")
        // The hub delivers live events only. A provider whose capabilities advertise replay must
        // seed the subscription itself before registering it; silently downgrading a replay request
        // to a live-only stream would let a subscriber resume from a checkpoint and never learn it
        // had skipped everything in between.
        if request.options.startFrom != StartFrom.Tip then
            throw UnsupportedSubscriptionException(
              s"this provider serves live subscriptions only; ${request.options.startFrom} needs " +
                  "replay, which it does not implement"
            )
        SubscriptionSupport.of(request, capabilities) match
            case SubscriptionSupport.Indexed => ()
            case SubscriptionSupport.Unindexed =>
                if !request.options.allowUnindexedScan then
                    throw UnsupportedSubscriptionException(
                      "this subscription cannot be served from an index and would require scanning " +
                          "every block; set SubscriptionOptions.allowUnindexedScan = true to accept " +
                          "the cost"
                    )
            case SubscriptionSupport.Unsupported(reason) =>
                throw UnsupportedSubscriptionException(reason)
    }

    // ------------------------------------------------------------------
    // Registration
    // ------------------------------------------------------------------

    /** Register a UTxO subscription, optionally seeding it from a snapshot.
      *
      * The seed is buffered under the same lock live events take, so a subscriber cannot observe a
      * live event before the seed it belongs after; it is delivered once the lock is released.
      *
      * Seeded events carry [[ChainPoint.origin]] rather than the current tip. They describe UTxOs
      * produced in blocks the subscription never saw, so stamping them with the tip would make a
      * later rollback past that tip instruct the subscriber to discard state that is still on
      * chain. `origin` says what is true: this came from a snapshot, not from a block you observed.
      */
    def registerUtxo(
        id: Long,
        query: UtxoEventQuery,
        opts: SubscriptionOptions,
        mailbox: Mailbox[UtxoEvent],
        seed: scalus.cardano.ledger.Utxos
    ): Unit = {
        synchronized {
            val sub = new UtxoSubscription(query, opts, mailbox)
            sub.lastEmitted = watermark(opts)
            utxoSubs.put(id, sub)
            val wantsCreated = query.types.contains(UtxoEventType.Created)
            if opts.includeExistingUtxos && wantsCreated then
                QueryMatching
                    .matching(query.query, seed)
                    .foreach(u =>
                        mailbox.offerBuffered(
                          UtxoEvent.Created(u, u.input.transactionId, ChainPoint.origin)
                        )
                    )
        }
        mailbox.flush()
    }

    def unregisterUtxo(id: Long): Unit = synchronized { utxoSubs.remove(id); () }

    def registerTransaction(
        id: Long,
        query: TransactionQuery,
        opts: SubscriptionOptions,
        mailbox: Mailbox[TransactionEvent]
    ): Unit = synchronized {
        val sub = new TxSubscription(query, opts, mailbox)
        sub.lastEmitted = watermark(opts)
        txSubs.put(id, sub)
        ()
    }

    def unregisterTransaction(id: Long): Unit = synchronized { txSubs.remove(id); () }

    def registerBlock(
        id: Long,
        query: BlockQuery,
        opts: SubscriptionOptions,
        mailbox: Mailbox[BlockEvent]
    ): Unit = synchronized {
        val sub = new BlockSubscription(query, opts, mailbox)
        sub.lastEmitted = watermark(opts)
        blockSubs.put(id, sub)
        ()
    }

    def unregisterBlock(id: Long): Unit = synchronized { blockSubs.remove(id); () }

    /** Register a tip subscription and immediately deliver the current tip — a latest-value stream
      * that made you wait for the next change before telling you anything would be useless as the
      * one-shot's dual.
      */
    def registerTip(id: Long, mailbox: Mailbox[ChainTip]): Unit = {
        synchronized {
            tipSubs.put(id, mailbox)
            mailbox.offerBuffered(tip)
        }
        mailbox.flush()
    }

    def unregisterTip(id: Long): Unit = synchronized { tipSubs.remove(id); () }

    def registerParams(id: Long, mailbox: Mailbox[ProtocolParams]): Unit = {
        synchronized {
            paramSubs.put(id, mailbox)
            mailbox.offerBuffered(params)
        }
        mailbox.flush()
    }

    def unregisterParams(id: Long): Unit = synchronized { paramSubs.remove(id); () }

    def registerTxStatus(
        id: Long,
        txHash: TransactionHash,
        mailbox: Mailbox[TransactionStatus]
    ): Unit = {
        synchronized {
            statusSubs.getOrElseUpdate(txHash, mutable.LinkedHashMap.empty).put(id, mailbox)
            mailbox.offerBuffered(statuses.getOrElse(txHash, TransactionStatus.NotFound))
        }
        mailbox.flush()
    }

    def unregisterTxStatus(txHash: TransactionHash, id: Long): Unit = synchronized {
        statusSubs.get(txHash).foreach { subs =>
            subs.remove(id)
            if subs.isEmpty then statusSubs.remove(txHash)
        }
    }

    // ------------------------------------------------------------------
    // Chain events
    // ------------------------------------------------------------------

    /** A transaction entered the mempool. */
    def notifySubmit(txHash: TransactionHash): Unit = {
        val touched = synchronized(setStatusLocked(txHash, TransactionStatus.Pending))
        touched.foreach(_.flush())
    }

    def updateParams(next: ProtocolParams): Unit = {
        val touched = synchronized {
            if next == params then Seq.empty
            else
                params = next
                paramSubs.values.toSeq.map { m =>
                    m.offerBuffered(next); m
                }
        }
        touched.foreach(_.flush())
    }

    /** Apply a block: advance the tip, then release whichever block each subscription's
      * confirmation depth now makes visible.
      */
    def applyBlock(block: AppliedBlock): Unit = {
        val touched = synchronized {
            recent.append(block)
            // Keep enough history to satisfy the deepest confirmation gate any subscription is
            // waiting on — a window sized only by securityParam would starve a subscriber that
            // asked to wait longer than the chain's own settlement depth.
            val deepestGate = (utxoSubs.values.map(s => effectiveDepth(s.opts)) ++
                txSubs.values.map(s => effectiveDepth(s.opts)) ++
                blockSubs.values.map(s => effectiveDepth(s.opts))).maxOption.getOrElse(0)
            // `+ 1` on a saturating max, so an absurd confirmation depth cannot wrap the window
            // negative and empty the deque on the next prune.
            val retain = math.max(securityParam, deepestGate).min(Int.MaxValue - 1) + 1
            while recent.size > retain do recent.removeHead()
            val newTip = ChainTip(block.point, block.blockNo)
            tip = newTip

            val tipTouched = tipSubs.values.toSeq.map { m =>
                m.offerBuffered(newTip); m
            }
            val statusTouched =
                block.txs.flatMap(a => setStatusLocked(a.txHash, TransactionStatus.Confirmed))
            tipTouched ++ statusTouched ++ releaseLocked()
        }
        touched.foreach(_.flush())
    }

    /** Roll back to `target`, which must still be within [[securityParam]] of the tip.
      *
      * Only subscriptions that actually emitted something past `target` see a `RolledBack` — a
      * subscription gated on confirmations may never have been told about the orphaned blocks at
      * all, and telling it to undo events it never received would be worse than silence.
      */
    def rollbackTo(target: ChainTip): Unit = {
        val touched = synchronized {
            if capabilities.rollbackHorizon.isEmpty then
                throw new IllegalStateException(
                  "this provider declares rollbackHorizon = None, so subscribers are entitled to " +
                      "assume RolledBack never arrives; it must not roll back"
                )
            if target.blockNo < tip.blockNo - securityParam then
                throw ResyncRequiredException(
                  s"rollback to block ${target.blockNo} is deeper than the $securityParam-block " +
                      s"horizon at tip ${tip.blockNo}; the events needed to reconcile subscribers " +
                      "are no longer available"
                )
            val orphaned = recent.filter(_.blockNo > target.blockNo).toSeq
            recent.dropRightInPlace(orphaned.size)
            tip = target

            val statusTouched = orphaned.flatMap(_.txs).flatMap { applied =>
                // Conservative: the transaction is no longer on chain. Whether it re-enters a
                // mempool and reappears is the provider's business, not the hub's.
                setStatusLocked(applied.txHash, TransactionStatus.NotFound)
            }

            val utxoTouched = utxoSubs.values.toSeq.flatMap { sub =>
                if sub.lastEmitted <= target.blockNo then Seq.empty
                else
                    sub.lastEmitted = target.blockNo
                    sub.mailbox.offerBuffered(UtxoEvent.RolledBack(target.point))
                    Seq(sub.mailbox)
            }
            val txTouched = txSubs.values.toSeq.flatMap { sub =>
                if sub.lastEmitted <= target.blockNo then Seq.empty
                else
                    sub.lastEmitted = target.blockNo
                    sub.mailbox.offerBuffered(TransactionEvent.RolledBack(target.point))
                    Seq(sub.mailbox)
            }
            val blockTouched = blockSubs.values.toSeq.flatMap { sub =>
                if sub.lastEmitted <= target.blockNo then Seq.empty
                else
                    sub.lastEmitted = target.blockNo
                    sub.mailbox.offerBuffered(BlockEvent.RolledBack(target.point))
                    Seq(sub.mailbox)
            }
            val tipTouched = tipSubs.values.toSeq.map { m =>
                m.offerBuffered(target); m
            }

            tipTouched ++ statusTouched ++ utxoTouched ++ txTouched ++ blockTouched
        }
        touched.foreach(_.flush())
    }

    def closeAll(): Unit = {
        val mailboxes = synchronized {
            closed = true
            val all: Seq[Mailbox[?]] =
                utxoSubs.values.toSeq.map(_.mailbox) ++
                    txSubs.values.toSeq.map(_.mailbox) ++
                    blockSubs.values.toSeq.map(_.mailbox) ++
                    tipSubs.values.toSeq ++
                    paramSubs.values.toSeq ++
                    statusSubs.values.flatMap(_.values).toSeq
            utxoSubs.clear()
            txSubs.clear()
            blockSubs.clear()
            tipSubs.clear()
            paramSubs.clear()
            statusSubs.clear()
            all
        }
        mailboxes.foreach(_.close())
    }

    // ------------------------------------------------------------------
    // Internals — all called under the monitor
    // ------------------------------------------------------------------

    /** Release, per subscription, every block that has now reached its confirmation depth. */
    private def releaseLocked(): Seq[Mailbox[?]] = {
        def deliveries[S](
            subs: Iterable[S],
            depthOf: S => Int,
            lastOf: S => BlockNo,
            advance: (S, BlockNo) => Unit,
            emit: (S, AppliedBlock) => Seq[Mailbox[?]]
        ): Seq[Mailbox[?]] = subs.toSeq.flatMap { sub =>
            val depth = depthOf(sub)
            // Everything at or below the visible height that this subscription has not seen yet.
            val visibleUpTo = tip.blockNo - depth
            val due = recent.filter(b => b.blockNo > lastOf(sub) && b.blockNo <= visibleUpTo).toSeq
            if due.isEmpty then Seq.empty
            else
                advance(sub, due.last.blockNo)
                due.flatMap(emit(sub, _))
        }

        val utxo = deliveries[UtxoSubscription](
          utxoSubs.values,
          s => effectiveDepth(s.opts),
          _.lastEmitted,
          (s, n) => s.lastEmitted = n,
          (s, b) => { utxoEvents(s, b).foreach(s.mailbox.offerBuffered); Seq(s.mailbox) }
        )
        val txs = deliveries[TxSubscription](
          txSubs.values,
          s => effectiveDepth(s.opts),
          _.lastEmitted,
          (s, n) => s.lastEmitted = n,
          (s, b) => { txEvents(s, b).foreach(s.mailbox.offerBuffered); Seq(s.mailbox) }
        )
        val blocks = deliveries[BlockSubscription](
          blockSubs.values,
          s => effectiveDepth(s.opts),
          _.lastEmitted,
          (s, n) => s.lastEmitted = n,
          (s, b) => { blockEvents(s, b).foreach(s.mailbox.offerBuffered); Seq(s.mailbox) }
        )
        utxo ++ txs ++ blocks
    }

    /** Shared with [[SubscriptionSupport.of]] by construction: the depth that decides whether a
      * subscription is *serviceable* and the depth it is actually *gated* at are the same
      * expression, evaluated in one place.
      */
    private def effectiveDepth(opts: SubscriptionOptions): Int =
        SubscriptionSupport.effectiveDepth(opts, capabilities)

    /** Where a new subscription's delivery watermark starts.
      *
      * Not `tip.blockNo`: a subscription gated on confirmations has been delivered nothing for the
      * last `depth` blocks, and marking those as already-emitted would make a later rollback into
      * that range offer it a `RolledBack` retracting events it never received — which is exactly
      * the guarantee `noRollback` sells.
      */
    private def watermark(opts: SubscriptionOptions): BlockNo =
        math.max(0L, tip.blockNo - effectiveDepth(opts))

    /** Idle signals need both sides to agree: the provider must be able to produce them and the
      * subscriber must want them. Checking only the option would let the hub emit a signal its own
      * provider declared it does not produce.
      */
    private def wantsIdle(opts: SubscriptionOptions): Boolean =
        capabilities.idleSignals && opts.idleSignals

    private def utxoEvents(sub: UtxoSubscription, block: AppliedBlock): Seq[UtxoEvent] = {
        val wantCreated = sub.query.types.contains(UtxoEventType.Created)
        val wantSpent = sub.query.types.contains(UtxoEventType.Spent)
        val events = block.txs.flatMap { applied =>
            val created =
                if !wantCreated then Seq.empty
                else
                    QueryMatching
                        .matching(sub.query.query, applied.created)
                        .map(UtxoEvent.Created(_, applied.txHash, block.point))
            val spent =
                if !wantSpent then Seq.empty
                else
                    QueryMatching
                        .matching(sub.query.query, applied.spent)
                        .map(UtxoEvent.Spent(_, applied.txHash, block.point))
            // Spends first: within one block a UTxO can be consumed and an identical-looking one
            // produced, and a subscriber folding these into a set needs the removal to land first.
            spent ++ created
        }
        if events.isEmpty && wantsIdle(sub.opts) then Seq(UtxoEvent.Idle(block.point))
        else events
    }

    private def txEvents(sub: TxSubscription, block: AppliedBlock): Seq[TransactionEvent] = {
        val events = block.txs
            .filter(QueryMatching.matchesTransaction(sub.query, _))
            .map(applied => TransactionEvent.Included(applied.tx, block.point))
        if events.isEmpty && wantsIdle(sub.opts) then Seq(TransactionEvent.Idle(block.point))
        else events
    }

    private def blockEvents(sub: BlockSubscription, applied: AppliedBlock): Seq[BlockEvent] = {
        val (inRange, past) = sub.query match
            case BlockQuery.All => (true, false)
            case BlockQuery.InSlotRange(from, to) =>
                (
                  applied.point.slot >= from && to.forall(applied.point.slot <= _),
                  to.exists(applied.point.slot > _)
                )
        // A bounded range ends the stream once the chain passes it, as BlockQuery.InSlotRange
        // promises. Leaving it open would park the subscriber forever on a range that can never
        // produce another event, while the registration kept inflating the retention window.
        if past then sub.mailbox.close()
        // A provider that declared it serves block subscriptions supplies the block; one that did
        // not never gets here, because `require` refused the subscription.
        if inRange then applied.block.toSeq.map(BlockEvent.Applied(_, applied.point))
        else Seq.empty
    }

    private def setStatusLocked(
        txHash: TransactionHash,
        status: TransactionStatus
    ): Seq[Mailbox[TransactionStatus]] = {
        if statuses.get(txHash).contains(status) then Seq.empty
        else
            statuses.put(txHash, status)
            statusSubs.get(txHash).toSeq.flatMap(_.values.toSeq).map { m =>
                m.offerBuffered(status)
                m
            }
    }
}
