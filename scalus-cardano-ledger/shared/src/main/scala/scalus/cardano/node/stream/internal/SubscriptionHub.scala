package scalus.cardano.node.stream.internal

import scalus.cardano.infra.{ResyncRequiredException, UnsupportedSubscriptionException}
import scalus.cardano.ledger.{CardanoInfo, ProtocolParams, TransactionHash, Utxos}
import scalus.cardano.node.{TransactionStatus, UtxoQuery, UtxoSource}
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

        /** Registered, but its snapshot seed has not arrived yet — see [[registerUtxoDeferred]].
          * Delivery is withheld while this is set, and the watermark stays put with it, so the
          * blocks that arrive in the meantime are still due once the seed lands.
          */
        var awaitingSeed: Boolean = false

        /** A block this subscription still needed fell out of `recent` before its seed arrived, so
          * the seed can no longer be reconciled with what will be replayed — see [[seedUtxo]].
          */
        var seedGapped: Boolean = false
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

    /** How far a subscription awaiting its snapshot may hold the retention window open.
      *
      * Generous — roughly forty minutes of mainnet blocks — because the alternative to holding is
      * failing the subscription, and a snapshot read that takes longer than this is not slow, it is
      * broken. Bounded at all because an awaiting subscription that is never completed must not be
      * able to grow `recent` without limit.
      */
    private val seedRetentionBound: Int = 128

    /** Blocks not yet released to every subscription, newest last. Bounded by [[securityParam]],
      * which is also the rollback horizon: a reorg deeper than this cannot be reconciled from here.
      */
    private val recent = mutable.ArrayDeque.empty[AppliedBlock]

    def currentTip: ChainTip = synchronized(tip)

    def latestParams: ProtocolParams = synchronized(params)

    /** The status this hub is currently reporting for a transaction, if it is tracking one.
      *
      * `None` means the hub has no opinion — the transaction was never submitted through this
      * provider and never appeared in a block it applied — and the caller should fall back to
      * whatever it considers authoritative.
      */
    def statusOf(txHash: TransactionHash): Option[TransactionStatus] =
        synchronized(statuses.get(txHash))

    def nextSubscriptionId(): Long = synchronized { nextId += 1; nextId }

    /** A closed hub applies no further blocks, so a subscription registered against it would never
      * receive anything and never terminate. Called under the lock.
      *
      * The latest-value streams need this as much as the delta ones, and are easier to miss: they
      * are not routed through [[require]], and `registerTip` would happily hand back a mailbox
      * holding the last tip observed and then park the caller forever on the next pull.
      */
    private def requireOpenLocked(): Unit =
        if closed then
            throw new IllegalStateException("provider is closed; it serves no new subscriptions")

    /** Throw unless this provider can serve the request — synchronously, before anything is
      * registered, so the exception arrives at the call that caused it.
      */
    def require(request: SubscriptionRequest): Unit = {
        synchronized(requireOpenLocked())
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
            seedLocked(sub, seed)
        }
        mailbox.flush()
    }

    /** Register a UTxO subscription whose snapshot seed is not available yet.
      *
      * For a provider that reads its snapshot over the network: `subscribe` must register eagerly —
      * that is what makes `subscribe(q); submit(tx)` race-free — but it cannot wait for a `Future`
      * without blocking a thread it may not have. So registration and seeding are split, and the
      * subscription is held back until [[seedUtxo]] completes it.
      *
      * Held back rather than started empty, because the seed has to reach the subscriber *before*
      * the live events it precedes: a `Spent` delivered ahead of the `Created` it belongs after
      * would leave a subscriber folding events into a set with a UTxO it can never remove. While
      * the seed is outstanding the subscription is neither delivered to nor advanced, so those
      * blocks stay due and are released, in order, behind the seed.
      *
      * The caller **must** finish the handshake with [[seedUtxo]] or [[failUtxo]]. A subscription
      * left awaiting a seed forever receives nothing, reports nothing and never terminates.
      */
    def registerUtxoDeferred(
        id: Long,
        query: UtxoEventQuery,
        opts: SubscriptionOptions,
        mailbox: Mailbox[UtxoEvent]
    ): Unit = synchronized {
        val sub = new UtxoSubscription(query, opts, mailbox)
        sub.lastEmitted = watermark(opts)
        sub.awaitingSeed = true
        utxoSubs.put(id, sub)
        ()
    }

    /** Complete a [[registerUtxoDeferred]] handshake with the snapshot that has now arrived.
      *
      * The snapshot describes the chain as of whenever it was read, which is later than the
      * subscription's watermark — and `rewindSeed` already winds a snapshot back over the blocks
      * still due, so a snapshot that arrives late needs no special handling beyond being wound back
      * further. Blocks that fell out of the retention window in the meantime are not a loss: their
      * effects are in the snapshot, and they will not be delivered, so the subscriber's view stays
      * consistent — it simply starts a little later than it might have.
      *
      * A no-op if the subscription was cancelled or the provider closed while the read was in
      * flight, which is why the caller may fire this from a `Future` continuation without checking.
      */
    def seedUtxo(id: Long, snapshot: scalus.cardano.ledger.Utxos): Unit = {
        val (touched, gapped) = synchronized {
            utxoSubs.get(id).filter(_.awaitingSeed) match
                case None => (Seq.empty, None)
                case Some(sub) if sub.seedGapped =>
                    utxoSubs.remove(id)
                    (Seq.empty, Some(sub.mailbox))
                case Some(sub) =>
                    seedLocked(sub, snapshot)
                    sub.awaitingSeed = false
                    (sub.mailbox +: releaseLocked(), None)
        }
        gapped.foreach(
          _.fail(
            ResyncRequiredException(
              "the snapshot this subscription was to be seeded from took longer to read than the " +
                  "hub can hold history for, so there is a range of blocks neither the seed nor " +
                  "the replay can be shown to cover; subscribe again"
            )
          )
        )
        touched.foreach(_.flush())
    }

    /** Abandon a [[registerUtxoDeferred]] handshake: the snapshot could not be read.
      *
      * Fails the subscription rather than starting it unseeded. A subscriber that asked for
      * `includeExistingUtxos` and silently got a live-only stream would believe its UTxO set was
      * complete when it was empty, and no later event would tell it otherwise.
      */
    def failUtxo(id: Long, cause: Throwable): Unit =
        synchronized(utxoSubs.remove(id)).foreach(_.mailbox.fail(cause))

    def unregisterUtxo(id: Long): Unit = synchronized { utxoSubs.remove(id); () }

    /** Buffer a subscription's snapshot seed. Called under the lock; the caller flushes. */
    private def seedLocked(sub: UtxoSubscription, snapshot: scalus.cardano.ledger.Utxos): Unit = {
        val wantsCreated = sub.query.types.contains(UtxoEventType.Created)
        if sub.opts.includeExistingUtxos && wantsCreated then
            val matched = QueryMatching.matching(sub.query.query, rewindSeed(snapshot, sub))
            // The seed is the subscription's initial state, not a backlog, so it does not spend the
            // buffer allowance that exists to catch a stalled consumer. Counted here, where the
            // exact number is known, rather than estimated by the caller — which is also the only
            // way a seed fetched asynchronously can be accounted for at all.
            sub.mailbox.allowExtra(matched.size)
            matched.foreach(u =>
                sub.mailbox.offerBuffered(
                  UtxoEvent.Created(u, u.input.transactionId, ChainPoint.origin)
                )
            )
    }

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
            requireOpenLocked()
            tipSubs.put(id, mailbox)
            mailbox.offerBuffered(tip)
        }
        mailbox.flush()
    }

    def unregisterTip(id: Long): Unit = synchronized { tipSubs.remove(id); () }

    def registerParams(id: Long, mailbox: Mailbox[ProtocolParams]): Unit = {
        synchronized {
            requireOpenLocked()
            paramSubs.put(id, mailbox)
            mailbox.offerBuffered(params)
        }
        mailbox.flush()
    }

    def unregisterParams(id: Long): Unit = synchronized { paramSubs.remove(id); () }

    /** Fail the protocol-parameter subscribers, leaving the rest of the provider running.
      *
      * A provider that can no longer read parameters has subscribers holding a value that may now
      * be wrong — a stale `minFeeA` is a rejected transaction, a stale execution-unit price is a
      * wrong budget — so they must be told rather than left believing it is current. Only them,
      * though: the chain feed is a different request against the same backend, and failing every
      * UTxO subscription because an hourly parameter poll got a 429 would be collateral damage.
      */
    def failParams(cause: Throwable): Unit = {
        val mailboxes = synchronized {
            val all = paramSubs.values.toSeq
            paramSubs.clear()
            all
        }
        mailboxes.foreach(_.fail(cause))
    }

    /** Follow one transaction's status.
      *
      * **The hub only knows about transactions it is already following** — ones submitted through
      * this provider, or already subscribed to (see [[setTrackedStatusLocked]]). For anything else
      * it starts at `NotFound`, and if that transaction was confirmed in a block the hub applied
      * *before* this call, nothing later revises it. A provider declaring
      * [[SubscriptionKind.TransactionStatus]] therefore owes it either a `notifySubmit` at
      * submission time or an authoritative initial read of its own; the emulator satisfies the
      * first, which is why it can declare the kind.
      */
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

    /** Report a protocol-parameter change to subscribers.
      *
      * The provider's job, not the hub's: only the provider knows what a parameter change looks
      * like on its own source — an epoch boundary observed over chain-sync, a fresh
      * `/epochs/latest/parameters`, a re-read of local state. The hub cannot detect one, so a
      * provider that never calls this has a `subscribeProtocolParams` stream that emits the value
      * it was constructed with and nothing further. For an emulator that is exactly right: its
      * parameters are fixed at construction and only its slot advances.
      */
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
            // A subscription still awaiting its seed needs every block above its watermark kept.
            // The snapshot's own height is not knowable — it is whatever the backend served — so a
            // block dropped from here is a block neither the seed nor the replay can be shown to
            // account for. Held up to a bound; past that the subscription is failed rather than
            // quietly short-changed.
            val seedFloor = utxoSubs.values
                .filter(_.awaitingSeed)
                .map(s => block.blockNo - s.lastEmitted)
                .maxOption
                .getOrElse(0L)
                .min(seedRetentionBound.toLong)
                .max(0L)
                .toInt
            val retain =
                math.max(math.max(securityParam, deepestGate), seedFloor).min(Int.MaxValue - 1) + 1
            // Pruned by *height*, not by entry count: under partial coverage several blocks can
            // share a height (one per set of sources probed there), and counting entries would
            // then evict heights that are still inside the rollback horizon. With one entry per
            // height — every Complete-coverage provider — this is the same window as before.
            val oldest = block.blockNo - retain
            while recent.nonEmpty && recent.head.blockNo <= oldest do recent.removeHead()
            // Belt and braces. The height window is the meaningful bound, but it bounds nothing if
            // a provider breaks the one-block-per-ascending-height contract `AppliedBlock`
            // documents — several entries per height, or a height that goes backwards, would let
            // `recent` grow without limit and make `releaseLocked` scan it for every subscription
            // on every block. A cap costs one comparison and removes the failure mode.
            while recent.size > retain do recent.removeHead()
            // Whatever survived pruning, a seed-pending subscription whose next block is no longer
            // here has a hole nothing can fill afterwards.
            recent.headOption.foreach { oldest =>
                utxoSubs.values.foreach { sub =>
                    if sub.awaitingSeed && sub.lastEmitted < oldest.blockNo - 1 then
                        sub.seedGapped = true
                }
            }
            val newTip = ChainTip(block.point, block.blockNo)
            tip = newTip

            val tipTouched = tipSubs.values.toSeq.map { m =>
                m.offerBuffered(newTip); m
            }
            val statusTouched =
                block.txs.flatMap(a =>
                    setTrackedStatusLocked(a.txHash, TransactionStatus.Confirmed)
                )
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
                // mempool and reappears is the provider's business, not the hub's. Tracked-only,
                // for the same reason `applyBlock` is: an orphaned transaction nobody was following
                // must not be the way an untracked hash enters the table.
                setTrackedStatusLocked(applied.txHash, TransactionStatus.NotFound)
            }

            val utxoTouched = utxoSubs.values.toSeq.flatMap { sub =>
                if sub.lastEmitted <= target.blockNo then Seq.empty
                else if sub.awaitingSeed then
                    // Its watermark still has to come down — it is a delivery cursor as well as a
                    // rollback trigger — but it has received nothing, and `RolledBack` as a
                    // subscription's very first event would retract what it never saw. The same
                    // reasoning as `noRollback`, and the same reasoning that keeps `releaseLocked`
                    // away from it.
                    sub.lastEmitted = target.blockNo
                    Seq.empty
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

    /** Fail every subscription with `cause`, and stop accepting new ones.
      *
      * Distinct from [[closeAll]], and the distinction is the subscriber's whole world: a closed
      * stream ended, so whatever it delivered was the truth; a failed stream means the view is
      * untrustworthy and must be rebuilt. A follower that loses track of the chain owes subscribers
      * the second, never the first.
      */
    def failAll(cause: Throwable): Unit = terminateAll(_.fail(cause))

    def closeAll(): Unit = terminateAll(_.close())

    /** Shared by both terminations so that a future subscription kind cannot be remembered on one
      * path and forgotten on the other — which would leak it silently on whichever was missed.
      */
    private def terminateAll(finish: Mailbox[?] => Unit): Unit = {
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
        mailboxes.foreach(finish)
    }

    private def releaseLocked(): Seq[Mailbox[?]] = {
        def deliveries[S](
            subs: Iterable[S],
            depthOf: S => Int,
            lastOf: S => BlockNo,
            advance: (S, BlockNo) => Unit,
            emit: (S, AppliedBlock) => Seq[Mailbox[?]],
            covers: (S, AppliedBlock) => Boolean
        ): Seq[Mailbox[?]] = subs.toSeq.flatMap { sub =>
            val depth = depthOf(sub)
            // Everything at or below the visible height that this subscription has not seen yet
            // *and* that the provider actually examined on its behalf. Skipping the coverage test
            // would not merely deliver an empty block: `emit` turns "no matches" into an `Idle`,
            // and `advance` then moves the watermark past a height whose real events had not been
            // looked for yet, so they could never be delivered afterwards.
            val visibleUpTo = tip.blockNo - depth
            val due = recent
                .filter(b => b.blockNo > lastOf(sub) && b.blockNo <= visibleUpTo && covers(sub, b))
                .toSeq
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
          (s, b) => { utxoEvents(s, b).foreach(s.mailbox.offerBuffered); Seq(s.mailbox) },
          // A subscription still awaiting its snapshot is covered by nothing: delivering to it
          // would put live events ahead of the seed they follow, and advancing it would drop the
          // blocks the seed has yet to be wound back over. Both are withheld together, which is
          // what makes the wait lossless rather than merely quiet.
          (s, b) => !s.awaitingSeed && coversUtxoQuery(s.query.query, b.coverage)
        )
        val txs = deliveries[TxSubscription](
          txSubs.values,
          s => effectiveDepth(s.opts),
          _.lastEmitted,
          (s, n) => s.lastEmitted = n,
          (s, b) => { txEvents(s, b).foreach(s.mailbox.offerBuffered); Seq(s.mailbox) },
          (s, b) => coversTxQuery(s.query, b.coverage)
        )
        val blocks = deliveries[BlockSubscription](
          blockSubs.values,
          s => effectiveDepth(s.opts),
          _.lastEmitted,
          (s, n) => s.lastEmitted = n,
          (s, b) => { blockEvents(s, b).foreach(s.mailbox.offerBuffered); Seq(s.mailbox) },
          // A block subscription wants the block itself, which only a complete observation has.
          (_, b) => b.coverage == BlockCoverage.Complete
        )
        utxo ++ txs ++ blocks
    }

    /** Shared with [[SubscriptionSupport.of]] by construction: the depth that decides whether a
      * subscription is *serviceable* and the depth it is actually *gated* at are the same
      * expression, evaluated in one place.
      */
    private def effectiveDepth(opts: SubscriptionOptions): Int =
        SubscriptionSupport.effectiveDepth(opts, capabilities)

    /** The seed as it stood at this subscription's watermark, rather than at the tip.
      *
      * A snapshot describes the chain *now*, but a subscription's watermark starts at
      * `tip - depth`, so the blocks in between are still to be delivered. Seeding from the current
      * snapshot therefore double-reports them: a UTxO created in one of those blocks arrives once
      * in the seed and again when the block is released. Worse in the other direction, a UTxO
      * created before the window and spent inside it is missing from the snapshot, so the
      * subscriber is handed a `Spent` for something it never saw created.
      *
      * The hub is holding those blocks already, and `AppliedTransaction.spent` carries the
      * *resolved* outputs, so the snapshot can simply be wound back over them: drop what they
      * created, restore what they consumed. A UTxO both created and spent inside the window belongs
      * to neither — it did not exist at the watermark — which is why `created` is subtracted from
      * the restored set too.
      *
      * **Only over blocks this subscription will actually be given.** The set wound back must be
      * exactly the set that will be replayed, or the two disagree in one direction or the other:
      * winding back a block that is never delivered removes its UTxOs from the seed and nothing
      * puts them back, while not winding back a block that is delivered reports them twice. Under
      * partial coverage those are different sets — a block assembled before this subscription's
      * sources were watched does not cover it, is not due, and its effects belong in the seed — so
      * the same predicate that decides delivery decides this.
      *
      * At the default depth of zero, with a seed taken at registration, there are no pending blocks
      * and this is the snapshot unchanged.
      */
    private def rewindSeed(snapshot: scalus.cardano.ledger.Utxos, sub: UtxoSubscription): Utxos = {
        val pending = recent
            .filter(b =>
                b.blockNo > sub.lastEmitted && coversUtxoQuery(sub.query.query, b.coverage)
            )
            .toSeq
            .flatMap(_.txs)
        if pending.isEmpty then snapshot
        else
            val createdSince = pending.flatMap(_.created.keys).toSet
            val spentSince = pending.flatMap(_.spent).toMap
            (snapshot -- createdSince) ++ (spentSince -- createdSince)
    }

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

    /** Whether a block's coverage is authoritative for a UTxO subscription.
      *
      * A union needs every arm covered — the events we would otherwise miss are exactly the ones
      * the uncovered arm would have found — which also means a query spanning two sources is
      * covered only by a block that probed both. That is why a provider is expected to emit one
      * block per height carrying the union of everything it probed there, rather than one block per
      * watcher.
      */
    private def coversUtxoQuery(query: UtxoQuery, coverage: BlockCoverage): Boolean =
        // Settled by one comparison for every complete-coverage provider — which is every provider
        // but the metered one — instead of walking the query tree per subscription per block.
        coverage == BlockCoverage.Complete || (query match
            case q: UtxoQuery.Simple => BlockCoverage.covers(coverage, q.source)
            case UtxoQuery.Or(l, r, _, _, _) =>
                coversUtxoQuery(l, coverage) && coversUtxoQuery(r, coverage))

    /** Whether a block's coverage is authoritative for a transaction subscription.
      *
      * Deliberately conservative: a leaf with no [[UtxoSource]] equivalent — `MintsPolicy` has no
      * per-asset source to probe, `InvolvesScript` and `Not` have no index at all — is treated as
      * uncovered unless the whole block was observed. Under-claiming coverage costs a subscription
      * some latency; over-claiming it loses events silently.
      */
    private def coversTxQuery(query: TransactionQuery, coverage: BlockCoverage): Boolean =
        coverage match
            case BlockCoverage.Complete => true
            case BlockCoverage.Sources(probed) =>
                def covered(q: TransactionQuery): Boolean = q match
                    case TransactionQuery.InvolvesAddress(a) =>
                        probed.contains(UtxoSource.FromAddress(a))
                    case TransactionQuery.MintsAsset(p, n) =>
                        probed.contains(UtxoSource.FromAsset(p, n))
                    case TransactionQuery.SpendsInput(i) =>
                        probed.exists {
                            case UtxoSource.FromInputs(inputs) => inputs.contains(i)
                            case _                             => false
                        }
                    // An intersection can be answered from any one covered arm, with the rest
                    // post-filtering data the block already contains.
                    case TransactionQuery.AllOf(qs) => qs.exists(covered)
                    // A union is only as covered as its worst arm.
                    case TransactionQuery.AnyOf(qs) => qs.nonEmpty && qs.forall(covered)
                    case TransactionQuery.All | _: TransactionQuery.MintsPolicy |
                        _: TransactionQuery.InvolvesScript | _: TransactionQuery.Not =>
                        false
                covered(query)

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

    /** Record a status for a transaction the hub is already following, and ignore the rest.
      *
      * Most of a block's transactions are nobody's business here. Recording every one would make
      * `statuses` grow for the life of the provider with no way to shrink — invisible in an
      * emulator, whose life is one test, and unbounded in a provider that follows a busy address
      * for weeks. So an observed transaction updates a status only when something is already
      * interested in it: it was submitted through this provider, or somebody subscribed to it.
      *
      * `statusOf` answering `None` for the others is exactly right rather than a loss — it means
      * the hub has no opinion, and the caller falls back to whatever it considers authoritative.
      */
    private def setTrackedStatusLocked(
        txHash: TransactionHash,
        status: TransactionStatus
    ): Seq[Mailbox[TransactionStatus]] =
        if statuses.contains(txHash) || statusSubs.contains(txHash) then
            setStatusLocked(txHash, status)
        else Seq.empty

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
