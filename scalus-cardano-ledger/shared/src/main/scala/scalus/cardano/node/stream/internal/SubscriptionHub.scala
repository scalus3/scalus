package scalus.cardano.node.stream.internal

import scalus.cardano.infra.UnsupportedSubscriptionException
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
  * @param securityParam
  *   depth at which a block is considered settled; `noRollback` subscriptions wait this long, and
  *   it bounds how far back [[rollbackTo]] can go
  */
final class SubscriptionHub(
    val cardanoInfo: CardanoInfo,
    val securityParam: Int,
    val capabilities: StreamCapabilities
) {

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
    def require(request: SubscriptionRequest): Unit =
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

    // ------------------------------------------------------------------
    // Registration
    // ------------------------------------------------------------------

    /** Register a UTxO subscription, optionally seeding it from a snapshot.
      *
      * The seed is emitted here, under the same lock that live events take, so a subscriber cannot
      * observe a live event before the seed it belongs after.
      */
    def registerUtxo(
        id: Long,
        query: UtxoEventQuery,
        opts: SubscriptionOptions,
        mailbox: Mailbox[UtxoEvent],
        seed: scalus.cardano.ledger.Utxos
    ): Unit = {
        val seeded = synchronized {
            val sub = new UtxoSubscription(query, opts, mailbox)
            sub.lastEmitted = tip.blockNo
            utxoSubs.put(id, sub)
            if !opts.includeExistingUtxos || seed.isEmpty then Seq.empty
            else
                QueryMatching
                    .matching(query.query, seed)
                    .map(u => UtxoEvent.Created(u, u.input.transactionId, tip.point))
        }
        seeded.foreach(mailbox.offer)
    }

    def unregisterUtxo(id: Long): Unit = synchronized { utxoSubs.remove(id); () }

    def registerTransaction(
        id: Long,
        query: TransactionQuery,
        opts: SubscriptionOptions,
        mailbox: Mailbox[TransactionEvent]
    ): Unit = synchronized {
        val sub = new TxSubscription(query, opts, mailbox)
        sub.lastEmitted = tip.blockNo
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
        sub.lastEmitted = tip.blockNo
        blockSubs.put(id, sub)
        ()
    }

    def unregisterBlock(id: Long): Unit = synchronized { blockSubs.remove(id); () }

    /** Register a tip subscription and immediately deliver the current tip — a latest-value stream
      * that made you wait for the next change before telling you anything would be useless as the
      * one-shot's dual.
      */
    def registerTip(id: Long, mailbox: Mailbox[ChainTip]): Unit = {
        val current = synchronized { tipSubs.put(id, mailbox); tip }
        mailbox.offer(current)
    }

    def unregisterTip(id: Long): Unit = synchronized { tipSubs.remove(id); () }

    def registerParams(id: Long, mailbox: Mailbox[ProtocolParams]): Unit = {
        val current = synchronized { paramSubs.put(id, mailbox); params }
        mailbox.offer(current)
    }

    def unregisterParams(id: Long): Unit = synchronized { paramSubs.remove(id); () }

    def registerTxStatus(
        id: Long,
        txHash: TransactionHash,
        mailbox: Mailbox[TransactionStatus]
    ): Unit = {
        val current = synchronized {
            statusSubs.getOrElseUpdate(txHash, mutable.LinkedHashMap.empty).put(id, mailbox)
            statuses.getOrElse(txHash, TransactionStatus.NotFound)
        }
        mailbox.offer(current)
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
    def notifySubmit(txHash: TransactionHash): Unit =
        setStatus(txHash, TransactionStatus.Pending).foreach { case (m, s) => m.offer(s) }

    def updateParams(next: ProtocolParams): Unit = {
        val targets = synchronized {
            if next == params then Seq.empty
            else
                params = next
                paramSubs.values.toSeq
        }
        targets.foreach(_.offer(next))
    }

    /** Apply a block: advance the tip, then release whichever block each subscription's
      * confirmation depth now makes visible.
      */
    def applyBlock(block: AppliedBlock): Unit = {
        val actions = synchronized {
            recent.append(block)
            // Keep enough history to satisfy the deepest confirmation gate any subscription is
            // waiting on — a window sized only by securityParam would starve a subscriber that
            // asked to wait longer than the chain's own settlement depth.
            val deepestGate = (utxoSubs.values.map(s => effectiveDepth(s.opts)) ++
                txSubs.values.map(s => effectiveDepth(s.opts)) ++
                blockSubs.values.map(s => effectiveDepth(s.opts))).maxOption.getOrElse(0)
            val retain = math.max(securityParam, deepestGate) + 1
            while recent.size > retain do recent.removeHead()
            tip = ChainTip(block.point, block.blockNo)

            val tipDeliveries: Seq[() => Unit] = tipSubs.values.toSeq.map(m => () => m.offer(tip))
            val statusDeliveries: Seq[() => Unit] = block.txs.flatMap { applied =>
                setStatusLocked(applied.txHash, TransactionStatus.Confirmed).map { case (m, s) =>
                    () => m.offer(s)
                }
            }
            tipDeliveries ++ statusDeliveries ++ releaseLocked()
        }
        actions.foreach(_())
    }

    /** Roll back to `target`, which must still be within [[securityParam]] of the tip.
      *
      * Only subscriptions that actually emitted something past `target` see a `RolledBack` — a
      * subscription gated on confirmations may never have been told about the orphaned blocks at
      * all, and telling it to undo events it never received would be worse than silence.
      */
    def rollbackTo(target: ChainTip): Unit = {
        val actions = synchronized {
            val orphaned = recent.filter(_.blockNo > target.blockNo).toSeq
            recent.dropRightInPlace(orphaned.size)
            tip = target

            val statusDeliveries: Seq[() => Unit] = orphaned.flatMap(_.txs).flatMap { applied =>
                // Conservative: the transaction is no longer on chain. Whether it re-enters a
                // mempool and reappears is the provider's business, not the hub's.
                setStatusLocked(applied.txHash, TransactionStatus.NotFound).map { case (m, s) =>
                    () => m.offer(s)
                }
            }

            val utxoDeliveries: Seq[() => Unit] = utxoSubs.values.toSeq.flatMap { sub =>
                if sub.lastEmitted <= target.blockNo then Seq.empty
                else
                    sub.lastEmitted = target.blockNo
                    Seq(() => sub.mailbox.offer(UtxoEvent.RolledBack(target.point)))
            }
            val txDeliveries: Seq[() => Unit] = txSubs.values.toSeq.flatMap { sub =>
                if sub.lastEmitted <= target.blockNo then Seq.empty
                else
                    sub.lastEmitted = target.blockNo
                    Seq(() => sub.mailbox.offer(TransactionEvent.RolledBack(target.point)))
            }
            val blockDeliveries: Seq[() => Unit] = blockSubs.values.toSeq.flatMap { sub =>
                if sub.lastEmitted <= target.blockNo then Seq.empty
                else
                    sub.lastEmitted = target.blockNo
                    Seq(() => sub.mailbox.offer(BlockEvent.RolledBack(target.point)))
            }

            val tipDeliveries: Seq[() => Unit] = tipSubs.values.toSeq.map(m => () => m.offer(tip))

            tipDeliveries ++ statusDeliveries ++ utxoDeliveries ++ txDeliveries ++ blockDeliveries
        }
        actions.foreach(_())
    }

    def closeAll(): Unit = {
        val mailboxes = synchronized {
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
    private def releaseLocked(): Seq[() => Unit] = {
        def deliveries[S](
            subs: Iterable[S],
            depthOf: S => Int,
            lastOf: S => BlockNo,
            advance: (S, BlockNo) => Unit,
            emit: (S, AppliedBlock) => Seq[() => Unit]
        ): Seq[() => Unit] = subs.toSeq.flatMap { sub =>
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
          (s, b) => utxoEvents(s, b).map(e => () => s.mailbox.offer(e))
        )
        val txs = deliveries[TxSubscription](
          txSubs.values,
          s => effectiveDepth(s.opts),
          _.lastEmitted,
          (s, n) => s.lastEmitted = n,
          (s, b) => txEvents(s, b).map(e => () => s.mailbox.offer(e))
        )
        val blocks = deliveries[BlockSubscription](
          blockSubs.values,
          s => effectiveDepth(s.opts),
          _.lastEmitted,
          (s, n) => s.lastEmitted = n,
          (s, b) => blockEvents(s, b).map(e => () => s.mailbox.offer(e))
        )
        utxo ++ txs ++ blocks
    }

    private def effectiveDepth(opts: SubscriptionOptions): Int =
        if opts.noRollback then math.max(opts.confirmations, securityParam) else opts.confirmations

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
        if events.isEmpty && sub.opts.idleSignals then Seq(UtxoEvent.Idle(block.point))
        else events
    }

    private def txEvents(sub: TxSubscription, block: AppliedBlock): Seq[TransactionEvent] = {
        val events = block.txs
            .filter(QueryMatching.matchesTransaction(sub.query, _))
            .map(applied => TransactionEvent.Included(applied.tx, block.point))
        if events.isEmpty && sub.opts.idleSignals then Seq(TransactionEvent.Idle(block.point))
        else events
    }

    private def blockEvents(sub: BlockSubscription, applied: AppliedBlock): Seq[BlockEvent] = {
        val inRange = sub.query match
            case BlockQuery.All => true
            case BlockQuery.InSlotRange(from, to) =>
                applied.point.slot >= from && to.forall(applied.point.slot <= _)
        // A provider that declared it serves block subscriptions supplies the block; one that did
        // not never gets here, because `require` refused the subscription.
        if inRange then applied.block.toSeq.map(BlockEvent.Applied(_, applied.point))
        else Seq.empty
    }

    private def setStatus(
        txHash: TransactionHash,
        status: TransactionStatus
    ): Seq[(Mailbox[TransactionStatus], TransactionStatus)] =
        synchronized(setStatusLocked(txHash, status))

    private def setStatusLocked(
        txHash: TransactionHash,
        status: TransactionStatus
    ): Seq[(Mailbox[TransactionStatus], TransactionStatus)] = {
        if statuses.get(txHash).contains(status) then Seq.empty
        else
            statuses.put(txHash, status)
            statusSubs.get(txHash).toSeq.flatMap(_.values.toSeq).map(_ -> status)
    }
}
