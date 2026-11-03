package scalus.cardano.node.stream

import scalus.cardano.ledger.*
import scalus.cardano.node.*
import scalus.cardano.node.emulator.{ForgeError, RollbackError}
import scalus.cardano.node.stream.internal.{ChainEvent, Mailbox, SubscriptionHub}
import scalus.uplc.builtin.{ByteString, Data}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

/** A streaming view of an [[scalus.cardano.node.EmulatorBase]].
  *
  * Every read delegates to the emulator, so the streaming and one-shot views cannot disagree —
  * there is literally one ledger behind both. Submission additionally drives the subscription hub,
  * so code that subscribes and reacts runs unchanged against a simulated chain.
  *
  * **The real chain.** Subscribers see the emulator's own blocks, spec section 12.1: after each
  * operation of this wrapper, the `diff` from the tip it last delivered to the emulator's tip goes
  * to the hub, one `RollForward` per block and a `RollBackward` on a branch switch or a rollback,
  * spec [SY-5]. The wrapper builds no blocks, spec [SY-7]. A transition made on the wrapped
  * emulator directly reaches subscribers at the next operation of this wrapper.
  *
  * **Events only follow acceptance.** A rejected transaction changes no tip, so it produces no
  * events. A transaction in the mempool reads `Pending`, spec [SY-8].
  *
  * **Callbacks run under a lock.** `submit`, `pollForConfirmation`, `produceBlock`, `rollback` and
  * `newEmptyBlock` deliver the events while they hold this wrapper's lock, so a subscriber callback
  * that runs inline on delivery runs under it too. Such a callback may call this wrapper again on
  * its own thread, but it must not wait for another thread that calls this wrapper: that thread
  * blocks on the lock, and the two deadlock.
  *
  * @param securityParam
  *   the rollback horizon this stream advertises, spec [SY-6]: `noRollback` subscribers wait this
  *   many blocks, none with the default `0`, as before the emulator had a chain. The hub still
  *   keeps `max(securityParam, k)` blocks, so a rollback or a fork the emulator makes within its
  *   own k reaches subscribers as `RolledBack`, also when it is deeper than this value, and also
  *   `noRollback` subscribers.
  */
class StreamingEmulator(val emulator: EmulatorBase, val securityParam: Int = 0)
    extends BlockchainStreamProvider {

    /** The emulator state whose tip the hub has last been given. */
    private var synced: EmulatorState = EmulatorBase.stateOf(emulator)

    private val capabilities: StreamCapabilities = StreamingEmulator.capabilities(securityParam)

    // spec [SY-6]: split horizon. The hub keeps k blocks too, so a rollback the emulator makes
    // never throws, while `noRollback` subscribers wait only the advertised `securityParam`.
    private val hub = new SubscriptionHub(
      emulator.cardanoInfo,
      capabilities,
      math.max(securityParam.toLong, synced.config.securityParam).min(Int.MaxValue).toInt
    )

    /** Events computed but not yet given to the hub, and whether a call is giving them. */
    private val undelivered = mutable.Queue.empty[ChainEvent]
    private var delivering: Boolean = false

    /** The txs the hub was last told are in the mempool. */
    private var reportedPending: Set[TransactionHash] = Set.empty

    @volatile private var closed: Boolean = false

    def executionContext: ExecutionContext = emulator.executionContext

    def streamCapabilities: StreamCapabilities = capabilities

    // ── one-shot reads: straight through to the emulator ────────────────────

    override def cardanoInfo: CardanoInfo = emulator.cardanoInfo
    def fetchLatestParams: Future[ProtocolParams] = emulator.fetchLatestParams
    def currentSlot: Future[SlotNo] = emulator.currentSlot
    def getDatum(datumHash: DataHash): Future[Option[Data]] = emulator.getDatum(datumHash)
    def findUtxos(query: UtxoQuery): Future[Either[UtxoQueryError, Utxos]] =
        emulator.findUtxos(query)

    /** The same cell `subscribeTransactionStatus` reads, falling back to the emulator for
      * transactions this provider never observed.
      *
      * The duality between a one-shot read and its subscription is only worth anything if the two
      * cannot disagree, and they can only be guaranteed not to by reading the same state. A
      * transaction submitted here is `Pending` then `Confirmed` in the hub; one that predates this
      * wrapper is answered by the ledger.
      */
    override def checkTransaction(txHash: TransactionHash): Future[TransactionStatus] =
        hub.statusOf(txHash) match
            case Some(status) => Future.successful(status)
            case None         => emulator.checkTransaction(txHash)

    /** The tip as the stream sees it: the emulator's tip as of the last operation of this wrapper,
      * or [[ChainTip.origin]] before the first block it delivered.
      */
    def currentTip: ChainTip = hub.currentTip

    // ── submission ──────────────────────────────────────────────────────────

    def submit(transaction: Transaction): Future[Either[SubmitError, TransactionHash]] =
        if closed then
            Future.successful(
              Left(NetworkSubmitError.ConnectionError("provider is closed", None))
            )
        else
            Future.successful(driven {
                val result = emulator.submitSync(transaction)
                // Pending before the block that may confirm it at once, as before the real chain
                result.foreach(hub.notifySubmit)
                result
            })

    /** Forge while the tx is pending, see `EmulatorBase.pollForConfirmation`, so subscribers get
      * the blocks, spec [SF-3], [SY-5].
      */
    override def pollForConfirmation(
        txHash: TransactionHash,
        maxAttempts: Int,
        delayMs: Long
    ): Future[TransactionStatus] = {
        driven(emulator.forgeWhilePending(txHash, maxAttempts))
        checkTransaction(txHash)
    }

    /** Forge a block from the mempool, see `EmulatorBase.produceBlock()`. */
    def produceBlock(): ChainPoint = driven(emulator.produceBlock())

    /** Forge a block of `txs` on `on`, see `EmulatorBase.produceBlock`. */
    def produceBlock(on: ChainPoint, txs: Seq[Transaction]): Either[ForgeError, ChainPoint] =
        driven(emulator.produceBlock(on, txs))

    /** Forge a block of `txs` at `slot` on `on`, see `EmulatorBase.produceBlock`. */
    def produceBlock(
        on: ChainPoint,
        slot: SlotNo,
        txs: Seq[Transaction]
    ): Either[ForgeError, ChainPoint] =
        driven(emulator.produceBlock(on, slot, txs))

    /** Remove the last `n` blocks, see `EmulatorBase.rollback`. */
    def rollback(n: Int): Either[RollbackError, ChainPoint] = driven(emulator.rollback(n))

    /** Advance the slot by one, then forge `produceBlock()` of the mempool, empty unless the
      * emulator is in the `Manual` preset, spec [FG-8].
      */
    def newEmptyBlock(): Unit = driven {
        emulator.tick(1)
        emulator.produceBlock()
        ()
    }

    // ── subscriptions ───────────────────────────────────────────────────────

    def subscribeUtxoQuery[C[_]: ScalusAsyncStreamAdapter](
        query: UtxoEventQuery,
        opts: SubscriptionOptions
    ): C[UtxoEvent] = {
        hub.require(SubscriptionRequest.Utxo(query, opts))
        val id = hub.nextSubscriptionId()
        val seed = emulator.utxos
        // `seed.size` over-counts (the hub enqueues only the UTxOs the query matches) and that is
        // deliberate: it is an upper bound, computed without running the match twice.
        val headroom = if opts.includeExistingUtxos then seed.size else 0
        val mailbox =
            Mailbox.delta[UtxoEvent](bufferSize(opts, headroom), () => hub.unregisterUtxo(id))
        hub.registerUtxo(id, query, opts, mailbox, seed)
        summon[ScalusAsyncStreamAdapter[C]].fromSource(mailbox)
    }

    def subscribeTransactionQuery[C[_]: ScalusAsyncStreamAdapter](
        query: TransactionQuery,
        opts: SubscriptionOptions
    ): C[TransactionEvent] = {
        hub.require(SubscriptionRequest.Transaction(query, opts))
        val id = hub.nextSubscriptionId()
        val mailbox =
            Mailbox.delta[TransactionEvent](bufferSize(opts), () => hub.unregisterTransaction(id))
        hub.registerTransaction(id, query, opts, mailbox)
        summon[ScalusAsyncStreamAdapter[C]].fromSource(mailbox)
    }

    def subscribeBlockQuery[C[_]: ScalusAsyncStreamAdapter](
        query: BlockQuery,
        opts: SubscriptionOptions
    ): C[BlockEvent] = {
        hub.require(SubscriptionRequest.Block(query, opts))
        val id = hub.nextSubscriptionId()
        val mailbox = Mailbox.delta[BlockEvent](bufferSize(opts), () => hub.unregisterBlock(id))
        hub.registerBlock(id, query, opts, mailbox)
        summon[ScalusAsyncStreamAdapter[C]].fromSource(mailbox)
    }

    def subscribeTip[C[_]: ScalusAsyncStreamAdapter](): C[ChainTip] = {
        val id = hub.nextSubscriptionId()
        val mailbox = Mailbox.latestValue[ChainTip](() => hub.unregisterTip(id))
        hub.registerTip(id, mailbox)
        summon[ScalusAsyncStreamAdapter[C]].fromSource(mailbox)
    }

    def subscribeProtocolParams[C[_]: ScalusAsyncStreamAdapter](): C[ProtocolParams] = {
        val id = hub.nextSubscriptionId()
        val mailbox = Mailbox.latestValue[ProtocolParams](() => hub.unregisterParams(id))
        // spec [ES-15], [SY-5]: sync first, so the seed is the emulator's pparams even after an
        // enactment made on the wrapped emulator, as `fetchLatestParams` reads them
        driven(())
        hub.registerParams(id, mailbox)
        summon[ScalusAsyncStreamAdapter[C]].fromSource(mailbox)
    }

    def subscribeTransactionStatus[C[_]: ScalusAsyncStreamAdapter](
        txHash: TransactionHash
    ): C[TransactionStatus] = {
        hub.require(SubscriptionRequest.TransactionStatus(txHash))
        val id = hub.nextSubscriptionId()
        val mailbox =
            Mailbox.latestValue[TransactionStatus](() => hub.unregisterTxStatus(txHash, id))
        hub.registerTxStatus(id, txHash, mailbox)
        summon[ScalusAsyncStreamAdapter[C]].fromSource(mailbox)
    }

    /** Terminal: subscriptions end, and the provider serves no new ones and submits nothing
      * further. A `close` that left the provider working would leak the very subscriptions it was
      * called to release.
      */
    def close(): Future[Unit] = {
        closed = true
        hub.closeAll()
        Future.unit
    }

    // ── internals ───────────────────────────────────────────────────────────

    /** Mailbox capacity for a delta subscription.
      *
      * `seedHeadroom` is the number of events the snapshot seed may enqueue before any live event
      * arrives (see `SubscriptionHub.registerUtxo`, which buffers one `Created` per matching UTxO).
      * It is added to the bound rather than counted against it: the seed is the subscription's
      * initial state, not evidence of a consumer falling behind, and a wallet with more UTxOs than
      * the bound must not be refused a subscription it can perfectly well keep up with.
      */
    private def bufferSize(opts: SubscriptionOptions, seedHeadroom: Int = 0): Int =
        opts.bufferPolicy match
            case DeltaBufferPolicy.Bounded(n) =>
                if seedHeadroom > Int.MaxValue - n then Int.MaxValue else n + seedHeadroom
            case DeltaBufferPolicy.Unbounded => Int.MaxValue

    /** Run `op` on the emulator, then give the hub the events of its tip change, spec [SY-5].
      *
      * One lock holds the emulator change and the delivery, so two calls cannot give the hub their
      * events out of order. A subscriber's continuation can run inline on delivery and call this
      * wrapper again on the same thread: that call queues its events behind the ones being
      * delivered, so the hub still sees the heights in order (`AppliedBlock` requires it). The hub
      * never calls back into this wrapper, so the lock order is always wrapper, then hub.
      *
      * The deadlock hazard this leaves for callbacks is in the class scaladoc.
      */
    private def driven[A](op: => A): A = synchronized {
        val result = op
        val now = EmulatorBase.stateOf(emulator)
        undelivered ++= EmulatorState.diff(synced, synced.tip, now, now.tip)
        synced = now
        if !delivering then
            delivering = true
            try
                while undelivered.nonEmpty do
                    undelivered.dequeue() match
                        case ChainEvent.RollForward(block) => hub.applyBlock(block)
                        case ChainEvent.RollBackward(to)   => hub.rollbackTo(to)
                // spec [ES-15], [EB-25]: a boundary may enact new pparams, and a rollback of the
                // blocks that ratified them restores the old; the hub emits only a changed value
                hub.updateParams(synced.context.env.params)
                // the hub marks the txs of a rolled-back block NotFound; those back in the
                // mempool are Pending, spec [SY-8], [OR-3]; those that left it without a block
                // are NotFound, as `EmulatorBase.checkTransaction` reports them
                val pending = synced.mempool.map(_.applied.txHash).toSet
                (reportedPending -- pending)
                    .filter(hub.statusOf(_).contains(TransactionStatus.Pending))
                    .foreach(hub.notifyDropped)
                pending.foreach(hub.notifySubmit)
                reportedPending = pending
            finally delivering = false
        result
    }
}

object StreamingEmulator {

    /** What an emulator honestly offers: the whole ledger is in memory, so every source is indexed
      * and a scan costs nothing either; there is no history to replay; and its blocks are real, so
      * a block subscription gets them.
      *
      * `rollbackHorizon` is `Some(securityParam)`, also for `0`: the emulator rolls back on
      * `rollback(n)` and on a fork switch, so `None`, "never signals rollbacks", would not be true.
      * `Some(0)` lets a `noRollback` subscriber wait for nothing. A [[StreamingEmulator]] passes
      * its own `securityParam` argument, spec [SY-6].
      */
    def capabilities(securityParam: Int): StreamCapabilities = StreamCapabilities(
      kinds = Set(
        SubscriptionKind.Utxo,
        SubscriptionKind.Transaction,
        SubscriptionKind.TransactionStatus,
        SubscriptionKind.Block
      ),
      pushdown = PushdownKind.all,
      scanning = ScanSupport.Free,
      replay = ReplaySupport.NoReplay,
      rollbackHorizon = Some(securityParam),
      maxConfirmations = None,
      idleSignals = true
    )

    /** Block identity derived from height. Distinct per block and stable across runs, which is what
      * a subscriber correlating or checkpointing needs; it is not a hash of anything.
      */
    @deprecated(
      "the stream delivers the emulator's real blocks, named by their header hash",
      "1.4.1"
    )
    def syntheticBlockHash(blockNo: BlockNo): BlockHash = {
        val bytes = new Array[Byte](32)
        var i = 0
        var n = blockNo
        while i < 8 do
            bytes(31 - i) = (n & 0xff).toByte
            n >>>= 8
            i += 1
        BlockHash.fromByteString(ByteString.fromArray(bytes))
    }
}
