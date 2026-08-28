package scalus.cardano.node.stream

import scalus.cardano.ledger.*
import scalus.cardano.node.*
import scalus.cardano.node.stream.internal.{AppliedBlock, AppliedTransaction, Mailbox, SubscriptionHub}
import scalus.uplc.builtin.{ByteString, Data}

import scala.concurrent.{ExecutionContext, Future}

/** A streaming view of an [[scalus.cardano.node.EmulatorBase]].
  *
  * Every read delegates to the emulator, so the streaming and one-shot views cannot disagree —
  * there is literally one ledger behind both. Submission additionally drives the subscription hub,
  * so code that subscribes and reacts runs unchanged against a simulated chain.
  *
  * **One transaction, one block.** The emulator has no notion of a block, so this wrapper
  * synthesises one per accepted transaction and one per [[newEmptyBlock]]. Block *identity* is
  * synthetic too — derived from the block number — which is why block subscriptions are declared
  * unsupported rather than served with a fabricated header. UTxO and transaction subscriptions,
  * which is what an application actually reacts to, are fully served.
  *
  * **Events only follow acceptance.** A block reaches the hub only on `Right(hash)` from
  * `submitSync`, that is, only after the emulator's validators and mutators have passed and the new
  * state is committed. A rejected transaction produces no events.
  *
  * @param securityParam
  *   settlement depth this emulator claims. `0` — the default — matches its behaviour: a linear
  *   emulator never forks, so nothing ever needs to settle. Raise it to exercise a subscriber's
  *   confirmation gating.
  */
class StreamingEmulator(val emulator: EmulatorBase, val securityParam: Int = 0)
    extends BlockchainStreamProvider {

    private val capabilities: StreamCapabilities =
        StreamingEmulator.capabilities(securityParam)

    private val hub = new SubscriptionHub(emulator.cardanoInfo, capabilities)

    private var blockNo: BlockNo = 0L
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

    /** The tip as the stream sees it — synthetic block height, and the emulator's own slot. */
    def currentTip: ChainTip = hub.currentTip

    // ── submission ──────────────────────────────────────────────────────────

    def submit(transaction: Transaction): Future[Either[SubmitError, TransactionHash]] =
        if closed then
            Future.successful(
              Left(NetworkSubmitError.ConnectionError("provider is closed", None))
            )
        else
            // Ledger commit and block production are one step. The wrapped emulator serialises its
            // own state, but block numbering lives here: two concurrent submits that interleaved
            // between the two would mint two blocks with the same height and hash, and the hub
            // would silently drop the second one's events as already-delivered.
            val applied = synchronized {
                emulator.submitSync(transaction) match
                    case Left(error) => Left(error)
                    case Right(hash) => Right(hash -> buildBlock(Seq(transaction -> hash)))
            }
            applied match
                case Left(error) => Future.successful(Left(error))
                case Right((hash, block)) =>
                    hub.notifySubmit(hash)
                    hub.applyBlock(block)
                    Future.successful(Right(hash))

    /** Advance the tip without a transaction. */
    def newEmptyBlock(): Unit = {
        val block = synchronized {
            emulator.tick(1)
            buildBlock(Seq.empty)
        }
        hub.applyBlock(block)
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
        hub.registerParams(id, mailbox)
        summon[ScalusAsyncStreamAdapter[C]].fromSource(mailbox)
    }

    def subscribeTransactionStatus[C[_]: ScalusAsyncStreamAdapter](
        txHash: TransactionHash
    ): C[TransactionStatus] = {
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

    private def buildBlock(txs: Seq[(Transaction, TransactionHash)]): AppliedBlock = {
        blockNo += 1
        val applied = txs.map { case (tx, hash) =>
            val created: Utxos = tx.body.value.outputs.zipWithIndex.map { case (out, index) =>
                TransactionInput(hash, index) -> out.value
            }.toMap
            // The emulator resolved the consumed outputs while validating; reuse that rather than
            // re-resolving against a UTxO set the transaction has already mutated. Absence is not
            // "nothing was spent" — it means the applied-tx index has been cleared or is not
            // maintained, and defaulting to empty would silently stop emitting Spent events while
            // subscribers went on believing their UTxO set was complete.
            val spent: Utxos = emulator
                .getAppliedTx(hash)
                .map(_.spent)
                .getOrElse(
                  throw new IllegalStateException(
                    s"emulator has no applied-transaction record for ${hash.toHex}; streaming " +
                        "cannot report what it spent. Do not call clearAppliedTxs() on an emulator " +
                        "wrapped by StreamingEmulator."
                  )
                )
            AppliedTransaction(tx, created, spent)
        }
        AppliedBlock(
          ChainPoint(emulator.currentSlotSync, StreamingEmulator.syntheticBlockHash(blockNo)),
          blockNo,
          applied
        )
    }
}

object StreamingEmulator {

    /** What an emulator honestly offers: the whole ledger is in memory, so every source is indexed
      * and a scan costs nothing either; there is no history to replay; and there are no real blocks
      * to hand to a block subscription.
      *
      * `rollbackHorizon` follows `securityParam`, and `0` means `None` — a linear emulator never
      * forks, so a subscriber is entitled to assume `RolledBack` never arrives. A non-zero
      * `securityParam` is what a test uses to say "pretend blocks settle this deep", and it becomes
      * the declared horizon so the hub and the classifier agree on the depth.
      */
    def capabilities(securityParam: Int): StreamCapabilities = StreamCapabilities(
      kinds = Set(SubscriptionKind.Utxo, SubscriptionKind.Transaction),
      pushdown = PushdownKind.all,
      scanning = ScanSupport.Free,
      replay = ReplaySupport.NoReplay,
      rollbackHorizon = if securityParam > 0 then Some(securityParam) else None,
      maxConfirmations = None,
      idleSignals = true
    )

    /** Block identity derived from height. Distinct per block and stable across runs, which is what
      * a subscriber correlating or checkpointing needs; it is not a hash of anything.
      */
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
