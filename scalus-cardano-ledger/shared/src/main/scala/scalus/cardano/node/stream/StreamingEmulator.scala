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

    private val hub = new SubscriptionHub(
      emulator.cardanoInfo,
      securityParam,
      StreamingEmulator.capabilities
    )

    private var blockNo: BlockNo = 0L

    def executionContext: ExecutionContext = emulator.executionContext

    def streamCapabilities: StreamCapabilities = StreamingEmulator.capabilities

    // ── one-shot reads: straight through to the emulator ────────────────────

    override def cardanoInfo: CardanoInfo = emulator.cardanoInfo
    def fetchLatestParams: Future[ProtocolParams] = emulator.fetchLatestParams
    def currentSlot: Future[SlotNo] = emulator.currentSlot
    def getDatum(datumHash: DataHash): Future[Option[Data]] = emulator.getDatum(datumHash)
    def findUtxos(query: UtxoQuery): Future[Either[UtxoQueryError, Utxos]] =
        emulator.findUtxos(query)
    override def checkTransaction(txHash: TransactionHash): Future[TransactionStatus] =
        emulator.checkTransaction(txHash)

    /** The tip as the stream sees it — synthetic block height, and the emulator's own slot. */
    def currentTip: ChainTip = hub.currentTip

    // ── submission ──────────────────────────────────────────────────────────

    def submit(transaction: Transaction): Future[Either[SubmitError, TransactionHash]] =
        emulator.submitSync(transaction) match
            case Left(error) => Future.successful(Left(error))
            case Right(hash) =>
                hub.notifySubmit(hash)
                hub.applyBlock(buildBlock(Seq(transaction -> hash)))
                Future.successful(Right(hash))

    /** Advance the tip without a transaction. */
    def newEmptyBlock(): Unit = {
        emulator.tick(1)
        hub.applyBlock(buildBlock(Seq.empty))
    }

    // ── subscriptions ───────────────────────────────────────────────────────

    def subscribeUtxoQuery[C[_]: ScalusAsyncStreamAdapter](
        query: UtxoEventQuery,
        opts: SubscriptionOptions
    ): C[UtxoEvent] = {
        hub.require(SubscriptionRequest.Utxo(query, opts))
        val id = hub.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent](bufferSize(opts), () => hub.unregisterUtxo(id))
        hub.registerUtxo(id, query, opts, mailbox, emulator.utxos)
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

    def close(): Future[Unit] = {
        hub.closeAll()
        Future.unit
    }

    // ── internals ───────────────────────────────────────────────────────────

    private def bufferSize(opts: SubscriptionOptions): Int = opts.bufferPolicy match
        case DeltaBufferPolicy.Bounded(n) => n
        case DeltaBufferPolicy.Unbounded  => Int.MaxValue

    private def buildBlock(txs: Seq[(Transaction, TransactionHash)]): AppliedBlock = {
        blockNo += 1
        val applied = txs.map { case (tx, hash) =>
            val created: Utxos = tx.body.value.outputs.zipWithIndex.map { case (out, index) =>
                TransactionInput(hash, index) -> out.value
            }.toMap
            // The emulator resolved the consumed outputs while validating; reuse that rather than
            // re-resolving against a UTxO set the transaction has already mutated.
            val spent: Utxos = emulator.getAppliedTx(hash).map(_.spent).getOrElse(Map.empty)
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

    /** What an emulator honestly offers: everything is indexed because the whole ledger is in
      * memory; there is no history to replay and no fork to roll back; and there are no real blocks
      * to hand to a block subscription.
      */
    val capabilities: StreamCapabilities = StreamCapabilities(
      kinds = Set(SubscriptionKind.Utxo, SubscriptionKind.Transaction),
      pushdown = PushdownKind.all,
      replay = ReplaySupport.NoReplay,
      rollbackHorizon = None,
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
