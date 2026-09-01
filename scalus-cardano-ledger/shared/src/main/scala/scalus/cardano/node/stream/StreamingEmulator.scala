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
    extends BlockchainStreaming {

    private val capabilities: StreamCapabilities =
        StreamingEmulator.capabilities(securityParam)

    private val hub = new SubscriptionHub(emulator.cardanoInfo, capabilities)

    private var blockNo: BlockNo = 0L
    @volatile private var closed: Boolean = false

    private given ExecutionContext = emulator.executionContext

    def streamCapabilities: StreamCapabilities = capabilities

    /** The tip as the stream sees it — synthetic block height, and the emulator's own slot. */
    def currentTip: ChainTip = hub.currentTip

    // ── the ledger drives the stream ────────────────────────────────────────

    /** Attached to the emulator rather than wrapping its `submit`.
      *
      * A wrapper would observe only what was submitted through it, so a transaction submitted
      * straight to the emulator — now the only way to submit, since this view is not a provider —
      * would be missing from the stream with no error. Attaching to the ledger means every applied
      * transaction is observed however it arrived.
      *
      * One transaction, one block: the emulator has no notion of a block, so this synthesises one
      * per applied transaction and one per [[newEmptyBlock]]. Block *identity* is synthetic too,
      * derived from the block number, which is why block subscriptions are declared unsupported
      * rather than served with a fabricated header.
      */
    private val detach: AutoCloseable = emulator.onTransactionApplied { applied =>
        val block = synchronized(buildBlock(Seq(applied)))
        hub.notifySubmit(applied.txHash)
        hub.applyBlock(block)
    }

    /** Advance the tip without a transaction. */
    def newEmptyBlock(): Unit = {
        val block = synchronized {
            emulator.tick(1)
            buildBlock(Seq.empty)
        }
        hub.applyBlock(block)
    }

    // ── subscriptions ───────────────────────────────────────────────────────

    def subscribeUtxoQuery(
        query: UtxoEventQuery,
        opts: SubscriptionOptions
    ): ScalusAsyncSource[UtxoEvent] = {
        hub.require(SubscriptionRequest.Utxo(query, opts))
        val id = hub.nextSubscriptionId()
        val mailbox = Mailbox.deltaFor[UtxoEvent](opts, () => hub.unregisterUtxo(id))
        hub.registerUtxo(id, query, opts, mailbox, emulator.utxos)
        mailbox
    }

    def subscribeTransactionQuery(
        query: TransactionQuery,
        opts: SubscriptionOptions
    ): ScalusAsyncSource[TransactionEvent] = {
        hub.require(SubscriptionRequest.Transaction(query, opts))
        val id = hub.nextSubscriptionId()
        val mailbox =
            Mailbox.deltaFor[TransactionEvent](opts, () => hub.unregisterTransaction(id))
        hub.registerTransaction(id, query, opts, mailbox)
        mailbox
    }

    def subscribeBlockQuery(
        query: BlockQuery,
        opts: SubscriptionOptions
    ): ScalusAsyncSource[BlockEvent] = {
        hub.require(SubscriptionRequest.Block(query, opts))
        val id = hub.nextSubscriptionId()
        val mailbox = Mailbox.deltaFor[BlockEvent](opts, () => hub.unregisterBlock(id))
        hub.registerBlock(id, query, opts, mailbox)
        mailbox
    }

    def subscribeTip(): ScalusAsyncSource[ChainTip] = {
        val id = hub.nextSubscriptionId()
        val mailbox = Mailbox.latestValue[ChainTip](() => hub.unregisterTip(id))
        hub.registerTip(id, mailbox)
        mailbox
    }

    def subscribeProtocolParams(): ScalusAsyncSource[ProtocolParams] = {
        val id = hub.nextSubscriptionId()
        val mailbox = Mailbox.latestValue[ProtocolParams](() => hub.unregisterParams(id))
        hub.registerParams(id, mailbox)
        mailbox
    }

    def subscribeTransactionStatus(
        txHash: TransactionHash
    ): ScalusAsyncSource[TransactionStatus] = {
        hub.require(SubscriptionRequest.TransactionStatus(txHash))
        val id = hub.nextSubscriptionId()
        val mailbox =
            Mailbox.latestValue[TransactionStatus](() => hub.unregisterTxStatus(txHash, id))
        hub.registerTxStatus(id, txHash, mailbox)
        mailbox
    }

    /** Terminal: subscriptions end, and the provider serves no new ones and submits nothing
      * further. A `close` that left the provider working would leak the very subscriptions it was
      * called to release.
      */
    def close(): Future[Unit] = {
        closed = true
        // Detach first: a block arriving between closing the hub and detaching would be offered to
        // mailboxes that are already terminated.
        detach.close()
        hub.closeAll()
        Future.unit
    }

    // ── internals ───────────────────────────────────────────────────────────

    private def buildBlock(txs: Seq[AppliedTx]): AppliedBlock = {
        blockNo += 1
        val applied = txs.map { a =>
            val created: Utxos = a.tx.body.value.outputs.zipWithIndex.map { case (out, index) =>
                TransactionInput(a.txHash, index) -> out.value
            }.toMap
            // `spent` arrives with the notification: the emulator resolved the consumed outputs
            // while validating, so there is nothing to re-resolve against a UTxO set the
            // transaction has already mutated — and nothing to fail on if `clearAppliedTxs()` was
            // called, which the previous index lookup could not survive.
            AppliedTransaction(a.tx, created, a.spent)
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
      kinds = Set(
        SubscriptionKind.Utxo,
        SubscriptionKind.Transaction,
        SubscriptionKind.TransactionStatus
      ),
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
