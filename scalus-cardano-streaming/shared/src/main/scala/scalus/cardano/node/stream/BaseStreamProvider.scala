package scalus.cardano.node.stream

import scalus.cardano.infra.{ScalusAsyncSink, ScalusAsyncStream}
import scalus.cardano.ledger.{Block, Transaction, TransactionInput, TransactionOutput}

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

/** Per-subscription identifier. Opaque to keep the type system honest
  * about the registry boundary.
  */
opaque type SubId = Long

object SubId {
    private val counter = new AtomicLong(0L)
    def next(): SubId = counter.incrementAndGet()
}

/** Common implementation glue for streaming providers across adapters.
  *
  * Holds per-event-kind subscriber registries and uses the
  * [[ScalusAsyncStream]] instance for `C` to allocate per-subscription
  * channels. Cancellation is wired through `ScalusAsyncStream.channel`'s
  * `onCancel` hook so the registry shrinks automatically when consumers
  * drop their streams.
  *
  * Adapters extend this and provide:
  *
  *   - a `ScalusAsyncStream[C]` `using` parameter (typically derived
  *     from the runtime resources the adapter requires — a
  *     `Dispatcher[IO]` for fs2, an `Ox` capability for ox);
  *   - the snapshot-side `BlockchainProviderTF[F]` methods, typically
  *     by composing with an existing `BlockchainProvider`.
  *
  * The base class exposes `protected` accessors to its registries so
  * the engine (real or synthetic test source) can fan events out
  * across active subscribers.
  */
abstract class BaseStreamProvider[F[_], C[_]](using val asyncStream: ScalusAsyncStream[C])
    extends BlockchainStreamProviderTF[F, C] {

    protected case class UtxoSub(
        query: UtxoEventQuery,
        opts: SubscriptionOptions,
        sink: ScalusAsyncSink[UtxoEvent]
    )
    protected case class TxSub(
        query: TransactionQuery,
        opts: SubscriptionOptions,
        sink: ScalusAsyncSink[TransactionEvent]
    )
    protected case class BlockSub(
        query: BlockQuery,
        opts: SubscriptionOptions,
        sink: ScalusAsyncSink[BlockEvent]
    )

    protected val utxoSubs = new AtomicReference[Map[SubId, UtxoSub]](Map.empty)
    protected val txSubs = new AtomicReference[Map[SubId, TxSub]](Map.empty)
    protected val blockSubs = new AtomicReference[Map[SubId, BlockSub]](Map.empty)
    protected val tipSubs = new AtomicReference[Map[SubId, ScalusAsyncSink[ChainPoint]]](Map.empty)

    final def subscribeUtxoQuery(
        query: UtxoEventQuery,
        opts: SubscriptionOptions
    ): C[UtxoEvent] = {
        val id = SubId.next()
        val (sink, stream) =
            asyncStream.channel[UtxoEvent](opts.bufferPolicy, () => unregisterUtxo(id))
        utxoSubs.updateAndGet(_ + (id -> UtxoSub(query, opts, sink)))
        stream
    }

    final def subscribeTransactionQuery(
        query: TransactionQuery,
        opts: SubscriptionOptions
    ): C[TransactionEvent] = {
        val id = SubId.next()
        val (sink, stream) =
            asyncStream.channel[TransactionEvent](opts.bufferPolicy, () => unregisterTx(id))
        txSubs.updateAndGet(_ + (id -> TxSub(query, opts, sink)))
        stream
    }

    final def subscribeBlockQuery(
        query: BlockQuery,
        opts: SubscriptionOptions
    ): C[BlockEvent] = {
        val id = SubId.next()
        val (sink, stream) =
            asyncStream.channel[BlockEvent](opts.bufferPolicy, () => unregisterBlock(id))
        blockSubs.updateAndGet(_ + (id -> BlockSub(query, opts, sink)))
        stream
    }

    final def subscribeTip(): C[ChainPoint] = {
        val id = SubId.next()
        val (sink, stream) =
            asyncStream.channel[ChainPoint](BufferPolicy.default, () => unregisterTip(id))
        tipSubs.updateAndGet(_ + (id -> sink))
        stream
    }

    // Macro lowering for the inline convenience methods is not yet
    // implemented (see UtxoEventQueryMacros). Adapters override if they
    // want the convenience; otherwise callers use subscribeXxxQuery.
    inline def subscribeUtxo(
        inline f: (TransactionInput, TransactionOutput) => Boolean
    ): C[UtxoEvent] = ???

    inline def subscribeTransaction(
        inline f: Transaction => Boolean
    ): C[TransactionEvent] = ???

    inline def subscribeBlock(
        inline f: Block => Boolean
    ): C[BlockEvent] = ???

    private def unregisterUtxo(id: SubId): Unit = { utxoSubs.updateAndGet(_ - id); () }
    private def unregisterTx(id: SubId): Unit = { txSubs.updateAndGet(_ - id); () }
    private def unregisterBlock(id: SubId): Unit = { blockSubs.updateAndGet(_ - id); () }
    private def unregisterTip(id: SubId): Unit = { tipSubs.updateAndGet(_ - id); () }

    /** Complete every active subscription's sink. Adapters call this
      * from their `close()` implementation. The returned futures are
      * not awaited — callers that need ordering should compose them in
      * their own effect type.
      */
    protected def closeAllSinks(): Unit = {
        utxoSubs.getAndSet(Map.empty).values.foreach(_.sink.complete())
        txSubs.getAndSet(Map.empty).values.foreach(_.sink.complete())
        blockSubs.getAndSet(Map.empty).values.foreach(_.sink.complete())
        tipSubs.getAndSet(Map.empty).values.foreach(_.complete())
    }
}
