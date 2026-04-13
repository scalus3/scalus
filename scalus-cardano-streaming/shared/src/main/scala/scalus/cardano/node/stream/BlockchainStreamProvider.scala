package scalus.cardano.node.stream

import scalus.cardano.infra.ScalusAsyncStream
import scalus.cardano.ledger.{Block, Transaction, TransactionInput, TransactionOutput}
import scalus.cardano.node.{BlockchainProviderTF, BlockchainReaderTF, UtxoQuery}

import scala.concurrent.Future

/** Read-only streaming view of a blockchain.
  *
  * Extends [[BlockchainReaderTF]] with rollback-aware event subscriptions.
  * Events carry a `ChainPoint` so subscribers can correlate, deduplicate,
  * and checkpoint.
  *
  * @tparam F
  *   effect type for one-shot operations (from `BlockchainReaderTF`)
  * @tparam C
  *   stream type used to deliver events; an instance of
  *   [[ScalusAsyncStream]] is supplied by a per-library adapter module
  */
trait BlockchainStreamReaderTF[F[_], C[_]: ScalusAsyncStream] extends BlockchainReaderTF[F] {

    /** Subscribe to UTxO events matching a structured query. */
    def subscribeUtxoQuery(
        query: UtxoQuery,
        opts: SubscriptionOptions = SubscriptionOptions()
    ): C[UtxoEvent]

    /** Subscribe to transaction events matching a structured query. */
    def subscribeTransactionQuery(
        query: TransactionQuery,
        opts: SubscriptionOptions = SubscriptionOptions()
    ): C[TransactionEvent]

    /** Subscribe to block events matching a structured query. */
    def subscribeBlockQuery(
        query: BlockQuery,
        opts: SubscriptionOptions = SubscriptionOptions()
    ): C[BlockEvent]

    /** Inline convenience: the predicate is lowered to a structured
      * `UtxoQuery` via macros where possible so the indexer can push the
      * filter down. Predicates that do not lower fall back to post-filter.
      */
    inline def subscribeUtxo(
        inline f: (TransactionInput, TransactionOutput) => Boolean
    ): C[UtxoEvent]

    /** Inline convenience, mirrors `subscribeUtxo`. */
    inline def subscribeTransaction(inline f: Transaction => Boolean): C[TransactionEvent]

    /** Inline convenience, mirrors `subscribeUtxo`. */
    inline def subscribeBlock(inline f: Block => Boolean): C[BlockEvent]

    /** Live stream of chain-tip updates. Emits the latest `ChainPoint`
      * each time a new block becomes the tip, and a synthetic point on
      * rollback.
      */
    def subscribeTip(): C[ChainPoint]

    /** Release resources held by this provider: protocol sockets, query
      * indexes, subscriber buffers. After `close` completes, all
      * subscriber streams terminate.
      */
    def close(): F[Unit]
}

/** Streaming blockchain provider: combines read-only streaming with
  * transaction submission.
  */
trait BlockchainStreamProviderTF[F[_], C[_]: ScalusAsyncStream]
    extends BlockchainProviderTF[F]
    with BlockchainStreamReaderTF[F, C]

/** Streaming provider specialised to `Future`. */
trait BlockchainStreamProvider[C[_]: ScalusAsyncStream] extends BlockchainStreamProviderTF[Future, C]

object BlockchainStreamProviderTF {

    /** Connect to a Cardano node endpoint and return a streaming provider.
      *
      * Not yet implemented — see the indexer design doc for the milestone
      * sequence (N2C chain-sync → rollback buffer → query engine → tx
      * submission).
      */
    def create[F[_], C[_]: ScalusAsyncStream](
        host: String,
        port: Int
    ): F[BlockchainStreamProvider[C]] = ???
}

object BlockchainStreamProvider {

    def create[C[_]: ScalusAsyncStream](host: String, port: Int): BlockchainStreamProvider[C] =
        ???
}
