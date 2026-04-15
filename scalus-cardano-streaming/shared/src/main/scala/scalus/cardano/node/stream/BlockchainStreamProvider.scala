package scalus.cardano.node.stream

import scalus.cardano.ledger.{Block, ProtocolParams, Transaction, TransactionHash, TransactionInput, TransactionOutput}
import scalus.cardano.node.{BlockchainProviderTF, BlockchainReaderTF, TransactionStatus}

import scala.concurrent.Future

/** Read-only streaming view of a blockchain.
  *
  * Extends [[BlockchainReaderTF]] with rollback-aware event subscriptions. Events carry a
  * [[ChainPoint]] so subscribers can correlate, deduplicate, and checkpoint.
  *
  * @tparam F
  *   effect type for one-shot operations
  * @tparam C
  *   stream type used to deliver events; a [[MailboxSource]] instance provided by the adapter
  *   module turns engine-owned mailboxes into `C[A]`.
  */
trait BlockchainStreamReaderTF[F[_], C[_]: MailboxSource] extends BlockchainReaderTF[F] {

    def subscribeUtxoQuery(
        query: UtxoEventQuery,
        opts: SubscriptionOptions = SubscriptionOptions()
    ): C[UtxoEvent]

    def subscribeTransactionQuery(
        query: TransactionQuery,
        opts: SubscriptionOptions = SubscriptionOptions()
    ): C[TransactionEvent]

    def subscribeBlockQuery(
        query: BlockQuery,
        opts: SubscriptionOptions = SubscriptionOptions()
    ): C[BlockEvent]

    inline def subscribeUtxo(
        inline f: (TransactionInput, TransactionOutput) => Boolean
    ): C[UtxoEvent]

    inline def subscribeTransaction(inline f: Transaction => Boolean): C[TransactionEvent]

    inline def subscribeBlock(inline f: Block => Boolean): C[BlockEvent]

    /** Latest-value stream of chain-tip updates — newer-wins semantics, so the subscriber always
      * sees the most recent tip when it pulls.
      */
    def subscribeTip(): C[ChainTip]

    /** Latest-value stream of protocol params — emits the current value on subscribe, then only on
      * change.
      */
    def subscribeProtocolParams(): C[ProtocolParams]

    /** Latest-value stream of a single transaction's status. Transitions
      * `NotFound → Pending → Confirmed` (or reverts on rollback) are the typical sequence.
      */
    def subscribeTransactionStatus(txHash: TransactionHash): C[TransactionStatus]

    def close(): F[Unit]
}

trait BlockchainStreamProviderTF[F[_], C[_]: MailboxSource]
    extends BlockchainProviderTF[F]
    with BlockchainStreamReaderTF[F, C]

trait BlockchainStreamProvider[C[_]: MailboxSource] extends BlockchainStreamProviderTF[Future, C]
