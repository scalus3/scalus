package scalus.cardano.node.stream

import scalus.cardano.ledger.{Block, CardanoInfo, ProtocolParams, Transaction, TransactionHash, TransactionInput, TransactionOutput, Utxos}
import scalus.cardano.node.{SubmitError, TransactionStatus, UtxoQuery, UtxoQueryError, UtxoSource}
import scalus.cardano.node.stream.engine.{Engine, Mailbox}

import scala.concurrent.{ExecutionContext, Future}

/** Common implementation glue for streaming providers across adapters. All subscription plumbing
  * and snapshot-method fall-through lives here; concrete adapters only need to:
  *
  *   - provide a `MailboxSource[C]` given instance;
  *   - implement [[liftFuture]] / [[pureF]];
  *   - implement [[close]].
  *
  * Subscription flow:
  *
  *   1. Caller invokes `subscribeXxx(...)` — synchronous.
  *   2. Adapter allocates the [[Mailbox]] with the appropriate semantics (delta/latest-value) and
  *      an `onCancel` hook that asks the engine to unregister.
  *   3. Mailbox is passed to the engine's async `registerXxx`; the engine stores it and (for UTxO
  *      subs) kicks off seed-from-backup.
  *   4. Adapter wraps the mailbox into its native stream via the `MailboxSource[C]` instance and
  *      returns it.
  */
abstract class BaseStreamProvider[F[_], C[_]](
    val engine: Engine
)(using val mailboxSource: MailboxSource[C])
    extends BlockchainStreamProviderTF[F, C] {

    /** Lightweight composition EC — callbacks do no heavy work. */
    private given composer: ExecutionContext = ExecutionContext.parasitic

    protected def liftFuture[A](fa: => Future[A]): F[A]
    protected def pureF[A](a: A): F[A]

    final def cardanoInfo: CardanoInfo = engine.cardanoInfo

    // ------------------------------------------------------------------
    // Stream subscriptions
    // ------------------------------------------------------------------

    final def subscribeUtxoQuery(
        query: UtxoEventQuery,
        opts: SubscriptionOptions
    ): C[UtxoEvent] = {
        val id = engine.nextSubscriptionId()
        val maxSize = opts.bufferPolicy match
            case DeltaBufferPolicy.Bounded(n) => n
            case DeltaBufferPolicy.Unbounded  => Int.MaxValue
        val mailbox = Mailbox.delta[UtxoEvent](
          maxSize,
          onCancel = () => { engine.unregisterUtxo(id); () }
        )
        engine.registerUtxoSubscription(id, query.query, opts.includeExistingUtxos, mailbox)
        mailboxSource.fromMailbox(mailbox)
    }

    final def subscribeTransactionQuery(
        query: TransactionQuery,
        opts: SubscriptionOptions
    ): C[TransactionEvent] = {
        // Not wired in M2 — return an immediately-completed stream.
        val mailbox = Mailbox.delta[TransactionEvent](Int.MaxValue)
        mailbox.close()
        mailboxSource.fromMailbox(mailbox)
    }

    final def subscribeBlockQuery(
        query: BlockQuery,
        opts: SubscriptionOptions
    ): C[BlockEvent] = {
        val mailbox = Mailbox.delta[BlockEvent](Int.MaxValue)
        mailbox.close()
        mailboxSource.fromMailbox(mailbox)
    }

    final def subscribeTip(): C[ChainTip] = {
        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.latestValue[ChainTip](
          onCancel = () => { engine.unregisterTip(id); () }
        )
        engine.registerTipSubscription(id, mailbox)
        mailboxSource.fromMailbox(mailbox)
    }

    final def subscribeProtocolParams(): C[ProtocolParams] = {
        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.latestValue[ProtocolParams](
          onCancel = () => { engine.unregisterParams(id); () }
        )
        engine.registerParamsSubscription(id, mailbox)
        mailboxSource.fromMailbox(mailbox)
    }

    final def subscribeTransactionStatus(txHash: TransactionHash): C[TransactionStatus] = {
        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.latestValue[TransactionStatus](
          onCancel = () => { engine.unregisterTxStatus(txHash, id); () }
        )
        engine.registerTxStatusSubscription(id, txHash, mailbox)
        mailboxSource.fromMailbox(mailbox)
    }

    inline def subscribeUtxo(
        inline f: (TransactionInput, TransactionOutput) => Boolean
    ): C[UtxoEvent] = ???

    inline def subscribeTransaction(inline f: Transaction => Boolean): C[TransactionEvent] = ???

    inline def subscribeBlock(inline f: Block => Boolean): C[BlockEvent] = ???

    // ------------------------------------------------------------------
    // Snapshot (one-shot) methods. Engine first; backup on fall-through.
    // ------------------------------------------------------------------

    final def currentSlot: F[scalus.cardano.ledger.SlotNo] = liftFuture {
        engine.currentTip match
            case Some(t) => Future.successful(t.slot)
            case None =>
                engine.backup match
                    case Some(bp) => bp.currentSlot
                    case None =>
                        Future.failed(Engine.NoBackupConfiguredException("currentSlot"))
    }

    final def fetchLatestParams: F[ProtocolParams] = liftFuture {
        Future.successful(engine.latestParams)
    }

    final def findUtxos(query: UtxoQuery): F[Either[UtxoQueryError, Utxos]] = liftFuture {
        engine.findUtxosLocal(query).flatMap {
            case Some(utxos) => Future.successful(Right(utxos))
            case None =>
                engine.backup match
                    case Some(bp) => bp.findUtxos(query)
                    case None =>
                        Future.successful(Left(UtxoQueryError.NotFound(noBackupSource(query))))
        }
    }

    final def checkTransaction(txHash: TransactionHash): F[TransactionStatus] = liftFuture {
        engine.txStatus(txHash).flatMap {
            case Some(status) => Future.successful(status)
            case None =>
                engine.backup match
                    case Some(bp) => bp.checkTransaction(txHash)
                    case None     => Future.successful(TransactionStatus.NotFound)
        }
    }

    final def submit(transaction: Transaction): F[Either[SubmitError, TransactionHash]] =
        liftFuture {
            engine.backup match
                case Some(bp) =>
                    bp.submit(transaction).flatMap {
                        case r @ Right(hash) => engine.notifySubmit(hash).map(_ => r)
                        case l @ Left(_)     => Future.successful(l)
                    }
                case None =>
                    Future.successful(Left(noBackupSubmitError))
        }

    final def pollForConfirmation(
        txHash: TransactionHash,
        maxAttempts: Int,
        delayMs: Long
    ): F[TransactionStatus] = liftFuture {
        engine.txStatus(txHash).flatMap {
            case Some(status) => Future.successful(status)
            case None =>
                engine.backup match
                    case Some(bp) => bp.pollForConfirmation(txHash, maxAttempts, delayMs)
                    case None     => Future.successful(TransactionStatus.NotFound)
        }
    }

    final def submitAndPoll(
        transaction: Transaction,
        maxAttempts: Int,
        delayMs: Long
    ): F[Either[SubmitError, TransactionHash]] = liftFuture {
        engine.backup match
            case Some(bp) =>
                bp.submit(transaction).flatMap {
                    case l @ Left(_) => Future.successful(l)
                    case Right(hash) =>
                        engine.notifySubmit(hash).flatMap { _ =>
                            bp.pollForConfirmation(hash, maxAttempts, delayMs).map {
                                case TransactionStatus.Confirmed => Right(hash)
                                case other =>
                                    Left(
                                      scalus.cardano.node.NetworkSubmitError.ConnectionError(
                                        s"tx $hash not confirmed, last status: $other"
                                      )
                                    )
                            }
                        }
                }
            case None => Future.successful(Left(noBackupSubmitError))
    }

    private def noBackupSource(q: UtxoQuery): UtxoSource =
        q match
            case UtxoQuery.Simple(source, _, _, _, _) => source
            case UtxoQuery.Or(left, _, _, _, _)       => noBackupSource(left)

    private def noBackupSubmitError: SubmitError =
        scalus.cardano.node.NetworkSubmitError.ConnectionError("no backup source configured")
}
