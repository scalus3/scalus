package scalus.cardano.node.stream.engine

import scalus.cardano.ledger.{CardanoInfo, ProtocolParams, TransactionHash, TransactionInput, TransactionOutput, Utxo, Utxos}
import scalus.cardano.node.{BlockchainProvider, TransactionStatus, UtxoQuery, UtxoQueryError}
import scalus.cardano.node.stream.{ChainPoint, ChainTip, UtxoEvent}

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.Try

/** Index-keyed in-memory engine that owns chain state and drives subscriber fan-out.
  *
  * ## Encapsulated single-thread execution
  *
  * All mutable state — the rollback buffer, per-key UTxO buckets, tx-hash index, protocol-params
  * cell, subscription registries — is only ever touched from the engine's dedicated worker thread.
  * The ONE path to that thread is the private [[submit]] helper: every public mutating method is a
  * one-liner that submits its body as a `Runnable` onto the worker. Lock-free reads
  * ([[currentTip]], [[latestParams]]) bypass the queue via atomics.
  *
  * Adapters never see the worker; they hold [[Mailbox]] references returned from `registerXxx` and
  * pull at their own pace.
  *
  * @param cardanoInfo
  *   static chain metadata and the initial protocol params
  * @param backup
  *   optional historical-query backend; `None` ⇔ `BackupSource.NoBackup`
  * @param securityParam
  *   rollback-buffer depth; 2160 on mainnet
  */
final class Engine(
    val cardanoInfo: CardanoInfo,
    val backup: Option[BlockchainProvider],
    val securityParam: Int
) {

    // ------------------------------------------------------------------
    // Worker — the only path to mutable state.
    // ------------------------------------------------------------------

    private val workQueue = new LinkedBlockingQueue[Runnable]()
    @volatile private var running: Boolean = true
    private val worker: Thread = {
        val t = new Thread(
          () => {
              while running do {
                  val task = workQueue.take()
                  try task.run()
                  catch {
                      case _: InterruptedException => Thread.currentThread.interrupt()
                      case _: Throwable =>
                          () // swallow; individual tasks complete their own Promise
                  }
              }
          },
          "scalus-stream-engine"
        )
        t.setDaemon(true)
        t.start()
        t
    }

    /** Internal — the only way to mutate engine state.
      *
      * Every public method that touches a mutable field MUST wrap its body in `submit { … }`.
      * Breaking this rule loses the single-thread invariant; breakage is loud because the
      * underlying collections (`ArrayDeque`, `mutable.Map`) are not thread-safe.
      */
    private def submit[R](thunk: => R): Future[R] = {
        val p = Promise[R]()
        workQueue.put(() => p.complete(Try(thunk)))
        p.future
    }

    /** Shut the worker down. Idempotent. */
    def shutdown(): Future[Unit] = submit {
        running = false
        // Submit a dummy task so the worker sees `running = false` and
        // exits the loop (otherwise `take()` blocks forever).
        workQueue.put(() => ())
    }

    private val idCounter = new AtomicLong(0L)
    def nextSubscriptionId(): Long = idCounter.incrementAndGet()

    // ------------------------------------------------------------------
    // Mutable state — only touched from inside `submit`.
    // ------------------------------------------------------------------

    private val rollbackBuffer = new RollbackBuffer(securityParam)
    private val txHashIndex = new TxHashIndex
    private val byKey = mutable.Map.empty[UtxoKey, Bucket]
    private val utxoSubs = mutable.Map.empty[Long, Engine.UtxoSubscription]
    private val tipSubs = mutable.Map.empty[Long, Mailbox[ChainTip]]
    private val paramsSubs = mutable.Map.empty[Long, Mailbox[ProtocolParams]]
    private val txStatusSubs =
        mutable.Map.empty[TransactionHash, mutable.Map[Long, Mailbox[TransactionStatus]]]

    // ------------------------------------------------------------------
    // Published atomics (lock-free reads).
    // ------------------------------------------------------------------

    private val tipRef = new AtomicReference[Option[ChainTip]](None)
    private val paramsRef = new AtomicReference[ProtocolParams](cardanoInfo.protocolParams)

    def currentTip: Option[ChainTip] = tipRef.get
    def latestParams: ProtocolParams = paramsRef.get

    // ------------------------------------------------------------------
    // EC-gated snapshot accessors (read-only; no mutation).
    // ------------------------------------------------------------------

    def txStatus(hash: TransactionHash): Future[Option[TransactionStatus]] = submit {
        txHashIndex.statusOf(hash)
    }

    /** Coverage resolution: if every key the query decomposes into has an active bucket, answer
      * from the union of those buckets (filtered by the full query predicate). Otherwise return
      * `None` so the caller falls through to the backup.
      */
    def findUtxosLocal(query: UtxoQuery): Future[Option[Utxos]] = submit {
        val keys = QueryDecomposer.keys(query)
        if keys.nonEmpty && keys.forall(byKey.contains) then {
            val union = mutable.Map.empty[TransactionInput, TransactionOutput]
            keys.foreach { k =>
                byKey(k).current.foreach { (i, o) =>
                    if QueryDecomposer.matches(query, i, o) then union.update(i, o)
                }
            }
            Some(union.toMap)
        } else None
    }

    // ------------------------------------------------------------------
    // Subscriber registration
    //
    // Each register* creates the mailbox inside the engine, stores it
    // in the subscription registry, emits an initial value if
    // relevant, and returns the mailbox to the adapter. The adapter's
    // stream side then pulls from the mailbox at its own pace.
    // ------------------------------------------------------------------

    /** Register a caller-allocated mailbox for a UTxO subscription. Returns a Future that completes
      * after initial seeding (if `includeExistingUtxos`); the mailbox is usable for pulling the
      * moment this call returns, even before the future completes.
      */
    def registerUtxoSubscription(
        id: Long,
        query: UtxoQuery,
        includeExistingUtxos: Boolean,
        mailbox: Mailbox[UtxoEvent]
    ): Future[Unit] = {
        val keys = QueryDecomposer.keys(query)
        submit {
            utxoSubs.update(id, Engine.UtxoSubscription(id, query, keys, mailbox))
            keys.foreach(acquireBucket)
        }.flatMap { _ =>
            if includeExistingUtxos then seedFromBackup(id, query, keys, mailbox)
            else Future.unit
        }(scala.concurrent.ExecutionContext.parasitic)
    }

    def unregisterUtxo(id: Long): Future[Unit] = submit {
        utxoSubs.remove(id).foreach { sub =>
            sub.keys.foreach(releaseBucket)
        }
    }

    def registerTipSubscription(id: Long, mailbox: Mailbox[ChainTip]): Future[Unit] = submit {
        tipSubs.update(id, mailbox)
        tipRef.get.foreach(mailbox.offer)
    }

    def unregisterTip(id: Long): Future[Unit] = submit {
        tipSubs.remove(id)
        ()
    }

    def registerParamsSubscription(
        id: Long,
        mailbox: Mailbox[ProtocolParams]
    ): Future[Unit] = submit {
        paramsSubs.update(id, mailbox)
        mailbox.offer(paramsRef.get)
    }

    def unregisterParams(id: Long): Future[Unit] = submit {
        paramsSubs.remove(id)
        ()
    }

    def registerTxStatusSubscription(
        id: Long,
        hash: TransactionHash,
        mailbox: Mailbox[TransactionStatus]
    ): Future[Unit] = submit {
        val bucket = txStatusSubs.getOrElseUpdate(hash, mutable.Map.empty)
        bucket.update(id, mailbox)
        mailbox.offer(txHashIndex.statusOf(hash).getOrElse(TransactionStatus.NotFound))
    }

    def unregisterTxStatus(hash: TransactionHash, id: Long): Future[Unit] = submit {
        txStatusSubs.get(hash).foreach { bucket =>
            bucket.remove(id)
            if bucket.isEmpty then txStatusSubs.remove(hash)
        }
    }

    // ------------------------------------------------------------------
    // Chain event ingestion
    // ------------------------------------------------------------------

    def onRollForward(block: AppliedBlock): Future[Unit] = submit {
        val evicted = rollbackBuffer.applyForward(block)
        evicted.foreach { b =>
            txHashIndex.forgetBlock(b.point)
            byKey.values.foreach(_.forgetUpTo(b.point))
        }
        txHashIndex.applyForward(block)
        tipRef.set(Some(block.tip))

        // Update every active bucket once.
        val bucketDeltas: Map[UtxoKey, Bucket.Delta] =
            byKey.iterator.map((k, b) => k -> b.applyForward(block)).toMap

        val point = block.point

        // Per-subscription fan-out: gather the sub's diff across its
        // keys, filter by the full query predicate, enqueue events.
        utxoSubs.values.foreach { sub =>
            val seenAdded = mutable.LinkedHashMap.empty[TransactionInput, Bucket.CreatedRec]
            val seenRemoved = mutable.LinkedHashMap.empty[TransactionInput, Bucket.SpentRec]
            sub.keys.foreach { k =>
                bucketDeltas.get(k).foreach { d =>
                    d.added.foreach { rec =>
                        if QueryDecomposer.matches(sub.query, rec.input, rec.output) then
                            seenAdded.update(rec.input, rec)
                    }
                    d.removed.foreach { rec =>
                        if QueryDecomposer.matches(sub.query, rec.input, rec.output) then
                            seenRemoved.update(rec.input, rec)
                    }
                }
            }
            seenAdded.values.foreach { rec =>
                sub.mailbox.offer(
                  UtxoEvent.Created(Utxo(rec.input, rec.output), rec.producedBy, point)
                )
            }
            seenRemoved.values.foreach { rec =>
                sub.mailbox.offer(
                  UtxoEvent.Spent(Utxo(rec.input, rec.output), rec.spentBy, point)
                )
            }
        }

        // Tx-status Confirmed for tracked hashes that appeared in this block.
        block.transactionIds.foreach { h =>
            txStatusSubs.get(h).foreach { subs =>
                subs.values.foreach(_.offer(TransactionStatus.Confirmed))
            }
        }

        // Tip fan-out.
        tipSubs.values.foreach(_.offer(block.tip))
    }

    def onRollBackward(to: ChainPoint): Future[Unit] = submit {
        rollbackBuffer.rollbackTo(to) match {
            case RollbackBuffer.RollbackOutcome.Reverted(reverted) =>
                val revertedHashes = reverted.flatMap(_ => txHashIndex.applyBackward()).toSet
                reverted.foreach(_ => byKey.values.foreach(_.applyBackward()))
                tipRef.set(rollbackBuffer.tip)
                if reverted.nonEmpty then {
                    utxoSubs.values.foreach(_.mailbox.offer(UtxoEvent.RolledBack(to)))
                    rollbackBuffer.tip.foreach(t => tipSubs.values.foreach(_.offer(t)))
                    revertedHashes.foreach { h =>
                        val newStatus =
                            txHashIndex.statusOf(h).getOrElse(TransactionStatus.NotFound)
                        txStatusSubs.get(h).foreach(_.values.foreach(_.offer(newStatus)))
                    }
                }

            case RollbackBuffer.RollbackOutcome.PastHorizon(_) =>
                val err = Engine.ResyncRequiredException(
                  s"rollback to $to is past the engine's rollback horizon; subscribers must resync"
                )
                utxoSubs.values.foreach(_.mailbox.fail(err))
                tipSubs.values.foreach(_.fail(err))
                txStatusSubs.values.foreach(_.values.foreach(_.fail(err)))
                paramsSubs.values.foreach(_.fail(err))
                utxoSubs.clear()
                tipSubs.clear()
                txStatusSubs.clear()
                paramsSubs.clear()
                byKey.clear()
                tipRef.set(None)
        }
    }

    // ------------------------------------------------------------------
    // Own-submission tracking
    // ------------------------------------------------------------------

    def notifySubmit(hash: TransactionHash): Future[Unit] = submit {
        txHashIndex.recordOwnSubmission(hash)
        txStatusSubs.get(hash).foreach(_.values.foreach(_.offer(TransactionStatus.Pending)))
    }

    def closeAllSubscribers(): Future[Unit] = submit {
        utxoSubs.values.foreach(_.mailbox.close())
        tipSubs.values.foreach(_.close())
        paramsSubs.values.foreach(_.close())
        txStatusSubs.values.foreach(_.values.foreach(_.close()))
        utxoSubs.clear()
        tipSubs.clear()
        paramsSubs.clear()
        txStatusSubs.clear()
        byKey.clear()
    }

    // ------------------------------------------------------------------
    // Test-only surface — raw fan-out bypassing the full pipeline, for
    // isolation tests of the adapter/stream plumbing.
    // ------------------------------------------------------------------

    private[stream] def testFanoutTip(t: ChainTip): Future[Unit] = submit {
        tipSubs.values.foreach(_.offer(t))
    }

    private[stream] def testFanoutUtxoEvent(event: UtxoEvent): Future[Unit] = submit {
        utxoSubs.values.foreach(_.mailbox.offer(event))
    }

    private[stream] def testCloseAll(): Future[Unit] = closeAllSubscribers()

    private[stream] def testFailAll(t: Throwable): Future[Unit] = submit {
        utxoSubs.values.foreach(_.mailbox.fail(t))
        tipSubs.values.foreach(_.fail(t))
        paramsSubs.values.foreach(_.fail(t))
        txStatusSubs.values.foreach(_.values.foreach(_.fail(t)))
        utxoSubs.clear()
        tipSubs.clear()
        paramsSubs.clear()
        txStatusSubs.clear()
        byKey.clear()
    }

    /** Snapshot of active subscription count. Not serialised through the worker — a dirty read is
      * fine for test assertions.
      */
    private[stream] def testActiveSubscriptionCount: Int =
        utxoSubs.size + tipSubs.size + paramsSubs.size + txStatusSubs.values.map(_.size).sum

    // ------------------------------------------------------------------
    // Internal helpers (serial lane only)
    // ------------------------------------------------------------------

    private def acquireBucket(key: UtxoKey): Unit = {
        val b = byKey.getOrElseUpdate(key, new Bucket(key))
        b.refCount += 1
    }

    private def releaseBucket(key: UtxoKey): Unit = {
        byKey.get(key).foreach { b =>
            b.refCount -= 1
            if b.refCount <= 0 then byKey.remove(key)
        }
    }

    private def seedFromBackup(
        subId: Long,
        query: UtxoQuery,
        keys: Set[UtxoKey],
        mailbox: Mailbox[UtxoEvent]
    ): Future[Unit] = backup match
        case None =>
            mailbox.fail(Engine.NoBackupConfiguredException(s"cannot seed subscription $subId"))
            Future.unit
        case Some(provider) =>
            provider
                .findUtxos(query)
                .flatMap {
                    case Left(err) =>
                        mailbox.fail(Engine.BackupSeedFailure(err))
                        Future.unit
                    case Right(utxos) =>
                        submit {
                            keys.foreach(k => byKey.get(k).foreach(_.seed(utxos)))
                            val seedPoint = tipRef.get.map(_.point).getOrElse(ChainPoint.origin)
                            utxos.iterator
                                .filter((i, o) => QueryDecomposer.matches(query, i, o))
                                .foreach { (input, output) =>
                                    mailbox.offer(
                                      UtxoEvent.Created(
                                        Utxo(input, output),
                                        input.transactionId,
                                        seedPoint
                                      )
                                    )
                                }
                        }
                }(provider.executionContext)
}

object Engine {

    val DefaultSecurityParam: Int = 2160

    private[engine] final case class UtxoSubscription(
        id: Long,
        query: UtxoQuery,
        keys: Set[UtxoKey],
        mailbox: Mailbox[UtxoEvent]
    )

    final case class NoBackupConfiguredException(detail: String)
        extends RuntimeException(s"no backup source configured: $detail")

    final case class BackupSeedFailure(underlying: UtxoQueryError)
        extends RuntimeException(s"backup seed failed: $underlying")

    /** Raised to every active subscription when a rollback targets a point older than the engine's
      * rollback-buffer horizon.
      */
    final case class ResyncRequiredException(detail: String)
        extends RuntimeException(s"engine resync required: $detail")
}
