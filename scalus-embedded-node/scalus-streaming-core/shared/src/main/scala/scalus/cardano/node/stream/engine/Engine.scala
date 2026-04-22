package scalus.cardano.node.stream.engine

import scalus.cardano.ledger.{CardanoInfo, ProtocolParams, TransactionHash, TransactionInput, TransactionOutput, Utxo, Utxos}
import scalus.cardano.node.{BlockchainReader, TransactionStatus, UtxoQuery, UtxoQueryError}
import scalus.cardano.node.stream.{ChainPoint, ChainTip, StartFrom, UtxoEvent}
import scalus.cardano.node.stream.engine.persistence.{AppliedBlockSummary, BucketDelta, BucketState, EnginePersistenceStore, EngineSnapshotFile, JournalRecord, PersistedEngineState}

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

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
  *   optional historical-query backend; `None` ⇔ `BackupSource.NoBackup`. Engine only calls
  *   read-side methods on this field, so a plain [[BlockchainReader]] is sufficient — a reader-only
  *   backup (e.g. `ImmutableEmulator.asReader`) is legal. The adapter's `submit` path
  *   runtime-checks for `BlockchainProvider` to decide whether delegation is possible.
  * @param securityParam
  *   rollback-buffer depth; 2160 on mainnet
  */
final class Engine(
    val cardanoInfo: CardanoInfo,
    val backup: Option[BlockchainReader],
    val securityParam: Int,
    val persistence: EnginePersistenceStore = EnginePersistenceStore.noop,
    val fallbackReplaySources: List[replay.ReplaySource] = Nil
) {

    import Engine.UtxoSubscription

    private val logger: scribe.Logger = scribe.Logger("scalus.cardano.node.stream.engine.Engine")

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

    /** Shut the worker down. Idempotent. Does NOT touch persistence — callers that want to flush or
      * compact before shutdown should do so via the `persistence` field first.
      */
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

    /** Register a caller-allocated mailbox for a UTxO subscription.
      *
      * `startFrom` selects the anchor:
      *
      *   - `Tip` (default) — live subscriber; seeds from backup when requested and then receives
      *     live events from the next applied block.
      *   - `At(point)` — checkpoint-driven. A per-subscription replay emits events for the blocks
      *     between `point` and `currentTip` into the mailbox, then the subscription continues
      *     receiving live events. Replay runs synchronously on the engine's worker so live events
      *     arriving during it queue behind it naturally — no buffering needed.
      *     `includeExistingUtxos` is ignored: the replay itself supplies the initial view.
      *   - `Origin` — rejected with [[replay.ReplayError.ReplaySourceExhausted]]. Dev-chain Origin
      *     support lands with peer replay (later M7 phase).
      *
      * The returned Future completes after the worker has installed the subscription (and, for Tip,
      * after backup seeding). The mailbox is usable for pulling the moment this call returns.
      */
    def registerUtxoSubscription(
        id: Long,
        query: UtxoQuery,
        includeExistingUtxos: Boolean,
        mailbox: Mailbox[UtxoEvent],
        startFrom: StartFrom = StartFrom.Tip
    ): Future[Unit] = {
        val keys = QueryDecomposer.keys(query)
        startFrom match {
            case StartFrom.Tip =>
                submit {
                    utxoSubs.update(id, UtxoSubscription(id, query, keys, mailbox))
                    keys.foreach(acquireBucket)
                }.flatMap { _ =>
                    if includeExistingUtxos then seedFromBackup(id, query, keys, mailbox)
                    else Future.unit
                }(scala.concurrent.ExecutionContext.parasitic)

            case StartFrom.At(point) =>
                registerUtxoAtCheckpoint(id, query, keys, mailbox, point)

            case StartFrom.Origin =>
                mailbox.fail(replay.ReplayError.ReplaySourceExhausted(ChainPoint.origin))
                Future.unit
        }
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
            val events = computeSubEvents(sub, bucketDeltas, point)
            events.foreach(deliverTo(sub, _))
        }

        // Tx-status Confirmed for tracked hashes that appeared in this block.
        block.transactionIds.foreach { h =>
            txStatusSubs.get(h).foreach { subs =>
                subs.values.foreach(_.offer(TransactionStatus.Confirmed))
            }
        }

        // Tip fan-out.
        tipSubs.values.foreach(_.offer(block.tip))

        // Persistence — journal the block's per-bucket deltas. Runs on the worker thread; errors
        // are swallowed by the store's appendSync contract (next flush/compact surfaces them).
        val persistedDeltas: Map[UtxoKey, BucketDelta] =
            bucketDeltas.view.mapValues(d => BucketDelta(d.added, d.removed)).toMap
        persistence.appendSync(
          JournalRecord.Forward(block.tip, block.transactionIds, persistedDeltas)
        )
    }

    def onRollBackward(to: ChainPoint): Future[Unit] = submit {
        rollbackBuffer.rollbackTo(to) match {
            case RollbackBuffer.RollbackOutcome.Reverted(reverted) =>
                val revertedHashes = reverted.flatMap(_ => txHashIndex.applyBackward()).toSet
                reverted.foreach(_ => byKey.values.foreach(_.applyBackward()))
                tipRef.set(rollbackBuffer.tip)
                if reverted.nonEmpty then {
                    utxoSubs.values.foreach(deliverTo(_, UtxoEvent.RolledBack(to)))
                    rollbackBuffer.tip.foreach(t => tipSubs.values.foreach(_.offer(t)))
                    revertedHashes.foreach { h =>
                        val newStatus =
                            txHashIndex.statusOf(h).getOrElse(TransactionStatus.NotFound)
                        txStatusSubs.get(h).foreach(_.values.foreach(_.offer(newStatus)))
                    }
                    persistence.appendSync(JournalRecord.Backward(to))
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
        persistence.appendSync(JournalRecord.OwnSubmitted(hash))
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

    /** Fail every subscriber with `cause` and clear the registries. Used by the chain-sync wiring
      * path when the applier's `done` future completes with an error (decode failure,
      * NoIntersection, unsupported era, transport-level teardown, …). Sends the cause via
      * `Mailbox.fail` so stream adapters surface it on their output — distinguishing a real failure
      * from a graceful close the way [[closeAllSubscribers]] does.
      */
    def failAllSubscribers(cause: Throwable): Future[Unit] = submit {
        utxoSubs.values.foreach(_.mailbox.fail(cause))
        tipSubs.values.foreach(_.fail(cause))
        paramsSubs.values.foreach(_.fail(cause))
        txStatusSubs.values.foreach(_.values.foreach(_.fail(cause)))
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
    // Persistence
    // ------------------------------------------------------------------

    /** Build a serialisable snapshot of the current engine state.
      *
      * `appId` and `networkMagic` are caller-provided (engine doesn't know about them) and travel
      * in the snapshot so a subsequent [[Engine.rebuildFrom]] can cross-check against the running
      * config. Runs on the worker thread so the view is internally consistent.
      */
    def takeSnapshot(appId: String, networkMagic: Long): Future[EngineSnapshotFile] = submit {
        val bucketSnaps: Map[UtxoKey, BucketState] =
            byKey.iterator.map { case (k, b) => k -> BucketState(k, b.snapshot) }.toMap
        // Rebuild per-block deltas for the volatile tail by aligning each block with the bucket
        // history entries whose point matches. Deltas are the same shape as the ones written by
        // onRollForward; here we reconstruct them from the (already-maintained) Bucket history.
        val tail: Seq[AppliedBlockSummary] = rollbackBuffer.volatileTail.map { blk =>
            val deltasForBlock: Map[UtxoKey, BucketDelta] =
                byKey.iterator.flatMap { case (key, b) =>
                    b.deltaAt(blk.point).map(d => key -> BucketDelta(d.added, d.removed))
                }.toMap
            AppliedBlockSummary(blk.tip, blk.transactionIds, deltasForBlock)
        }
        EngineSnapshotFile(
          schemaVersion = EngineSnapshotFile.CurrentSchemaVersion,
          appId = appId,
          networkMagic = networkMagic,
          tip = tipRef.get,
          ownSubmissions = txHashIndex.ownSubmissionsSnapshot,
          volatileTail = tail,
          buckets = bucketSnaps
        )
    }

    /** Replay state from a loaded [[PersistedEngineState]] into this engine. Called by
      * [[Engine.rebuildFrom]] on a freshly-constructed engine; the engine must be empty (no
      * registered subscriptions, no applied blocks) or the replay's bucket-acquisition semantics
      * won't match the live path.
      */
    private def restoreInto(state: PersistedEngineState): Future[Unit] = submit {
        state.snapshot.foreach { snap =>
            // ProtocolParams are not persisted in M6 — the cell stays at
            // `cardanoInfo.protocolParams` on restart, which matches live behaviour until
            // on-chain param tracking lands (M12b).
            snap.ownSubmissions.foreach(txHashIndex.recordOwnSubmission)
            snap.buckets.foreach { case (key, bs) =>
                val b = byKey.getOrElseUpdate(key, new Bucket(key))
                bs.current.foreach { (in, out) => b.current.update(in, out) }
            }
            snap.volatileTail.foreach { summary =>
                val synthetic = AppliedBlock(
                  summary.tip,
                  summary.txIds.toSeq.map { h =>
                      AppliedTransaction(h, Set.empty, IndexedSeq.empty)
                  }
                )
                rollbackBuffer.applyForward(synthetic)
                txHashIndex.applyForward(synthetic)
                summary.bucketDeltas.foreach { case (key, delta) =>
                    byKey.get(key).foreach { b =>
                        b.history += Bucket.Delta(summary.tip.point, delta.added, delta.removed)
                    }
                }
            }
            snap.tip.foreach(t => tipRef.set(Some(t)))
        }
        state.journal.foreach(replayRecord)
    }

    /** Apply a single journal record through the same private paths the live mutators use, minus
      * the subscriber fan-out. Replay runs with no active subscribers (subscribers always
      * re-register after a restart), so skipping the mailbox side is both correct and cheaper.
      */
    private def replayRecord(rec: JournalRecord): Unit = rec match {
        case JournalRecord.Forward(tip, txIds, deltas) =>
            val synthetic = AppliedBlock(
              tip,
              txIds.toSeq.map { h =>
                  AppliedTransaction(h, Set.empty, IndexedSeq.empty)
              }
            )
            val evicted = rollbackBuffer.applyForward(synthetic)
            evicted.foreach { b =>
                txHashIndex.forgetBlock(b.point)
                byKey.values.foreach(_.forgetUpTo(b.point))
            }
            txHashIndex.applyForward(synthetic)
            tipRef.set(Some(tip))
            deltas.foreach { case (key, delta) =>
                val b = byKey.getOrElseUpdate(key, new Bucket(key))
                // Replay the live onRollForward effect on `current` without re-scanning txs.
                delta.removed.foreach(rec => b.current.remove(rec.input))
                delta.added.foreach(rec => b.current.update(rec.input, rec.output))
                b.history += Bucket.Delta(tip.point, delta.added, delta.removed)
            }
        case JournalRecord.Backward(to) =>
            rollbackBuffer.rollbackTo(to) match {
                case RollbackBuffer.RollbackOutcome.Reverted(reverted) =>
                    reverted.foreach(_ => txHashIndex.applyBackward())
                    reverted.foreach(_ => byKey.values.foreach(_.applyBackward()))
                    tipRef.set(rollbackBuffer.tip)
                case RollbackBuffer.RollbackOutcome.PastHorizon(_) =>
                    // Journal contains a rollback whose target isn't in the replayed buffer —
                    // the snapshot was sealed at a point that has since been rolled past. Drop
                    // state: the next startup after a full replay has nothing correct to keep.
                    byKey.clear()
                    tipRef.set(None)
            }
        case JournalRecord.OwnSubmitted(h) => txHashIndex.recordOwnSubmission(h)
        case JournalRecord.OwnForgotten(h) => txHashIndex.clearOwnSubmission(h)
    }

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

    /** Route a single event to either the sub's replay buffer (if an async replay is in flight) or
      * its mailbox. Also surfaces a one-shot warn when a buffer first crosses
      * [[Engine.ReplayBufferWarnThreshold]] so a wedged prefetch is visible in logs rather than
      * silently eating memory.
      */
    private def deliverTo(sub: UtxoSubscription, event: UtxoEvent): Unit = sub.replayBuffer match {
        case Some(buffer) =>
            buffer.addOne(event)
            if buffer.size == Engine.ReplayBufferWarnThreshold then
                logger.warn(
                  s"subscription ${sub.id}: replay buffer reached ${buffer.size} events while " +
                      s"an async prefetch is in flight; buffer is unbounded in M7, a slow peer " +
                      s"or stalled prefetch will keep growing it"
                )
        case None =>
            sub.mailbox.offer(event)
    }

    // ------------------------------------------------------------------
    // Replay coordinator — M7.
    // ------------------------------------------------------------------

    /** Register a [[StartFrom.At]] subscription and replay the window `(checkpoint, currentTip]`
      * into the mailbox.
      *
      * Source precedence: the engine tries the in-memory rollback buffer first, then each entry of
      * [[fallbackReplaySources]] in order. Synchronous sources
      * ([[replay.RollbackBufferReplaySource]], [[replay.ChainStoreReplaySource]]) are exhausted
      * before any [[replay.AsyncReplaySource]] is asked to prefetch, so a covered window pays no
      * network cost.
      *
      * Sync path (Phase 2a): the whole replay runs inside a single `submit` block on the engine
      * worker. Because the worker is single-threaded, no live `onRollForward` interleaves with the
      * replay — live events simply queue behind it. No per-subscription buffering needed.
      *
      * Async path (Phase 2b): the subscription is installed in [[replayPending]] immediately so
      * live events land in its buffer instead of its mailbox. The prefetch runs off the worker.
      * When it completes, a Phase B task on the worker applies the prefetched blocks, drains the
      * buffer, and flips the subscription back to live fan-out. See [[runAsyncReplay]].
      */
    private def registerUtxoAtCheckpoint(
        id: Long,
        query: UtxoQuery,
        keys: Set[UtxoKey],
        mailbox: Mailbox[UtxoEvent],
        checkpoint: ChainPoint
    ): Future[Unit] = {
        given ExecutionContext = ExecutionContext.parasitic
        submit {
            val currentTip = tipRef.get.getOrElse(ChainTip.origin)
            if checkpoint.slot > currentTip.slot then
                Left(replay.ReplayError.ReplayCheckpointAhead(checkpoint, currentTip))
            else {
                val emptyReplay =
                    checkpoint == currentTip.point || currentTip.point == ChainPoint.origin
                val sub = new UtxoSubscription(id, query, keys, mailbox)
                utxoSubs.update(id, sub)
                keys.foreach(acquireBucket)

                if emptyReplay then Right(None)
                else {
                    val rollbackSource =
                        new replay.RollbackBufferReplaySource(rollbackBuffer.volatileTail)
                    val allSources = rollbackSource :: fallbackReplaySources
                    firstCoveringSyncSource(allSources, checkpoint, currentTip.point) match {
                        case Some(blocks) =>
                            applyReplayBlocksToSub(sub, blocks)
                            Right(None)
                        case None =>
                            val asyncSources = fallbackReplaySources.collect {
                                case a: replay.AsyncReplaySource => a
                            }
                            if asyncSources.isEmpty then {
                                utxoSubs.remove(id).foreach(_.keys.foreach(releaseBucket))
                                Left(replay.ReplayError.ReplaySourceExhausted(checkpoint))
                            } else {
                                sub.replayBuffer = Some(mutable.ArrayDeque.empty)
                                Right(Some((asyncSources, currentTip)))
                            }
                    }
                }
            }
        }.flatMap {
            case Right(None) => Future.unit
            case Right(Some((sources, snapshotTip))) =>
                runAsyncReplay(id, mailbox, checkpoint, snapshotTip, sources)
            case Left(err) =>
                mailbox.fail(err)
                Future.failed(err)
        }
    }

    /** Apply a sync replay iterator directly to the subscription's mailbox. Runs on the worker; no
      * buffering because the whole loop is one `submit` block.
      */
    private def applyReplayBlocksToSub(
        sub: UtxoSubscription,
        blocks: Iterator[AppliedBlock]
    ): Unit = {
        val virtualCurrent =
            mutable.Map.empty[UtxoKey, mutable.Map[TransactionInput, TransactionOutput]]
        blocks.foreach { blk =>
            val deltas = replayBlockDeltas(sub.keys, blk, virtualCurrent)
            computeSubEvents(sub, deltas, blk.point).foreach(sub.mailbox.offer)
        }
    }

    /** Drive an async replay cascade. Called off the worker after [[registerUtxoAtCheckpoint]] has
      * installed the subscription with an empty replay buffer.
      *
      * `ReplaySourceExhausted` from a source means "I cannot cover this window" — cascade to the
      * next async source. Any other `ReplayError` (or a failed Future) is treated as unrecoverable
      * for this subscription: fail it with the typed cause but leave live subscribers untouched.
      */
    private def runAsyncReplay(
        id: Long,
        mailbox: Mailbox[UtxoEvent],
        checkpoint: ChainPoint,
        snapshotTip: ChainTip,
        sources: List[replay.AsyncReplaySource]
    )(using ExecutionContext): Future[Unit] = sources match {
        case Nil =>
            failAsyncReplay(id, mailbox, replay.ReplayError.ReplaySourceExhausted(checkpoint))
        case head :: tail =>
            head.prefetch(checkpoint, snapshotTip.point).transformWith {
                case Success(Left(_: replay.ReplayError.ReplaySourceExhausted)) =>
                    runAsyncReplay(id, mailbox, checkpoint, snapshotTip, tail)
                case Success(Left(err)) =>
                    failAsyncReplay(id, mailbox, err)
                case Success(Right(blocks)) =>
                    applyAsyncReplayOnWorker(id, blocks)
                case Failure(NonFatal(t)) =>
                    failAsyncReplay(
                      id,
                      mailbox,
                      replay.ReplayError.ReplayInterrupted("prefetch failed", t)
                    )
                case Failure(t) => Future.failed(t)
            }
    }

    /** Phase B on the worker: apply prefetched blocks to the sub's mailbox, drain any live events
      * that the fan-out captured into the buffer during prefetch, clear the buffer so subsequent
      * fan-out goes direct to the mailbox. The whole task runs inside one `submit` so no live
      * `onRollForward` can interleave between the drain and the clear.
      */
    private def applyAsyncReplayOnWorker(
        id: Long,
        blocks: Seq[AppliedBlock]
    ): Future[Unit] = submit {
        // A subscription unregistered during prefetch is simply not here; discard and move on.
        utxoSubs.get(id).foreach { sub =>
            applyReplayBlocksToSub(sub, blocks.iterator)
            sub.replayBuffer.foreach(_.foreach(sub.mailbox.offer))
            sub.replayBuffer = None
        }
    }

    /** Clean-up path for an async replay that cannot recover: unregister the subscription (which
      * also drops its replay buffer) and fail the mailbox. The outer `registerUtxoAtCheckpoint`
      * future then fails with the same typed cause via the flatMap below.
      */
    private def failAsyncReplay(
        id: Long,
        mailbox: Mailbox[UtxoEvent],
        err: replay.ReplayError
    )(using ExecutionContext): Future[Unit] =
        submit {
            utxoSubs.remove(id).foreach(_.keys.foreach(releaseBucket))
            mailbox.fail(err)
        }.flatMap(_ => Future.failed(err))

    /** Try `sources` in order; return the iterator from the first whose `iterate` returns `Right`.
      * `None` means no synchronous source covered the window (async sources always miss here — they
      * answer via [[replay.AsyncReplaySource.prefetch]] instead).
      */
    private def firstCoveringSyncSource(
        sources: List[replay.ReplaySource],
        from: ChainPoint,
        to: ChainPoint
    ): Option[Iterator[AppliedBlock]] =
        sources.iterator.map(_.iterate(from, to)).collectFirst { case Right(it) => it }

    /** Compute per-key deltas for a single replayed block.
      *
      * Source precedence, per key:
      *
      *   1. The matching entry in the live bucket's `history` — this covers the two cases where the
      *      block's transactions are not directly available: (a) blocks restored from persistence
      *      carry empty transactions (see [[restoreInto]]) because the on-disk format retains
      *      per-bucket deltas, not full transactions; (b) the live `onRollForward` has already
      *      written a per-block delta per key, so re-using it avoids recomputing the same diff
      *      under replay.
      *   2. Recompute from `block.transactions`, threading the caller's `virtualCurrent` map as the
      *      replay-local UTxO set for that key. This path fires only for fresh subscription keys
      *      whose bucket has no history yet — i.e. keys first observed at registration.
      *
      * Either path produces a [[Bucket.Delta]] with the same shape; the engine's fan-out logic does
      * not care which source supplied it. Neither path mutates live bucket state.
      */
    private def replayBlockDeltas(
        keys: Set[UtxoKey],
        block: AppliedBlock,
        virtualCurrent: mutable.Map[UtxoKey, mutable.Map[TransactionInput, TransactionOutput]]
    ): Map[UtxoKey, Bucket.Delta] = {
        keys.iterator.map { key =>
            byKey.get(key).flatMap(_.deltaAt(block.point)) match {
                case Some(delta) => key -> delta
                case None =>
                    val current = virtualCurrent.getOrElseUpdate(key, mutable.Map.empty)
                    key -> computeDeltaFromTransactions(key, block, current)
            }
        }.toMap
    }

    /** Fallback path for [[replayBlockDeltas]]: recompute the per-key delta directly from the
      * block's transactions, mutating the caller-owned `current` scratch map so a UTxO created in
      * an earlier replayed block can be matched as spent in a later one.
      */
    private def computeDeltaFromTransactions(
        key: UtxoKey,
        block: AppliedBlock,
        current: mutable.Map[TransactionInput, TransactionOutput]
    ): Bucket.Delta = {
        val added = mutable.ArrayBuffer.empty[Bucket.CreatedRec]
        val removed = mutable.ArrayBuffer.empty[Bucket.SpentRec]
        block.transactions.foreach { tx =>
            tx.inputs.foreach { input =>
                current.remove(input).foreach { out =>
                    removed += Bucket.SpentRec(input, out, tx.id)
                }
            }
            tx.outputs.iterator.zipWithIndex.foreach { (out, idx) =>
                val input = TransactionInput(tx.id, idx)
                if UtxoKey.matches(key, input, out) then {
                    current.update(input, out)
                    added += Bucket.CreatedRec(input, out, tx.id)
                }
            }
        }
        Bucket.Delta(block.point, added.toSeq, removed.toSeq)
    }

    /** The per-subscription filter used by both live fan-out and replay. Factored out so both paths
      * share one definition of "what events does this subscription see for this block".
      */
    private def computeSubEvents(
        sub: UtxoSubscription,
        bucketDeltas: Map[UtxoKey, Bucket.Delta],
        point: ChainPoint
    ): Seq[UtxoEvent] = {
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
        val out = mutable.ArrayBuffer.empty[UtxoEvent]
        seenAdded.values.foreach { rec =>
            out += UtxoEvent.Created(Utxo(rec.input, rec.output), rec.producedBy, point)
        }
        seenRemoved.values.foreach { rec =>
            out += UtxoEvent.Spent(Utxo(rec.input, rec.output), rec.spentBy, point)
        }
        out.toSeq
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

    /** Build an [[Engine]] and seed it from a loaded [[PersistedEngineState]]. The returned Future
      * completes after the worker has applied the snapshot + journal — callers should wait on it
      * before wiring up subscribers or starting chain-sync so they see a consistent `currentTip`.
      *
      * @param state
      *   as returned by [[EnginePersistenceStore.load]]. An empty state (both snapshot and journal
      *   empty) produces an engine indistinguishable from a fresh constructor call.
      */
    def rebuildFrom(
        state: PersistedEngineState,
        cardanoInfo: CardanoInfo,
        backup: Option[BlockchainReader],
        securityParam: Int,
        persistence: EnginePersistenceStore,
        fallbackReplaySources: List[replay.ReplaySource] = Nil
    )(using scala.concurrent.ExecutionContext): Future[Engine] = {
        val engine =
            new Engine(cardanoInfo, backup, securityParam, persistence, fallbackReplaySources)
        engine.restoreInto(state).map(_ => engine)
    }

    /** A registered UTxO subscription. `replayBuffer` is non-`None` only while an
      * [[replay.AsyncReplaySource]] is prefetching this subscription's window; live `onRollForward`
      * / `onRollBackward` append into the buffer instead of the mailbox, and Phase B of the async
      * replay drains it. Only mutated on the engine worker thread.
      */
    private[engine] final class UtxoSubscription(
        val id: Long,
        val query: UtxoQuery,
        val keys: Set[UtxoKey],
        val mailbox: Mailbox[UtxoEvent]
    ) {
        var replayBuffer: Option[mutable.ArrayDeque[UtxoEvent]] = None
    }

    /** Warn once per subscription when its replay buffer crosses this size. The buffer itself is
      * unbounded (the design doc lists a typed overflow as follow-up work); the warn at least makes
      * a stalled prefetch visible in logs.
      */
    private[engine] val ReplayBufferWarnThreshold: Int = 10_000

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
