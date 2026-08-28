package scalus.cardano.node.stream

import scalus.cardano.infra.UnsupportedSubscriptionException
import scalus.cardano.ledger.*
import scalus.cardano.node.*
import scalus.cardano.node.stream.internal.*
import scalus.uplc.builtin.Data

import scala.collection.mutable
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** A streaming view of Blockfrost, built out of per-address polling.
  *
  * The first provider here that pays real money per request, and its whole shape follows from that.
  * It never fetches a block: it asks `/blocks/{hash}/next` what has happened, then asks
  * `/addresses/{a}/transactions` what each *watched* address did in each new block. Cost therefore
  * scales with how many addresses are subscribed, not with how busy the chain is — flat in the
  * number of subscribers, flat in chain activity, and settable by one dial (`pollInterval`).
  *
  * ## What that costs in capability, and why it is declared rather than discovered
  *
  * A provider built entirely out of per-source lookups cannot answer a question it was not asked in
  * advance. So this one declares `ScanSupport.Unsupported` and `pushdown = {Address}`, and
  * [[SubscriptionSupport.of]] refuses everything outside that — including with
  * `allowUnindexedScan`, because that flag consents to an expense, and here the scan is not
  * expensive but impossible. It also declares no `Block` kind (it holds no blocks) and no
  * `TransactionStatus` kind (it has no way to follow a hash it was never asked to watch, and
  * reporting `Pending` forever would be indistinguishable from a transaction that had not landed).
  *
  * A caller consulting `SubscriptionSupport.of` before subscribing therefore gets exactly the
  * answer `subscribe` will give.
  *
  * ## Reorgs are detected, not reconciled
  *
  * `rollbackHorizon = None`, and that stays truthful: this provider never emits `RolledBack`. When
  * Blockfrost tells it the block it last reported is no longer on the chain, it fails every
  * subscription with [[scalus.cardano.infra.ResyncRequiredException]] instead of quietly diverging.
  * A subscriber that wants to sit out short forks sets `confirmations`; one that wants to survive
  * them wants a provider with a rollback ring, which this is not.
  *
  * ## Ordering at `subscribe`
  *
  * Being registered is not the same as being observed. Telling the follower to watch an address and
  * registering the subscription are two steps, and a block processed between them would be covered
  * by neither. So `subscribe` watches first, *then* reads the snapshot, *then* hands the snapshot to
  * the hub — a snapshot taken after the watch already contains the effects of any block the watch
  * did not reach in time. Subscriptions that opt out of the seed (`includeExistingUtxos = false`)
  * and transaction subscriptions, which have no snapshot to take, keep the residual: they can miss
  * events in the one block spanning their own `subscribe` call. That is documented on
  * [[SubscriptionOptions.includeExistingUtxos]] rather than papered over.
  *
  * @param provider
  *   the one-shot Blockfrost client. Every read delegates to it, so the streaming and one-shot
  *   views of this provider cannot disagree.
  * @param follower
  *   where chain events come from. Injected so the provider's own logic — capability refusal,
  *   watch/register ordering, seeding — is testable without a network.
  * @param paramsRefreshInterval
  *   how often to re-read protocol parameters. Parameters change at epoch boundaries, five days
  *   apart, so this is a few dozen requests a day at any sane setting.
  * @param delay
  *   how to wait. Injected so tests can drive the loops without real time.
  */
class StreamingBlockfrostProvider private[stream] (
    val provider: BlockfrostProvider,
    follower: ChainFollower,
    paramsRefreshInterval: FiniteDuration,
    delay: FiniteDuration => Future[Unit]
) extends BlockchainStreamProvider {

    private given ExecutionContext = provider.executionContext

    private val capabilities: StreamCapabilities = StreamingBlockfrostProvider.capabilities

    private val hub = new SubscriptionHub(provider.cardanoInfo, capabilities)
    private val driver = new HubDriver(hub, follower)

    /** What each live subscription asked to have watched.
      *
      * Guarded by `this`, and so is every call to `follower.watch`, because that call *replaces*
      * the watched set rather than adding to it: two subscribers each passing only their own
      * sources would leave one of them silently unwatched. Holding the lock across the call is also
      * what makes `close` safe — a cancellation racing it either watches before the follower stops
      * or sees `closed` and does not try.
      */
    private val watchedBy = mutable.LinkedHashMap.empty[Long, Set[UtxoSource]]

    private var closed: Boolean = false

    def executionContext: ExecutionContext = provider.executionContext

    def streamCapabilities: StreamCapabilities = capabilities

    /** Begin following the chain. Idempotent.
      *
      * Explicit rather than done in the constructor: this starts spending a metered quota, and a
      * caller is entitled to build the provider, inspect it, and decide. [[StreamingBlockfrostProvider.apply]]
      * returns one already started, which is what almost every caller wants.
      */
    def start(): Unit = {
        driver.start()
        refreshParamsLater()
    }

    // ── one-shot reads: straight through to the Blockfrost client ───────────

    override def cardanoInfo: CardanoInfo = provider.cardanoInfo
    def fetchLatestParams: Future[ProtocolParams] = provider.fetchLatestParams
    def currentSlot: Future[SlotNo] = provider.currentSlot
    def getDatum(datumHash: DataHash): Future[Option[Data]] = provider.getDatum(datumHash)
    def findUtxos(query: UtxoQuery): Future[Either[UtxoQueryError, Utxos]] =
        provider.findUtxos(query)

    /** Straight to Blockfrost, deliberately not through the hub.
      *
      * The emulator answers this from the same cell `subscribeTransactionStatus` reads, so the two
      * cannot disagree. Here there is nothing to agree with: this provider does not serve status
      * subscriptions at all, and the hub only ever hears about transactions that touched a watched
      * address — so consulting it would answer some hashes from a partial view and others from the
      * network, which is worse than answering all of them from the network.
      */
    override def checkTransaction(txHash: TransactionHash): Future[TransactionStatus] =
        provider.checkTransaction(txHash)

    override def pollForConfirmation(
        txHash: TransactionHash,
        maxAttempts: Int,
        delayMs: Long
    ): Future[TransactionStatus] = provider.pollForConfirmation(txHash, maxAttempts, delayMs)

    /** The tip as this provider's stream has observed it — `ChainTip.origin` until the first block
      * arrives, which is up to one `pollInterval` after [[start]].
      */
    def currentTip: ChainTip = hub.currentTip

    // ── submission ──────────────────────────────────────────────────────────

    /** Submit through the Blockfrost client.
      *
      * The hub is not told, unlike in the emulator. `notifySubmit` exists to move a transaction's
      * status to `Pending` for `subscribeTransactionStatus`, which this provider does not serve —
      * so the only effect would be an entry in the hub's status table that nothing ever reads and
      * nothing ever removes.
      */
    def submit(transaction: Transaction): Future[Either[SubmitError, TransactionHash]] =
        provider.submit(transaction)

    // ── subscriptions ───────────────────────────────────────────────────────

    def subscribeUtxoQuery[C[_]: ScalusAsyncStreamAdapter](
        query: UtxoEventQuery,
        opts: SubscriptionOptions
    ): C[UtxoEvent] = {
        hub.require(SubscriptionRequest.Utxo(query, opts))
        val sources = requireWatchable(StreamingBlockfrostProvider.utxoQuerySources(query.query))
        val id = hub.nextSubscriptionId()
        val mailbox = Mailbox.deltaFor[UtxoEvent](opts, () => releaseUtxo(id))
        // Watch, then snapshot, then register — see the class doc. The point `watch` returns is not
        // needed here: this provider closes the gap with the snapshot rather than by starting the
        // subscription at a named position, and it is the *ordering* that makes the snapshot
        // sufficient. A provider that resumed from a caller's checkpoint would need the value.
        watch(id, sources)
        val wantsSeed = opts.includeExistingUtxos && query.types.contains(UtxoEventType.Created)
        if wantsSeed then {
            hub.registerUtxoDeferred(id, query, opts, mailbox)
            provider.findUtxos(query.query).onComplete {
                case Success(Right(utxos)) => hub.seedUtxo(id, utxos)
                case Success(Left(error)) =>
                    hub.failUtxo(
                      id,
                      new IllegalStateException(
                        s"could not read the snapshot this subscription is seeded from: $error"
                      )
                    )
                case Failure(t) => hub.failUtxo(id, t)
            }
        } else hub.registerUtxo(id, query, opts, mailbox, Map.empty)
        summon[ScalusAsyncStreamAdapter[C]].fromSource(mailbox)
    }

    def subscribeTransactionQuery[C[_]: ScalusAsyncStreamAdapter](
        query: TransactionQuery,
        opts: SubscriptionOptions
    ): C[TransactionEvent] = {
        hub.require(SubscriptionRequest.Transaction(query, opts))
        val sources = requireWatchable(StreamingBlockfrostProvider.transactionQuerySources(query))
        val id = hub.nextSubscriptionId()
        val mailbox = Mailbox.deltaFor[TransactionEvent](opts, () => releaseTransaction(id))
        // No seed to take: "the transactions that already happened" is not a state a snapshot can
        // describe, so this one keeps the residual the class doc names.
        watch(id, sources)
        hub.registerTransaction(id, query, opts, mailbox)
        summon[ScalusAsyncStreamAdapter[C]].fromSource(mailbox)
    }

    def subscribeBlockQuery[C[_]: ScalusAsyncStreamAdapter](
        query: BlockQuery,
        opts: SubscriptionOptions
    ): C[BlockEvent] = {
        // Always refuses: `capabilities.kinds` omits Block, because a follower assembled from
        // per-address transaction lists never holds a block to hand over. Routed through `require`
        // rather than thrown here so the refusal and its wording come from the same place a caller
        // asking `SubscriptionSupport.of` in advance would get them.
        hub.require(SubscriptionRequest.Block(query, opts))
        throw new IllegalStateException(
          "unreachable: this provider does not declare SubscriptionKind.Block"
        )
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
        // Always refuses, for the reason `SubscriptionKind.TransactionStatus` exists to let a
        // provider state: this one observes only what it was asked to watch, so a hash it was never
        // asked about would sit at `Pending` forever — indistinguishable, to the subscriber, from a
        // transaction that genuinely had not landed. `checkTransaction` answers the same question
        // from the network, and does it honestly.
        hub.require(SubscriptionRequest.TransactionStatus(txHash))
        throw new IllegalStateException(
          "unreachable: this provider does not declare SubscriptionKind.TransactionStatus"
        )
    }

    /** Terminal: the follower stops, every subscription ends, and nothing new is served. */
    def close(): Future[Unit] = {
        synchronized { closed = true }
        driver.close()
        Future.unit
    }

    // ── internals ───────────────────────────────────────────────────────────

    /** Refuse a subscription this provider could accept but never cover.
      *
      * Two ways that happens, and both are silent if unchecked — no events, no `Idle`, no error,
      * forever:
      *
      *   - **No address to watch at all.** `SubscriptionSupport` and this extraction have to agree
      *     about what "indexed by address" means; if they ever drift, this is where it shows, and a
      *     defect here is better raised than registered.
      *   - **An address Blockfrost cannot be asked about**, i.e. anything but Shelley. That one is
      *     a caller's input rather than a defect, so it gets the facade's refusal exception. Caught
      *     here rather than in the poll loop because the follower feeds every subscriber: one
      *     unwatchable address discovered there would fail all of them.
      */
    private def requireWatchable(sources: Set[UtxoSource]): Set[UtxoSource] = {
        if sources.isEmpty then
            throw new IllegalStateException(
              "this subscription was classified as indexed but names no address to watch; " +
                  "SubscriptionSupport and StreamingBlockfrostProvider disagree about pushdown"
            )
        val unwatchable = sources.collect {
            case UtxoSource.FromAddress(a) if !BlockfrostRestChainApi.isWatchable(a) => a
        }
        if unwatchable.nonEmpty then
            throw UnsupportedSubscriptionException(
              s"Blockfrost addresses these by Bech32, which ${unwatchable.mkString(", ")} has no " +
                  "form of; only Shelley addresses can be watched"
            )
        sources
    }

    private def watch(id: Long, sources: Set[UtxoSource]): Unit = synchronized {
        watchedBy.put(id, sources)
        follower.watch(watchedBy.values.flatten.toSet)
        ()
    }

    /** Stop watching what only this subscription wanted.
      *
      * Shrinking matters on a metered backend: a cancelled subscription that kept its address in
      * the watched set would go on costing one request per block for the life of the provider.
      */
    private def unwatch(id: Long): Unit = synchronized {
        // A closed provider has a stopped follower, which refuses `watch` — rightly, since a
        // position it returned could not be honoured. Nothing is left to stop watching anyway.
        if watchedBy.remove(id).isDefined && !closed then
            follower.watch(watchedBy.values.flatten.toSet)
        ()
    }

    private def releaseUtxo(id: Long): Unit = {
        hub.unregisterUtxo(id)
        unwatch(id)
    }

    private def releaseTransaction(id: Long): Unit = {
        hub.unregisterTransaction(id)
        unwatch(id)
    }

    /** Keep protocol parameters current, on their own slow clock.
      *
      * Parameters change at epoch boundaries and nowhere else, so this is a handful of requests a
      * day. It goes through `refreshCardanoInfo` so that the value `cardanoInfo` reports and the
      * value the subscription stream carries are the same one — the same duality every other read
      * here keeps.
      *
      * A failed read fails the parameter subscribers and stops this loop, and nothing else. They
      * are the ones now holding a value that may be wrong; the chain feed is a different request
      * and is still working, and failing every UTxO subscription over an hourly poll that got a
      * `429` would be collateral damage.
      */
    private def refreshParamsLater(): Unit =
        if !synchronized(closed) then
            delay(paramsRefreshInterval)
                .flatMap(_ =>
                    if synchronized(closed) then Future.unit
                    else provider.refreshCardanoInfo.map(i => hub.updateParams(i.protocolParams))
                )
                .onComplete {
                    case Success(_) => refreshParamsLater()
                    case Failure(t) => hub.failParams(t)
                }
}

object StreamingBlockfrostProvider {

    /** How long to wait between polls, by default.
      *
      * Below Cardano's ~20s average block time, so a block is normally reported within one interval
      * of appearing, and well above the rate at which a free-tier quota would notice: one request
      * per interval is about 8,600 a day against a 50,000 daily cap, leaving the bulk of the budget
      * for the per-address and per-transaction reads that actually carry events.
      */
    val defaultPollInterval: FiniteDuration = 10.seconds

    /** How often to re-read protocol parameters, by default. Parameters change at epoch boundaries,
      * five days apart, so hourly is already far more often than it can matter.
      */
    val defaultParamsRefreshInterval: FiniteDuration = 1.hour

    /** What this provider honestly offers. See the class doc for why each value is what it is. */
    val capabilities: StreamCapabilities = StreamCapabilities(
      kinds = Set(SubscriptionKind.Utxo, SubscriptionKind.Transaction),
      pushdown = Set(PushdownKind.Address),
      scanning = ScanSupport.Unsupported,
      replay = ReplaySupport.NoReplay,
      rollbackHorizon = None,
      maxConfirmations = None,
      idleSignals = true
    )

    /** A started provider polling at [[defaultPollInterval]]. */
    def apply(provider: BlockfrostProvider): StreamingBlockfrostProvider =
        apply(provider, defaultPollInterval)

    /** A started provider polling at `pollInterval` — the quota dial. */
    def apply(
        provider: BlockfrostProvider,
        pollInterval: FiniteDuration
    ): StreamingBlockfrostProvider = {
        given ExecutionContext = provider.executionContext
        val delay: FiniteDuration => Future[Unit] = d =>
            BlockfrostProviderPlatform.delayFuture(d.toMillis)
        val follower = new BlockfrostChainFollower(
          new BlockfrostRestChainApi(provider),
          pollInterval,
          delay
        )
        val streaming = new StreamingBlockfrostProvider(
          provider,
          follower,
          defaultParamsRefreshInterval,
          delay
        )
        streaming.start()
        streaming
    }

    /** The address sources a UTxO query needs watched, as flat leaves.
      *
      * Flattened rather than kept as the composite the caller wrote, because that is the shape both
      * consumers want: the follower probes one address endpoint at a time, and `BlockCoverage`
      * tests membership of exactly these leaves when deciding whether a block covers the query.
      *
      * An intersection contributes whatever addresses it has, which is enough — one covered arm
      * answers an `And`, and the rest post-filters data already in hand. A union whose other arm is
      * not an address never reaches here: [[SubscriptionSupport.of]] refuses it, because a union is
      * only as indexed as its worst arm.
      */
    private[stream] def utxoQuerySources(query: UtxoQuery): Set[UtxoSource] = {
        def fromSource(source: UtxoSource): Set[UtxoSource] = source match
            case a: UtxoSource.FromAddress => Set(a)
            case UtxoSource.Or(l, r)       => fromSource(l) ++ fromSource(r)
            case UtxoSource.And(l, r)      => fromSource(l) ++ fromSource(r)
            case _                         => Set.empty
        query match
            case q: UtxoQuery.Simple         => fromSource(q.source)
            case UtxoQuery.Or(l, r, _, _, _) => utxoQuerySources(l) ++ utxoQuerySources(r)
    }

    /** The address sources a transaction query needs watched, on the same principle as
      * [[utxoQuerySources]] — and matching how `SubscriptionHub` decides whether a block's coverage
      * answers such a query.
      */
    private[stream] def transactionQuerySources(query: TransactionQuery): Set[UtxoSource] =
        query match
            case TransactionQuery.InvolvesAddress(a) => Set(UtxoSource.FromAddress(a))
            case TransactionQuery.AllOf(qs)          => qs.flatMap(transactionQuerySources).toSet
            case TransactionQuery.AnyOf(qs)          => qs.flatMap(transactionQuerySources).toSet
            case _                                   => Set.empty
}
