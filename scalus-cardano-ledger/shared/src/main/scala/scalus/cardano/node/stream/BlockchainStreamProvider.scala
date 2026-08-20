package scalus.cardano.node.stream

import scalus.cardano.ledger.{ProtocolParams, TransactionHash}
import scalus.cardano.node.*

import scala.concurrent.Future

/** Read-only streaming view of a blockchain: [[BlockchainReaderTF]] plus rollback-aware event
  * subscriptions.
  *
  * ## Stream / one-shot duality
  *
  * Everything that changes over time has both a one-shot read and a subscription, and the two read
  * the same state — a one-shot is semantically `subscribeXxx().head`. That is what stops two
  * methods disagreeing about the current value. Polling disappears with it:
  * `pollForConfirmation(h)` becomes `subscribeTransactionStatus(h)` with no sleep loop and no
  * missed-update window.
  *
  * ## Subscriptions are live when `subscribe` returns
  *
  * Registration is eager and synchronous. Two consequences, both load-bearing:
  *
  *   - `subscribe(q)` followed by `submit(tx)` on the same thread is race-free — the subscription
  *     is registered before `subscribe` returns, so the submitted transaction's events reach it.
  *     This is what makes emulator-driven tests deterministic.
  *   - **The caller owns releasing the subscription.** A subscription that is never consumed still
  *     accumulates events. Adapters expose this as a resource (fs2 `Resource`, pekko `KillSwitch`,
  *     an ox scope); the underlying release is `ScalusAsyncSource.cancel()`.
  *
  * ## Refusal
  *
  * A request the provider cannot serve throws
  * [[scalus.cardano.infra.UnsupportedSubscriptionException]] synchronously, from the call that
  * caused it, before anything is registered — exactly when
  * `SubscriptionSupport.of(request, streamCapabilities)` says `Unsupported`, or says `Unindexed`
  * and the request did not set `allowUnindexedScan`. Callers that want to decide *before* asking
  * consult the same function.
  *
  * ## Choosing a stream type
  *
  * The stream type is chosen per call, not per provider, so one provider instance can hand fs2
  * streams to application code while a test pulls raw [[ScalusAsyncSource]]s from it. `C` is
  * inferred from the expected type:
  *
  * {{{
  * val tips: Stream[IO, ChainTip]        = provider.subscribeTip()  // needs the fs2 adapter given
  * val raw:  ScalusAsyncSource[ChainTip] = provider.subscribeTip()  // no adapter module needed
  * }}}
  *
  * Where there is no expected type, `C` is decided by which adapter is in implicit scope — an
  * imported adapter outranks the [[ScalusAsyncStreamAdapter.identity]] instance in this companion.
  * Importing an adapter is therefore what makes `subscribe` return that library's type. Two
  * adapters imported into one file with no expected type is an ambiguity, and the compiler says so;
  * annotate or pass the type argument explicitly.
  *
  * @tparam F
  *   effect type for one-shot operations
  */
trait BlockchainStreamReaderTF[F[_]] extends BlockchainReaderTF[F] {

    /** What this provider can do. The only thing an implementation declares; per-request support is
      * derived from it by [[SubscriptionSupport.of]].
      */
    def streamCapabilities: StreamCapabilities

    def subscribeUtxoQuery[C[_]: ScalusAsyncStreamAdapter](
        query: UtxoEventQuery,
        opts: SubscriptionOptions
    ): C[UtxoEvent]

    def subscribeTransactionQuery[C[_]: ScalusAsyncStreamAdapter](
        query: TransactionQuery,
        opts: SubscriptionOptions
    ): C[TransactionEvent]

    def subscribeBlockQuery[C[_]: ScalusAsyncStreamAdapter](
        query: BlockQuery,
        opts: SubscriptionOptions
    ): C[BlockEvent]

    /** Subscribe with default options. */
    def subscribeUtxoQuery[C[_]: ScalusAsyncStreamAdapter](query: UtxoEventQuery): C[UtxoEvent] =
        subscribeUtxoQuery[C](query, SubscriptionOptions())

    /** Subscribe with default options. */
    def subscribeTransactionQuery[C[_]: ScalusAsyncStreamAdapter](
        query: TransactionQuery
    ): C[TransactionEvent] =
        subscribeTransactionQuery[C](query, SubscriptionOptions())

    /** Subscribe with default options. */
    def subscribeBlockQuery[C[_]: ScalusAsyncStreamAdapter](query: BlockQuery): C[BlockEvent] =
        subscribeBlockQuery[C](query, SubscriptionOptions())

    /** Latest-value stream of chain-tip updates — newer wins, so a subscriber always sees the most
      * recent tip when it pulls rather than a backlog of stale ones.
      */
    def subscribeTip[C[_]: ScalusAsyncStreamAdapter](): C[ChainTip]

    /** Latest-value stream of protocol parameters: the current value on subscribe, then changes. */
    def subscribeProtocolParams[C[_]: ScalusAsyncStreamAdapter](): C[ProtocolParams]

    /** Latest-value stream of one transaction's status, following it through the mempool into a
      * block — and back out again if a rollback orphans it.
      */
    def subscribeTransactionStatus[C[_]: ScalusAsyncStreamAdapter](
        txHash: TransactionHash
    ): C[TransactionStatus]

    def close(): F[Unit]
}

/** A streaming provider: [[BlockchainStreamReaderTF]] plus submission. */
trait BlockchainStreamProviderTF[F[_]]
    extends BlockchainProviderTF[F]
    with BlockchainStreamReaderTF[F]

/** `Future`-based streaming reader, mirroring [[BlockchainReader]].
  *
  * Wanted for read-only surfaces — a scenario runner's post-run reader, an emulator projection —
  * where handing out a provider would imply a submit path that does not exist.
  */
trait BlockchainStreamReader extends BlockchainStreamReaderTF[Future] with BlockchainReader

/** `Future`-based streaming provider, mirroring [[BlockchainProvider]].
  *
  * This is the layer the in-repo implementations extend: `Emulator` and `BlockfrostProvider` are
  * already `BlockchainProvider`s, so `F` is fixed to `Future` while `C` stays open — a caller gets
  * fs2, ox or pekko streams from them by having the right given in scope, and gets a working stream
  * with no adapter at all by using `ScalusAsyncSource`.
  */
trait BlockchainStreamProvider
    extends BlockchainStreamProviderTF[Future]
    with BlockchainStreamReader
    with BlockchainProvider
