package scalus.cardano.node.stream

import scalus.cardano.ledger.{ProtocolParams, TransactionHash}
import scalus.cardano.node.*

import scala.concurrent.Future

/** Rollback-aware event subscriptions over a blockchain.
  *
  * Obtained from the provider it observes — `provider.streaming` — never constructed alongside it.
  * That is the whole shape of this API: **there is one object representing the blockchain, and
  * streaming is capability you ask it for**, not a second kind of provider you choose between at
  * construction and then carry through every signature in the program.
  *
  * It deliberately does *not* extend [[scalus.cardano.node.BlockchainProviderTF]]. Whether a
  * backend can stream is a question [[StreamCapabilities]] already answers, honestly and
  * per-request; answering it a second time in the type system only answered it worse, since a
  * static `BlockchainStreamProvider` never told a caller that any particular subscription would
  * work — the capabilities still decided. Reads and submission stay on the provider, which the
  * caller still holds.
  *
  * ## Registration is synchronous; observation follows consumption
  *
  * `subscribe` registers before it returns. Two consequences, both load-bearing:
  *
  *   - `subscribe(q)` followed by `submit(tx)` on the same thread is race-free — the subscription
  *     exists before `subscribe` returns, so the submitted transaction's events reach it. This is
  *     what makes emulator-driven tests deterministic.
  *   - **The caller owns releasing the subscription.** Release is [[ScalusAsyncSource.cancel]];
  *     adapters expose it as a resource (fs2 `Resource`, pekko `KillSwitch`, an ox scope).
  *
  * A backend that has to go and look — a poller — begins observing at the first
  * [[ScalusAsyncSource.pull]], not at `subscribe`. Registering costs nothing, so a caller registers
  * the set it wants and the whole set shares one starting position; a subscription added after the
  * first pull is a latecomer and is covered by the usual seeding path. Nothing is spent on a view
  * nobody consumes.
  *
  * ## Refusal
  *
  * A request the backend cannot serve throws
  * [[scalus.cardano.infra.UnsupportedSubscriptionException]] synchronously, from the call that
  * caused it, before anything is registered — exactly when `SubscriptionSupport.of(request,
  * streamCapabilities)` says `Unsupported`, or says `Unindexed` and the request did not set
  * `allowUnindexedScan`. Callers that want to decide *before* asking consult the same function, or
  * [[supportFor]].
  *
  * ## Streams
  *
  * Every subscription is a [[ScalusAsyncSource]], which is a complete stream with nothing beyond
  * the standard library — consume it with a `pull()` loop. A stream library is one call away, from
  * that library's adapter module:
  *
  * {{{
  * val raw:      ScalusAsyncSource[ChainTip] = s.subscribeTip()
  * val streamed: Stream[IO, ChainTip]        = s.subscribeTip().toStream   // scalus-streaming-fs2
  * }}}
  *
  * @tparam F
  *   effect type of [[close]], matching the provider this view came from.
  */
trait BlockchainStreamingTF[F[_]] {

    /** What this view can do. The only thing an implementation declares; per-request support is
      * derived from it by [[SubscriptionSupport.of]].
      */
    def streamCapabilities: StreamCapabilities

    /** Whether a request would be served, refused, or served only with `allowUnindexedScan` —
      * answered without registering anything.
      */
    def supportFor(request: SubscriptionRequest): SubscriptionSupport =
        SubscriptionSupport.of(request, streamCapabilities)

    def subscribeUtxoQuery(
        query: UtxoEventQuery,
        opts: SubscriptionOptions
    ): ScalusAsyncSource[UtxoEvent]

    def subscribeTransactionQuery(
        query: TransactionQuery,
        opts: SubscriptionOptions
    ): ScalusAsyncSource[TransactionEvent]

    def subscribeBlockQuery(
        query: BlockQuery,
        opts: SubscriptionOptions
    ): ScalusAsyncSource[BlockEvent]

    /** Subscribe with default options. */
    def subscribeUtxoQuery(query: UtxoEventQuery): ScalusAsyncSource[UtxoEvent] =
        subscribeUtxoQuery(query, SubscriptionOptions())

    /** Subscribe with default options. */
    def subscribeTransactionQuery(query: TransactionQuery): ScalusAsyncSource[TransactionEvent] =
        subscribeTransactionQuery(query, SubscriptionOptions())

    /** Subscribe with default options. */
    def subscribeBlockQuery(query: BlockQuery): ScalusAsyncSource[BlockEvent] =
        subscribeBlockQuery(query, SubscriptionOptions())

    /** Latest-value stream of chain-tip updates — newer wins, so a subscriber always sees the most
      * recent tip when it pulls rather than a backlog of stale ones.
      */
    def subscribeTip(): ScalusAsyncSource[ChainTip]

    /** Latest-value stream of protocol parameters: the current value on subscribe, then changes. */
    def subscribeProtocolParams(): ScalusAsyncSource[ProtocolParams]

    /** Latest-value stream of one transaction's status, following it through the mempool into a
      * block — and back out again if a rollback orphans it.
      *
      * Not the same question as the provider's `checkTransaction`, which reads the ledger. This
      * follows a lifecycle, including a `Pending` step a ledger that confirms instantly never has.
      */
    def subscribeTransactionStatus(txHash: TransactionHash): ScalusAsyncSource[TransactionStatus]

    /** Release every subscription this view holds. Bulk teardown, not an obligation: a view whose
      * subscriptions have all been cancelled has already stopped observing.
      */
    def close(): F[Unit]
}

/** `Future`-based streaming view — what `BlockchainProvider.streaming` returns. */
type BlockchainStreaming = BlockchainStreamingTF[Future]

@deprecated(
  "renamed to BlockchainStreaming; it is no longer a BlockchainProvider — reads and " +
      "submission stay on the provider you obtained it from",
  "0.14.2"
)
type BlockchainStreamProvider = BlockchainStreaming

@deprecated("renamed to BlockchainStreamingTF", "0.14.2")
type BlockchainStreamProviderTF[F[_]] = BlockchainStreamingTF[F]

@deprecated(
  "the streaming view no longer distinguishes read from write; use BlockchainStreaming",
  "0.14.2"
)
type BlockchainStreamReader = BlockchainStreaming

@deprecated(
  "the streaming view no longer distinguishes read from write; use BlockchainStreamingTF",
  "0.14.2"
)
type BlockchainStreamReaderTF[F[_]] = BlockchainStreamingTF[F]
