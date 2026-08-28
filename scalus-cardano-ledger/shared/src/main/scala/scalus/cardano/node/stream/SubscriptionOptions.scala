package scalus.cardano.node.stream

/** Per-subscription configuration for delta streams (UTxO, transaction, block).
  *
  * Latest-value streams — tip, protocol params, transaction status — take no options: the provider
  * always uses a size-1 coalescing buffer for them, because "the newest value wins" is the only
  * sensible semantics for a single-source-of-truth cell.
  */
case class SubscriptionOptions(
    startFrom: StartFrom = StartFrom.Tip,

    /** Block confirmations to wait before emitting. `0` emits tentatively — the caller must handle
      * `RolledBack`. Usually set indirectly via [[noRollback]] rather than picked by hand.
      */
    confirmations: Int = 0,

    /** Guarantee that this subscription never sees `RolledBack`, by waiting at least the network's
      * security parameter of confirmations before emitting. Convenience for subscribers that do not
      * implement rollback handling; costs latency proportional to the depth.
      *
      * When both are set the provider uses `max(confirmations, securityParam)`.
      */
    noRollback: Boolean = false,

    /** Seed the subscription from the provider's snapshot view: emit a synthetic
      * [[UtxoEvent.Created]] for every UTxO already matching the query, before live deltas start.
      * `false` yields a live-only stream. Irrelevant for non-UTxO subscriptions.
      *
      * Not purely a matter of taste on a provider that fetches per source rather than whole blocks.
      * Such a provider only observes a query's sources from the point it is told to watch them, and
      * the block spanning that moment may already have been assembled without them. The seed is
      * what covers that block — it is a snapshot of state *after* it. Turning the seed off
      * therefore accepts that events in that one block may never be delivered, with no error and no
      * `Idle`. A live-only subscriber that cannot tolerate this should subscribe, then reconcile
      * against its own `findUtxos` read.
      */
    includeExistingUtxos: Boolean = true,

    /** Deliver [[UtxoEvent.Idle]] / [[TransactionEvent.Idle]] progress signals for blocks in which
      * nothing matched. Off by default because most subscribers do not want the traffic; on, it
      * gives a narrow subscription a checkpointable position and a liveness signal.
      */
    idleSignals: Boolean = false,

    /** Accept a subscription the provider cannot serve from an index.
      *
      * Off by default, and the default is the point: on a rate-limited backend an unindexed
      * subscription is not an error but it is expensive — a Blockfrost query with no pushdownable
      * source costs a full block scan, tens of requests per block, against a daily quota. Failing
      * loudly at `subscribe` is better than discovering it at the end of the month. Providers that
      * index everything ignore this flag.
      */
    allowUnindexedScan: Boolean = false,
    bufferPolicy: DeltaBufferPolicy = DeltaBufferPolicy.default
)

/** Overflow behaviour for a delta buffer.
  *
  * There is no backpressure to be had here, and that is a property of the source rather than of
  * this implementation: a chain produces blocks whether or not anyone is keeping up, so the only
  * decisions available are how much to hold and what to do when that is not enough.
  *
  * Chain-sourced events must never be dropped silently — a missed `Spent` corrupts the subscriber's
  * view of state permanently, and it has no way to notice. So the two options are "never drop" and
  * "fail loudly":
  *
  *   - `Bounded(n)` — on overflow the subscription terminates with
  *     [[scalus.cardano.infra.ScalusBufferOverflowException]], so the subscriber knows its view is
  *     untrustworthy and must resync. Default, see [[DeltaBufferPolicy.default]].
  *   - `Unbounded` — memory is the only bound. For a subscriber that would rather be killed by the
  *     OOM killer than resync.
  *
  * Deliberately not offered: a bounded buffer that drops. It would turn the corruption this policy
  * exists to prevent into the default behaviour, and the subscriber could not detect it.
  */
enum DeltaBufferPolicy {
    case Bounded(size: Int)
    case Unbounded
}

object DeltaBufferPolicy {

    /** Live events a delta subscription may fall behind by before it is failed.
      *
      * Sized so that only a broken consumer reaches it. Mainnet produces a block roughly every 20
      * seconds, so even a subscription matching every transaction accumulates on the order of tens
      * of events per block; 10,000 is hours of falling behind, while a subscriber that is merely
      * bursty is nowhere near it. The snapshot seed does not count against it — see
      * `StreamingEmulator.bufferSize`.
      */
    val defaultBound: Int = 10_000

    /** Bounded rather than unbounded, on the same principle as the rest of this policy: a
      * subscriber that falls hopelessly behind gets a `ScalusBufferOverflowException` naming its
      * subscription, instead of an out-of-memory error naming nothing.
      */
    val default: DeltaBufferPolicy = Bounded(defaultBound)
}
