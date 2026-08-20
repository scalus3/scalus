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
  * Chain-sourced events must never be dropped silently — a missed `Spent` corrupts the subscriber's
  * view of state permanently, and it has no way to notice. So the two options are "never drop" and
  * "fail loudly":
  *
  *   - `Unbounded` — memory is the only bound. Default.
  *   - `Bounded(n)` — on overflow the subscription terminates with
  *     [[scalus.cardano.infra.ScalusBufferOverflowException]], so the subscriber knows its view is
  *     untrustworthy and must resync.
  */
enum DeltaBufferPolicy {
    case Bounded(size: Int)
    case Unbounded
}

object DeltaBufferPolicy {
    val default: DeltaBufferPolicy = Unbounded
}
