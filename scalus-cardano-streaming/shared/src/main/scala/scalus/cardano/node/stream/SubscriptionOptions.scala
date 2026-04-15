package scalus.cardano.node.stream

/** Per-subscription configuration for delta streams (UTxO, tx, block). Latest-value streams (tip,
  * params, tx-status) have no user-facing options — the engine always uses a size-1 coalescing
  * mailbox for them.
  */
case class SubscriptionOptions(
    startFrom: StartFrom = StartFrom.Tip,
    /** Number of block confirmations to wait before emitting an event. `0` (default) emits
      * tentatively. A value >= the network's security parameter guarantees no rollbacks but adds
      * latency.
      *
      * Usually set implicitly via [[noRollback]] rather than picked by hand.
      */
    confirmations: Int = 0,
    /** If `true`, the subscription is guaranteed never to see `RolledBack` events: the provider
      * internally waits at least the network's security parameter of confirmations before emitting.
      * Convenience for subscribers that don't implement rollback handling (wallets, batchers,
      * dashboards).
      *
      * When both [[confirmations]] and `noRollback` are set, the provider uses
      * `max(confirmations, securityParam)`.
      */
    noRollback: Boolean = false,
    /** If `true` (default), a UTxO subscription first asks the backup source for UTxOs matching the
      * query as of the current tip, seeds the per-key index, and emits synthetic
      * [[UtxoEvent.Created]] events for each of them before live delta events start flowing.
      *
      * Setting this to `false` yields a "live only" stream: the subscriber sees only UTxOs created
      * after subscription start. Irrelevant for non-UTxO subscriptions.
      */
    includeExistingUtxos: Boolean = true,
    bufferPolicy: DeltaBufferPolicy = DeltaBufferPolicy.default
)

/** Overflow behaviour for a delta mailbox.
  *
  * Chain-sourced events (UTxO / transaction / block) must never be dropped silently — a missed
  * event corrupts the subscriber's view of on-chain state. So the two options are:
  *
  *   - `Unbounded` — never drop. Memory is the only bound. Default.
  *   - `Bounded(n)` — a finite buffer; on overflow the subscription terminates with
  *     [[scalus.cardano.infra.ScalusBufferOverflowException]] so the subscriber knows its view is
  *     no longer trustworthy and must resync (new subscription, possibly with a fresh seed).
  */
enum DeltaBufferPolicy {
    case Bounded(size: Int)
    case Unbounded
}

object DeltaBufferPolicy {
    val default: DeltaBufferPolicy = Unbounded
}
