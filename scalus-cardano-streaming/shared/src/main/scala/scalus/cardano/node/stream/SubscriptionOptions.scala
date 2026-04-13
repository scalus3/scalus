package scalus.cardano.node.stream

/** Per-subscription configuration. */
case class SubscriptionOptions(
    startFrom: StartFrom = StartFrom.Tip,
    /** Number of block confirmations to wait before emitting an event.
      * `0` (default) emits tentatively. A value >= the network's
      * security parameter guarantees no rollbacks but adds latency.
      *
      * Usually set implicitly via [[noRollback]] rather than picked by
      * hand.
      */
    confirmations: Int = 0,
    /** If `true`, the subscription is guaranteed never to see
      * `RolledBack` events: the provider internally waits at least the
      * network's security parameter of confirmations before emitting.
      * Convenience for subscribers that don't implement rollback
      * handling (wallets, batchers, dashboards).
      *
      * When both [[confirmations]] and `noRollback` are set, the
      * provider uses `max(confirmations, securityParam)`.
      */
    noRollback: Boolean = false,
    bufferPolicy: BufferPolicy = BufferPolicy.default
)

/** How an individual subscriber's buffer behaves when the consumer cannot
  * keep up with the producer.
  */
enum BufferPolicy {
    case Bounded(size: Int, overflow: Overflow)
    case Unbounded
}

object BufferPolicy {
    val default: BufferPolicy = Bounded(1024, Overflow.DropOldest)
}

/** Overflow strategy for a bounded subscriber buffer. */
enum Overflow {

    /** Drop the oldest buffered event to make room for the new one. */
    case DropOldest

    /** Drop the incoming event; leave the buffer as is. */
    case DropNewest

    /** Fail the subscription stream with a buffer-overflow error. */
    case Fail
}
