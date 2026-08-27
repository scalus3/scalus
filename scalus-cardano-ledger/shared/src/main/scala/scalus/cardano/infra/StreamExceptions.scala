package scalus.cardano.infra

/** Exceptions raised by streaming subscriptions.
  *
  * Two channels, deliberately distinct:
  *
  *   - [[UnsupportedSubscriptionException]] is an *argument* error. It is thrown synchronously from
  *     `subscribeXxx`, at the call site that caused it, before any registration happens.
  *   - The others are *runtime* failures. They surface through the stream's failure channel — a
  *     failed `Future` from `ScalusAsyncSource.pull()` — because by then the subscription exists
  *     and the caller is consuming it.
  */
class UnsupportedSubscriptionException(val reason: String) extends RuntimeException(reason)

/** A bounded delta buffer overflowed. The subscriber's view of chain state is no longer
  * trustworthy: events were produced faster than they were consumed, and dropping them silently
  * would corrupt derived state. The subscription is terminated so the consumer resyncs.
  */
class ScalusBufferOverflowException(message: String) extends RuntimeException(message)

/** The chain rolled back further than the provider's rollback horizon, so the events needed to
  * reconcile the subscriber's view are no longer available. The consumer must resubscribe, and
  * generally re-seed from a snapshot.
  */
class ResyncRequiredException(message: String) extends RuntimeException(message)
