package scalus.cardano.n2n

import scala.concurrent.duration.FiniteDuration

/** A per-mini-protocol scope bundle: a [[CancelSource]] linked to the connection root plus a
  * [[Timer]] for scheduling deadline-driven cancels. Mini-protocol drivers (handshake, keep-alive,
  * chain-sync later) take a `ProtocolScope` in their constructors instead of threading a
  * `CancelSource` and a `Timer` independently.
  *
  * Ergonomics over the underlying primitives:
  *   - [[linked]] creates a child `CancelSource` linked to [[scope.token]] — used for per-call
  *     scopes (one request / response exchange) whose lifetime is shorter than the owning
  *     protocol's.
  *   - [[schedule]] is the common case: on expiry, cancel the passed source with a typed cause.
  *     Returns the scheduler's [[Cancellable]] so callers can abort the timer on early completion.
  *
  * See `docs/local/claude/indexer/n2n-transport.md` § *Cancellation* for the scope-hierarchy
  * contract, and § *Timeouts* for the schedule-then-cancel-on-completion idiom.
  */
final class ProtocolScope(val scope: CancelSource, val timer: Timer) {

    /** The protocol's cancellation token. Cancelling [[scope]] fires this. */
    def token: CancelToken = scope.token

    /** Fresh child source linked to [[scope.token]]. Cancel it narrowly (e.g. request success) and
      * the parent stays live; cancel the parent and the child fans out.
      */
    def linked(): CancelSource = CancelSource.linkedTo(scope.token)

    /** Schedule `target.cancel(cause)` to fire after `delay` on [[timer]]. Returns the scheduler
      * handle so the caller can abort the timer when the waited-for event completes early —
      * standard pattern:
      * {{{
      *   val timeout = protoScope.schedule(30.seconds, reqScope, TimeoutException("request"))
      *   waitForResponse().onComplete(_ => timeout.cancel())
      * }}}
      */
    def schedule(delay: FiniteDuration, target: CancelSource, cause: Throwable): Cancellable =
        timer.schedule(delay)(target.cancel(cause))
}

object ProtocolScope {

    /** Convenience factory linked to a parent token — the common call site. */
    def linkedTo(parent: CancelToken, timer: Timer): ProtocolScope =
        new ProtocolScope(CancelSource.linkedTo(parent), timer)
}
