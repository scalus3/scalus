package scalus.cardano.infra

import scala.concurrent.duration.FiniteDuration

/** An owned cancellation scope with a [[Timer]] attached for deadline-driven cancels. Bundles a
  * [[CancelSource]] (the scope's lifetime) and a [[Timer]] (scheduling into that lifetime) so
  * callers take one parameter instead of threading two independently. Reusable beyond
  * mini-protocols — any subsystem that wants the "cancel me if I don't finish in T" pattern.
  *
  * Ergonomics over the underlying primitives:
  *   - [[linked]] creates a child `CancelSource` linked to [[source]]'s token — used for per-call
  *     scopes (one request / response exchange) whose lifetime is shorter than the enclosing
  *     scope's.
  *   - [[schedule]] is the common case: on expiry, cancel the passed source with a typed cause.
  *     Returns the scheduler's [[Cancellable]] so callers can abort the timer on early completion.
  *
  * See `docs/local/claude/indexer/n2n-transport.md` § *Cancellation* for the scope-hierarchy
  * contract, and § *Timeouts* for the schedule-then-cancel-on-completion idiom.
  */
final class CancelScope(val source: CancelSource, val timer: Timer) {

    /** Observable cancel token for this scope. Cancelling [[source]] fires this. */
    def token: CancelToken = source.token

    /** Fresh child source linked to this scope's token. Cancel it narrowly (e.g. request success)
      * and the parent stays live; cancel the parent and the child fans out.
      */
    def linked(): CancelSource = CancelSource.linkedTo(source.token)

    /** Schedule `target.cancel(cause)` to fire after `delay` on [[timer]]. Returns the scheduler
      * handle so the caller can abort the timer when the waited-for event completes early —
      * standard pattern:
      * {{{
      *   val timeout = cancelScope.schedule(30.seconds, reqScope, TimeoutException("request"))
      *   waitForResponse().onComplete(_ => timeout.cancel())
      * }}}
      */
    def schedule(delay: FiniteDuration, target: CancelSource, cause: Throwable): Cancellable =
        timer.schedule(delay)(target.cancel(cause))
}

object CancelScope {

    /** Convenience factory linked to a parent token — the common call site. */
    def linkedTo(parent: CancelToken, timer: Timer): CancelScope =
        new CancelScope(CancelSource.linkedTo(parent), timer)
}
