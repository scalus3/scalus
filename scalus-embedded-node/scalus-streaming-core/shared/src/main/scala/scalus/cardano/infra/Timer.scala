package scalus.cardano.infra

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise}

/** Handle to cancel a scheduled timer action or deregister a [[CancelToken.onCancel]] listener.
  * Idempotent — if the action has already run (or the listener already fired), calling [[cancel]]
  * is a no-op.
  */
trait Cancellable {
    def cancel(): Unit
}

object Cancellable {

    /** Pre-allocated no-op handle. Returned by [[CancelToken.onCancel]] when the token is already
      * cancelled at registration time (nothing to deregister) and by [[CancelToken.never]].
      */
    val noop: Cancellable = () => ()
}

/** Platform-neutral scheduled-action primitive, used to schedule `CancelSource.cancel(...)` at
  * timeout deadlines. It is NOT a general-purpose executor.
  *
  * Implementations: [[scalus.cardano.infra.jvm.JvmTimer]] (production JVM, backed by
  * `ScheduledExecutorService`), `FakeTimer` (test-only, virtual-time via `advance`).
  */
trait Timer {

    /** Schedule `action` to run after `delay` elapses. The returned [[Cancellable]] aborts the
      * scheduled firing if called before the deadline.
      *
      * Implementations run `action` on a short-lived daemon-style thread or their platform
      * equivalent; callers are expected to keep the action small (e.g. `source.cancel()`).
      * Exceptions thrown by the action must be logged by the implementation (never silently eaten)
      * so that a buggy scheduled action is debuggable via the log channel. See
      * [[scalus.cardano.infra.jvm.JvmTimer]] for the production policy.
      */
    def schedule(delay: FiniteDuration)(action: => Unit): Cancellable

    /** Sleep for `duration`, or fail the returned future early if `cancel` fires. The default
      * implementation is a thin wrapper over [[schedule]] — it works against any `Timer`
      * (production or `FakeTimer` in tests). `cancel` is honoured both pre-call (immediate fail)
      * and mid-wait (scheduled firing is aborted, future fails with the cancel cause). Listener is
      * deregistered on completion to avoid leaking on long-lived cancel sources.
      */
    def sleep(
        duration: FiniteDuration,
        cancel: CancelToken = CancelToken.never
    )(using ExecutionContext): Future[Unit] = {
        val p = Promise[Unit]()
        if cancel.isCancelled then
            p.tryFailure(cancel.cause.getOrElse(new CancelledException("pre-sleep")))
        else {
            val scheduled = schedule(duration) { val _ = p.trySuccess(()) }
            val listener = cancel.onCancel { () =>
                scheduled.cancel()
                val _ = p.tryFailure(
                  cancel.cause.getOrElse(new CancelledException("sleep cancelled"))
                )
            }
            p.future.onComplete(_ => listener.cancel())
        }
        p.future
    }
}
