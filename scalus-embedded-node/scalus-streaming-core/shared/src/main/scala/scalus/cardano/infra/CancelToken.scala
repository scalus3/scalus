package scalus.cardano.infra

import scala.util.control.NonFatal

/** Thrown into `Future`s that were aborted via a [[CancelToken]]. The `reason` describes the cancel
  * site (e.g. `"pre-read"`, `"keep-alive beat timeout"`).
  */
final class CancelledException(reason: String, cause: Throwable = null)
    extends RuntimeException(reason, cause)

/** Point-in-time cancellation *probe*. Callers that only need to check "has this been cancelled?"
  * at specific points in their code accept a [[CancelCheck]]. Compared to [[CancelToken]], this is
  * a strictly read-only capability — no listener registration and therefore no bookkeeping cost for
  * either side.
  *
  * API authors SHOULD accept the weakest type they actually honour. An entry guard that only does
  * `if cancel.isCancelled then ...` should take `CancelCheck`; an operation that aborts a pending
  * `Future` mid-wait via a registered listener should take [[CancelToken]]. Matching the type to
  * the capability makes the API honest: readers know "will this be observed mid-wait?" by looking
  * at the signature.
  *
  * Every [[CancelToken]] is also a [[CancelCheck]] — pass a token where a check is expected; Scala
  * handles the promotion automatically.
  */
trait CancelCheck {

    /** `true` once the owning source has been cancelled. */
    def isCancelled: Boolean

    /** If cancelled, the `Throwable` stored on the source at cancel time — otherwise `None`. */
    def cause: Option[Throwable]
}

object CancelCheck {

    /** Check that is never cancelled — useful for unit tests and non-cancellable call sites. */
    val never: CancelCheck = CancelToken.never
}

/** Full observable cancellation signal. Extends [[CancelCheck]] with [[onCancel]] for listener
  * registration — the mechanism implementations use to abort in-flight `Future`s mid-wait.
  * Consumers receive one of these and can inspect it or register cleanup actions; they cannot
  * trigger cancellation themselves — the [[CancelSource.cancel]] capability stays with the scope
  * owner.
  *
  * See `docs/local/claude/indexer/n2n-transport.md` § *Cancellation* for the scope-hierarchy
  * contract.
  */
trait CancelToken extends CancelCheck {

    /** Register a callback. Fires exactly once — immediately and synchronously on the caller's
      * thread if the token is already cancelled, otherwise on the first [[CancelSource.cancel]].
      *
      * Listener exception policy: every thrown exception is logged via the source's configured
      * logger and the cancel propagation continues — all listeners run, [[CancelSource.cancel]]
      * never throws because of a buggy callback, and the log captures the full cause. This means a
      * bug in one listener is debuggable (it's in the log) without breaking cascading cleanup
      * elsewhere.
      *
      * The returned [[Cancellable]] deregisters the listener from the source's listener list.
      * Matters for long-lived tokens with many short-lived linked children: without deregistration,
      * the parent's listener list would grow unboundedly with stale closures referencing
      * already-cancelled children. [[CancelSource.linkedTo]] uses this to clean up after itself on
      * child cancel.
      */
    def onCancel(action: () => Unit): Cancellable
}

object CancelToken {

    /** Token that is never cancelled — useful for unit tests and non-cancellable call sites. */
    val never: CancelToken = new CancelToken {
        def isCancelled: Boolean = false
        def cause: Option[Throwable] = None
        def onCancel(action: () => Unit): Cancellable = Cancellable.noop
    }

    /** Returns a token that reports cancelled if either `a` or `b` is cancelled, and whose
      * `onCancel` listener fires at most once (even if both inputs later cancel).
      *
      * Used to compose two independent cancel sources — e.g. a route-level cancel (mux-owned) with
      * a per-call cancel (caller-supplied) — into a single token that downstream transport
      * (`AsyncByteChannel.write`) can observe.
      */
    def or(a: CancelToken, b: CancelToken): CancelToken = new CancelToken {
        def isCancelled: Boolean = a.isCancelled || b.isCancelled
        def cause: Option[Throwable] = a.cause.orElse(b.cause)
        def onCancel(action: () => Unit): Cancellable = {
            val fired = new java.util.concurrent.atomic.AtomicBoolean(false)
            val once: () => Unit = () => if fired.compareAndSet(false, true) then action()
            val h1 = a.onCancel(once)
            val h2 = b.onCancel(once)
            () => { h1.cancel(); h2.cancel() }
        }
    }
}

/** Controlling side of a cancellation. Only the owner of the source has the capability to call
  * [[cancel]]; consumers receive the [[token]].
  */
trait CancelSource {
    def token: CancelToken

    /** Trigger cancellation with a stored cause. Idempotent — only the first call has effect.
      * Listeners registered on the token run synchronously; subsequent registrations on the token
      * fire synchronously at registration time.
      */
    def cancel(cause: Throwable): Unit

    /** Convenience: cancel with a default [[CancelledException]]. */
    final def cancel(): Unit = cancel(CancelledException(CancelSource.DefaultReason))
}

object CancelSource {

    /** Default reason string used when a source is cancelled without an explicit cause. */
    val DefaultReason: String = "cancelled"

    /** Reason used on linked children when the parent fires and did not store a cause. */
    val ParentCancelledReason: String = "parent cancelled"

    private val defaultLogger: scribe.Logger = scribe.Logger[CancelSource]

    /** Fresh, unlinked source. `logger` is used to record listener exceptions alongside propagation
      * — see [[CancelToken.onCancel]]'s listener contract.
      */
    def apply(logger: scribe.Logger = defaultLogger): CancelSource = new CancelSourceImpl(logger)

    /** Linked source: cancelling `parent` cancels this one too (and carries the parent's cause
      * through). Cancelling this source does NOT cancel the parent — scope isolation is
      * downward-only.
      *
      * If `parent` is already cancelled, the returned source is cancelled immediately with the
      * parent's cause.
      *
      * The link is symmetric about cleanup: on child cancel (for any reason), the child deregisters
      * from the parent's listener list so a long-lived parent doesn't accumulate stale closures
      * from short-lived children.
      */
    def linkedTo(parent: CancelToken, logger: scribe.Logger = defaultLogger): CancelSource = {
        val child = new CancelSourceImpl(logger)
        val parentHandle = parent.onCancel { () =>
            child.cancel(parent.cause.getOrElse(CancelledException(ParentCancelledReason)))
        }
        // Deregister the child's entry from parent on child-cancel so the parent's listener
        // list doesn't grow unboundedly with closures referencing already-cancelled children.
        val _ = child.token.onCancel(() => parentHandle.cancel())
        child
    }
}

/** Single shared impl for both unlinked and linked sources — linking is done by the companion's
  * factory, which registers an `onCancel` listener on the parent.
  *
  * Listener exception policy: each listener runs under a `try`; any thrown exception is logged via
  * `logger` (so it's debuggable) and then swallowed so cancel propagation continues and `cancel()`
  * itself never throws because of a buggy callback. Information is preserved via the log stream,
  * not via the return / throw channel.
  */
private final class CancelSourceImpl(logger: scribe.Logger) extends CancelSource {
    private val lock = new AnyRef

    @volatile private var fired: Boolean = false
    @volatile private var storedCause: Option[Throwable] = None

    // Listeners are stored in registration order. Guarded by `lock`.
    private val listeners = scala.collection.mutable.ArrayBuffer.empty[() => Unit]

    val token: CancelToken = new CancelToken {
        def isCancelled: Boolean = fired

        def cause: Option[Throwable] = storedCause

        def onCancel(action: () => Unit): Cancellable = {
            if fired then {
                runFastPath(action)
                Cancellable.noop
            } else {
                val runNow = lock.synchronized {
                    if fired then true
                    else {
                        listeners.append(action)
                        false
                    }
                }
                if runNow then {
                    runFastPath(action)
                    Cancellable.noop
                } else deregisterHandle(action)
            }
        }
    }

    def cancel(cause: Throwable): Unit = {
        val toFire = lock.synchronized {
            if fired then Array.empty[() => Unit]
            else {
                fired = true
                storedCause = Some(cause)
                val snapshot = listeners.toArray
                listeners.clear() // release references
                snapshot
            }
        }
        var i = 0
        while i < toFire.length do {
            try toFire(i)()
            catch {
                case NonFatal(t) => logger.error("onCancel listener threw", t)
            }
            i += 1
        }
    }

    /** Run a listener registered on an already-fired source. Log + swallow: the calling code is
      * just registering, and a buggy callback must not surprise them.
      */
    private def runFastPath(action: () => Unit): Unit = {
        try action()
        catch {
            case NonFatal(t) =>
                logger.error("onCancel listener threw (already-fired fast path)", t)
        }
    }

    /** Identity-matched removal handle for a registered listener. Used by [[CancelSource.linkedTo]]
      * to clean up after itself on child cancel; exposed generally so callers can proactively
      * release long-lived listener registrations.
      */
    private def deregisterHandle(action: () => Unit): Cancellable = () => {
        val _ = lock.synchronized {
            val idx = listeners.indexWhere(_ eq action)
            if idx >= 0 then listeners.remove(idx)
        }
    }

    /** Test-only introspection — current number of registered listeners. Returns 0 after cancel. */
    private[infra] def listenerCount: Int = lock.synchronized(listeners.size)
}
