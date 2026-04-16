package scalus.cardano.n2n

/** Thrown into `Future`s that were aborted via a [[CancelToken]]. The `reason` describes the
  * cancel site (e.g. `"pre-read"`, `"keep-alive beat timeout"`).
  */
final class CancelledException(reason: String, cause: Throwable = null)
    extends RuntimeException(reason, cause)

/** Observable side of a cancellation. Consumers receive one of these and can inspect it or
  * register cleanup actions; they cannot trigger cancellation themselves — the
  * [[CancelSource.cancel]] capability stays with the scope owner.
  *
  * See `docs/local/claude/indexer/n2n-transport.md` § *Cancellation* for the scope-hierarchy
  * contract.
  */
trait CancelToken {

    /** `true` once the owning source has been cancelled. */
    def isCancelled: Boolean

    /** If cancelled, the `Throwable` stored on the source at cancel time — otherwise `None`. */
    def cause: Option[Throwable]

    /** Register a callback. Fires exactly once — immediately and synchronously on the caller's
      * thread if the token is already cancelled, otherwise on the first [[CancelSource.cancel]].
      * Exceptions thrown by the callback are caught and discarded so one listener's failure does
      * not prevent the rest from firing.
      *
      * The returned [[Cancellable]] deregisters the listener from the source's listener list.
      * This matters for long-lived tokens with many short-lived linked children: without
      * deregistration, the parent's listener list would grow unboundedly with stale closures
      * referencing already-cancelled children. [[CancelSource.linkedTo]] uses this path to
      * clean up after itself on child cancel.
      *
      * If the token is already cancelled when `onCancel` is called, the listener fires
      * synchronously and the returned handle is a no-op.
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

    /** Fresh, unlinked source. */
    def apply(): CancelSource = new CancelSourceImpl

    /** Linked source: cancelling `parent` cancels this one too (and carries the parent's cause
      * through). Cancelling this source does NOT cancel the parent — scope isolation is
      * downward-only.
      *
      * If `parent` is already cancelled, the returned source is cancelled immediately with the
      * parent's cause.
      *
      * The link is symmetric about cleanup: on child cancel (for any reason), the child
      * deregisters from the parent's listener list so a long-lived parent doesn't accumulate
      * stale closures from short-lived children.
      */
    def linkedTo(parent: CancelToken): CancelSource = {
        val child = new CancelSourceImpl
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
  */
private final class CancelSourceImpl extends CancelSource {
    private val lock = new AnyRef

    @volatile private var fired: Boolean = false
    @volatile private var storedCause: Option[Throwable] = None

    // Listeners are stored in registration order. Guarded by `lock`.
    private val listeners = scala.collection.mutable.ArrayBuffer.empty[() => Unit]

    val token: CancelToken = new CancelToken {
        def isCancelled: Boolean = fired

        def cause: Option[Throwable] = storedCause

        def onCancel(action: () => Unit): Cancellable = {
            // Fast path: already fired — run synchronously without taking the lock.
            if fired then {
                runSafely(action)
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
                    runSafely(action)
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
            runSafely(toFire(i))
            i += 1
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

    private def runSafely(action: () => Unit): Unit = {
        try action()
        catch { case _: Throwable => () } // isolate listeners
    }

    /** Test-only introspection — current number of registered listeners. Returns 0 after cancel. */
    private[n2n] def listenerCount: Int = lock.synchronized(listeners.size)
}
