package scalus.cardano.infra

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.{Duration, FiniteDuration}

/** Test-only [[Timer]] with virtual wall-clock. Actions scheduled via [[schedule]] do not run until
  * the test advances time via [[advance]]. Fire order on a single `advance` call is FIFO by
  * `fireAt` deadline; ties broken by scheduling order. Cancelling a scheduled action removes it
  * from the pending list.
  *
  * Used throughout handshake / keep-alive / mux tests to deterministically exercise timeouts
  * without real wall-clock waits.
  */
final class FakeTimer extends Timer {

    private val lock = new AnyRef
    private var now: FiniteDuration = Duration.Zero
    private val pending = ArrayBuffer.empty[FakeTimer.Entry]
    private var nextId: Long = 0L

    def schedule(delay: FiniteDuration)(action: => Unit): Cancellable = {
        val id = lock.synchronized {
            val myId = nextId
            nextId += 1
            pending += FakeTimer.Entry(now + delay, myId, () => action)
            myId
        }
        () =>
            val _ = lock.synchronized {
                val idx = pending.indexWhere(_.id == id)
                if idx >= 0 then pending.remove(idx)
            }
    }

    /** Advance virtual time by `duration` and fire every action whose deadline is now `<=` the new
      * wall-clock. Actions are fired in deadline order (ties broken by scheduling order). Actions
      * throwing exceptions propagate to the caller — tests that want to assert on the failure
      * should `intercept` around the `advance` call.
      */
    def advance(duration: FiniteDuration): Unit = {
        val toFire = lock.synchronized {
            now = now + duration
            val (fire, keep) = pending.partition(_.fireAt <= now)
            pending.clear()
            pending ++= keep
            fire.sortBy(e => (e.fireAt, e.id)).toArray
        }
        var i = 0
        while i < toFire.length do {
            toFire(i).action()
            i += 1
        }
    }

    /** Read the current virtual time — useful for assertions. */
    def currentTime: FiniteDuration = lock.synchronized(now)

    /** Count of currently-pending (non-cancelled, non-fired) entries. */
    def pendingCount: Int = lock.synchronized(pending.size)
}

object FakeTimer {
    private final case class Entry(fireAt: FiniteDuration, id: Long, action: () => Unit)
}
