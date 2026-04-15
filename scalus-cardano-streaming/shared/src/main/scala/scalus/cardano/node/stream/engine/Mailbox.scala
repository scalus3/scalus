package scalus.cardano.node.stream.engine

import scalus.cardano.infra.ScalusBufferOverflowException

import java.util.ArrayDeque
import scala.concurrent.{Future, Promise}

/** Producer-synchronous, consumer-asynchronous bridge between the engine (sole producer, serialised
  * on the engine's worker thread) and an adapter's stream (sole consumer, any runtime).
  *
  * Two concrete shapes depending on the event semantics:
  *
  *   - [[DeltaMailbox]] — FIFO for state-mutating events ([[scalus.cardano.node.stream.UtxoEvent]],
  *     [[scalus.cardano.node.stream.TransactionEvent]], [[scalus.cardano.node.stream.BlockEvent]]).
  *     Silent drops are never allowed: on overflow the mailbox terminates with
  *     [[ScalusBufferOverflowException]] so the subscriber knows to resync.
  *   - [[LatestValueMailbox]] — size-1 newer-wins for single-source-of-truth streams (tip, protocol
  *     params, tx status). Dropping stale intermediate values is the correct semantics; the
  *     subscriber only cares about "the latest."
  *
  * Both share a single interface; adapters pull without caring which kind of mailbox they're
  * reading from.
  */
sealed trait Mailbox[A] {

    /** Synchronously enqueue (or overwrite, for [[LatestValueMailbox]]) the next value. Called only
      * by the engine, from inside its single-thread worker.
      */
    def offer(a: A): Unit

    /** Signal clean end-of-stream. Idempotent. */
    def close(): Unit

    /** Signal producer failure. Idempotent. */
    def fail(t: Throwable): Unit

    /** Pull the next signal.
      *   - `Future.successful(Some(a))` — next value.
      *   - `Future.successful(None)` — clean end-of-stream.
      *   - Failed future — producer signalled failure or (for [[DeltaMailbox]]) the bounded buffer
      *     overflowed.
      *
      * Called only by the adapter's stream consumer, single-threaded per mailbox.
      */
    def pull(): Future[Option[A]]

    /** Consumer-side signal that no further `pull` calls will happen (stream cancelled / downstream
      * finished). Fires the registered `onCancel` exactly once; further `offer`s become no-ops.
      */
    def cancel(): Unit

    def isClosed: Boolean
}

object Mailbox {

    /** Build a FIFO delta mailbox. `maxSize = Int.MaxValue` means unbounded; any finite value
      * applies fail-on-overflow semantics.
      */
    def delta[A](maxSize: Int = Int.MaxValue, onCancel: () => Unit = () => ()): Mailbox[A] =
        new DeltaMailbox[A](maxSize, onCancel)

    /** Build a size-1 latest-value mailbox. */
    def latestValue[A](onCancel: () => Unit = () => ()): Mailbox[A] =
        new LatestValueMailbox[A](onCancel)
}

/** FIFO, single-producer single-consumer, with fail-on-overflow for bounded configuration. Never
  * drops silently — every enqueued event reaches the consumer unless the mailbox terminates.
  */
final class DeltaMailbox[A] private[engine] (
    maxSize: Int,
    onCancel: () => Unit
) extends Mailbox[A] {

    private val lock = new AnyRef
    private val buffer = new ArrayDeque[A]()
    private var terminal: Option[Either[Throwable, Unit]] = None
    private var pending: Promise[Option[A]] = null
    private var cancelled: Boolean = false

    def offer(a: A): Unit = lock.synchronized {
        if terminal.isDefined then return ()
        val waiter = pending
        if waiter != null then {
            pending = null
            waiter.success(Some(a))
            return ()
        }
        if buffer.size < maxSize then {
            buffer.addLast(a)
        } else {
            val exc = new ScalusBufferOverflowException()
            terminal = Some(Left(exc))
            // Leave the buffer intact so already-queued events can
            // still drain to the consumer; the overflow surfaces as
            // a failed pull after the buffer is exhausted.
        }
    }

    def close(): Unit = lock.synchronized {
        if terminal.isDefined then return ()
        terminal = Some(Right(()))
        if pending != null && buffer.isEmpty then {
            val p = pending
            pending = null
            p.success(None)
        }
    }

    def fail(t: Throwable): Unit = lock.synchronized {
        if terminal.isDefined then return ()
        terminal = Some(Left(t))
        if pending != null && buffer.isEmpty then {
            val p = pending
            pending = null
            p.failure(t)
        }
    }

    def pull(): Future[Option[A]] = lock.synchronized {
        if !buffer.isEmpty then Future.successful(Some(buffer.removeFirst()))
        else
            terminal match {
                case Some(Right(())) => Future.successful(None)
                case Some(Left(t))   => Future.failed(t)
                case None =>
                    val p = Promise[Option[A]]()
                    pending = p
                    p.future
            }
    }

    def cancel(): Unit = {
        val shouldFire = lock.synchronized {
            if cancelled then false
            else {
                cancelled = true
                if terminal.isEmpty then terminal = Some(Right(()))
                true
            }
        }
        if shouldFire then onCancel()
    }

    def isClosed: Boolean = lock.synchronized(terminal.isDefined)
}

/** Size-1 mailbox with newer-wins semantics. Each offer overwrites any unread value; a pull returns
  * the current value or parks until the next offer.
  *
  * Right shape for single-source-of-truth streams where the subscriber only cares about the current
  * value — missing intermediate values between pulls is correct, not a defect.
  */
final class LatestValueMailbox[A] private[engine] (
    onCancel: () => Unit
) extends Mailbox[A] {

    private val lock = new AnyRef
    private var slot: Option[A] = None
    private var terminal: Option[Either[Throwable, Unit]] = None
    private var pending: Promise[Option[A]] = null
    private var cancelled: Boolean = false

    def offer(a: A): Unit = lock.synchronized {
        if terminal.isDefined then return ()
        val waiter = pending
        if waiter != null then {
            pending = null
            waiter.success(Some(a))
        } else {
            // Overwrite any previous unread value — this is the
            // "newer wins" semantics.
            slot = Some(a)
        }
    }

    def close(): Unit = lock.synchronized {
        if terminal.isDefined then return ()
        terminal = Some(Right(()))
        if pending != null && slot.isEmpty then {
            val p = pending
            pending = null
            p.success(None)
        }
    }

    def fail(t: Throwable): Unit = lock.synchronized {
        if terminal.isDefined then return ()
        terminal = Some(Left(t))
        if pending != null && slot.isEmpty then {
            val p = pending
            pending = null
            p.failure(t)
        }
    }

    def pull(): Future[Option[A]] = lock.synchronized {
        slot match {
            case Some(a) =>
                slot = None
                Future.successful(Some(a))
            case None =>
                terminal match {
                    case Some(Right(())) => Future.successful(None)
                    case Some(Left(t))   => Future.failed(t)
                    case None =>
                        val p = Promise[Option[A]]()
                        pending = p
                        p.future
                }
        }
    }

    def cancel(): Unit = {
        val shouldFire = lock.synchronized {
            if cancelled then false
            else {
                cancelled = true
                if terminal.isEmpty then terminal = Some(Right(()))
                true
            }
        }
        if shouldFire then onCancel()
    }

    def isClosed: Boolean = lock.synchronized(terminal.isDefined)
}
