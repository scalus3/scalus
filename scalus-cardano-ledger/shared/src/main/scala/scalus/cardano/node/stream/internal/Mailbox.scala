package scalus.cardano.node.stream.internal

import scalus.cardano.infra.ScalusBufferOverflowException
import scalus.cardano.node.stream.ScalusAsyncSource

import scala.collection.mutable
import scala.concurrent.{Future, Promise}

/** Producer-synchronous, consumer-asynchronous bridge between a provider and a subscriber's stream.
  *
  * Two shapes, because two kinds of stream want opposite things on overflow:
  *
  *   - [[DeltaMailbox]] — FIFO for state-mutating events. Silent drops are never acceptable: a
  *     missed `Spent` corrupts the subscriber's view permanently and invisibly. A bounded buffer
  *     that fills therefore *fails* the subscription rather than dropping.
  *   - [[LatestValueMailbox]] — size-1, newer wins, for single-source-of-truth cells (tip, params,
  *     transaction status). Here dropping intermediate values is precisely correct.
  *
  * Not public API: this is one implementation of [[ScalusAsyncSource]], and adapters are written
  * against that interface so the buffering strategy stays free to change.
  *
  * Thread-safety: all state is guarded by `this`, and promise completion happens outside the
  * monitor so a consumer's inline continuation cannot run while the lock is held. On Scala.js
  * `synchronized` is an identity — correct there, since there is nothing to race with.
  */
sealed trait Mailbox[A] extends ScalusAsyncSource[A] {

    /** Enqueue the next value. Called by the producer; a no-op once closed, failed or cancelled. */
    def offer(a: A): Unit

    /** Signal clean end of stream. Idempotent. */
    def close(): Unit

    /** Signal producer failure. Idempotent. */
    def fail(t: Throwable): Unit

    def isClosed: Boolean
}

object Mailbox {

    /** FIFO delta mailbox. `maxSize = Int.MaxValue` means unbounded; any finite value fails the
      * subscription on overflow.
      */
    def delta[A](maxSize: Int = Int.MaxValue, onCancel: () => Unit = () => ()): Mailbox[A] =
        new DeltaMailbox[A](maxSize, onCancel)

    /** Size-1 latest-value mailbox. */
    def latestValue[A](onCancel: () => Unit = () => ()): Mailbox[A] =
        new LatestValueMailbox[A](onCancel)
}

private[stream] abstract class BaseMailbox[A](onCancel: () => Unit) extends Mailbox[A] {

    protected var pending: Option[Promise[Option[A]]] = None
    protected var closed: Boolean = false
    protected var failure: Option[Throwable] = None
    protected var cancelled: Boolean = false

    /** Take the next buffered value, if any. Called under the monitor. */
    protected def takeNext(): Option[A]

    /** Buffer a value the consumer is not waiting for. Called under the monitor; may set
      * [[failure]] to reject the whole subscription.
      */
    protected def enqueue(a: A): Unit

    protected def clearBuffer(): Unit

    final def offer(a: A): Unit = {
        val waiter = synchronized {
            if closed || cancelled || failure.isDefined then None
            else
                pending match
                    case Some(p) =>
                        pending = None
                        Some(p)
                    case None =>
                        enqueue(a)
                        None
        }
        // Completed outside the monitor: the consumer's continuation may run inline.
        waiter.foreach(_.success(Some(a)))
    }

    final def pull(): Future[Option[A]] = {
        val result = synchronized {
            failure match
                case Some(t) => Left(Future.failed[Option[A]](t))
                case None =>
                    takeNext() match
                        case Some(a) => Left(Future.successful(Some(a)))
                        case None =>
                            if closed || cancelled then Left(Future.successful(Option.empty[A]))
                            else
                                // Idempotent while outstanding: pulling again before the first
                                // completes hands back the same future rather than orphaning a
                                // promise nobody will ever complete.
                                pending match
                                    case Some(p) => Right(p)
                                    case None =>
                                        val p = Promise[Option[A]]()
                                        pending = Some(p)
                                        Right(p)
        }
        result match
            case Left(f)  => f
            case Right(p) => p.future
    }

    final def close(): Unit = {
        val waiter = synchronized {
            if closed then None
            else
                closed = true
                val w = pending
                pending = None
                w
        }
        waiter.foreach(_.success(None))
    }

    final def fail(t: Throwable): Unit = {
        val waiter = synchronized {
            if failure.isDefined || closed then None
            else
                failure = Some(t)
                val w = pending
                pending = None
                w
        }
        waiter.foreach(_.failure(t))
    }

    final def cancel(): Unit = {
        val (fireHook, waiter) = synchronized {
            if cancelled then (false, None)
            else
                cancelled = true
                clearBuffer()
                val w = pending
                pending = None
                (true, w)
        }
        waiter.foreach(_.success(None))
        if fireHook then onCancel()
    }

    final def isClosed: Boolean = synchronized(closed || cancelled || failure.isDefined)
}

/** FIFO, single-producer single-consumer, fail-on-overflow when bounded. */
private[stream] final class DeltaMailbox[A](maxSize: Int, onCancel: () => Unit)
    extends BaseMailbox[A](onCancel) {

    private val buffer = mutable.Queue.empty[A]

    protected def takeNext(): Option[A] = if buffer.isEmpty then None else Some(buffer.dequeue())

    protected def enqueue(a: A): Unit =
        if buffer.size >= maxSize then
            buffer.clear()
            failure = Some(
              ScalusBufferOverflowException(
                s"delta subscription buffer overflowed at $maxSize events; " +
                    "the consumer's view of chain state is no longer trustworthy and must be resynced"
              )
            )
        else buffer.enqueue(a)

    protected def clearBuffer(): Unit = buffer.clear()
}

/** Size-1, newer wins. */
private[stream] final class LatestValueMailbox[A](onCancel: () => Unit)
    extends BaseMailbox[A](onCancel) {

    private var latest: Option[A] = None

    protected def takeNext(): Option[A] = {
        val v = latest
        latest = None
        v
    }

    protected def enqueue(a: A): Unit = latest = Some(a)

    protected def clearBuffer(): Unit = latest = None
}
