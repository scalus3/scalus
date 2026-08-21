package scalus.cardano.node.stream.internal

import scalus.cardano.infra.{CancelToken, CancelledException, ScalusBufferOverflowException}
import scalus.cardano.node.stream.ScalusAsyncSource

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}

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
  * ## Buffering and delivery are separable, on purpose
  *
  * [[offer]] is buffer-then-deliver, and a producer that needs several events to land atomically
  * relative to its own state can split the two: [[offerBuffered]] under its lock, [[flush]] after
  * releasing it. That is what lets the hub register a subscription and enqueue its seed as one
  * indivisible step without ever running a consumer's continuation while holding the hub monitor.
  *
  * Nothing observable escapes during the buffered phase — no promise is completed, no `onCancel`
  * hook runs. Both are deferred to [[flush]].
  *
  * ## Termination always unregisters
  *
  * Every way a mailbox can die — explicit [[cancel]], clean [[close]], producer [[fail]], or a
  * bounded buffer overflowing — fires `onCancel` exactly once. A subscription that ends by any
  * route must stop costing the provider work; the alternative is a dead subscription matched
  * against every block for the lifetime of the process.
  */
sealed trait Mailbox[A] extends ScalusAsyncSource[A] {

    /** Enqueue the next value and deliver it. A no-op once closed, failed or cancelled. */
    def offer(a: A): Unit

    /** Enqueue without delivering: completes no promise and fires no hook. Must be followed by
      * [[flush]], which is what makes the value visible.
      */
    def offerBuffered(a: A): Unit

    /** Deliver whatever [[offerBuffered]] left pending: complete a waiting `pull`, and fire
      * `onCancel` if the mailbox died while buffering. Idempotent and cheap when there is nothing
      * to do.
      */
    def flush(): Unit

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

    /** What a state change owes the outside world, computed under the monitor and performed after
      * releasing it — so a consumer's continuation never runs while this mailbox, or whatever
      * producer lock encloses it, is held.
      */
    private case class Effects(waiter: Option[Promise[Option[A]]], hook: Boolean)
    private object Effects { val none: Effects = Effects(None, false) }

    protected var pending: Option[Promise[Option[A]]] = None
    protected var closed: Boolean = false
    protected var failure: Option[Throwable] = None
    protected var cancelled: Boolean = false

    /** Set by a state change that has not yet fired its hook, so [[flush]] can. */
    private var hookPending: Boolean = false
    private var hookFired: Boolean = false

    protected def takeNext(): Option[A]

    /** Buffer a value. Called under the monitor; may set [[failure]] to reject the subscription. */
    protected def enqueue(a: A): Unit

    protected def clearBuffer(): Unit

    private def perform(e: Effects, value: Option[A]): Unit = {
        e.waiter.foreach { p =>
            failure match
                case Some(t) => p.failure(t)
                case None    => p.success(value)
        }
        if e.hook then onCancel()
    }

    /** Claim the hook if it has not fired yet. Called under the monitor. */
    private def claimHook(): Boolean =
        if hookFired then false
        else
            hookFired = true
            true

    final def offer(a: A): Unit = {
        offerBuffered(a)
        flush()
    }

    final def offerBuffered(a: A): Unit = synchronized {
        if closed || cancelled || failure.isDefined then ()
        else
            enqueue(a)
            // enqueue may have overflowed and failed the mailbox.
            if failure.isDefined then hookPending = claimHook()
    }

    final def flush(): Unit = {
        val (effects, value) = synchronized {
            val hook = hookPending
            hookPending = false
            failure match
                case Some(_) =>
                    val w = pending
                    pending = None
                    (Effects(w, hook), None)
                case None =>
                    if pending.isEmpty then (Effects(None, hook), None)
                    else
                        takeNext() match
                            case Some(v) =>
                                val w = pending
                                pending = None
                                (Effects(w, hook), Some(v))
                            case None => (Effects(None, hook), None)
        }
        perform(effects, value)
    }

    final def pull(cancelToken: CancelToken): Future[Option[A]] = {
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
            case Left(f) => f
            case Right(p) =>
                if cancelToken.isCancelled then abortPull(p)
                else if !(cancelToken eq CancelToken.never) then
                    val handle = cancelToken.onCancel(() => abortPull(p))
                    // Deregister once the pull resolves, so a long-lived token does not accumulate
                    // one stale closure per event delivered.
                    p.future.onComplete(_ => handle.cancel())(ExecutionContext.parasitic)
                p.future
    }

    /** Abandon an outstanding pull without ending the subscription: clear the waiter slot so the
      * next pull starts fresh, and fail the abandoned future so nothing waits on it forever.
      */
    private def abortPull(p: Promise[Option[A]]): Unit = {
        synchronized {
            if pending.contains(p) then pending = None
        }
        val _ = p.tryFailure(CancelledException("pull cancelled"))
    }

    final def close(): Unit = {
        val effects = synchronized {
            if closed then Effects.none
            else
                closed = true
                val w = pending
                pending = None
                Effects(w, claimHook())
        }
        perform(effects, None)
    }

    final def fail(t: Throwable): Unit = {
        val effects = synchronized {
            if failure.isDefined || closed then Effects.none
            else
                failure = Some(t)
                val w = pending
                pending = None
                Effects(w, claimHook())
        }
        perform(effects, None)
    }

    final def cancel(): Unit = {
        val effects = synchronized {
            if cancelled then Effects.none
            else
                cancelled = true
                clearBuffer()
                val w = pending
                pending = None
                Effects(w, claimHook())
        }
        perform(effects, None)
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
