package scalus.examples.streaming.oxbatcher

import ox.channels.{Channel, ChannelClosed}
import ox.flow.Flow
import scalus.cardano.infra.{ScalusAsyncSink, ScalusAsyncStream, ScalusSinkClosedException}
import scalus.cardano.node.stream.BufferPolicy

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{ExecutionContext, Future}

/** ox `Flow` adapter for [[ScalusAsyncStream]].
  *
  * Each channel is backed by an `ox.channels.Channel[A]`; producer-side
  * signalling uses the channel's native `done()` / `error(t)`, so
  * elements travel as bare `A` rather than wrapped in
  * `Either[Throwable, Option[A]]` (the way fs2 handles it). The
  * consumer-facing `Flow[A]` is `Flow.fromSource(channel)` plus an
  * `onComplete` finalizer that wires the cancel hook back into the
  * provider registry.
  *
  * `offer / complete / fail` need to return `Future[Unit]` from
  * synchronous, blocking ox primitives. Each call submits a tiny task
  * to a virtual-thread executor which then invokes the blocking ox
  * call; the carrier thread stays free, and backpressure works
  * naturally because the virtual thread parks on `channel.send` when
  * the buffer is full.
  *
  * The given instance is process-global because it doesn't need an
  * `Ox` capability — `Flow.fromSource` doesn't run anything until
  * consumed, and channels live independently of any Ox scope.
  */
object OxScalusAsyncStream {

    /** Identity effect: ox is direct-style, so `provider.submit(tx)`
      * returns `Either[SubmitError, TransactionHash]` without an outer
      * wrapper.
      */
    type Id[A] = A

    /** Shared virtual-thread executor used to convert ox's blocking
      * `Channel.send / done / error` calls into `Future[Unit]`. Each
      * submitted task is a single short-lived virtual thread, so there
      * is no carrier-thread pinning; the underlying ox calls park
      * gracefully when waiting for buffer space.
      */
    private lazy val sinkEC: ExecutionContext =
        ExecutionContext.fromExecutorService(
          Executors.newVirtualThreadPerTaskExecutor()
        )

    given oxAsyncStream: ScalusAsyncStream[Flow] = new ScalusAsyncStream[Flow] {
        def channel[A](
            bufferPolicy: BufferPolicy,
            onCancel: () => Unit
        ): (ScalusAsyncSink[A], Flow[A]) = {
            // ox channels don't expose overflow strategies; we honour
            // the size bound and fall back to backpressure regardless
            // of the requested overflow policy.
            val ch = bufferPolicy match {
                case BufferPolicy.Bounded(size, _) => Channel.buffered[A](size)
                case BufferPolicy.Unbounded        => Channel.unlimited[A]
            }
            val closed = new AtomicBoolean(false)

            val sink = new ScalusAsyncSink[A] {
                def offer(a: A): Future[Unit] =
                    if closed.get() then Future.failed(new ScalusSinkClosedException())
                    else
                        Future {
                            ch.sendOrClosed(a) match {
                                case _: ChannelClosed =>
                                    throw new ScalusSinkClosedException("channel closed")
                                case _ => ()
                            }
                        }(sinkEC)

                def complete(): Future[Unit] =
                    if closed.compareAndSet(false, true) then
                        Future { ch.doneOrClosed(); () }(sinkEC)
                    else Future.unit

                def fail(t: Throwable): Future[Unit] =
                    if closed.compareAndSet(false, true) then
                        Future { ch.errorOrClosed(t); () }(sinkEC)
                    else Future.unit
            }

            val flow: Flow[A] = Flow.fromSource(ch).onComplete {
                closed.set(true)
                onCancel()
            }

            (sink, flow)
        }
    }
}
