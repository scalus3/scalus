package scalus.examples.streaming.fs2batcher

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.std.Queue
import fs2.Stream
import scalus.cardano.infra.{
    ScalusAsyncSink,
    ScalusAsyncStream,
    ScalusBufferOverflowException,
    ScalusSinkClosedException
}
import scalus.cardano.node.stream.{BufferPolicy, Overflow}

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{ExecutionContext, Future}

/** fs2 / cats-effect `IO` adapter for [[ScalusAsyncStream]]. Cross-built
  * for JVM and JS — the queue allocation is asynchronous (returns a
  * `Future` from `Dispatcher.unsafeToFuture`) rather than blocking via
  * `unsafeRunSync`, so it works on the JS event loop too.
  *
  * Each channel is backed by a `cats.effect.std.Queue[IO, Option[Either[Throwable, A]]]`
  * carrying:
  *   - `Some(Right(a))` for a delivered event,
  *   - `Some(Left(t))` for a producer-signalled failure,
  *   - `None` for clean end-of-stream.
  *
  * The sink's operations all flow through `dispatcher.unsafeToFuture`,
  * never `unsafeRunSync`. The consumer-facing stream uses
  * `Stream.eval(IO.fromFuture(IO(queueFuture)))` to inject the
  * eventually-available queue.
  *
  * The given instance requires both a `Dispatcher[IO]` and an
  * `ExecutionContext` to be in scope. Callers own both lifecycles —
  * typically `Dispatcher.parallel[IO].use { ... }` plus the standard
  * `scala.concurrent.ExecutionContext.global` (or any other EC).
  */
object Fs2ScalusAsyncStream {

    /** Identity-shape alias matching the fs2 stream type. Lets the
      * provider trait be specialised to a concrete `S[_]`.
      */
    type IOStream[A] = Stream[IO, A]

    given fs2AsyncStream(using
        dispatcher: Dispatcher[IO],
        ec: ExecutionContext
    ): ScalusAsyncStream[IOStream] =
        new ScalusAsyncStream[IOStream] {
            def channel[A](
                bufferPolicy: BufferPolicy,
                onCancel: () => Unit
            ): (ScalusAsyncSink[A], IOStream[A]) = {

                val queueIO: IO[Queue[IO, Option[Either[Throwable, A]]]] =
                    bufferPolicy match {
                        case BufferPolicy.Bounded(size, _) =>
                            Queue.bounded[IO, Option[Either[Throwable, A]]](size)
                        case BufferPolicy.Unbounded =>
                            Queue.unbounded[IO, Option[Either[Throwable, A]]]
                    }

                // Async queue allocation: works on JVM and JS. The
                // Future is shared between sink and stream; both wait
                // for it to complete before touching the queue.
                val queueFuture: Future[Queue[IO, Option[Either[Throwable, A]]]] =
                    dispatcher.unsafeToFuture(queueIO)

                val closed = new AtomicBoolean(false)

                val sink = new ScalusAsyncSink[A] {
                    private def withQueue[T](
                        f: Queue[IO, Option[Either[Throwable, A]]] => IO[T]
                    ): Future[T] =
                        queueFuture.flatMap(q => dispatcher.unsafeToFuture(f(q)))

                    def offer(a: A): Future[Unit] =
                        if closed.get() then Future.failed(new ScalusSinkClosedException())
                        else
                            bufferPolicy match {
                                case BufferPolicy.Bounded(_, Overflow.Fail) =>
                                    withQueue { q =>
                                        q.tryOffer(Some(Right(a))).flatMap {
                                            case true  => IO.unit
                                            case false =>
                                                val ex = new ScalusBufferOverflowException()
                                                IO(closed.set(true)) *>
                                                    q.tryOffer(Some(Left(ex))) *>
                                                    IO.raiseError(ex)
                                        }
                                    }

                                case BufferPolicy.Bounded(_, Overflow.DropNewest) =>
                                    withQueue(_.tryOffer(Some(Right(a))).void)

                                case BufferPolicy.Bounded(_, Overflow.DropOldest) =>
                                    withQueue { q =>
                                        q.tryOffer(Some(Right(a))).flatMap {
                                            case true  => IO.unit
                                            case false =>
                                                q.tryTake *>
                                                    q.tryOffer(Some(Right(a))).void
                                        }
                                    }

                                case BufferPolicy.Unbounded =>
                                    withQueue(_.offer(Some(Right(a))))
                            }

                    def complete(): Future[Unit] =
                        if closed.compareAndSet(false, true) then withQueue(_.offer(None))
                        else Future.unit

                    def fail(t: Throwable): Future[Unit] =
                        if closed.compareAndSet(false, true) then
                            withQueue(_.offer(Some(Left(t))))
                        else Future.unit
                }

                val stream: IOStream[A] =
                    Stream
                        .eval(IO.fromFuture(IO(queueFuture)))
                        .flatMap { q =>
                            Stream
                                .fromQueueNoneTerminated(q)
                                .rethrow
                        }
                        .onFinalize(IO {
                            closed.set(true)
                            onCancel()
                        })

                (sink, stream)
            }
        }
}
