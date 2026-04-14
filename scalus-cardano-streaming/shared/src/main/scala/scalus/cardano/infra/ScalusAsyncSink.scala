package scalus.cardano.infra

import scala.concurrent.Future

/** Producer-side endpoint of an async channel allocated by
  * [[ScalusAsyncStream.channel]].
  *
  * The data-node engine (or, in tests, a synthetic source) holds a sink
  * and pushes events into it. Each operation returns a `Future[Unit]` that
  * completes when the channel has accepted the call:
  *
  *   - `offer` completes when the consumer-side buffer has room for the
  *     element. This is the backpressure signal — the producer naturally
  *     paces itself by awaiting the future before sending the next event.
  *   - `complete` completes when the end-of-stream marker has been
  *     accepted. Subsequent `offer` calls fail.
  *   - `fail` completes when the error marker has been accepted. The
  *     consumer-facing stream terminates with `t`. Subsequent `offer`
  *     calls fail.
  *
  * `Future` is the lingua franca: it is in stdlib, available on JVM and
  * JS, and lets adapters bridge their native effect (cats-effect `IO`,
  * direct-style ox) without any of those concepts leaking into the core
  * interface.
  */
trait ScalusAsyncSink[A] {

    /** Push an event into the channel. The returned future completes
      * once the consumer-side buffer has accepted it (real backpressure).
      * Fails if the channel is already closed via [[complete]] or
      * [[fail]] or the consumer cancelled.
      */
    def offer(a: A): Future[Unit]

    /** Signal clean end-of-stream. Idempotent. After this, subsequent
      * `offer` calls return a failed future.
      */
    def complete(): Future[Unit]

    /** Signal a fatal error. The consumer-facing stream terminates with
      * `t`. Idempotent. After this, subsequent `offer` calls return a
      * failed future.
      */
    def fail(t: Throwable): Future[Unit]
}

/** Thrown by sink operations after the channel has been closed (via
  * [[ScalusAsyncSink.complete]], [[ScalusAsyncSink.fail]], or because
  * the consumer-facing stream was cancelled).
  */
class ScalusSinkClosedException(message: String = "sink closed")
    extends RuntimeException(message)

/** Thrown into the consumer stream when the producer overflowed a
  * `BufferPolicy.Bounded` channel configured with `Overflow.Fail`.
  */
class ScalusBufferOverflowException(message: String = "subscriber buffer overflow")
    extends RuntimeException(message)
