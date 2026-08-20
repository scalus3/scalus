package scalus.cardano.node.stream

import scala.concurrent.Future

/** The consumer side of a subscription: pull the next event, or stop pulling.
  *
  * `Future` is the lingua franca here on purpose. It is in the standard library, it exists on both
  * JVM and JS, and it lets every adapter bridge its own effect — cats-effect `IO`, direct-style ox,
  * pekko materialization — without any of those concepts leaking into this module.
  *
  * Buffering lives behind this interface rather than in front of it: coalescing latest-value
  * streams and failing bounded delta buffers on overflow are subtle enough that reimplementing them
  * once per stream library would be several chances to get it wrong.
  */
trait ScalusAsyncSource[A] {

    /** Pull the next signal.
      *
      *   - `Future.successful(Some(a))` — the next value
      *   - `Future.successful(None)` — clean end of stream
      *   - failed `Future` — the producer failed, or a bounded buffer overflowed
      *
      * Single-consumer: exactly one caller pulls at a time, per source.
      */
    def pull(): Future[Option[A]]

    /** No further `pull` calls will happen — the stream was cancelled or the consumer finished.
      * Unregisters the subscription. Idempotent.
      */
    def cancel(): Unit
}

/** Typeclass for building a stream type `S[_]` from a [[ScalusAsyncSource]].
  *
  * One instance per stream library, each a handful of lines. The identity instance below means a
  * caller who does not want to choose a library does not have to.
  */
trait ScalusAsyncStream[S[_]] {
    def fromSource[A](src: ScalusAsyncSource[A]): S[A]
}

object ScalusAsyncStream {

    def apply[S[_]](using s: ScalusAsyncStream[S]): ScalusAsyncStream[S] = s

    /** A [[ScalusAsyncSource]] is already a stream, so
      * `BlockchainStreamProvider[ScalusAsyncSource]` is a complete streaming API with nothing
      * beyond the standard library — consume it with a `pull()` loop, or bridge it wherever. This
      * is the form to reach for in examples, so the first thing a reader meets does not force a
      * choice of stream library.
      */
    given identity: ScalusAsyncStream[ScalusAsyncSource] with {
        def fromSource[A](src: ScalusAsyncSource[A]): ScalusAsyncSource[A] = src
    }
}
