package scalus.cardano.node.stream

import scalus.cardano.infra.CancelToken

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

    /** Pull the next signal, abortable through `cancelToken`.
      *
      *   - `Future.successful(Some(a))` — the next value
      *   - `Future.successful(None)` — clean end of stream
      *   - failed `Future` — the producer failed, a bounded buffer overflowed, or the token fired
      *     (a [[scalus.cardano.infra.CancelledException]])
      *
      * The token is how a `Future`-returning API becomes abortable at all. `Future` has no
      * cancellation of its own, so an effect system that needs to interrupt a parked pull — an fs2
      * `.timeout`, a cancelled `Resource.use`, a runtime shutting down — has nothing to pull the
      * plug on unless the capability is passed in. Adapters bridge their own cancellation onto this
      * token; without it they can only mask cancellation and hang.
      *
      * Cancelling a pull does **not** end the subscription: the next `pull` starts a fresh wait.
      * [[cancel]] is what ends the subscription.
      *
      * Single-consumer: exactly one caller pulls at a time, per source. Pulling again while a pull
      * is outstanding returns the same future — so cancelling one cancels both.
      */
    def pull(cancelToken: CancelToken): Future[Option[A]]

    /** Pull with nothing to cancel on. Convenient for synchronous callers and tests; anything that
      * may need to interrupt a parked wait should pass a real token.
      */
    final def pull(): Future[Option[A]] = pull(CancelToken.never)

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
trait ScalusAsyncStreamAdapter[S[_]] {
    def fromSource[A](src: ScalusAsyncSource[A]): S[A]
}

object ScalusAsyncStreamAdapter {

    /** A [[ScalusAsyncSource]] is already a stream, so `BlockchainStreaming[ScalusAsyncSource]` is
      * a complete streaming API with nothing beyond the standard library — consume it with a
      * `pull()` loop, or bridge it wherever. This is the form to reach for in examples, so the
      * first thing a reader meets does not force a choice of stream library.
      */
    given identity: ScalusAsyncStreamAdapter[ScalusAsyncSource] with {
        def fromSource[A](src: ScalusAsyncSource[A]): ScalusAsyncSource[A] = src
    }
}
