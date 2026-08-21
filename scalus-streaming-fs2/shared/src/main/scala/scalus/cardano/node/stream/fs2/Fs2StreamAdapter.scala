package scalus.cardano.node.stream.fs2

import cats.effect.{IO, Resource}
import scalus.cardano.infra.CancelSource
import fs2.Stream
import scalus.cardano.node.stream.{ScalusAsyncSource, ScalusAsyncStreamAdapter}

/** fs2 / cats-effect adapter for the streaming facade.
  *
  * The whole adapter is the instance below plus one resource helper — everything subtle about
  * buffering, coalescing and overflow lives behind [[ScalusAsyncSource]], so there is nothing here
  * to get subtly different from the ox and pekko adapters.
  */
object Fs2StreamAdapter {

    type IOStream[A] = Stream[IO, A]

    given fs2Adapter: ScalusAsyncStreamAdapter[IOStream] with {
        def fromSource[A](src: ScalusAsyncSource[A]): Stream[IO, A] =
            Stream.repeatEval(pull(src)).unNoneTerminate.onFinalize(IO(src.cancel()))
    }

    /** One cancellable pull.
      *
      * `IO.fromFuture` alone would be a trap here: it parks on an uncancelable async node, so a
      * stream waiting on a quiet chain could not be interrupted at all — `.timeout` would never
      * fire, a cancelled `Resource.use` would hang, and the finalizer that would have woken the
      * pull cannot run until the cancellation it is waiting on completes. Passing a
      * [[CancelSource]] through to the source gives cats-effect something real to pull the plug on.
      */
    private def pull[A](src: ScalusAsyncSource[A]): IO[Option[A]] =
        IO.fromFutureCancelable(IO {
            val cancellation = CancelSource()
            (src.pull(cancellation.token), IO(cancellation.cancel()))
        })

    extension [A](src: ScalusAsyncSource[A]) {

        /** The subscription as an fs2 stream. Cancellation travels the other way: when the consumer
          * stops pulling, the finalizer cancels the source, which unregisters the subscription.
          */
        def toStream: Stream[IO, A] = fs2Adapter.fromSource(src)
    }

    /** A subscription as a resource.
      *
      * The facade registers subscriptions eagerly — that is what makes `subscribe` then `submit`
      * race-free — which means a subscription nobody consumes still accumulates events. Acquiring
      * through a `Resource` is how an fs2 program discharges that obligation: the release
      * unregisters whether or not the stream was ever run.
      *
      * {{{
      * Fs2StreamAdapter.subscribe(IO(provider.subscribeUtxoQuery(query))).use { events =>
      *     events.take(10).compile.toList
      * }
      * }}}
      */
    def subscribe[A](acquire: IO[ScalusAsyncSource[A]]): Resource[IO, Stream[IO, A]] =
        Resource.make(acquire)(src => IO(src.cancel())).map(fs2Adapter.fromSource)
}
