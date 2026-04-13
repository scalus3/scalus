package scalus.examples.streaming.fs2batcher

import cats.effect.IO
import fs2.Stream
import scalus.cardano.infra.ScalusAsyncStream

/** fs2 / cats-effect `IO` instance of `ScalusAsyncStream`.
  *
  * `ScalusAsyncStream` is currently producer-only and exposes no
  * methods, so any constructor satisfies it. This trivial instance will
  * be replaced by the real fs2 adapter once producer methods land on
  * the typeclass.
  */
object Fs2ScalusAsyncStream {

    type IOStream[A] = Stream[IO, A]

    given ScalusAsyncStream[IOStream] = new ScalusAsyncStream[IOStream] {}
}
