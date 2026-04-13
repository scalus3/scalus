package scalus.examples.streaming.fs2batcher

import cats.effect.IO
import scalus.cardano.node.stream.BlockchainStreamProviderTF

import Fs2ScalusAsyncStream.IOStream

/** A `BlockchainStreamProvider` specialised to cats-effect `IO` and the
  * fs2 stream type.
  *
  * Without this specialisation the snapshot side of the generic trait
  * is fixed to `Future`, so calls like `provider.submit(tx)` would
  * return `Future[…]` and require `IO.fromFuture(IO(…))` bridging at
  * every call site. With this trait, an fs2 batcher stays in `IO`
  * end-to-end.
  *
  * Belongs in a future `scalus-streaming-fs2` adapter module.
  */
trait Fs2BlockchainStreamProvider extends BlockchainStreamProviderTF[IO, IOStream]
