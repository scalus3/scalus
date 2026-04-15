package scalus.examples.streaming.fs2batcher

import cats.effect.IO
import fs2.Stream
import scalus.cardano.node.stream.MailboxSource
import scalus.cardano.node.stream.engine.Mailbox

/** fs2 / cats-effect adapter: build a [[Stream]] by pulling events from an engine-owned
  * [[Mailbox]]. Single-consumer per mailbox — the stream is the only thing calling `pull()`.
  *
  * Cancellation goes the other direction: when the consumer stops pulling (stream drained, `take`,
  * error, etc.), the fs2 `onFinalize` hook calls `mailbox.cancel()`, which fires the engine's
  * `onCancel` and removes the subscription.
  */
object Fs2ScalusAsyncStream {

    type IOStream[A] = Stream[IO, A]

    given fs2MailboxSource: MailboxSource[IOStream] = new MailboxSource[IOStream] {
        def fromMailbox[A](mailbox: Mailbox[A]): Stream[IO, A] =
            Stream
                .repeatEval(IO.fromFuture(IO(mailbox.pull())))
                .takeWhile(_.isDefined)
                .map(_.get)
                .onFinalize(IO(mailbox.cancel()))
    }
}
