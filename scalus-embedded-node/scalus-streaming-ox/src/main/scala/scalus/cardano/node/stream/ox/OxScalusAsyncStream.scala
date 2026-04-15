package scalus.cardano.node.stream.ox

import ox.flow.Flow
import scalus.cardano.node.stream.MailboxSource
import scalus.cardano.node.stream.engine.Mailbox

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/** ox adapter: build a `Flow[A]` that pulls from a [[Mailbox]].
  *
  * ox is direct-style so we simply `Await.result` on each `pull()`. Virtual-thread scheduling keeps
  * the blocking call cheap; the carrier thread is free while the mailbox is empty. Cancellation
  * wiring: when the flow consumer stops pulling, calls `cancel()` via the final onComplete hook.
  */
object OxScalusAsyncStream {

    /** Identity effect — ox is direct-style. */
    type Id[A] = A

    given oxMailboxSource: MailboxSource[Flow] = new MailboxSource[Flow] {
        def fromMailbox[A](mailbox: Mailbox[A]): Flow[A] =
            Flow
                .repeatEvalWhileDefined[A](Await.result(mailbox.pull(), Duration.Inf))
                .onComplete(mailbox.cancel())
    }
}
