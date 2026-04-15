package scalus.cardano.node.stream

import scalus.cardano.node.stream.engine.Mailbox

/** Typeclass witnessing "stream type `S[_]` can be built from a [[Mailbox]]."
  *
  * Each adapter module provides one instance: fs2 wraps the mailbox's `pull()` into a
  * `Stream[IO, A]`; ox wraps it into a `Flow[A]`. The engine hands out mailboxes; the adapter turns
  * them into streams on demand.
  */
trait MailboxSource[S[_]] {

    /** Build a consumer-side stream that pulls sequentially from the mailbox until the mailbox is
      * closed or fails.
      */
    def fromMailbox[A](mailbox: Mailbox[A]): S[A]
}
