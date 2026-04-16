package scalus.cardano.n2n

import scalus.uplc.builtin.ByteString

import scala.concurrent.Future

/** Per-mini-protocol byte-stream handle. The mux reassembles SDUs into a logical byte stream going
  * each way: [[receive]] delivers inbound chunks (whatever the wire gave us, following SDU
  * boundaries but NOT guaranteed to align with mini-protocol message boundaries), [[send]] splits a
  * mini-protocol message into one or more SDUs before handing them to the outbound write queue.
  *
  * The handle has no `close` — tearing a protocol down is a capability of the mux's internal
  * [[RoutingOps]], not of the consumer. Consumers observe termination via [[scope]]: when the
  * protocol-level `CancelSource` fires, [[receive]] / [[send]] calls carrying [[scope]] as cancel
  * complete with [[CancelledException]] and the owning state machine's own scope fires in turn.
  *
  * See `docs/local/claude/indexer/n2n-transport.md` § *CBOR framing across SDUs*.
  */
trait MiniProtocolBytes {

    /** Protocol-level cancel scope. Cancelled when the mux tears this protocol down, the connection
      * root cancels, or the owning state machine triggers its own source.
      */
    def scope: CancelToken

    /** Next inbound chunk. `None` signals end-of-stream — peer's MsgDone for this route has been
      * observed OR the connection is gone. Default `cancel` is [[scope]]; state machines may pass a
      * narrower linked token for per-call cancellation (e.g. request timeout).
      */
    def receive(cancel: CancelToken = scope): Future[Option[ByteString]]

    /** Send a whole mini-protocol message. The mux splits payloads larger than
      * [[Sdu.MaxPayloadSize]] across multiple SDUs internally — callers pass the logical message as
      * one `ByteString`.
      */
    def send(message: ByteString, cancel: CancelToken = scope): Future[Unit]
}
