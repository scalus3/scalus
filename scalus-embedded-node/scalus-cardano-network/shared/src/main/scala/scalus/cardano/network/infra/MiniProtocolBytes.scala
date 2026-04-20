package scalus.cardano.network.infra

import scalus.cardano.infra.CancelToken
import scalus.uplc.builtin.ByteString

import scala.concurrent.Future

/** Per-mini-protocol byte-stream handle. The mux reassembles SDUs into a logical byte stream going
  * each way: [[receive]] delivers inbound chunks (whatever the wire gave us, following SDU
  * boundaries but NOT guaranteed to align with mini-protocol message boundaries), [[send]] splits a
  * mini-protocol message into one or more SDUs before handing them to the outbound write queue.
  *
  * The handle has no `close` — tearing a protocol down is a capability of the mux's internal
  * [[RoutingOps]], not of the consumer. End-of-stream surfaces as `receive()` returning `None`;
  * transport failures surface as a failed `Future` from either method.
  *
  * Cancellation: each method takes an optional [[CancelToken]] that is honoured both at entry and
  * mid-wait. When `cancel` fires while a `receive` is pending, the returned `Future` fails with the
  * token's stored cause (or `CancelledException` if none). Callers typically pass their driver's
  * scope token here; per-request deadlines use a linked source.
  *
  * See `docs/local/claude/indexer/n2n-transport.md` § *CBOR framing across SDUs*.
  */
trait MiniProtocolBytes {

    /** Next inbound chunk. `None` signals end-of-stream — peer's MsgDone for this route has been
      * observed OR the connection is gone. A firing `cancel` fails the returned future mid-wait
      * with `CancelledException`. Late-arriving bytes are consumed but discarded (no consumer) —
      * fine for M4, revisit for M5+ if chain-sync retries on the same connection.
      */
    def receive(cancel: CancelToken = CancelToken.never): Future[Option[ByteString]]

    /** Send a whole mini-protocol message. The mux splits payloads larger than
      * [[Sdu.MaxPayloadSize]] across multiple SDUs internally — callers pass the logical message as
      * one `ByteString`. The `cancel` token aborts an in-flight write (honoured by the underlying
      * [[AsyncByteChannel]]).
      */
    def send(message: ByteString, cancel: CancelToken = CancelToken.never): Future[Unit]
}
