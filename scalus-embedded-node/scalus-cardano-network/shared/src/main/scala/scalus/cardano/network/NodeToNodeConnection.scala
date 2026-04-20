package scalus.cardano.network

import scalus.cardano.infra.CancelToken
import scalus.cardano.network.handshake.NegotiatedVersion
import scalus.cardano.network.infra.{MiniProtocolBytes, MiniProtocolId}

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

/** A live Node-to-Node connection: post-handshake, keep-alive running, mini-protocol byte channels
  * available.
  *
  * The connection is obtained via a platform factory (e.g.
  * `scalus.cardano.network.jvm.NodeToNodeClient.connect`) and lives until either the peer / transport
  * terminates it or the caller invokes [[close]]. Lifecycle events are observable via [[rootToken]]
  * and [[closed]].
  *
  * See `docs/local/claude/indexer/n2n-transport.md` § *The public surface of M4*.
  */
trait NodeToNodeConnection {

    /** The version negotiated during handshake (v14 or v16 for M4). */
    def negotiatedVersion: NegotiatedVersion

    /** Per-mini-protocol byte-stream handle. Calls for the same `proto` return the same handle —
      * idempotent. M4 consumes [[MiniProtocolId.KeepAlive]] internally; others are handed off to
      * M5+ state machines (chain-sync, block-fetch, tx-submission).
      */
    def channel(proto: MiniProtocolId): MiniProtocolBytes

    /** Observable root token — cancelled on any connection-fatal event (peer EOF, socket error,
      * keep-alive timeout, cookie mismatch, explicit [[close]]). The cause is stored on the token
      * and is also the completion value of [[closed]].
      */
    def rootToken: CancelToken

    /** Most recent round-trip time from the keep-alive loop. `None` until the first beat completes.
      */
    def rtt: Option[FiniteDuration]

    /** Completes when the connection closes. Succeeds on user-initiated [[close]] or clean peer
      * EOF; fails with the stored cause on any fault path (handshake error, keep-alive timeout,
      * transport exception).
      */
    def closed: Future[Unit]

    /** Tear down the connection. Fires the connection root, causing every subscriber to see the
      * teardown via their observed scopes; subsequently closes the underlying socket and drains
      * pending operations. The returned future completes when cleanup finishes.
      */
    def close(): Future[Unit]
}
