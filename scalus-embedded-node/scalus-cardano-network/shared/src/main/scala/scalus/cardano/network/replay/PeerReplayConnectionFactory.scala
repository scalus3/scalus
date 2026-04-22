package scalus.cardano.network.replay

import scalus.cardano.network.{ClientConfig, NetworkMagic, NodeToNodeClient, NodeToNodeConnection}

import scala.concurrent.{ExecutionContext, Future}

/** Opens a transient [[NodeToNodeConnection]] for a [[PeerReplaySource]] prefetch.
  *
  * Each prefetch opens a fresh connection; the source owns its lifetime and closes it when the
  * prefetch finishes (success or failure). Concrete implementations are platform-specific — the JVM
  * provider builds one over [[scalus.cardano.network.NodeToNodeClient.connect]].
  *
  * A transient connection is required — not a second ChainSync route on the live applier's mux —
  * because Ouroboros Node-to-Node specifies exactly one ChainSync state machine per connection.
  */
trait PeerReplayConnectionFactory {

    /** Produce a freshly-handshaken connection ready for ChainSync + BlockFetch. The caller owns
      * the returned connection and must close it; [[PeerReplaySource]] does this at the end of
      * every prefetch, whether the prefetch succeeds or fails.
      */
    def open()(using ExecutionContext): Future[NodeToNodeConnection]
}

object PeerReplayConnectionFactory {

    /** Default factory — each `open()` calls [[NodeToNodeClient.connect]] against the given
      * endpoint. Shared by both the Fs2 and Ox providers (see their `connectN2N`) and by IT.
      */
    def forEndpoint(
        host: String,
        port: Int,
        magic: NetworkMagic,
        config: ClientConfig = ClientConfig.default
    ): PeerReplayConnectionFactory = new PeerReplayConnectionFactory {
        def open()(using ec: ExecutionContext): Future[NodeToNodeConnection] =
            NodeToNodeClient.connect(host, port, magic, config)
    }
}
