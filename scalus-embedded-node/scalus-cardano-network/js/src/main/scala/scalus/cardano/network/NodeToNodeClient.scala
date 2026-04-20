package scalus.cardano.network

import scalus.cardano.infra.Timer
import scalus.cardano.network.handshake.NegotiatedVersion

import scala.concurrent.{ExecutionContext, Future}

/** Scala.js stub for [[NodeToNodeClient]]. Scala.js has no raw TCP primitive, so the connect
  * path fails eagerly with [[UnsupportedOperationException]]. A real JS implementation would
  * need a Node `net.Socket`-backed `AsyncByteChannel` (or a WebSocket bridge to Ogmios); until
  * that lands, JS callers should use `ChainSyncSource.BlockfrostPolling` (M13) which is pure
  * HTTPS and works in-process.
  *
  * Keeping the object FQN identical to the JVM version lets cross-built callers (the
  * `Fs2BlockchainStreamProvider` factory, the M5 `ChainApplier` wiring) reference
  * `NodeToNodeClient.connect` uniformly; the JS build compiles cleanly, and a JS caller that
  * actually picks `ChainSyncSource.N2N` gets a descriptive runtime failure.
  */
object NodeToNodeClient {

    /** Default logger. Unused on JS, kept so the JVM and JS call signatures stay identical. */
    private val defaultLogger: scribe.Logger = scribe.Logger("scalus.cardano.n2n.NodeToNodeClient")

    def connect(
        host: String,
        port: Int,
        networkMagic: NetworkMagic,
        config: ClientConfig = ClientConfig.default,
        timer: Timer = null,
        logger: scribe.Logger = defaultLogger
    )(using ExecutionContext): Future[NodeToNodeConnection] =
        Future.failed(
          new UnsupportedOperationException(
            s"NodeToNodeClient.connect($host:$port, magic=$networkMagic) is not available on " +
                s"Scala.js — no raw TCP primitive. Use ChainSyncSource.BlockfrostPolling (M13) " +
                s"or run on JVM."
          )
        )
}
