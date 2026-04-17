package scalus.cardano.n2n.jvm

import scalus.cardano.infra.jvm.JvmTimer
import scalus.cardano.infra.{CancelSource, CancelToken, CancelledException, Timer}
import scalus.cardano.n2n.*
import scalus.cardano.n2n.handshake.{HandshakeDriver, NegotiatedVersion}
import scalus.cardano.n2n.keepalive.KeepAliveDriver

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal

/** JVM factory for [[NodeToNodeConnection]]. Opens a TCP socket, runs handshake, starts the
  * keep-alive loop, and returns the live connection. See
  * `docs/local/claude/indexer/n2n-transport.md` § *The public surface of M4*.
  */
object NodeToNodeClient {

    private val defaultLogger: scribe.Logger = scribe.Logger("scalus.cardano.n2n.NodeToNodeClient")

    /** Establish a Node-to-Node connection. The returned future completes once the handshake is
      * accepted and the keep-alive loop is running. Any failure before that point (socket error,
      * handshake refusal, handshake timeout) fails the returned future and tears down all transient
      * resources.
      */
    def connect(
        host: String,
        port: Int,
        networkMagic: NetworkMagic,
        config: ClientConfig = ClientConfig.default,
        timer: Timer = JvmTimer.shared,
        logger: scribe.Logger = defaultLogger
    )(using ExecutionContext): Future[NodeToNodeConnection] = {
        for
            channel <- JvmAsyncByteChannel.connect(host, port)
            connection <- buildConnection(channel, networkMagic, config, timer, logger)
                .recoverWith { case NonFatal(t) =>
                    // Handshake or wiring failed — close the socket we just opened and surface
                    // the original failure.
                    channel.close().transformWith(_ => Future.failed(t))
                }
        yield connection
    }

    private def buildConnection(
        channel: AsyncByteChannel,
        networkMagic: NetworkMagic,
        config: ClientConfig,
        timer: Timer,
        logger: scribe.Logger
    )(using ExecutionContext): Future[NodeToNodeConnection] = {
        val connectionRoot = CancelSource()
        val mux = new Multiplexer(
          channel,
          connectionRoot,
          MuxConfig(
            sduMaxPayload = config.sduMaxPayload,
            mailboxCapacity = config.mailboxCapacity
          ),
          logger = logger
        )

        // Mux reader completing (clean EOF or fault) triggers teardown so `closed` fires.
        mux.closed.onComplete {
            case scala.util.Success(_) =>
                if !connectionRoot.token.isCancelled then
                    connectionRoot.cancel(new AsyncByteChannel.ChannelClosedException("peer EOF"))
            case scala.util.Failure(t) =>
                if !connectionRoot.token.isCancelled then connectionRoot.cancel(t)
        }

        // Ensure the socket closes on any teardown, not just explicit close().
        val _ = connectionRoot.token.onCancel { () =>
            val _ = channel.close()
        }

        // Handshake uses an isolated linked scope — timeout on handshake does NOT take down
        // connection root. Handshake refusal is a different story (policy in HandshakeDriver).
        val handshakeScope = CancelSource.linkedTo(connectionRoot.token)
        val handshakeHandle = mux.channel(MiniProtocolId.Handshake)

        val handshakeFuture = HandshakeDriver.run(
          handshakeHandle,
          networkMagic,
          handshakeScope,
          timer,
          timeout = config.handshakeTimeout,
          logger = logger
        )

        handshakeFuture.map { negotiated =>
            logger.info(s"handshake complete, negotiated v${negotiated.version}")
            val rttRef = new AtomicReference[Option[FiniteDuration]](None)
            startKeepAlive(mux, connectionRoot, timer, config, logger, rttRef)
            buildConnectionObject(mux, channel, connectionRoot, negotiated, rttRef)
        }
    }

    // Starts keep-alive on a background Future. Errors (beat timeout / cookie mismatch) fire
    // `connectionRoot` internally; we don't observe the returned future beyond that.
    private def startKeepAlive(
        mux: Multiplexer,
        connectionRoot: CancelSource,
        timer: Timer,
        config: ClientConfig,
        logger: scribe.Logger,
        rttRef: AtomicReference[Option[FiniteDuration]]
    )(using ExecutionContext): Unit = {
        val keepAliveHandle = mux.channel(MiniProtocolId.KeepAlive)
        val _ = KeepAliveDriver.run(
          keepAliveHandle,
          connectionRoot,
          timer,
          rttRef = rttRef,
          interval = config.keepAliveInterval,
          beatTimeout = config.keepAliveTimeout,
          logger = logger
        )
    }

    private def buildConnectionObject(
        mux: Multiplexer,
        channel: AsyncByteChannel,
        connectionRoot: CancelSource,
        negotiated: NegotiatedVersion,
        rttRef: AtomicReference[Option[FiniteDuration]]
    )(using ExecutionContext): NodeToNodeConnection = {
        val closedPromise = Promise[Unit]()
        val _ = connectionRoot.token.onCancel { () =>
            val cause = connectionRoot.token.cause
                .getOrElse(new CancelledException("connection closed"))
            cause match {
                case _: AsyncByteChannel.ChannelClosedException =>
                    val _ = closedPromise.trySuccess(())
                case other =>
                    val _ = closedPromise.tryFailure(other)
            }
        }

        new NodeToNodeConnection {
            def negotiatedVersion: NegotiatedVersion = negotiated
            def channel(proto: MiniProtocolId): MiniProtocolBytes = mux.channel(proto)
            def rootToken: CancelToken = connectionRoot.token
            def rtt: Option[FiniteDuration] = rttRef.get
            def closed: Future[Unit] = closedPromise.future
            def close(): Future[Unit] = {
                if !connectionRoot.token.isCancelled then
                    connectionRoot.cancel(new AsyncByteChannel.ChannelClosedException("user close"))
                // `closedPromise` completes via the onCancel listener; we recover the failure so
                // close() always succeeds (the caller asked for close, not for the cause to
                // propagate).
                closedPromise.future.recover { case _ => () }
            }
        }
    }
}
