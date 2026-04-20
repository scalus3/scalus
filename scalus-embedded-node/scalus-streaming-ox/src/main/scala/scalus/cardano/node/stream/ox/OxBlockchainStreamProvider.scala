package scalus.cardano.node.stream.ox

import ox.flow.Flow
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.network.{ChainApplier, ChainApplierHandle, ClientConfig, NetworkMagic, NodeToNodeClient, NodeToNodeConnection}
import scalus.cardano.node.{BlockchainProvider, BlockfrostProvider}
import scalus.cardano.node.stream.{BackupSource, BlockfrostNetwork, ChainSyncSource, StartFrom, StreamProviderConfig, UnsupportedSourceException}
import scalus.cardano.node.stream.BaseStreamProvider
import scalus.cardano.node.stream.engine.Engine

import OxScalusAsyncStream.{Id, given}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/** ox specialisation of [[BaseStreamProvider]].
  *
  * `Id[A] = A`, so `liftFuture` blocks on the future. That matches ox's direct-style story: the
  * caller owns a virtual thread already, so blocking is cheap and ergonomic.
  *
  * `preClose` runs before the engine's subscribers are shut down — used by the N2N wiring path
  * to cancel the [[ChainApplier]] and close the underlying [[NodeToNodeConnection]] so the sync
  * loop unwinds cleanly before subscribers fail.
  */
class OxBlockchainStreamProvider(
    engine: Engine,
    preClose: () => Future[Unit] = () => Future.unit
)(using ExecutionContext)
    extends BaseStreamProvider[Id, Flow](engine) {

    protected def liftFuture[A](fa: => Future[A]): Id[A] =
        Await.result(fa, Duration.Inf)

    protected def pureF[A](a: A): Id[A] = a

    def close(): Id[Unit] = {
        liftFuture(preClose())
        liftFuture(engine.closeAllSubscribers())
    }
}

object OxBlockchainStreamProvider {

    /** Lightweight overload mirroring `Fs2BlockchainStreamProvider.create`. */
    def create(
        cardanoInfo: CardanoInfo,
        chainSync: ChainSyncSource,
        backup: BackupSource
    )(using ExecutionContext): OxBlockchainStreamProvider =
        create(StreamProviderConfig(cardanoInfo, chainSync, backup))

    /** Full configuration. */
    def create(
        config: StreamProviderConfig
    )(using ExecutionContext): OxBlockchainStreamProvider = {
        config.chainSync match
            case ChainSyncSource.Synthetic =>
                val backup = buildBackup(config.backup)
                new OxBlockchainStreamProvider(
                  new Engine(config.cardanoInfo, backup, Engine.DefaultSecurityParam)
                )
            case n: ChainSyncSource.N2N =>
                connectN2N(config, n)
            case ChainSyncSource.N2C(_) =>
                throw UnsupportedSourceException("ChainSyncSource.N2C is not wired until M7")
    }

    /** Open an N2N connection to `n.host:n.port`, spawn a [[ChainApplier]] driving the engine
      * from it, and return a provider whose `close()` tears down the applier + connection +
      * engine subscribers in that order.
      *
      * Blocks on connection establishment (handshake + keep-alive start). Failures there
      * propagate as the original `Future`'s cause via `Await.result`.
      */
    private def connectN2N(
        config: StreamProviderConfig,
        n: ChainSyncSource.N2N
    )(using ExecutionContext): OxBlockchainStreamProvider = {
        val backup = buildBackup(config.backup)
        val engine = new Engine(config.cardanoInfo, backup, Engine.DefaultSecurityParam)
        val conn: NodeToNodeConnection = Await.result(
          NodeToNodeClient.connect(n.host, n.port, NetworkMagic(n.networkMagic), ClientConfig.default),
          Duration.Inf
        )
        val handle: ChainApplierHandle =
            ChainApplier.spawn(conn, engine, startFrom = StartFrom.Tip)

        // Surface applier-loop failures (decode error, NoIntersection, etc.) to subscribers
        // with the typed cause rather than a silent EOS.
        handle.done.onComplete {
            case scala.util.Failure(t) => engine.failAllSubscribers(t)
            case _                     => ()
        }
        // If the connection goes away independently (peer EOF, keep-alive timeout, socket
        // error), collapse the engine's subscribers — with the cause on failure, or graceful
        // close on clean shutdown.
        conn.closed.onComplete {
            case scala.util.Failure(t) => engine.failAllSubscribers(t)
            case _                     => engine.closeAllSubscribers()
        }

        val preClose: () => Future[Unit] = () => {
            // Cancel applier first so the sync loop unwinds before we close the socket.
            for {
                _ <- handle.cancel()
                _ <- conn.close()
            } yield ()
        }
        new OxBlockchainStreamProvider(engine, preClose)
    }

    /** Test-only synthetic helper — no backup, no chain-sync. */
    def synthetic(
        cardanoInfo: CardanoInfo,
        backup: Option[BlockchainProvider] = None
    )(using ExecutionContext): OxBlockchainStreamProvider = {
        val engine = new Engine(cardanoInfo, backup, Engine.DefaultSecurityParam)
        new OxBlockchainStreamProvider(engine)
    }

    private def buildBackup(
        source: BackupSource
    )(using ExecutionContext): Option[BlockchainProvider] = source match
        case BackupSource.Blockfrost(apiKey, network, maxConcurrent) =>
            val fut = network match
                case BlockfrostNetwork.Mainnet => BlockfrostProvider.mainnet(apiKey, maxConcurrent)
                case BlockfrostNetwork.Preview => BlockfrostProvider.preview(apiKey, maxConcurrent)
                case BlockfrostNetwork.Preprod => BlockfrostProvider.preprod(apiKey, maxConcurrent)
            Some(Await.result(fut, Duration.Inf))
        case BackupSource.Custom(provider) => Some(provider)
        case BackupSource.NoBackup         => None
        case BackupSource.LocalStateQuery(_) =>
            throw UnsupportedSourceException("BackupSource.LocalStateQuery is not wired until M11")
}
