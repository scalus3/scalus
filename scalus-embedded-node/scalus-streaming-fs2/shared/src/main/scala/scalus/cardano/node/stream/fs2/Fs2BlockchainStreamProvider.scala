package scalus.cardano.node.stream.fs2

import cats.effect.IO
import cats.effect.std.Dispatcher
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.network.{ChainApplier, ChainApplierHandle, ClientConfig, NetworkMagic, NodeToNodeClient, NodeToNodeConnection}
import scalus.cardano.node.{BlockchainProvider, BlockfrostProvider}
import scalus.cardano.node.stream.{BackupSource, BlockfrostNetwork, ChainSyncSource, StartFrom, StreamProviderConfig, UnsupportedSourceException}
import scalus.cardano.node.stream.BaseStreamProvider
import scalus.cardano.node.stream.engine.Engine

import Fs2ScalusAsyncStream.{IOStream, given}

import scala.concurrent.{ExecutionContext, Future}

/** fs2 / cats-effect specialisation of [[BaseStreamProvider]].
  *
  * `preClose` runs before the engine's subscribers are shut down — used by the N2N wiring path
  * to cancel the [[ChainApplier]] and close the underlying [[NodeToNodeConnection]] so the sync
  * loop unwinds cleanly before subscribers fail.
  */
class Fs2BlockchainStreamProvider(
    engine: Engine,
    preClose: () => Future[Unit] = () => Future.unit
) extends BaseStreamProvider[IO, IOStream](engine) {

    protected def liftFuture[A](fa: => Future[A]): IO[A] = IO.fromFuture(IO(fa))

    protected def pureF[A](a: A): IO[A] = IO.pure(a)

    def close(): IO[Unit] =
        liftFuture(preClose()) *> liftFuture(engine.closeAllSubscribers())
}

object Fs2BlockchainStreamProvider {

    def create(
        cardanoInfo: CardanoInfo,
        chainSync: ChainSyncSource,
        backup: BackupSource
    )(using Dispatcher[IO], ExecutionContext): IO[Fs2BlockchainStreamProvider] =
        create(StreamProviderConfig(cardanoInfo, chainSync, backup))

    def create(
        config: StreamProviderConfig
    )(using Dispatcher[IO], ExecutionContext): IO[Fs2BlockchainStreamProvider] = {
        config.chainSync match
            case ChainSyncSource.Synthetic =>
                buildBackup(config.backup).map { backup =>
                    new Fs2BlockchainStreamProvider(
                      new Engine(config.cardanoInfo, backup, Engine.DefaultSecurityParam)
                    )
                }
            case n: ChainSyncSource.N2N =>
                connectN2N(config, n)
            case ChainSyncSource.N2C(_) =>
                IO.raiseError(
                  UnsupportedSourceException("ChainSyncSource.N2C is not wired until M7")
                )
    }

    /** Open an N2N connection to `n.host:n.port`, spawn a [[ChainApplier]] driving the engine
      * from it, and return a provider whose `close()` tears down applier + connection + engine
      * subscribers in order.
      *
      * On JVM [[NodeToNodeClient.connect]] opens a real TCP socket; on Scala.js it currently
      * fails eagerly — see the `NodeToNodeClient` JS stub. Either outcome is reported via the
      * returned `IO`'s error channel.
      */
    private def connectN2N(
        config: StreamProviderConfig,
        n: ChainSyncSource.N2N
    )(using Dispatcher[IO], ExecutionContext): IO[Fs2BlockchainStreamProvider] =
        for {
            backup <- buildBackup(config.backup)
            conn <- IO.fromFuture(
              IO(
                NodeToNodeClient.connect(
                  n.host,
                  n.port,
                  NetworkMagic(n.networkMagic),
                  ClientConfig.default
                )
              )
            )
            engine = new Engine(config.cardanoInfo, backup, Engine.DefaultSecurityParam)
            handle = ChainApplier.spawn(conn, engine, startFrom = StartFrom.Tip)
            _ = handle.done.onComplete {
                case scala.util.Failure(t) =>
                    // Surface the typed cause to subscribers so their stream fails with the
                    // real error instead of a silent EOS. Aligns with the design doc's error
                    // model ("consumers observe the stored cause of the root cancel").
                    engine.failAllSubscribers(t)
                case _ => ()
            }
            _ = conn.closed.onComplete {
                case scala.util.Failure(t) => engine.failAllSubscribers(t)
                case _                     => engine.closeAllSubscribers()
            }
        } yield {
            val preClose: () => Future[Unit] = () =>
                handle.cancel().flatMap(_ => conn.close())
            new Fs2BlockchainStreamProvider(engine, preClose)
        }

    /** Test-only synthetic helper — no backup, no chain-sync. */
    def synthetic(
        cardanoInfo: CardanoInfo,
        backup: Option[BlockchainProvider] = None
    ): Fs2BlockchainStreamProvider = {
        val engine = new Engine(cardanoInfo, backup, Engine.DefaultSecurityParam)
        new Fs2BlockchainStreamProvider(engine)
    }

    private def buildBackup(
        source: BackupSource
    )(using ExecutionContext): IO[Option[BlockchainProvider]] = source match
        case BackupSource.Blockfrost(apiKey, network, maxConcurrent) =>
            IO.fromFuture(IO(network match {
                case BlockfrostNetwork.Mainnet => BlockfrostProvider.mainnet(apiKey, maxConcurrent)
                case BlockfrostNetwork.Preview => BlockfrostProvider.preview(apiKey, maxConcurrent)
                case BlockfrostNetwork.Preprod => BlockfrostProvider.preprod(apiKey, maxConcurrent)
            })).map(Some(_))
        case BackupSource.Custom(provider) =>
            IO.pure(Some(provider))
        case BackupSource.NoBackup =>
            IO.pure(None)
        case BackupSource.LocalStateQuery(_) =>
            IO.raiseError(
              UnsupportedSourceException("BackupSource.LocalStateQuery is not wired until M11")
            )
}
