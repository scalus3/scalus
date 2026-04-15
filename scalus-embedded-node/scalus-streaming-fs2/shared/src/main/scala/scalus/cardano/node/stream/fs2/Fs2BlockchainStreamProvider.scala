package scalus.cardano.node.stream.fs2

import cats.effect.IO
import cats.effect.std.Dispatcher
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.node.{BlockchainProvider, BlockfrostProvider}
import scalus.cardano.node.stream.{BackupSource, BlockfrostNetwork, ChainSyncSource, StreamProviderConfig, UnsupportedSourceException}
import scalus.cardano.node.stream.BaseStreamProvider
import scalus.cardano.node.stream.engine.Engine

import Fs2ScalusAsyncStream.{IOStream, given}

import scala.concurrent.{ExecutionContext, Future}

/** fs2 / cats-effect specialisation of [[BaseStreamProvider]]. */
class Fs2BlockchainStreamProvider(engine: Engine) extends BaseStreamProvider[IO, IOStream](engine) {

    protected def liftFuture[A](fa: => Future[A]): IO[A] = IO.fromFuture(IO(fa))

    protected def pureF[A](a: A): IO[A] = IO.pure(a)

    def close(): IO[Unit] = liftFuture(engine.closeAllSubscribers())
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
            case ChainSyncSource.Synthetic => ()
            case ChainSyncSource.N2N(_, _) =>
                return IO.raiseError(
                  UnsupportedSourceException("ChainSyncSource.N2N is not wired until M4/M5")
                )
            case ChainSyncSource.N2C(_) =>
                return IO.raiseError(
                  UnsupportedSourceException("ChainSyncSource.N2C is not wired until M7")
                )

        buildBackup(config.backup).map { backup =>
            new Fs2BlockchainStreamProvider(
              new Engine(config.cardanoInfo, backup, Engine.DefaultSecurityParam)
            )
        }
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
