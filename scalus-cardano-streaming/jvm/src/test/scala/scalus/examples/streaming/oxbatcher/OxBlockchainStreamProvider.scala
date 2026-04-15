package scalus.examples.streaming.oxbatcher

import ox.flow.Flow
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.node.{BlockchainProvider, BlockfrostProvider}
import scalus.cardano.node.stream.{BackupSource, BlockfrostNetwork, ChainSyncSource, StreamProviderConfig, UnsupportedSourceException}
import scalus.cardano.node.stream.BaseStreamProvider
import scalus.cardano.node.stream.engine.Engine

import OxScalusAsyncStream.{Id, given}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/** ox specialisation of [[BaseStreamProvider]].
  *
  * `Id[A] = A`, so `liftFuture` blocks on the future. That matches ox's direct-style story: the
  * caller owns a virtual thread already, so blocking is cheap and ergonomic.
  */
class OxBlockchainStreamProvider(engine: Engine)(using ExecutionContext)
    extends BaseStreamProvider[Id, Flow](engine) {

    protected def liftFuture[A](fa: => Future[A]): Id[A] =
        Await.result(fa, Duration.Inf)

    protected def pureF[A](a: A): Id[A] = a

    def close(): Id[Unit] = liftFuture(engine.closeAllSubscribers())
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
            case ChainSyncSource.Synthetic => ()
            case ChainSyncSource.N2N(_, _) =>
                throw UnsupportedSourceException("ChainSyncSource.N2N is not wired until M4/M5")
            case ChainSyncSource.N2C(_) =>
                throw UnsupportedSourceException("ChainSyncSource.N2C is not wired until M7")

        val backup = buildBackup(config.backup)
        new OxBlockchainStreamProvider(
          new Engine(config.cardanoInfo, backup, Engine.DefaultSecurityParam)
        )
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
