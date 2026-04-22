package scalus.cardano.node.stream.ox

import ox.flow.Flow
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.network.{ChainApplier, ChainApplierHandle, ClientConfig, NetworkMagic, NodeToNodeClient, NodeToNodeConnection}
import scalus.cardano.network.replay.{PeerReplayConnectionFactory, PeerReplaySource}
import scalus.cardano.node.{BlockchainProvider, BlockfrostProvider}
import scalus.cardano.node.stream.{BackupSource, BlockfrostNetwork, ChainSyncSource, StartFrom, StreamProviderConfig, UnsupportedSourceException}
import scalus.cardano.node.stream.BaseStreamProvider
import scalus.cardano.node.stream.engine.Engine
import scalus.cardano.node.stream.engine.persistence.{EnginePersistenceStore, FileEnginePersistenceStore}
import scalus.cardano.node.stream.engine.replay.ReplaySource

import OxScalusAsyncStream.{Id, given}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/** ox specialisation of [[BaseStreamProvider]].
  *
  * `Id[A] = A`, so `liftFuture` blocks on the future. That matches ox's direct-style story: the
  * caller owns a virtual thread already, so blocking is cheap and ergonomic.
  *
  * `preClose` runs before the engine's subscribers are shut down — used by the N2N wiring path to
  * cancel the [[ChainApplier]] and close the underlying [[NodeToNodeConnection]] so the sync loop
  * unwinds cleanly before subscribers fail.
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
        appId: String,
        cardanoInfo: CardanoInfo,
        chainSync: ChainSyncSource,
        backup: BackupSource
    )(using ExecutionContext): OxBlockchainStreamProvider =
        create(StreamProviderConfig(appId, cardanoInfo, chainSync, backup))

    /** Full configuration. */
    def create(
        config: StreamProviderConfig
    )(using ExecutionContext): OxBlockchainStreamProvider = {
        val persistence = resolvePersistence(config)
        config.chainSync match
            case ChainSyncSource.Synthetic =>
                val backup = buildBackup(config.backup)
                bootstrapIfNeeded(config, persistence)
                val engine =
                    buildEngine(config, backup, persistence, config.fallbackReplaySources)
                new OxBlockchainStreamProvider(
                  engine,
                  persistenceTeardown(persistence, engine, config)
                )
            case n: ChainSyncSource.N2N =>
                connectN2N(config, n, persistence)
            case ChainSyncSource.N2C(_) =>
                throw UnsupportedSourceException("ChainSyncSource.N2C is not wired until M7")
    }

    /** Synchronous snapshot bootstrap — direct-style. */
    private def bootstrapIfNeeded(
        config: StreamProviderConfig,
        persistence: EnginePersistenceStore
    )(using ExecutionContext): Unit = config.bootstrap.foreach { source =>
        val warmTip =
            Await.result(persistence.load(), Duration.Inf).flatMap(_.snapshot.flatMap(_.tip))
        if warmTip.isEmpty then {
            val store = config.chainStore.getOrElse(
              throw scalus.cardano.node.stream.engine.snapshot.SnapshotError
                  .SnapshotConfigError("bootstrap requires chainStore")
            )
            val _ = Await.result(
              new scalus.cardano.node.stream.engine.snapshot.ChainStoreRestorer(store)
                  .restore(source),
              Duration.Inf
            )
        }
    }

    private def resolvePersistence(
        config: StreamProviderConfig
    )(using ExecutionContext): EnginePersistenceStore =
        Option(config.enginePersistence).getOrElse(
          FileEnginePersistenceStore.fileForApp(config.appId)
        )

    private def buildEngine(
        config: StreamProviderConfig,
        backup: Option[BlockchainProvider],
        persistence: EnginePersistenceStore,
        fallbackReplaySources: List[ReplaySource]
    )(using ExecutionContext): Engine = {
        Await.result(persistence.load(), Duration.Inf) match {
            case None =>
                new Engine(
                  config.cardanoInfo,
                  backup,
                  Engine.DefaultSecurityParam,
                  persistence,
                  fallbackReplaySources,
                  config.chainStore
                )
            case Some(state) =>
                Await.result(
                  Engine.rebuildFrom(
                    state,
                    config.cardanoInfo,
                    backup,
                    Engine.DefaultSecurityParam,
                    persistence,
                    fallbackReplaySources,
                    config.chainStore
                  ),
                  Duration.Inf
                )
        }
    }

    private def persistenceTeardown(
        persistence: EnginePersistenceStore,
        engine: Engine,
        config: StreamProviderConfig
    )(using ExecutionContext): () => Future[Unit] = () =>
        for {
            _ <- persistence.flush()
            snap <- engine.takeSnapshot(config.appId, networkMagicFor(config))
            _ <- persistence.compact(snap)
            _ <- persistence.close()
            _ = config.chainStore.foreach(_.close())
        } yield ()

    private def networkMagicFor(config: StreamProviderConfig): Long = config.chainSync match {
        case ChainSyncSource.N2N(_, _, magic) => magic
        case _                                => 0L
    }

    /** Open an N2N connection to `n.host:n.port`, spawn a [[ChainApplier]] driving the engine from
      * it, and return a provider whose `close()` tears down the applier + connection + engine
      * subscribers in that order.
      *
      * Blocks on connection establishment (handshake + keep-alive start). Failures there propagate
      * as the original `Future`'s cause via `Await.result`.
      */
    private def connectN2N(
        config: StreamProviderConfig,
        n: ChainSyncSource.N2N,
        persistence: EnginePersistenceStore
    )(using ExecutionContext): OxBlockchainStreamProvider = {
        val backup = buildBackup(config.backup)
        bootstrapIfNeeded(config, persistence)
        val fallbacks = config.fallbackReplaySources :+ buildPeerReplaySource(n)
        val engine = buildEngine(config, backup, persistence, fallbacks)
        val conn: NodeToNodeConnection = Await.result(
          NodeToNodeClient
              .connect(n.host, n.port, NetworkMagic(n.networkMagic), ClientConfig.default),
          Duration.Inf
        )
        val startFrom = engine.currentTip match {
            case Some(tip) => StartFrom.At(tip.point)
            case None      => StartFrom.Tip
        }
        val handle: ChainApplierHandle =
            ChainApplier.spawn(conn, engine, startFrom = startFrom)

        // Surface applier-loop failures (decode error, NoIntersection, etc.) to subscribers
        // with the typed cause rather than a silent EOS. User-initiated close() cancels the
        // applier with a `CancelledException`; that's a graceful stop and should not be
        // surfaced as a failure — the later `conn.closed` hook calls closeAllSubscribers.
        handle.done.onComplete {
            case scala.util.Failure(_: scalus.cardano.infra.CancelledException) => ()
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

        val persistenceClose = persistenceTeardown(persistence, engine, config)
        val preClose: () => Future[Unit] = () => {
            // Cancel applier first so the sync loop unwinds before we close the socket,
            // then seal persistence.
            for {
                _ <- handle.cancel()
                _ <- conn.close()
                _ <- persistenceClose()
            } yield ()
        }
        new OxBlockchainStreamProvider(engine, preClose)
    }

    private def buildPeerReplaySource(n: ChainSyncSource.N2N): PeerReplaySource =
        new PeerReplaySource(
          PeerReplayConnectionFactory.forEndpoint(n.host, n.port, NetworkMagic(n.networkMagic))
        )

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
