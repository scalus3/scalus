package scalus.cardano.node.stream.fs2

import cats.effect.IO
import cats.effect.std.Dispatcher
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.network.{ChainApplier, ClientConfig, NetworkMagic, NodeToNodeClient, NodeToNodeConnection}
import scalus.cardano.network.replay.{PeerReplayConnectionFactory, PeerReplaySource}
import scalus.cardano.node.{BlockchainProvider, BlockfrostProvider}
import scalus.cardano.node.stream.{BackupSource, BlockfrostNetwork, ChainSyncSource, StartFrom, StreamProviderConfig, UnsupportedSourceException}
import scalus.cardano.node.stream.BaseStreamProvider
import scalus.cardano.node.stream.engine.Engine
import scalus.cardano.node.stream.engine.persistence.{EnginePersistenceStore, FileEnginePersistenceStore}
import scalus.cardano.node.stream.engine.replay.ReplaySource

import Fs2ScalusAsyncStream.{IOStream, given}

import scala.concurrent.{ExecutionContext, Future}

/** fs2 / cats-effect specialisation of [[BaseStreamProvider]].
  *
  * `preClose` runs before the engine's subscribers are shut down — used by the N2N wiring path to
  * cancel the [[ChainApplier]] and close the underlying [[NodeToNodeConnection]] so the sync loop
  * unwinds cleanly before subscribers fail.
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
        appId: String,
        cardanoInfo: CardanoInfo,
        chainSync: ChainSyncSource,
        backup: BackupSource
    )(using Dispatcher[IO], ExecutionContext): IO[Fs2BlockchainStreamProvider] =
        create(StreamProviderConfig(appId, cardanoInfo, chainSync, backup))

    def create(
        config: StreamProviderConfig
    )(using Dispatcher[IO], ExecutionContext): IO[Fs2BlockchainStreamProvider] = {
        val persistence = resolvePersistence(config)
        config.chainSync match
            case ChainSyncSource.Synthetic =>
                for {
                    backup <- buildBackup(config.backup)
                    _ <- bootstrapIfNeeded(config, persistence)
                    engine <- buildEngine(config, backup, persistence, config.fallbackReplaySources)
                } yield new Fs2BlockchainStreamProvider(
                  engine,
                  persistenceTeardown(persistence, engine, config)
                )
            case n: ChainSyncSource.N2N =>
                connectN2N(config, n, persistence)
            case ChainSyncSource.N2C(_) =>
                IO.raiseError(
                  UnsupportedSourceException("ChainSyncSource.N2C is not wired until M7")
                )
    }

    /** Run the snapshot-bootstrap path when the config has a source AND the engine has no warm
      * restart tip. Warm-restart wins on disagreement (see
      * `docs/local/claude/indexer/snapshot-bootstrap-m10.md` § *Interaction with existing
      * milestones*).
      */
    private def bootstrapIfNeeded(
        config: StreamProviderConfig,
        persistence: EnginePersistenceStore
    )(using ExecutionContext): IO[Unit] = config.bootstrap match {
        case None => IO.unit
        case Some(source) =>
            IO.fromFuture(IO(persistence.load())).flatMap { persisted =>
                val warmTip = persisted.flatMap(_.snapshot.flatMap(_.tip))
                if warmTip.isDefined then IO.unit
                else {
                    // StreamProviderConfig's `require` enforces `bootstrap ⇒ chainStore.isDefined`,
                    // so the .get is safe: a malformed config can't reach this point.
                    val store = config.chainStore.get
                    IO.fromFuture(
                      IO(
                        new scalus.cardano.node.stream.engine.snapshot.ChainStoreRestorer(store)
                            .restore(source)
                      )
                    ).void
                }
            }
    }

    /** Resolve `config.enginePersistence`: an explicit value wins; `null` falls back to a
      * file-backed store named after `config.appId`. Users who want Cold-restart semantics pass
      * `EnginePersistenceStore.noop` explicitly (the canonical test/demo pattern).
      */
    private def resolvePersistence(
        config: StreamProviderConfig
    )(using ExecutionContext): EnginePersistenceStore =
        Option(config.enginePersistence).getOrElse(
          FileEnginePersistenceStore.fileForApp(config.appId)
        )

    /** Load any persisted state and either rebuild the engine from it or construct a cold one. The
      * returned engine is ready for subscriber registration before chain-sync starts.
      *
      * Callers supply the resolved replay-source fallback list. Synthetic chain-sync uses
      * `config.fallbackReplaySources` directly; the N2N path appends a [[PeerReplaySource]] tied to
      * the live N2N endpoint so checkpoint replays past the rollback-buffer horizon fall through to
      * the peer.
      */
    private def buildEngine(
        config: StreamProviderConfig,
        backup: Option[BlockchainProvider],
        persistence: EnginePersistenceStore,
        fallbackReplaySources: List[ReplaySource]
    )(using ExecutionContext): IO[Engine] = {
        IO.fromFuture(IO(persistence.load())).flatMap {
            case None =>
                IO.pure(
                  new Engine(
                    config.cardanoInfo,
                    backup,
                    Engine.DefaultSecurityParam,
                    persistence,
                    fallbackReplaySources,
                    config.chainStore
                  )
                )
            case Some(state) =>
                IO.fromFuture(
                  IO(
                    Engine.rebuildFrom(
                      state,
                      config.cardanoInfo,
                      backup,
                      Engine.DefaultSecurityParam,
                      persistence,
                      fallbackReplaySources,
                      config.chainStore
                    )
                  )
                )
        }
    }

    /** Flush + compact + close the store on shutdown, in that order. Called by the provider's
      * `preClose` along with the chain-sync teardown so persistence state is sealed before the
      * engine's worker thread stops.
      */
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

    /** Best-effort network-magic lookup. Only N2N carries the value on the source; Synthetic and
      * N2C fall back to 0. Used for the cross-check field on persisted snapshots.
      */
    private def networkMagicFor(config: StreamProviderConfig): Long = config.chainSync match {
        case ChainSyncSource.N2N(_, _, magic) => magic
        case _                                => 0L
    }

    /** Open an N2N connection to `n.host:n.port`, spawn a [[ChainApplier]] driving the engine from
      * it, and return a provider whose `close()` tears down applier + connection + engine
      * subscribers in order.
      *
      * On JVM [[NodeToNodeClient.connect]] opens a real TCP socket; on Scala.js it currently fails
      * eagerly — see the `NodeToNodeClient` JS stub. Either outcome is reported via the returned
      * `IO`'s error channel.
      */
    private def connectN2N(
        config: StreamProviderConfig,
        n: ChainSyncSource.N2N,
        persistence: EnginePersistenceStore
    )(using Dispatcher[IO], ExecutionContext): IO[Fs2BlockchainStreamProvider] =
        for {
            backup <- buildBackup(config.backup)
            _ <- bootstrapIfNeeded(config, persistence)
            fallbacks = config.fallbackReplaySources :+ buildPeerReplaySource(n)
            engine <- buildEngine(config, backup, persistence, fallbacks)
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
            startFrom = engine.currentTip match {
                case Some(tip) => StartFrom.At(tip.point)
                case None      => StartFrom.Tip
            }
            handle = ChainApplier.spawn(conn, engine, startFrom = startFrom)
            _ = handle.done.onComplete {
                case scala.util.Failure(_: scalus.cardano.infra.CancelledException) =>
                    // User-initiated shutdown via provider.close → handle.cancel fires a
                    // CancelledException. That's a graceful stop request; leave subscribers
                    // for the conn.closed path below which calls closeAllSubscribers cleanly.
                    ()
                case scala.util.Failure(t) =>
                    // Genuine applier failure (decode error, NoIntersection, unsupported era).
                    // Surface the typed cause to subscribers so their stream fails with the
                    // real error instead of a silent EOS.
                    engine.failAllSubscribers(t)
                case _ => ()
            }
            _ = conn.closed.onComplete {
                case scala.util.Failure(t) => engine.failAllSubscribers(t)
                case _                     => engine.closeAllSubscribers()
            }
        } yield {
            val persistenceClose = persistenceTeardown(persistence, engine, config)
            val preClose: () => Future[Unit] = () =>
                handle.cancel().flatMap(_ => conn.close()).flatMap(_ => persistenceClose())
            new Fs2BlockchainStreamProvider(engine, preClose)
        }

    private def buildPeerReplaySource(n: ChainSyncSource.N2N): PeerReplaySource =
        new PeerReplaySource(
          PeerReplayConnectionFactory.forEndpoint(n.host, n.port, NetworkMagic(n.networkMagic))
        )

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
