package scalus.cardano.node.stream

import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.node.stream.engine.ChainStore
import scalus.cardano.node.stream.engine.persistence.EnginePersistenceStore
import scalus.cardano.node.stream.engine.replay.{ChainStoreReplaySource, ReplaySource}

/** Configuration for a streaming provider.
  *
  * `appId` is a reverse-DNS identifier for this application instance (e.g. `com.mycompany.batcher`)
  * and names the engine's persistent state location. Must be non-empty so two Scalus apps on the
  * same machine don't collide on state files. See *App identity and persistence location* in
  * `docs/local/claude/indexer/indexer-node.md`.
  *
  * `cardanoInfo` names the network and seeds the protocol-params cell. `chainSync` picks the
  * live-event source; `backup` answers historical and out-of-window queries that the engine cannot
  * serve from local state; `storage` selects the engine's memory/durability profile.
  *
  * `enginePersistence` controls warm-restart durability. `null` (the default) asks the factory to
  * wire the platform-appropriate default — a file-backed store keyed by `appId` on the JVM; JS
  * adapters may fall back to `.noop`. Explicitly passing `EnginePersistenceStore.noop` opts into
  * Cold-restart semantics (tests, demos, one-shots).
  *
  * `chainStore` is an opt-in durable block-history source used as a fallback for checkpoint replay
  * ([[StartFrom.At]]) when the in-memory rollback buffer doesn't cover the checkpoint. M7 defines
  * the trait shape (see [[scalus.cardano.node.stream.engine.ChainStore]]); concrete backends land
  * with M9. Most apps leave it `None`.
  *
  * See the indexer design doc at `docs/local/claude/indexer/indexer-node.md` for milestone-level
  * context.
  */
case class StreamProviderConfig(
    appId: String,
    cardanoInfo: CardanoInfo,
    chainSync: ChainSyncSource,
    backup: BackupSource,
    storage: StorageProfile = StorageProfile.Light,
    enginePersistence: EnginePersistenceStore | Null = null,
    chainStore: Option[ChainStore] = None
) {
    require(appId.nonEmpty, "StreamProviderConfig.appId must be non-empty")

    /** Replay sources derived from this config, in the order the engine should try them after the
      * rollback buffer. Today that's just the optional `chainStore`; Phase 2b will add a peer
      * source when `chainSync` is an N2N configuration.
      */
    def fallbackReplaySources: List[ReplaySource] =
        chainStore.toList.map(new ChainStoreReplaySource(_))
}

/** Where live chain events come from. */
sealed trait ChainSyncSource
object ChainSyncSource {

    /** No protocol — events are pushed in by tests or the emulator. */
    case object Synthetic extends ChainSyncSource

    /** Connect to a public relay over Ouroboros Node-to-Node (TCP).
      *
      * @param networkMagic
      *   32-bit Cardano network identifier, sent in the handshake. Mainnet = 764824073, Preview
      *   = 2, Preprod = 1, yaci-devkit default = 42. A plain `Long` (not the opaque
      *   `scalus.cardano.network.NetworkMagic`) to keep this ADT free of a dependency on the
      *   cardano-network module. The JVM provider wraps it into `NetworkMagic` at the boundary.
      *
      * Wiring for this variant is JVM-only — the JS build of `Fs2BlockchainStreamProvider`
      * still raises `UnsupportedSourceException` because `NodeToNodeClient` requires raw TCP
      * which Scala.js doesn't provide out of the box.
      */
    case class N2N(host: String, port: Int, networkMagic: Long) extends ChainSyncSource

    /** Connect to a local cardano-node over N2C (Unix socket). Not wired until M7. String path
      * (rather than `java.nio.file.Path`) keeps this case class JS-compatible.
      */
    case class N2C(socketPath: String) extends ChainSyncSource
}

/** Where the engine looks when it cannot answer a snapshot query from its own state (historical
  * UTxOs, tx status outside the rollback buffer, submit before N2N `TxSubmission2` lands, …).
  */
sealed trait BackupSource
object BackupSource {

    /** Bloxbean's `BlockfrostProvider` — answers both historical UTxO seeding and `submit` in the
      * M2 stack.
      */
    case class Blockfrost(
        apiKey: String,
        network: BlockfrostNetwork,
        maxConcurrentRequests: Int = 5
    ) extends BackupSource

    /** LSQ-backed `BlockchainProviderTF` over an N2C local socket. Not wired until M11.
      */
    case class LocalStateQuery(socketPath: String) extends BackupSource

    /** Escape hatch — pass an existing `BlockchainProvider`. Useful for tests, chained providers,
      * or custom backends.
      */
    case class Custom(provider: BlockchainProvider) extends BackupSource

    /** Explicit no-backup. Snapshot methods that would need the backup return a typed "no source
      * configured" error. Suitable for write-only chain followers and fresh-script use cases where
      * there are genuinely no historical UTxOs.
      */
    case object NoBackup extends BackupSource
}

/** Blockfrost has three separate API hosts. `Network` from `scalus.cardano.address` only
  * distinguishes `Testnet` from `Mainnet` so we need our own tag to pick between the two testnets.
  */
enum BlockfrostNetwork {
    case Mainnet, Preview, Preprod
}

/** What the engine maintains locally. */
sealed trait StorageProfile
object StorageProfile {

    /** Per-active-subscription UTxO indexes only; rollback buffer is always present. Memory bounded
      * by `securityParam` × subscription footprint. Default.
      */
    case object Light extends StorageProfile

    /** Maintain the full UTxO set locally. `findUtxos` becomes a local lookup for any query.
      * Multi-GB on mainnet; practically requires Mithril bootstrap (M10). Not wired until M9/M10.
      */
    case object Heavy extends StorageProfile
}

/** Signalled by the factory when a [[ChainSyncSource]] or [[BackupSource]] is named in
  * configuration but the milestone that implements it has not landed yet.
  */
final case class UnsupportedSourceException(message: String) extends RuntimeException(message)
