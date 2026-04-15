package scalus.cardano.node.stream

import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.node.BlockchainProvider

/** Configuration for a streaming provider.
  *
  * `cardanoInfo` names the network and seeds the protocol-params cell. `chainSync` picks the
  * live-event source; `backup` answers historical and out-of-window queries that the engine cannot
  * serve from local state; `storage` selects the engine's memory/durability profile.
  *
  * See the indexer design doc at `docs/local/claude/indexer/indexer-node.md` for milestone-level
  * context.
  */
case class StreamProviderConfig(
    cardanoInfo: CardanoInfo,
    chainSync: ChainSyncSource,
    backup: BackupSource,
    storage: StorageProfile = StorageProfile.Light
)

/** Where live chain events come from. */
sealed trait ChainSyncSource
object ChainSyncSource {

    /** No protocol — events are pushed in by tests or the emulator. */
    case object Synthetic extends ChainSyncSource

    /** Connect to a public relay over N2N (TCP). Not wired until M4/M5. */
    case class N2N(host: String, port: Int) extends ChainSyncSource

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
