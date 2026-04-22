package scalus.cardano.node.stream

import scalus.uplc.builtin.ByteString

import java.nio.file.Path

/** Where to fetch a [[scalus.cardano.node.stream.engine.snapshot.ChainStoreSnapshot]] from at cold
  * start. The `ChainStoreRestorer` resolves each variant to an `InputStream`, drives the snapshot
  * reader, and bulk-loads the configured `ChainStore`.
  *
  * See `docs/local/claude/indexer/snapshot-bootstrap-m10.md` § *SnapshotSource + Restorer + startup
  * integration* for the cold-vs-warm startup rules.
  */
sealed trait SnapshotSource

object SnapshotSource {

    /** Local file on disk. The simplest source — bundled fixtures, internal snapshot rotation, CI
      * test setups.
      */
    case class File(path: Path) extends SnapshotSource

    /** HTTP(S) download. `expectedSha256`, when set, is compared against a SHA-256 computed over
      * the entire downloaded byte stream before the restorer trusts its content — lets third-party
      * mirrors serve the snapshot without the consumer trusting the transport. `None` relies on the
      * snapshot's internal footer hash alone (body integrity only, not header tampering).
      */
    case class Url(url: String, expectedSha256: Option[ByteString] = None) extends SnapshotSource

    /** Mithril aggregator source. Ships as a stub in M10 — the full cryptographic verifier
      * (certificate chain, MuSig2 threshold signatures, tarball parsing) lands with M10b in
      * `scalus-chain-store-mithril`. Using this variant today raises [[UnsupportedSourceException]]
      * at provider-construction time.
      */
    case class Mithril(aggregatorUrl: String, genesisVerificationKey: String) extends SnapshotSource
}
