package scalus.cardano.node.stream.engine.snapshot.mithril

import scalus.cardano.node.stream.{SnapshotSource, UnsupportedSourceException}

/** Placeholder for the Mithril-verified snapshot acquisition path. `scalus-chain-store-mithril`
  * will fill this in across two follow-up milestones:
  *
  *   - **M10b** — cardano-node DB parser (tar + zstd extraction, ImmutableDB chunk parser,
  *     LedgerState snapshot parser). Babbage+ eras only. Consumes a cardano-node `db/` directory
  *     (extracted from a Mithril `.tar.zst`) and produces the streaming shape
  *     `ChainStoreRestorer` expects.
  *   - **M10c** — Mithril Aggregator HTTP client + certificate chain walk + MuSig2 threshold
  *     signature verifier. Consumes the M10b parser's output to compute the Merkle root it
  *     authenticates against.
  *
  * See `docs/local/claude/indexer/indexer-node.md` milestones 10b / 10c and
  * `docs/local/claude/indexer/snapshot-bootstrap-m10.md` for the full design.
  *
  * Using [[SnapshotSource.Mithril]] today raises [[UnsupportedSourceException]] pointing at
  * those milestones — there is nothing in this module yet that can resolve a Mithril source.
  */
object MithrilSnapshotClient {

    /** Future entry point for turnkey Mithril-verified restore. Raises
      * [[UnsupportedSourceException]] in M10 — see class docstring for the M10b / M10c roadmap.
      */
    def resolve(source: SnapshotSource.Mithril): Nothing =
        throw UnsupportedSourceException(
          s"MithrilSnapshotClient.resolve($source) is not yet implemented — " +
              "cardano-node DB parser lands with M10b, verifier with M10c. " +
              "See docs/local/claude/indexer/indexer-node.md milestones 10b / 10c."
        )
}
