# Snapshot bootstrap — fast cold-start for Heavy-mode indexers (M10)

**Status.** Design phase.

M10 lands the **snapshot-restore path**: a streaming interchange
format Scalus owns, a `ChainStoreRestorer` that loads it into an empty
`ChainStore`, a `SnapshotSource` ADT with multiple acquisition modes,
and provider-level startup integration so a cold start on a configured
`SnapshotSource` bootstraps the store before resuming chain-sync.

The design is **Mithril-shaped** but **Mithril-independent**. The
`Mithril(...)` variant of `SnapshotSource` is scoped in M10 but ships
as a stub that throws `UnsupportedSourceException`. Turnkey Mithril
support arrives via **two parallel paths** tracked as separate
milestones (see `indexer-node.md` M10b, M10c):

- **M10b — WASM-embedded Mithril client (primary).** Bundles the
  upstream `@mithril-dev/mithril-client-wasm` blob via Chicory (pure
  JVM WASM runtime), so the full Rust Mithril client — certificate
  chain + MuSig2 verifier + Aggregator HTTP client + tar/zstd
  extractor + ImmutableDB parser + LedgerState parser — runs
  in-process without native deps. We track upstream Mithril by
  bumping the .wasm version pin; CIP-0084 / CIP-165 are the
  Mithril team's problem, not ours. 3–5 days of wasm-bindgen ABI
  wiring (115 imports, mostly mechanical once fetch/streams/timer
  are in place). Status: P1 + P2-start landed; P2 finish + P3
  integration ahead.
- **M10c — CLI shell-out fallback.** `SnapshotSource.ExternalCli`
  spawns a Mithril-related CLI (most likely a Scalus-owned
  `scalus-snapshot-dump` Rust companion, or an upstream extension
  of `mithril-client tools utxo-hd snapshot-converter`) which
  streams a `ChainStoreSnapshot` into `SnapshotReader`. Useful
  when in-process WASM is undesirable. Scala side is ~200 lines;
  the companion binary is a separate Rust project. Independent
  of M10b and can ship before or after.

The two variants coexist. M10b is the default recommendation; M10c
is the escape hatch and the natural upstream-contribution path if
the Scalus snapshot format gets added to `mithril-client` itself.

Apps that can acquire a trusted snapshot via other channels (their
own signed bundles, a proxied external mithril-client, CI test
fixtures) get the Scalus-native restore path in M10 without waiting
for either follow-up.

## Why now

Two practical problems are unblocked:

- **Heavy-mode findUtxos is unreachable on mainnet without snapshot
  restore.** Syncing UTxO state from genesis takes days and hundreds
  of GB of intermediate state. A populated ChainStore from a verified
  snapshot + live chain-sync forward is the only practical path.
- **The M7 replay cascade's ChainStore branch stays a stub.** M9
  landed the store but has no way to bulk-populate it. Checkpoint
  replay past the rollback-buffer horizon still always falls through
  to PeerReplaySource (i.e. opens a new N2N connection) unless the
  store has already been filled by live sync from origin. Snapshot
  restore fills the store before the first live block arrives.

M10 also closes the loop on M9.P3 (Heavy-mode `findUtxos`): the UTxO
keyspace gets real data, so `Engine.findUtxosLocal` can answer
address-level queries from local disk.

## Scope for M10

Four sub-phases.

### M10.P1 — `ChainStoreSnapshot` interchange format

Binary streaming format owned by Scalus. CBOR-framed, one record per
CBOR value, top-level `Header → (Block | UtxoEntry)* → Footer`
sequence. Schema-versioned so M14 (advanced persistence) can evolve it.

```
Header:
    schemaVersion: u32
    networkMagic:  u64
    tip:           ChainTip         # the snapshot's anchor point
    blockCount:    u64              # advisory; validator cross-checks
    utxoCount:     u64              # advisory
    contentFlags:  u32              # bit 0: blocks included; bit 1: UTxO set included

Body:
    Repeated, in order:
        BlockRecord(AppliedBlock)       # M9's existing AppliedBlock CBOR
        UtxoEntry(TransactionInput, TransactionOutput)

Footer:
    sha256: bytes[32]   # running hash of all body records' bytes
    sentinel: u32 = 0xEC1057A1
```

**Sorting conventions.**

- `BlockRecord`s are in ascending (slot, hash) order — matches the
  `KvChainStore` `blocks` keyspace.
- `UtxoEntry`s are in ascending `TransactionInput` CBOR-bytes order —
  deterministic across producers so snapshot hashes are reproducible.

**Readers:** `SnapshotReader(InputStream)` — streaming decode; at most
one record in memory at a time. Returns an `Iterator[Record]` that the
restorer consumes without materialising the whole snapshot.

**Writers:** `SnapshotWriter(OutputStream)` — streaming encode;
includes a small internal buffer to emit the running sha256 correctly
at the footer. Used by (a) test fixtures, (b) Mithril restore for the
M10b export path, (c) apps that want to export their own ChainStore
for rebooting co-workers.

**Out-of-scope compression.** The format itself is uncompressed; users
who care wrap the stream in `GZIPOutputStream` / `GZIPInputStream`.
Streaming-native compression (zstd-seekable) is a follow-up if
profiling ever shows it matters.

### M10.P2 — `ChainStoreUtxoSet` + UTxO keyspace on `KvChainStore`

Add a sub-trait to `ChainStore` (not a mandatory method — implementors
can choose not to support Heavy mode):

```scala
trait ChainStoreUtxoSet { self: ChainStore =>
    /** Query UTxOs matching `q` from the local store. Decomposes via
      * the existing QueryDecomposer; returns `None` if the store
      * doesn't maintain a UTxO set (keeps the optional nature explicit).
      */
    def findUtxos(q: UtxoQuery): Option[Utxos]

    /** Bulk-load utxos from a snapshot. Replaces any existing set.
      * Consumed by M10.P3's restorer.
      */
    def restoreUtxoSet(tip: ChainTip, utxos: Iterator[(TransactionInput, TransactionOutput)]): Unit
}
```

`KvChainStore` implements this by maintaining three additional
keyspaces:

| Keyspace | Key | Value | Purpose |
|---|---|---|---|
| `utxo` | `<input-cbor>` | `<output-cbor>` | The current UTxO set |
| `utxo-by-key` | `<ukey-tag:1><ukey-bytes><input-cbor>` | empty | Secondary index, one entry per `(UtxoKey, input)` |
| `delta` | `<slot-BE:8><hash:32>` | CBOR `(added: Seq[(in, out)], spent: Seq[(in, out)])` | Per-block reverse-delta for rollback |

**Live-delta maintenance.** `appendBlock(b)` now also:
- For each spent input in `b`, reads the current output from `utxo`;
  if present, records `(in, out)` as a reversible-spend in `delta`;
  removes the `utxo` entry and the matching `utxo-by-key` entries.
- For each created output, writes `utxo`, writes the `utxo-by-key`
  entries for every `UtxoKey` that decomposes from the output, and
  records the input as a reversible-create in `delta`.
- All of the above plus the existing block / index / tip writes land
  in a single `KvStore.batch(...)` so a crash mid-write can't leave
  keyspaces inconsistent.

**Rollback.** `rollbackTo(to)` inverts `delta` entries for every
removed block — re-inserts reversible-spend outputs, deletes
reversible-create outputs.

**Missing-input tolerance.** If a spend references an input the store
has never seen (common on a ChainStore that didn't start from
genesis), the spend is silently dropped from `delta` — rollback can't
restore something we never knew. This is the only case where a
rollback leaves the UTxO set subtly wrong; since the store wasn't
consistent with genesis anyway, the tradeoff is acceptable. Apps that
need perfect rollback consistency must either start the ChainStore
from origin, or restore from a snapshot that covers every input their
live window will touch.

### M10.P3 — `SnapshotSource` + `ChainStoreRestorer` + startup integration

```scala
sealed trait SnapshotSource
object SnapshotSource {
    case class File(path: java.nio.file.Path) extends SnapshotSource
    case class Url(url: String, expectedSha256: Option[ByteString] = None) extends SnapshotSource
    /** Reserved — the full client lives in scalus-chain-store-mithril (M10b).
      * M10 ships a stub that throws UnsupportedSourceException.
      */
    case class Mithril(aggregatorUrl: String, genesisVerificationKey: String) extends SnapshotSource
}
```

Added to `StreamProviderConfig`:

```scala
case class StreamProviderConfig(
    ...
    chainStore: Option[ChainStore] = None,
    bootstrap: Option[SnapshotSource] = None   // NEW
)
```

Invariants:

- `bootstrap` without `chainStore` is a config error — snapshot restore
  needs a store to restore *into*. The provider factory fails loud.
- If `chainStore.tip.isDefined` AND `enginePersistence.load()` has
  content, the provider treats the app as warm-started and skips the
  bootstrap entirely — snapshot restore is a *cold-only* path.

**`ChainStoreRestorer`** — one class in `scalus-streaming-core`:

```scala
final class ChainStoreRestorer(store: ChainStore & ChainStoreUtxoSet) {
    def restore(source: SnapshotSource)(using ExecutionContext): Future[ChainTip]
}
```

Flow:

1. Acquire an `InputStream` from the source — File reads from disk;
   Url downloads (+ sha256 if supplied); Mithril throws for now.
2. Drive `SnapshotReader`, routing `BlockRecord`s to
   `store.appendBlock(...)` and streaming `UtxoEntry`s into
   `store.restoreUtxoSet(tip, iter)`.
3. Cross-check header's `blockCount` / `utxoCount` / `tip.point`
   against what was actually applied. Mismatch → fail with a typed
   `SnapshotCorrupted` error so the caller can trigger a fresh
   download.
4. Return the restored tip.

**Provider wiring.** Fs2 and Ox `connectN2N` (and `Synthetic` for
demos) check `config.bootstrap` before starting chain-sync:

```scala
for {
    persistedTip = persistence.load().flatMap(_.tip)
    _ <- bootstrapIfNeeded(config, chainStore, persistedTip)
    engine <- buildEngine(...)
    conn <- NodeToNodeClient.connect(...)
    startFrom = engine.currentTip match {
        case Some(tip) => StartFrom.At(tip.point)
        case None      => StartFrom.Tip  // no bootstrap, no persistence → live fresh
    }
    ...
} yield ...
```

### M10.P4 — `scalus-chain-store-mithril` stub module

New JVM-only module in `scalus-embedded-node/scalus-chain-store-mithril/`.
Depends on `scalus-streaming-core`. Ships:

- `MithrilSnapshotClient` — resolves `SnapshotSource.Mithril(...)` to
  an `InputStream` for `ChainStoreRestorer`. M10 implementation
  throws `UnsupportedSourceException` pointing at M10b.
- `README.md` — brief note on what M10b will fill in (Aggregator HTTP
  client, certificate-chain walk, MuSig2 verifier, tarball parser)
  and the pinned-fixtures approach for regression testing.

This slot reserves the module name / package path so M10b's landing
doesn't require shuffling users' dependencies.

## Interaction with existing milestones

**M6 (engine persistence).** Orthogonal. M6 persists engine-local
state for warm-restart; M10 restores chain history + UTxO set. If a
warm-restarted engine AND a configured bootstrap disagree on tip,
warm-restart wins (don't blow away engine state for a snapshot the
user forgot to disable); the bootstrap is silently skipped. An
explicit "wipe everything and re-bootstrap" API can land with M14.

**M7 (checkpoint replay).** `ChainStoreReplaySource` (already wired)
now has real content for checkpoints that pre-date the engine's
rollback buffer but post-date the snapshot tip. No code changes in
M7; just coverage that grew.

**M9 (pluggable ChainStore).** M10.P2 extends the ChainStore surface
with `ChainStoreUtxoSet` for Heavy mode. `KvChainStore` gains the
three new keyspaces; `InMemoryChainStore` (via
`KvChainStore(InMemoryKvStore())`) Just Works in tests.

**M9.P3 (originally "Heavy-mode findUtxos").** Folded into M10.P2 —
the UTxO keyspace surface lands naturally with snapshot restore, and
the two together make Heavy mode functional.

## Error model

| Failure | Where | Cause | Observers see |
|---|---|---|---|
| Snapshot file not found | restorer | Java IO | provider-construction Future fails |
| Snapshot schema version mismatch | restorer | `SnapshotSchemaMismatch(expected, got)` | provider-construction fails; no engine |
| Running sha256 mismatches footer | restorer | `SnapshotCorrupted("hash mismatch")` | ditto |
| Block count / UTxO count / tip mismatch between header and body | restorer | `SnapshotCorrupted(detail)` | ditto |
| Mithril source in M10 | restorer | `UnsupportedSourceException("Mithril not yet implemented — see M10b")` | ditto |
| Bootstrap configured but no chain store | factory | `SnapshotConfigError("bootstrap requires chainStore")` | ditto |
| URL source + provided expectedSha256 fails | restorer | `SnapshotCorrupted("sha256 mismatch")` | ditto |

All are fatal to provider construction (never mid-flight), so no
subscriber fan-out is affected.

## Out of scope for M10

- Mithril cryptographic verification — M10b.
- Compression (gzip/zstd) — users wrap the stream.
- Parallel / incremental snapshot update — M14.
- Signed Scalus-format snapshots — not a goal; trust is source's
  concern. `SnapshotSource.Url(..., expectedSha256)` is the only
  integrity check M10 enforces itself.
- Exporting a ChainStore as a snapshot — M14 territory (advanced
  persistence) once the schema has stabilised.
- Automated snapshot refresh on schema upgrade — M14.

## References

- `docs/local/claude/indexer/indexer-node.md` — M10 row in *Milestones*
  (rewritten for the Option A scope); M10b row for the JVM verifier
  follow-up.
- `docs/local/claude/indexer/chain-store-m9.md` — the `ChainStore` /
  `KvStore` layer that M10 builds on.
- `docs/local/claude/indexer/engine-persistence-minimal.md` — M6; the
  warm-restart tip handling that bootstrap defers to.
- `docs/local/claude/indexer/checkpoint-restart-m7.md` —
  `ChainStoreReplaySource` gets its coverage from M10 populating the
  store.

## Cardano Database V2 format (research 2026-04-23)

Mithril's `mithril-client` with the `fs` feature downloads a
"Cardano Database V2" artifact. The format is NOT Mithril-specific —
it IS cardano-node's on-disk DB directory layout. Mithril just
certifies the per-file hashes via a Merkle tree and delivers the
files over HTTP (usually pulled from S3-like cloud storage).

### Directory layout

```
<target-dir>/
  immutable/
    00000.chunk / 00000.primary / 00000.secondary
    00001.chunk / 00001.primary / 00001.secondary
    ...
  ledger/
    <slot>.<state-file>                    (legacy, pre-UTxO-HD)
  ledger/
    <folder>/                              (UTxO-HD)
      meta
      state
      tables/tvar                          (in-memory backend;
                                            LSM backend has others)
  volatile/                                (NOT signed/downloaded)
  bootstrap.node                           (generated by client;
                                            marks dir as ready)
```

Every "immutable trio" (`.chunk`, `.primary`, `.secondary`) is one
ouroboros-consensus `ImmutableDB` chunk:
- `.chunk`   — raw serialised block bytes concatenated
- `.primary` — per-block offsets into `.chunk`
- `.secondary` — block metadata (slot, block-header-hash, etc.)

Ledger-state format varies by cardano-node version:
- Legacy: single CBOR-encoded `ExtLedgerState`.
- UTxO-HD in-memory: `meta` + `state` + `tables/tvar`.
- UTxO-HD LSM: `meta` + `state` + LSM tree files.

No magic bytes or headers in immutable files — versioning is implicit
in the directory shape (`ledger/<dir>/tables/` → UTxO-HD V2).

### Key Rust reference locations

- Immutable-file discovery:
  `~/packages/input-output-hk/mithril/internal/cardano-node/mithril-cardano-node-internal-database/src/entities/immutable_file.rs:36-44`
- Ledger-dir detection (legacy vs. UTxO-HD):
  `.../ledger_state_snapshot.rs:13-35`
- Mithril download + unpack:
  `~/packages/input-output-hk/mithril/mithril-client/src/cardano_database_client/download_unpack/internal_downloader.rs:50-123`
- Signable message (Merkle root of digests):
  `.../internal/cardano-node/mithril-cardano-node-internal-database/src/signable_builder/cardano_database.rs`
- API struct `CardanoDatabaseSnapshot`:
  `~/packages/input-output-hk/mithril/mithril-common/src/entities/cardano_database.rs:25-53`

### Canonical Haskell reference

The actual byte format of `.chunk` / `.primary` / `.secondary` /
ledger-state files lives in `ouroboros-consensus`

ouroboros-consensus:  /Users/rssh/packages/IntersectMBO/ouroboros-consensus

### Amaru

Amamru:  /Users/rssh/packages/pragma-org/amaru


### Implications for M10 / M10b

- The current M10 scope (Scalus-native snapshot format with our own
  framing) is independent of Cardano Database V2 — still useful as an
  interchange for Scalus-owned snapshots.
- M10b's Mithril path requires parsing V2 artefacts on JVM. The
  parsers we need (in rough order):
  1. ImmutableDB chunk/primary/secondary — straightforward binary
     formats, Babbage+ block CBOR is well-understood.
  2. Ledger-state CBOR — large (GBs), Babbage+ only per user
     constraint; schema documented in cardano-ledger + consensus
     modules.
  3. UTxO-HD tables — only needed if we support UTxO-HD nodes; can
     be deferred.

## M10b progress (2026-04-24)

The pieces landed so far in `scalus-chain-store-mithril`:

- **Bulk download** — `MithrilClient.downloadCardanoDatabaseV2(meta,
  destDir, ...)` streams every `immutable-{1..N}.tar.zst` + `ancillary.tar.zst`
  + `digests.tar.zst` from the aggregator's CDN, with bounded concurrency,
  `.extracted` markers for resumability, and an `ImmutableFileRange` parameter
  for CDN-window-limited testnets (preview retains only ~last 15 K chunks).
  Verified end-to-end at preview scale: 2.37 M blocks / 5.35 GB, ~5 min.
- **On-disk format decoders** — `ImmutableDb{Parser,Reader}` decodes the
  `.primary`/`.secondary`/`.chunk` trios (Shelley+ entry shape only);
  `DigestsVerifier` streams SHA-256 against the manifest for integrity.
- **HFC disk-block decoder** — `HfcDiskBlockDecoder` peels cardano-node's
  `[listLen 2, word8 era, inner_block]` on-disk wrapper and routes the inner
  bytes through `scalus-cardano-ledger`'s `Block` CBOR decoder. Verified
  against the full preview snapshot: 100% of 2.37 M blocks decode (eras 6 and
  7, Conway + Dijkstra).
- **Block-history restore** — `ImmutableDbRestorer(store).restore(dir)`
  walks the ImmutableDB, decodes every block, and calls
  `store.appendBlock(AppliedBlock.fromRaw(tip, block))` in slot order.
  Round-tripped through `KvChainStore(InMemoryKvStore())`.

### The remaining gap: ledger-state → UTxO-set restore

**`ImmutableDbRestorer` alone does NOT produce a correct UTxO set when the
restore window excludes genesis.** `KvChainStore.appendBlock` maintains the
UTxO set via per-block deltas, and silently drops spends whose input isn't
already in the local UTxO set (the documented "missing-input tolerance").
On preview — where the aggregator CDN retains only `[~10000..tip]`, not
genesis — every tx input referencing a UTxO created before the earliest
retained chunk is dropped. The store ends up with all created outputs in the
retained window, minus the subset of spends the deltas happened to see —
not the actual UTxO set at the tip.

The intended flow is unchanged from M10.P3:

```
1. download V2 artefact           (done)
2. verify digests                 (file-level done; Merkle+cert-chain pending)
3. parse ledger-state CBOR        → Iterator[(TransactionInput, TransactionOutput)]   ← MISSING
4. chainStoreUtxoSet.restoreUtxoSet(tip, iter)                                        (trait ready; consumer missing)
5. ImmutableDbRestorer.restore(dir, store)  (done; populates block history for replay)
```

Step 3 is the next concrete milestone. Block-history restore on its own is
still useful for `ChainStoreReplaySource` (checkpoint replay only reads
blocks) but Heavy-mode `findUtxos` needs steps 3+4 first.

### Ledger-state parser — scope for M10b.next

Mithril's `ancillary.tar.zst` extracts a directory under `ledger/<slot>/`
containing (UTxO-HD InMemory layout):

- `meta` — a small CBOR blob with tip + protocol version + snapshot metadata.
- `state` — the `NewEpochState` sans tables (shelley-ledger type), CBOR,
  typically 10s–100s of MB. Governance, stake-pool registration, treasury,
  reward accounts, etc.
- `tables/tvar` — the UTxO set itself as a serialised `DiffMK (UTxO)` — a
  CBOR-encoded map `TransactionInput → TransactionOutput`. This is the only
  file the M10 restorer actually needs to seed `restoreUtxoSet`.

Legacy (pre-UTxO-HD) snapshots ship a single CBOR-encoded `ExtLedgerState`
file with the UTxO map nested inside. Preview and mainnet moved to UTxO-HD
in the Chang hard-fork era; we target UTxO-HD InMemory only. LSM backend
tables are a different on-disk format — deferred to **M10d**
(see `indexer-node.md`). Briefly: InMemory is the upstream end-user default
(per `ouroboros-consensus` `LedgerDB.Args.defaultArgs`), is what Mithril
aggregators currently sign and serve for preview/mainnet, and is the only
layout our M10b.next parser needs to handle. LSM is a newer, stake-pool-
oriented opt-in backend — its on-disk format lives in a separate sub-library
(`ouroboros-consensus-lsm`) and evolves independently; users who run it
locally can still convert between flavors via `mithril-client tools
utxo-hd snapshot-converter`. Pre-UTxO-HD legacy is not on the roadmap at
all (no aggregator serves it, no upstream produces it on a post-Chang
chain).

The sniffer should **reject** legacy and LSM layouts with a typed
`UnsupportedLedgerSnapshotFormat(which)` rather than silently degrade.

Concrete work items:
- **Wire-format decoders**: `TransactionInput`/`TransactionOutput` already
  have Borer `Decoder`s in `scalus-cardano-ledger` (used by the block path).
  Reuse them for the `tables/tvar` map values; the key encoding matches too.
- **Streaming reader**: decode one `(input, output)` at a time, never
  materialise the full map. The file can be 1–2 GB uncompressed on mainnet;
  `InputStream`-backed Borer reader with a `DiffMK`-shaped outer loop.
- **Cross-check against `meta.tip`**: the ledger-state dir's tip must match
  the immutable-db tip the restorer is about to apply; abort on mismatch.
- **Pathfinding**: `ledger/` contains one or more `<slot>/` subdirs; use
  the highest-slot dir. If there's a `bootstrap.node` marker file at the
  top of the extracted snapshot, honor Mithril's usual "ready" signal.
- **Integration**: new `LedgerStateRestorer(store).restore(ledgerDir)` that
  calls `store.asInstanceOf[ChainStoreUtxoSet].restoreUtxoSet(tip, iter)`.
  Followed by `ImmutableDbRestorer` for block history.

Estimated scope: 2–4 days including decoder work, streaming plumbing,
round-trip test against the committed preview fixture, and a full-preview
probe. Canonical byte-level reference: `cardano-ledger` Haskell package's
`Cardano.Ledger.Shelley.LedgerState` + `Ouroboros.Consensus.Ledger.Tables`.
Amaru's Rust ledger-state parser in `/Users/rssh/packages/pragma-org/amaru`
is an existing port we can cross-check against.

### Other deferred work

- **Merkle-root + cert-chain verification** of the digests manifest.
  Current `DigestsVerifier.verify` checks the manifest matches what's on
  disk; it doesn't check the manifest itself was signed. The Merkle root
  would be computed client-side and fed to the WASM client's
  `verify_certificate_chain` to anchor it in the Mithril aggregator's
  signed-message chain.
- **Byron-era block decoding**. The ImmutableDB parser rejects Byron
  chunks (28-byte hashes); mainnet full-history restores would hit them
  in the first ~200 chunks. Preview has aged Byron out of its CDN so
  this isn't blocking.
