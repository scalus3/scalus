# Pluggable `ChainStore` — durable block & UTxO history (M9)

**Status.** Design phase.

M7 shipped the `ChainStore` trait as plumbing only: a covering trait on
`StreamProviderConfig` with a one-method surface (`blocksBetween`) that
`ChainStoreReplaySource` delegates to during checkpoint replay. No
backends existed, so the ChainStore branch of the replay cascade was
unreachable in practice — every M7 replay past the rollback-buffer
horizon fell through to `PeerReplaySource` (transient N2N connection).

M9 closes that gap. It lands:

1. An expanded `ChainStore` trait that covers both the M7 read path
   (block history for replay) and the write path needed to populate the
   store from the live applier on every `onRollForward` / `onRollBackward`.
2. A narrow `KvStore` abstraction in shared code — six methods (`get`,
   `put`, `delete`, `rangeScan`, `batch`, `close`) — so the block-layout
   and UTxO-layout logic in `ChainStore` stays platform-agnostic and the
   backend choice becomes swappable.
3. `InMemoryKvStore` in shared code for tests + JS.
4. `RocksDbKvStore` in a new JVM-only module `scalus-chain-store-rocksdb`,
   keeping the native dependency out of the core streaming JAR.
5. Engine wiring: when a `ChainStore` is configured, live blocks flow in
   via `Engine.onRollForward`; live rollbacks trim it via
   `Engine.onRollBackward`; the replay path's
   `ChainStoreReplaySource` now returns real blocks instead of the M7
   stub-always-exhausts behaviour.

`StorageProfile.Heavy` and Mithril restore (M10) are out of scope for M9
as delivered here; they extend the `ChainStore` surface with UTxO-set
queries and bulk-restore and are tracked as M9.P3 / M10 respectively.

## Why now

The design doc ordering ran M8 (`TxSubmission2`) before M9 (ChainStore).
The order was swapped for two reasons:

- **M7 is only 90% functional without M9.** A checkpoint older than the
  rollback buffer but inside a disk-resident store (typical wallet case:
  last-processed point was a few hours ago; user wipes rollback buffer
  by restarting; re-subscribe with `At(point)`) currently pays the cost
  of a fresh N2N connection + re-intersect + re-fetch every replay.
  With a populated `ChainStore`, the same replay is a local disk scan.
- **M10 (Mithril bootstrap) needs a restore target.** Mithril produces a
  signed UTxO-set snapshot; restoring it requires a populated
  `ChainStore` to load into. Doing M10 before M9 would be designing
  against a mock target.

M8 is genuinely independent — `submit` delegation through the backup
provider already works — so its delivery slot is flexible.

## Scope for M9

Three sub-phases, mirroring the M7 shape (Phase 1/2a/2b).

### M9.P1 — trait + KvStore + in-memory backend

- Expand `ChainStore`:
  ```scala
  trait ChainStore {
      // Read — existing M7 surface.
      def blocksBetween(from, to)
          : Either[ReplayError.ReplaySourceExhausted, Iterator[AppliedBlock]]

      // Write — new.
      def appendBlock(block: AppliedBlock): Unit
      def rollbackTo(to: ChainPoint): Unit
      def tip: Option[ChainTip]

      // Lifecycle.
      def close(): Unit
  }
  ```
  Writes are idempotent by `(slot, hash)` so replay of the same block
  (during restart after a crash mid-write) does not double-count.
- Add `KvStore` trait in `shared/`: six methods, byte-level API. All
  methods are `synchronous` — ChainStore invokes them from the engine
  worker so there's no thread-safety burden on implementations.
- Add `InMemoryKvStore(impl: mutable.TreeMap[ByteString, ByteString])` in
  `shared/` — deterministic, usable in JS + JVM tests.
- Add `KvChainStore(kv: KvStore)` in `shared/` — the only place that
  knows the CBOR encoding of blocks, the key layout, and the tip
  marker. Backends implement `KvStore` only.
- Engine wiring: `StreamProviderConfig.chainStore: Option[ChainStore]`
  stays optional. When present, `Engine.onRollForward` calls
  `chainStore.appendBlock(block)` after updating its own state;
  `Engine.onRollBackward` calls `chainStore.rollbackTo(to)`; the
  provider closes the store in its `preClose` teardown hook.
- Tests:
  - `KvStore` contract suite (run against `InMemoryKvStore` first;
    other backends satisfy the same contract).
  - `KvChainStore` unit tests: append → blocksBetween round-trip;
    rollback trims; tip tracks head.
  - Engine integration: enable ChainStore, apply N blocks, subscribe
    with `At(point)`, observe that replay serves from the store
    instead of falling through to the peer source. Assertable by
    instrumenting the in-memory store's access counter (already the
    pattern `CountingFactory` used in M7 tests).

### M9.P2 — RocksDB backend

- New module `scalus-chain-store-rocksdb` (JVM-only) depending on
  `scalus-streaming-core`. Pulls `org.rocksdb:rocksdbjni` (~30 MB native
  payload; split modules so the core lib stays light).
- `RocksDbKvStore(dbPath: Path)` implements `KvStore` over column
  families (one per keyspace the `KvChainStore` maintains:
  `blocks`, `index`, `tip`).
- Write batching: `KvStore.batch(writes)` maps to a `WriteBatch` with
  `put`/`delete` ops applied atomically. Append-block is a single
  batch of (`blocks/<slot>/<hash> → cbor`, `index/<slot> → <hash>`,
  `tip → <slot>/<hash>/<blockNo>`).
- Tests:
  - `KvStore` contract suite re-runs against `RocksDbKvStore` in a
    tmpdir (JVM-only test scope).
  - End-to-end: open store, apply 100 blocks, close, reopen, verify tip
    and blocksBetween still correct.
  - Lifecycle: close is idempotent; second open of the same path in
    the same JVM fails cleanly (RocksDB's own LOCK enforces this).

### M9.P3 — Heavy-mode UTxO-set queries

- `ChainStoreUtxoSet` mixin (or separate trait the store implements
  optionally):
  ```scala
  trait ChainStoreUtxoSet {
      def findUtxos(query: UtxoQuery): Utxos
      def applyUtxoDelta(
          added: Iterable[(TransactionInput, TransactionOutput)],
          removed: Iterable[TransactionInput]
      ): Unit
      def restoreFromSnapshot(
          utxos: Iterator[(TransactionInput, TransactionOutput)],
          tip: ChainTip
      ): Unit
  }
  ```
- Secondary index in `KvChainStore`: per-`UtxoKey` keyspace maintained
  alongside block writes. `findUtxos(q)` decomposes the query into
  `UtxoKey`s via the existing `QueryDecomposer`, looks up each keyspace,
  post-filters by the full query predicate.
- `Engine.findUtxosLocal` falls through to the `ChainStoreUtxoSet` when
  active buckets don't cover the query and `StorageProfile.Heavy` is
  configured.
- Unlocks M10 (Mithril) — `restoreFromSnapshot` is the Mithril hook.

## Key/value layout (M9.P1/P2)

Six keyspaces; RocksDB hosts them as column families, in-memory store
uses prefixed keys in one `TreeMap`.

| Keyspace | Key | Value | Purpose |
|---|---|---|---|
| `blocks` | `<slot-BE:8><hash:32>` | CBOR-encoded `AppliedBlock` | Range scan for `blocksBetween` |
| `index` | `<slot-BE:8>` | `<hash:32>` | Fast `(slot → hash)` lookup, unique per canonical chain |
| `tip` | singleton `"tip"` | `<slot:8><hash:32><blockNo:8>` | Head-of-chain pointer; engine restart reads this |
| `utxo` (M9.P3) | `<input-cbor>` | `<output-cbor>` | Full UTxO set for Heavy mode |
| `utxo-by-key` (M9.P3) | `<ukey-tag:1><ukey-bytes><input-cbor>` | empty | Secondary index for `findUtxos(query)` |
| `meta` | `"schema-version"` | `<u32>` | Forward-compat guard against format changes |

Slots are encoded big-endian so RocksDB's lexicographic ordering
matches chain order — a cheap property that makes range scans naturally
return blocks in ascending slot order.

On rollback to `to`, the store deletes every `blocks` / `index` entry
with `slot > to.slot` and rewrites `tip`. Done in a single `WriteBatch`
so a crash mid-rollback can't leave the store inconsistent (either the
whole rewind lands or nothing does).

## Interaction with engine persistence (M6)

`EnginePersistenceStore` (M6) and `ChainStore` (M9) are orthogonal:

- `EnginePersistenceStore` persists engine-local state (tip, protocol
  params, active bucket deltas, own submissions) — tiny, journal-style,
  optimised for warm restart of an already-running engine.
- `ChainStore` persists the chain itself (block history, UTxO set) —
  potentially multi-GB, optimised for range scans + bulk restore.

Both can coexist: the file-backed M6 store journals bucket deltas for
active subscriptions only; the M9 ChainStore holds the full chain. On
startup the engine seeds from M6 (fast path, no chain-sync needed for
warm restart), and if a checkpoint replay lands further back than M6's
journal, `ChainStoreReplaySource` lights up from M9 disk state.

No format sharing between the two stores — M6 is journal, M9 is KV.
Advanced persistence (M14) is the milestone that could unify them if
profiling shows value.

## Out of scope for M9

- **Schema migration between library releases.** The `meta/schema-version`
  key exists so M9.P2 can fail loud on an incompatible on-disk format.
  Actual migration infrastructure lands with M14.
- **Async / background compaction.** RocksDB handles its own LSM
  compaction. Anything beyond that (e.g. chunked Mithril restore
  without blocking the engine) lands with M10 / M14.
- **Pruning policy.** `ChainStore` retains everything by default. An
  explicit `pruneBefore(horizonPoint)` method is part of the trait but
  the engine doesn't call it automatically — apps that want bounded
  disk choose their own horizon.
- **Concurrent readers.** The engine worker is the only writer; reads
  go through the same worker serialised for simplicity. Multi-reader
  performance (e.g. a separate read thread for Heavy-mode `findUtxos`)
  is a follow-up.
- **Cross-process sharing.** One `ChainStore` instance per process.
  RocksDB's own LOCK file enforces this at the backend layer.

## References

- `docs/local/claude/indexer/indexer-node.md` — milestone 9 row; *Engine
  state profiles*; *Storage strategy → Chain store*.
- `docs/local/claude/indexer/checkpoint-restart-m7.md` — `ChainStore` as
  the M7 replay-source branch; the source-precedence cascade becomes
  meaningful once M9's backends land.
- `scalus-embedded-node/scalus-streaming-core/shared/src/main/scala/scalus/cardano/node/stream/engine/ChainStore.scala`
  — current (M7) trait surface.
- `scalus-embedded-node/scalus-streaming-core/shared/src/main/scala/scalus/cardano/node/stream/engine/replay/ChainStoreReplaySource.scala`
  — the ReplaySource that delegates to ChainStore.
