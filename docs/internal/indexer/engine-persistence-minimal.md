# Engine persistence — minimal (Warm-restart v1)

**Status.** Design doc for milestone M6 of
`docs/local/claude/indexer/indexer-node.md`. Not yet implemented.
M5 (`cardano-network-chainsync.md`) is the substrate: a real N2N
chain-sync loop feeding `AppliedBlock` / `RollBackward` into the
engine. M6 makes that state survive a process restart — minimally
— so the Warm-restart contract advertised in the indexer-node
doc becomes real by default.

This is the **minimal** half of the original M6 plan. The
**advanced** half — snapshot-restore reconciliation, schema
migration, compaction beyond trivial trim-on-shutdown — is
deferred until after M10 (ChainStore) and M11 (Mithril) land,
when the design can be informed by how snapshot restore
actually works.

## Scope

M6 delivers:

1. A `EnginePersistenceStore` trait plus a file-backed
   implementation keyed by `appId` (reverse-DNS), landing the
   path / directory layout described in *App identity and
   persistence location* in `indexer-node.md`.
2. The `appId: String` field becomes required on
   `StreamProviderConfig`, and the config gains
   `enginePersistence: EnginePersistenceStore` defaulting to
   `EnginePersistenceStore.fileForApp(appId)`.
3. A **tip + params + ownSubmissions + per-bucket delta-log**
   persisted shape. No separate "bucket snapshot" format —
   buckets are reconstructed on startup by replaying the
   delta-log (with an occasional full rewrite as a compaction
   step).
4. Durable write path: append-only log appended on every
   `onRollForward` / `onRollBackward` and on `notifySubmit`;
   full-rewrite on clean shutdown or when the log grows past
   a configurable threshold.
5. Startup path: load the log, rebuild `RollbackBuffer` /
   `Bucket[*]` / `TxHashIndex.ownSubmissions` / `paramsRef` /
   `tipRef`, then ask chain-sync to resume from the loaded
   tip via `FindIntersect([savedTip])`.
6. Horizon-aged-peer handling: if `FindIntersect([savedTip])`
   returns `MsgIntersectNotFound`, the provider's `create`
   future fails with `ChainSyncError.NoIntersection(tip)` —
   same typed failure M5 already surfaces — and the app
   decides whether to wipe persistence and cold-start.
7. Warm-restart becomes the **default**: anyone setting
   `appId` gets it for free; the Cold path is explicit opt-out
   via `EnginePersistenceStore.noop`.

M6 does **not** deliver:

- **Snapshot-restore reconciliation.** There is no ChainStore
  to restore from yet (M10) and no Mithril bootstrap (M11).
  When those land, advanced persistence will need a policy
  for "engine persistence says tip X, ChainStore says tip Y
  — which wins?" — designing that here, blind, would be
  throwaway work.
- **Schema migration across library versions.** A single
  schema version tag is written; readers that see an unknown
  version fail fast with `EnginePersistenceError.SchemaMismatch`
  and the caller chooses to wipe or abort. Formal migration
  infrastructure is M6b/M12.
- **Multi-consumer durability, checksums on every record,
  fsync-per-record tuning.** The log writes use
  `rename-over-temp` for full rewrites and `FileChannel.force`
  on a configurable cadence for appends; we do not chase
  single-record durability. Apps with at-least-once per-event
  requirements should use the Checkpoint-driven contract (M7),
  not rely on engine persistence.
- **`StartFrom.At(point)` per-subscription replay.** Still
  M7. M6 replays the *connection*'s start-point from the
  saved tip; per-subscription replay is an orthogonal
  engine-level feature.
- **Bucket-internal compression.** The log is plain CBOR-framed
  records. Gzip / dictionary schemes are deferred; compaction
  via full-rewrite is sufficient for M6's steady-state shape.
- **Cross-platform native file handling.** JVM-only in M6.
  Scala.js (Node FS) and Scala Native targets are additions
  in a later pass, cross-compiled once the JVM path stabilises.

## Why now

M5 proves the read path end-to-end: we can stay connected to
a public relay, ingest blocks, fan them out. The next worst
experience a user hits is restarting the process and
silently losing correctness: the window between shutdown
and restart drops `Spent` events on the floor, `Pending`
submissions disappear, `fetchLatestParams` resets to the
stale `cardanoInfo.protocolParams` seed.

`indexer-node.md` already names these as the reason to ship
M6 directly on top of M5:

> Directly on top of (5) because `Synthetic` has nothing to
> persist; once N2N chain-sync is real, every non-warm
> restart wastes a full seed-from-backup pass per
> subscription, loses `Pending` submissions, and (more
> seriously) silently drops any `Spent` event that happened
> during downtime — a correctness footgun for any per-event
> side-effect consumer.

What changes in this revised M6 is the *shape* of what we
persist: a delta-log covering the rollback window, rather
than a periodic full snapshot. Two reasons:

- **Write cost stays proportional to chain activity**, not
  to bucket size. A batcher watching one address with 10
  UTxOs and an empty block stream writes approximately
  zero bytes per block; the same app watching a busy script
  writes a handful of CBOR records per block. No
  "every-N-seconds scan the whole world" pass.
- **Ageing out for free.** The rollback buffer already
  trims per-block delta-log entries (`forgetUpTo` in
  `Bucket`, `forgetBlock` in `TxHashIndex`). The on-disk log
  inherits the same semantics: records are tagged by
  `ChainPoint` and the reader discards anything past the
  horizon. No separate retention policy.

Compaction to a cold snapshot remains useful — the log
would grow monotonically otherwise — but it's a
rewrite-on-clean-shutdown or rewrite-on-threshold step,
not the steady-state path.

## Architecture at a glance

```
  Engine
     │ onRollForward / onRollBackward / notifySubmit
     │ close()
     ▼
  EnginePersistenceJournal        ◄── append path
     │ write delta record
     │ periodic force()
     ▼
  EnginePersistenceStore          (trait — pluggable)
     │
     ▼
  FileEnginePersistenceStore      (file-backed; M6 impl)
     │
     ▼
  <appId>.log        <appId>.snapshot        (directory per appId)
  ^^ append-only    ^^ occasional full rewrite ("compaction")


  Engine (startup)
     ▲ reload tip / params / ownSubmissions / rollback buffer / buckets
     │
  EnginePersistenceStore.load()
     │ reads snapshot (if present) then replays log on top
     ▼
  EngineSnapshot (in-memory struct handed to Engine's constructor)

  Engine (ready) → Provider starts chain-sync with
  FindIntersect([savedTip])
```

Two files per `appId`:

- `<appId>.snapshot` — the last fully compacted state. Rewritten
  atomically (temp + rename) on clean shutdown and on log-size
  threshold. Absent on first run.
- `<appId>.log` — append-only journal of records produced since
  the last snapshot. Truncated on each rewrite.

This is a minimally durable WAL+snapshot pair — the same shape
RocksDB/LMDB/LevelDB use, implemented in a few hundred lines of
pure-Scala file I/O because the record set is small and the
concurrency model is trivial (the engine's single worker
thread is the only writer).

## Module layout

M6 extends `scalus-streaming-core`; no new subprojects. All the
engine-side state already lives there, and the persistence
concerns are cross-adapter.

```
scalus-streaming-core/
    shared/src/main/scala/scalus/cardano/node/stream/engine/
        Engine.scala                    ← MODIFIED (persistence hooks, rebuildFrom)
        Bucket.scala                    ← unchanged (history is populated via engine)
        TxHashIndex.scala               ← MODIFIED (ownSubmissionsSnapshot accessor)
        RollbackBuffer.scala            ← unchanged
        persistence/                    ← NEW (all shared — JVM / Native / Node-JS)
            EnginePersistenceStore.scala    // trait + .noop + .inMemory
            JournalRecord.scala             // record ADT, BucketDelta, AppliedBlockSummary
            EngineSnapshot.scala            // EngineSnapshotFile, PersistedEngineState
            EnginePersistenceError.scala
            PersistenceCodecs.scala         // borer codecs + encode/decode helpers
            FileEnginePersistenceStore.scala  // file-backed impl; NIO + FileChannel
            PlatformPaths.scala               // XDG / macOS / Windows data root
    shared/src/test/scala/…             // codec / in-memory round-trip
    jvm/src/test/scala/…                // filesystem tests (real disk)
```

### Platform support (realised M6 shape)

- **JVM / Scala Native**: `FileEnginePersistenceStore` runs natively on NIO — file
  persistence works as designed.
- **Scala.js on Node**: the same class compiles because Scala.js's stdlib stubs out
  `java.nio.file` and `java.nio.channels`. Runtime behaviour on Node is untested and
  likely partial until a dedicated Node-`fs` backend lands. JS-on-Node users who need
  persistence today pass `EnginePersistenceStore.noop` explicitly.
- **Scala.js in browser**: cannot open files at all. Users pass
  `EnginePersistenceStore.noop` (Cold-restart semantics).

Raw TCP (N2N) and file storage are orthogonal platform concerns — N2N fails in the
browser because browsers can't open raw sockets, and file storage fails in the
browser because browsers have no filesystem. Both back to `.noop` / explicit opt-out.

`scalus-streaming-fs2`, `scalus-streaming-ox`, and
`scalus-cardano-network` do **not** change — they only thread
`config.enginePersistence` through to the engine constructor.

Dependency additions: none. The CBOR codec uses the existing
`borer` dependency already in the module.

## Persistence record model

A log file is a sequence of length-prefixed CBOR-encoded
`JournalRecord`s.

```scala
sealed trait JournalRecord
object JournalRecord {
    /** Emitted on every `onRollForward`. Carries the
      * per-bucket deltas pre-extracted by the engine so the
      * reader can rebuild buckets without replaying the full
      * block.
      */
    final case class Forward(
        tip:           ChainTip,
        txIds:         Set[TransactionHash],
        bucketDeltas:  Map[UtxoKey, BucketDelta]
    ) extends JournalRecord

    /** Emitted on every `onRollBackward`. The reader pops from
      * the rebuilt rollback buffer until tip == to, mirroring
      * the live behaviour.
      */
    final case class Backward(to: ChainPoint) extends JournalRecord

    /** Emitted on every `notifySubmit`. ownSubmissions is
      * append-only; forgetting is a separate explicit record.
      */
    final case class OwnSubmitted(hash: TransactionHash) extends JournalRecord

    /** Emitted when the app calls `clearOwnSubmission(h)`. Rare. */
    final case class OwnForgotten(hash: TransactionHash) extends JournalRecord

    /** Emitted when the engine observes a protocol-params change.
      * Until on-chain param tracking lands (outside M6 scope)
      * this only fires when `backup.fetchLatestParams` is called
      * and returns a different value from the current cell.
      */
    final case class ParamsChanged(params: ProtocolParams) extends JournalRecord
}

/** Serialisable form of a bucket's per-block effect. Mirrors
  * `Bucket.Delta` but CBOR-encodable without forcing the engine
  * to depend on codec machinery.
  */
final case class BucketDelta(
    added:   Seq[CreatedRec],
    removed: Seq[SpentRec]
)
```

A snapshot file is one CBOR-encoded `EngineSnapshotFile`:

```scala
final case class EngineSnapshotFile(
    schemaVersion: Int,                // starts at 1; bump on incompatible change
    appId:         String,             // cross-check against config
    networkMagic:  Long,               // cross-check against config.cardanoInfo
    tip:           Option[ChainTip],
    params:        ProtocolParams,
    ownSubmissions:Set[TransactionHash],
    volatileTail:  Seq[AppliedBlockSummary],  // last securityParam blocks (tx-ids + per-bucket deltas)
    buckets:       Map[UtxoKey, BucketState]  // `current` map of each live bucket
)

final case class BucketState(
    key:     UtxoKey,
    current: Map[TransactionInput, TransactionOutput]
)

final case class AppliedBlockSummary(
    tip:           ChainTip,
    txIds:         Set[TransactionHash],
    bucketDeltas:  Map[UtxoKey, BucketDelta]
)
```

The snapshot carries `volatileTail` so the rollback buffer is
faithfully rebuilt — not just the tip.

Size expectations:

- **Snapshot.** Batcher watching one address with ~10 UTxOs:
  a few hundred bytes. Marketplace script with 2000 active
  UTxOs: a few hundred KB. `volatileTail` dominates for
  write-heavy subscriptions: up to `securityParam` × average
  per-block delta size, typically a few MB for a busy script
  on mainnet.
- **Log (since last compaction).** Proportional to block count
  × per-block delta size. A single idle block ≈ 50 bytes
  (Forward with empty `bucketDeltas`); a busy block a few KB.
  Compaction threshold default `logSizeThreshold = 4 MiB`; at
  mainnet block cadence (one per ~20s) that's roughly 30 min
  of sync for an average-traffic subscription.

### Why store `bucketDeltas` per forward (not raw blocks)

The engine already extracts per-bucket deltas in `Bucket.applyForward`
as a side-effect of fan-out. Writing them straight to the
journal reuses that work, avoids re-filtering blocks on
reload, and keeps the on-disk format aligned with the
in-memory `Bucket.history`. Raw blocks would be an order of
magnitude larger (the same `TransactionOutput` data is already
on the wire, in borer cache, and in `byKey` — no reason to
write it a fourth time).

## Startup flow

```scala
// Pseudocode — real impl inside `Engine.restoreFrom` or a
// companion `EngineLoader`.
def createWarm(config: StreamProviderConfig): Future[Engine] =
    config.enginePersistence.load().flatMap {
        case None =>
            // First run for this appId — cold start.
            Future.successful(Engine.cold(config))
        case Some(persisted) =>
            // Cross-check — wrong network? wrong appId? fail fast.
            if persisted.appId != config.appId ||
               persisted.networkMagic != config.cardanoInfo.networkMagic then
                Future.failed(EnginePersistenceError.Mismatched(persisted, config))
            else
                Future.successful(Engine.rebuildFrom(persisted, config))
    }
```

`Engine.rebuildFrom`:

1. Create the engine as usual.
2. Populate `paramsRef`, `tipRef`, `txHashIndex.ownSubmissions`,
   `byKey[*]` (from snapshot `buckets`), and
   `rollbackBuffer` (from snapshot `volatileTail`).
3. Re-apply each `JournalRecord` in the log in order, through
   the **same** private mutators the live engine uses — no
   parallel code path for replay.
4. Return the restored engine. The chain-sync applier reads
   `engine.currentTip` and uses it as the `StartFrom.At(point)`
   argument to `FindIntersect`.

On reload-then-fail:

- If journal CBOR decoding fails mid-record, truncate the log
  at the last fully-decoded record's offset (we know the
  length-prefix framing boundary) and continue. Logged at
  warning level; tested via corrupt-tail fixtures.
- If the snapshot's CBOR decode fails or `schemaVersion` is
  unknown, return `EnginePersistenceError.SchemaMismatch` to
  the caller. The caller's only recourse in M6 is to delete
  the files (`EnginePersistenceStore.reset(appId)` helper
  provided) and cold-start. An app that wraps that reset in a
  "blow away and re-seed from backup" is making an informed
  choice.

## Runtime write path

Engine's existing private `submit { … }` worker already
serialises every state mutation. We hook persistence inside
each mutator, **after** the in-memory state update succeeds
and **before** the `Future[Unit]` returned to the applier
completes — so the applier's backpressure naturally extends
to persistence:

```scala
// Inside Engine.onRollForward, after the existing body:
//   ... tipRef.set / byKey updates / fan-out …
val record = JournalRecord.Forward(block.tip, block.transactionIds, bucketDeltas)
persistence.appendSync(record)   // synchronous within this worker lane
```

`appendSync` on the file-backed store:

- Serialises the record to CBOR.
- Writes length-prefix + bytes to the log `FileChannel`.
- Does **not** fsync on every write — that would add ~1–10 ms
  per block and M6 does not promise per-event durability.
  Instead, a background ticker calls `channel.force(false)`
  every `fsyncInterval` (default 1 s) and on clean shutdown.

Buffering: the log `FileChannel` is wrapped in a small user-space
`BufferedOutputStream`-equivalent (`ByteBuffer` of 16 KiB) so
idle blocks don't trigger syscalls per record. Buffer is
flushed on each fsync tick.

Crash-safety guarantee (M6): on SIGKILL, at most the last
`fsyncInterval` of records are lost. On clean shutdown
(`engine.shutdown()` or provider `close()`), the engine does
a final `appendFlush()` then writes a compacted snapshot,
then truncates the log. No records are lost on clean
shutdown.

This is weaker than M7's Checkpoint-driven contract and
stronger than M2's Cold. Documented; users with stricter
needs opt into Checkpoint.

## Compaction

Two triggers: **size** and **shutdown**. Same mechanism in both
cases: take a consistent in-memory view (we're on the worker
thread already; no lock needed), serialise an
`EngineSnapshotFile` to `<appId>.snapshot.tmp`, fsync, rename
over `<appId>.snapshot`, truncate `<appId>.log` to zero length,
fsync the log file.

```scala
private def compactIfNeeded(): Unit = {
    if logSizeEstimate > logSizeThreshold then compact()
}

private def compact(): Unit = {
    val snap = takeSnapshot()                 // in-memory build; O(N_buckets + securityParam)
    persistence.writeSnapshot(snap)           // tmp + rename + fsync
    persistence.truncateLog()                 // zero the log, fsync
    logSizeEstimate = 0
}
```

`takeSnapshot()` is O(total live UTxO count across buckets).
For the batcher case (one address, 10 UTxOs) it's microseconds;
for a marketplace watching thousands it's a single-digit ms
on a modern JVM. The worker blocks on it, so fan-out pauses
during compaction — acceptable given the rare triggering.

### Why not background compaction

Because the worker-thread invariant is load-bearing: mutating
state from two threads is how you lose causality in fan-out.
The cost of a blocking compaction is bounded by the snapshot
size, which is bounded by the subscription footprint, which
apps already size for. If a future profile needs async
compaction, it can copy the in-memory state onto a private
structure in the worker and hand the copy off to a dedicated
writer thread — but that's M6b work, not M6.

## `EnginePersistenceStore` surface

```scala
trait EnginePersistenceStore {
    /** Load the persisted state, if any. Returns `None` when
      * no prior state exists for this `appId`. Returns a
      * failed Future with `EnginePersistenceError.*` on
      * corruption or schema mismatch.
      */
    def load(): Future[Option[PersistedEngineState]]

    /** Append a journal record synchronously from the engine's
      * worker thread. Implementations may buffer; they may
      * not throw for reasons the engine can't meaningfully
      * recover from (OOM on the buffer is the realistic edge).
      */
    def appendSync(record: JournalRecord): Unit

    /** Flush and fsync. Called by the engine on clean shutdown
      * and by the background ticker. */
    def flush(): Future[Unit]

    /** Rewrite the snapshot and truncate the log. */
    def compact(snapshot: EngineSnapshotFile): Future[Unit]

    /** Close file handles. Engine calls this after `flush()`
      * during shutdown. */
    def close(): Future[Unit]
}

object EnginePersistenceStore {
    def fileForApp(appId: String): EnginePersistenceStore =
        fileForApp(appId, snapshotEvery = 30.seconds, fsyncInterval = 1.second,
                   logSizeThreshold = 4.megabytes)

    def fileForApp(
        appId: String,
        snapshotEvery: FiniteDuration,
        fsyncInterval: FiniteDuration,
        logSizeThreshold: Long
    ): EnginePersistenceStore

    /** Explicit path override. */
    def file(path: Path, ...): EnginePersistenceStore

    /** In-memory, for tests. `load` returns whatever the
      * previous session appended. */
    def inMemory(): EnginePersistenceStore

    /** No-op. Accepting this is accepting Cold semantics —
      * see *Restart semantics* in indexer-node.md. */
    val noop: EnginePersistenceStore

    /** Helper for callers that need to manually wipe state. */
    def reset(appId: String): Future[Unit]
}

final case class PersistedEngineState(
    snapshot: Option[EngineSnapshotFile],
    journal:  Seq[JournalRecord]
)

sealed trait EnginePersistenceError extends RuntimeException
object EnginePersistenceError {
    final case class SchemaMismatch(found: Int, expected: Int) extends EnginePersistenceError
    final case class Mismatched(detail: String)              extends EnginePersistenceError
    final case class Io(cause: Throwable)                    extends EnginePersistenceError
    final case class Corrupt(at: Long, cause: Throwable)     extends EnginePersistenceError
}
```

`appendSync` is intentionally synchronous + infallible: the
engine's worker thread holds the only handle, and any
`IOException` turns into a `Failure` stored on the append
future… except there is no append future in steady state —
the journal is fire-and-forget. Failures surface at `flush`
or at the next `compact`, where the engine's `Future[Unit]`
contract is meaningful again.

## Cross-check fields on load

Three values on `EngineSnapshotFile` exist to catch the
"person edited config, forgot to wipe state" case early:

- `appId` — caught by the factory. File sitting at
  `~/.local/share/scalus-stream/com.foo.bar/` with a different
  `appId` inside → user renamed something; fail before
  damaging things further.
- `networkMagic` — caught by the factory. Switching from
  preview to mainnet with a stale snapshot would corrupt the
  rollback buffer (wrong chain entirely).
- `schemaVersion` — future-proofing. M6 ships version 1; M6b
  / M12 will define migrations.

Mismatched → `EnginePersistenceError.Mismatched`; caller
explicitly `reset`s or aborts.

## Interaction with M5 chain-sync

The applier currently calls `IntersectSeeker.pointsFor(startFrom)`
with `startFrom = StartFrom.Tip`. In M6 the provider threads the
persisted tip through:

```scala
val startPoint =
    engine.currentTip.map(t => StartFrom.At(t.point)).getOrElse(StartFrom.Tip)
ChainApplier.spawn(connection, engine, startPoint, logger)
```

Three cases:

- **Fresh install** (no persistence): `currentTip = None` →
  `StartFrom.Tip`, two-step dance as M5 does today.
- **Warm restart** (persistence loaded, tip within peer's
  horizon): `FindIntersect([savedTip])` succeeds, chain-sync
  resumes, replay drives `onRollForward` through each
  downtime block. Engine deduplicates naturally (tip already
  matches; next forward is genuinely new).
- **Warm restart, past peer horizon** (app was down for days,
  relay's header store rolled past our checkpoint):
  `MsgIntersectNotFound` → applier's `done` future fails with
  `ChainSyncError.NoIntersection(tip)`; provider's `create`
  future fails with the same cause. App logs, decides to
  reset and cold-start, or to pick a different relay.

The applier's loop path is unchanged — once `FindIntersect`
succeeds, blocks flow through the normal `onRollForward` path.
Every replayed block is also persisted; on the next clean
shutdown the log grows and a compaction lands the combined
state.

## Provider wiring

### Fs2

```scala
// Fs2BlockchainStreamProvider.create, M6 shape:
for
    persisted  <- IO.fromFuture(IO(config.enginePersistence.load()))
    engine      = persisted match
        case None            => new Engine(config.cardanoInfo, backup, securityParam, config.enginePersistence)
        case Some(state)     => Engine.rebuildFrom(state, config.cardanoInfo, backup, securityParam, config.enginePersistence)
    startFrom   = engine.currentTip match
        case Some(t) => StartFrom.At(t.point)
        case None    => StartFrom.Tip
    connection <- connectForSource(config.chainSync, config.cardanoInfo.networkMagic)
    handle      = ChainApplier.spawn(connection, engine, startFrom, logger)
    provider    = new Fs2BlockchainStreamProvider(engine, ...)
    _           = bindTeardown(connection, handle, provider, config.enginePersistence)
yield provider
```

### Ox

Same shape inside an Ox scope; persistence flush + close are
part of the scope teardown sequence.

### `bindTeardown` additions

M5 handled connection + handle + provider. M6 adds:

- `provider.close()` ⇒ `engine.shutdown()` ⇒
  `persistence.flush()` ⇒ `persistence.compact(takeSnapshot())`
  ⇒ `persistence.close()`. Clean-shutdown compaction is the
  cheap way to start the next session with an empty log.
- On `handle.done` failure (chain-sync blew up), still flush
  and close persistence — the state up to the failure is
  legitimately durable; failing the provider doesn't mean
  failing what we've already written.

## Restart semantics, revisited

The *Restart semantics* table in `indexer-node.md` promises
Warm as default and Checkpoint as opt-in. M6 lights up the
Warm column:

| | Cold (opt-out) | Warm (this milestone) | Checkpoint-driven (opt-in, M7) |
|---|---|---|---|
| **What persists** | nothing | tip, params, `ownSubmissions`, rollback buffer, active-bucket state | engine state *plus* app-owned checkpoints |
| **Downtime events** | lost as discrete events | delivered on resume (replayed through chain-sync) | delivered from the checkpoint forward |
| **`subscribeTransactionStatus(h)` `Pending`** | lost | preserved | preserved iff app persisted the hash |
| **Default config** | opt-out (`.noop`) | default | opt-in |

The Warm row's "delivered on resume" bullet relies on chain-sync
replay driven by our saved tip. Nothing synthetic is emitted;
every `Created`/`Spent` the subscriber receives on resume is
the real event from the real block the peer hands us, routed
through the same `onRollForward` path as live operation.

## Error model

Adds to M5's.

| Failure | Where | Cause | Observers see |
|---|---|---|---|
| Load finds unknown schema version | `create` | `EnginePersistenceError.SchemaMismatch` | provider future fails |
| Load finds mismatched `appId` / `networkMagic` | `create` | `EnginePersistenceError.Mismatched` | provider future fails |
| Log-tail CBOR decode fails | `create` | warning log; truncated | continues, startup proceeds |
| Snapshot CBOR decode fails | `create` | `EnginePersistenceError.Corrupt` | provider future fails |
| `FindIntersect([savedTip])` not found | applier | `ChainSyncError.NoIntersection(tip)` (M5 cause) | subscribers fail on resume |
| I/O failure during `appendSync` | worker | swallowed + logged; `flush()` surfaces it next tick | subscribers unaffected unless flush/compact fails |
| I/O failure during `compact` | worker | `EnginePersistenceError.Io` on the compact Future | engine logs; continues running (next compact retries) |
| I/O failure during shutdown flush | shutdown path | logged; shutdown completes | process exits; next start may replay from stale log, caught by cross-check |

A soft-and-log policy on per-record append failures is
deliberate: losing a record is the same as losing a restart
window of records (chain-sync will replay either way). Hard
failures happen only at load time (where we can fail the
`create` future) or at the explicit flush/compact boundaries
(where the engine is already in a shutdown-ish state).

## Test strategy

Four tiers, mirroring prior milestones.

### 1. Unit tests — shared, JVM + JS (when JS lands)

- **`JournalRecord` codec round-trip**: every variant.
- **`EngineSnapshotFile` codec round-trip**: empty, single-bucket,
  many-bucket, full-`volatileTail`.
- **`Engine.rebuildFrom`** against handcrafted
  `PersistedEngineState` fixtures. Asserts `currentTip`,
  `latestParams`, `ownSubmissions` membership, `byKey` cardinality
  and contents, `rollbackBuffer.size` and `tip`.
- **Log-tail truncation on partial record**: write N records, then
  write half a record's bytes; loader returns N records and
  truncates to the N-th record boundary.
- **Schema mismatch** surfaces `EnginePersistenceError.SchemaMismatch`.
- **appId / networkMagic mismatch** surfaces
  `EnginePersistenceError.Mismatched`.

### 2. In-memory store tests — shared

Uses `EnginePersistenceStore.inMemory()` plus a
`ChainSyncSource.Synthetic`-driven engine. No file I/O.

- **Round-trip**: apply 20 forwards, 2 rollbacks, 3 submits;
  capture `takeSnapshot()`; reconstruct; assert state equality.
- **Write-path ordering**: journal records appear in the
  same order as the mutators that produced them.
- **Compaction correctness**: fill the log, trigger
  `compact()`, reconstruct from the new snapshot (ignoring the
  now-truncated log); assert equality to pre-compaction state.
- **`ownSubmissions` preserved across Forward → Backward**:
  submit `h`; apply block containing `h`; rollback past it;
  reconstruct; `statusOf(h) == Pending` (not `NotFound`).

### 3. File store tests — JVM

- **First-run directory layout**: `fileForApp("com.test.fixture")`
  creates the XDG-appropriate directory lazily (on first write).
- **Write / close / reopen**: apply forwards, close, reopen,
  assert load returns the state.
- **SIGKILL simulation**: write records, do not flush, close the
  underlying `RandomAccessFile` without `force`; reopen, assert
  records written before last fsync tick are present.
- **Corrupt-tail**: flip bits in the last 100 bytes of the log;
  reopen, assert clean decode of the preceding records and
  warning log for the truncation.
- **Concurrent `fileForApp(sameId)` from two engines**: second
  engine's constructor fails with
  `EnginePersistenceError.Locked` (implementation uses
  `FileLock`). Catches the two-processes-same-appId footgun.

### 4. End-to-end with N2N + yaci

Extends `scalusCardanoNetworkIt`.

- **Warm-restart correctness**: run the batcher against yaci;
  capture subscribed `UtxoEvent`s; kill the process (not clean
  shutdown); restart; feed yaci a few more blocks; assert the
  restarted subscriber sees the downtime events exactly once
  with correct `ChainPoint` attribution.
- **Clean-shutdown compaction**: run, close cleanly, assert the
  log file is zero-length and the snapshot reflects the final
  tip.
- **Horizon-aged peer**: persist a tip, leave yaci running
  long enough for the tip to age past its header store horizon
  (configurable on yaci), restart, assert provider `create`
  fails with `ChainSyncError.NoIntersection`.
- **`appId` suffix switch**: start with `com.test.v1`, stop,
  restart with `com.test.v2`, assert cold-start behaviour (no
  mistaken reuse).

## Out of scope for M6 — explicit list

- Snapshot-restore reconciliation (ChainStore not here yet).
- Schema migration between library versions.
- Scala.js / Scala Native file backends.
- Async / background compaction.
- Per-record fsync durability (M7 Checkpoint territory).
- Automatic wipe on `Mismatched` / `SchemaMismatch` — caller's
  call.
- Full-snapshot-only format (we ship snapshot+log from day 1
  because of write-amplification on big subscriptions).

## Open questions

- **Fsync tuning defaults.** Is 1 s fsync interval too lax for
  the target audience? Tighter → more syscalls → fewer batches;
  looser → more loss on SIGKILL. Start at 1 s, revisit after
  seeing real apps use it.
- **`FileLock` scope.** Per-file (log + snapshot both locked)
  vs. single lockfile. Per-file is simpler but doubles the
  locks; single lockfile is more idiomatic. Default to single
  lockfile `<appId>.lock`.
- **`reset(appId)` path vs. user curiosity.** Clearing state
  requires filesystem access. Provide a documented
  `EnginePersistenceStore.reset(appId)` that does the delete
  behind a `Future`, or just tell users "delete the directory"?
  Pick `reset` — cross-platform path juggling is worth hiding.
- **Rebuild performance on huge buckets.** Loading a snapshot
  with 50k active UTxOs is a few MB of CBOR decode; probably
  sub-second. If it bites, buckets can be loaded lazily (first
  access) — but that complicates `currentTip` semantics. Defer
  unless measured.
- **Interaction with `includeExistingUtxos = true` on resume.**
  A warm-restarted subscriber that re-subscribes with
  `includeExistingUtxos = true` should *not* re-hit the
  backup for UTxOs we already have durably. Needs a check:
  if the subscription's key is already a live bucket with
  current contents, use those; only call `seedFromBackup` when
  the key is fresh. Engine already tracks `byKey` ref-counts
  — the rule is "if bucket existed pre-register, don't seed."
  Needs test coverage; noted so the implementation doesn't
  regress it.
- **Time source for compaction triggers.** Steady-state the
  size threshold dominates; `snapshotEvery` is a wall-clock
  ticker. On a chain that hasn't produced a block in an hour,
  should we still rewrite? Probably not (waste of I/O for no
  state change). Track `dirtySinceLastSnapshot: Boolean`; skip
  ticker compaction when false.
- **What happens when `backup` changes between runs?** Example:
  user switches from Blockfrost to `LocalStateQuery`. The
  snapshot's bucket state doesn't depend on which backup
  filled it, so reload is safe. Document that, don't guard it.
- **Advanced persistence splitting point.** The natural seam
  for M6b is: keep this milestone's *shape* (snapshot + log)
  but re-design reconciliation when snapshot-restore exists.
  All on-disk format decisions made here should stay stable
  into M6b — the advanced milestone only adds records and
  policies, doesn't rewrite.

## References

- `docs/local/claude/indexer/indexer-node.md` — milestone 6
  row; *Restart semantics*; *Engine persistence (on by
  default, M6)*; *App identity and persistence location*.
- `docs/local/claude/indexer/cardano-network-chainsync.md` —
  M5 substrate; how chain-sync applier handles the
  `StartFrom.At(point)` case the applier receives in M6.
- `scalus-embedded-node/scalus-streaming-core/.../engine/` —
  `Engine`, `Bucket`, `TxHashIndex`, `RollbackBuffer` as
  they stand post-M5; read these alongside this doc.
- XDG Base Directory Specification — default path layout on
  Linux / BSD.
- `java.nio.channels.FileLock` — process-exclusion primitive
  for the `<appId>.lock` file.
