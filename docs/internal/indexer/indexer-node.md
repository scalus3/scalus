# Embedded Indexer / Data Node

## Goals

Add support for data indexing in Scalus by embedding a lightweight data node
inside the application process.

From the application developer's point of view:

- Obtain a `BlockchainStreamProvider` from an N2N or N2C endpoint of a Cardano
  node. The embedded data node is spawned under the hood.
- Subscribe via a query (HOAS-embedded DSL or a plain `Query` as in
  `BlockchainProvider`) to UTxO events, transactions, and blocks.
- Receive a stream (in the stream library of choice) and process updates.

The same infrastructure should work with `Emulator` / `ImmutableEmulator` so
developers can test against the same streaming API they use in production.
(Research item: how streaming composes with scenario mode, which is
deterministic replay.)

### Non-goals

- Not itself a general-purpose chain indexer (Yaci-Store, Kupo, Carp). Such
  a service *can* be built on top of this library — plug in a persistent
  `ChainStore`, subscribe with an unrestricted query, and expose a
  GraphQL/REST surface — but that is a downstream project, not part of the
  core deliverable.
- Not a node replacement — we still sync from an upstream Cardano node over
  N2N/N2C.
- No requirement for the user to run a separate auxiliary process (Ogmios,
  db-sync). Direct protocol integration is the main option.

## Motivating example

Batching payment receiver:

- On-chain validator checks that an outgoing transaction is signed by the
  operator key.
- Off-chain batcher subscribes to UTxOs on the receive address. When either a
  time window elapses or a max-UTxO threshold is reached on that address, it
  builds a transaction that sweeps them to a private-key address.

The batcher is a plain application process — no external indexer, no extra
infrastructure. It obtains a `BlockchainStreamProvider`, subscribes to one
address, and consumes events.

## Architecture

```
  Cardano node (remote or local socket)
        │
        ▼   N2N / N2C chain-sync + local-state-query
  ┌──────────────────────────────┐
  │     Protocol client          │  (pure Scala; pallas as reference)
  └──────────────┬───────────────┘
                 ▼
  ┌──────────────────────────────┐
  │  Rollback buffer             │  (~k blocks, k = securityParam)
  │  (immutable tip + volatile)  │
  └──────────────┬───────────────┘
                 ▼
  ┌──────────────────────────────┐
  │  Query engine / indexes      │  (per-subscription state)
  └──────────────┬───────────────┘
                 ▼
  ┌──────────────────────────────┐
  │  Fan-out to subscribers      │  (ScalusAsyncStream adapters)
  └──────────────────────────────┘
```

## Public API

A single trait, `BlockchainStreamProviderTF[F, C]`, extends
`BlockchainProviderTF[F]` so a developer holds one provider and can
both subscribe to events and run point-in-time queries against it.

```scala
trait BlockchainStreamReaderTF[F[_], C[_]: ScalusAsyncStream] extends BlockchainReaderTF[F] {
    // Event subscriptions — push.
    def subscribeUtxoQuery(query: UtxoEventQuery, opts: SubscriptionOptions): C[UtxoEvent]
    def subscribeTransactionQuery(query: TransactionQuery, opts: SubscriptionOptions): C[TransactionEvent]
    def subscribeBlockQuery(query: BlockQuery, opts: SubscriptionOptions): C[BlockEvent]

    // Single-source-of-truth value subscriptions — push.
    def subscribeTip(): C[ChainPoint]
    def subscribeProtocolParams(): C[ProtocolParams]
    def subscribeTransactionStatus(h: TransactionHash): C[TransactionStatus]

    // Inline conveniences lowered to structured queries via macros.
    inline def subscribeUtxo(inline f: (TransactionInput, TransactionOutput) => Boolean): C[UtxoEvent]
    inline def subscribeTransaction(inline f: Transaction => Boolean): C[TransactionEvent]
    inline def subscribeBlock(inline f: Block => Boolean): C[BlockEvent]

    def close(): F[Unit]
}

trait BlockchainStreamProviderTF[F[_], C[_]: ScalusAsyncStream]
    extends BlockchainProviderTF[F] with BlockchainStreamReaderTF[F, C]
```

### Stream / one-shot duality

Anything that mutates over time has both a stream and a one-shot read.
The two share the same engine state — the one-shot is semantically
`subscribeXxx().head`. This guarantees consistency: no two methods can
disagree about the current value because they both read from the same
cell.

| One-shot (from `BlockchainProviderTF`) | Stream (added here)                            |
|----------------------------------------|------------------------------------------------|
| `currentSlot`                          | `subscribeTip()` (slot is in `ChainPoint`)     |
| `fetchLatestParams`                    | `subscribeProtocolParams()`                    |
| `checkTransaction(h)`                  | `subscribeTransactionStatus(h)`                |
| `findUtxos(query)`                     | `subscribeUtxoQuery(query)` (initial + deltas) |

Polling disappears: `pollForConfirmation(h)` becomes
`subscribeTransactionStatus(h).find(_ == Confirmed).head` — no sleep
loops, no missed-update windows.

### Conventions

- **Start point.** Subscriptions take a `startFrom: ChainPoint`
  (`Tip | Origin | At(slot, hash)`). Default `Tip`.
- **Rollback in the event stream.** Each event enum carries a
  rollback variant:
  ```scala
  enum UtxoEvent:
      case Created(utxo: Utxo, producedBy: TransactionHash, at: ChainPoint)
      case Spent(utxo: Utxo, spentBy: TransactionHash, at: ChainPoint)
      case RolledBack(to: ChainPoint)
  ```
  Same pattern for transaction/block streams. Subscribers treat
  `RolledBack` as "discard everything past `to`."
- **Confirmation depth.** Per-subscription `noRollback` / explicit
  `confirmations` flag in `SubscriptionOptions` — subscribers that
  can't undo state get only stable events.
- **Query pushdown.** Inline lambda variants lower via macros into a
  structured query so the engine can filter before fan-out;
  non-lowering predicates fall back to post-filter.

## ScalusAsyncStream

The uniform async-channel contract that every adapter implements. Lives
in `scalus.cardano.infra` and pairs with [[ScalusAsyncSink]]:

```scala
trait ScalusAsyncSink[A] {
    def offer(a: A): Future[Unit]      // backpressure via Future
    def complete(): Future[Unit]
    def fail(t: Throwable): Future[Unit]
}

trait ScalusAsyncStream[S[_]] {
    def channel[A](
        bufferPolicy: BufferPolicy,
        onCancel: () => Unit
    ): (ScalusAsyncSink[A], S[A])
}
```

`Future` is the lingua franca: it is in stdlib, available on JVM and JS,
and lets adapters bridge their native effect (cats-effect `IO`,
direct-style ox) without leaking those concepts into core.

The data-node engine writes
`summon[ScalusAsyncStream[C]].channel(opts.bufferPolicy, () => unregister(id))`
and never branches on the underlying stream library. Adapters back the
channel with native primitives; the surface they expose is identical.

Concrete instances live in adapter modules (currently in the test tree
of `scalus-cardano-streaming`; future split into `scalus-streaming-fs2`,
`scalus-streaming-pekko`, `scalus-streaming-ox` per the publishing
plan). Core has no direct dependency on any of them.

## Construction

The provider is built from a configuration object that names the
chain-sync source, the backup source, the app's persistent
identity, and (optionally) the storage profile. A lightweight
overload covers the common case; the full config object is for
users who need to override defaults.

```scala
case class StreamProviderConfig(
    /** Reverse-DNS identifier for this application instance (e.g.
      * `com.mycompany.batcher`). Used to name the engine's
      * persistent state file so two Scalus apps on the same
      * machine don't collide. Must be non-empty. First field so
      * every user writing a config is immediately forced to
      * think about their app's persistent identity.
      */
    appId: String,
    cardanoInfo: CardanoInfo,
    chainSync: ChainSyncSource,
    backup: BackupSource,
    storage: StorageProfile = StorageProfile.Light,
    /** M6: warm-restart durability. Default is a file-backed store
      * at a platform-appropriate path derived from `appId`, so
      * every restart is Warm unless the user explicitly opts out
      * with `EnginePersistenceStore.noop` (tests, demos, one-shots).
      * See the *Restart semantics* and *App identity and
      * persistence location* sections below.
      */
    enginePersistence: EnginePersistenceStore =
        EnginePersistenceStore.fileForApp(appId),
    /** M9: opt-in durable chain store for Heavy mode / indexer
      * infrastructure. Off by default — most apps don't need it.
      */
    chainStore: Option[ChainStore] = None
    // future: subscription defaults, buffer sizes, …
)

sealed trait ChainSyncSource
object ChainSyncSource {
    /** No protocol — events are pushed in by tests / emulator. */
    case object Synthetic extends ChainSyncSource
    /** Connect to a public relay over N2N (TCP). */
    case class N2N(host: String, port: Int) extends ChainSyncSource
    /** Connect to a local cardano-node over N2C (Unix socket). */
    case class N2C(socketPath: Path) extends ChainSyncSource

    /** Pull-mode Blockfrost streaming — engine polls `/blocks/latest`
      * on a timer, fetches block bodies and tx UTxO views via REST,
      * synthesises `RollBackward` from previous-block hash walks when
      * the observed chain doesn't line up with the engine's tip. No
      * self-hosted bridge required; tip latency floor is
      * `pollInterval`. See *Blockfrost as a chain-sync source* for
      * rate-limit and rollback-semantics caveats. */
    case class BlockfrostPolling(
        apiKey: String,
        network: Network,
        pollInterval: FiniteDuration = 15.seconds
    ) extends ChainSyncSource

    /** Push-mode Blockfrost streaming — engine connects to a
      * self-hosted `blockfrost-websocket-link` bridge for tip events
      * and fetches block bodies and tx UTxO views via Blockfrost REST.
      * Lower tip-notification latency than `BlockfrostPolling`; adds a
      * deployed-bridge operational dep. Same REST fetch path and same
      * hash-walk rollback detection as polling — the WS protocol
      * doesn't carry block bodies or reorg events. */
    case class BlockfrostWebSocketLink(
        bridgeWsUrl: String,
        apiKey: String,
        network: Network
    ) extends ChainSyncSource
}

sealed trait BackupSource
object BackupSource {
    /** Bloxbean's BlockfrostProvider — works on N2N and N2C alike,
      * answers everything the engine can't fill from local state. */
    case class Blockfrost(apiKey: String, network: Network) extends BackupSource

    /** LSQ-backed BlockchainProviderTF over an N2C local socket.
      * Authoritative live answers for protocol params, UTxOs,
      * mempool. M12. */
    case class LocalStateQuery(socketPath: Path, network: Network) extends BackupSource

    /** Escape hatch — pass an existing BlockchainProviderTF (test
      * stubs, custom backends, chained providers). */
    case class Custom[F[_]](provider: BlockchainProviderTF[F]) extends BackupSource

    /** Explicit no-backup. Methods that need the backup return a
      * typed "no source configured" error. Suitable for write-only
      * chain followers and fresh-script use cases where there are
      * genuinely no historical UTxOs. */
    case object NoBackup extends BackupSource
}

sealed trait StorageProfile
object StorageProfile {
    /** Per-active-subscription UTxO indexes only; rollback buffer is
      * always present. Memory bounded by securityParam ×
      * subscription footprint. Default. */
    case object Light extends StorageProfile

    /** Maintain the full UTxO set locally. `findUtxos` becomes a
      * local lookup for any query; multi-GB on mainnet; in practice
      * requires Mithril bootstrap (see milestone 10 — Mithril,
      * renumbered). Right answer for indexer infrastructure. */
    case object Heavy extends StorageProfile
}

object Fs2BlockchainStreamProvider {

    /** Lightweight overload — `appId` first, then chain-sync and
      * backup. Everything else defaults.
      */
    def create(
        appId: String,
        cardanoInfo: CardanoInfo,
        chainSync: ChainSyncSource,
        backup: BackupSource
    )(using Dispatcher[IO], ExecutionContext): IO[Fs2BlockchainStreamProvider] =
        create(StreamProviderConfig(appId, cardanoInfo, chainSync, backup))

    /** Full configuration. */
    def create(config: StreamProviderConfig)(using
        Dispatcher[IO],
        ExecutionContext
    ): IO[Fs2BlockchainStreamProvider]
}
```

The factory returns `IO[…]` (or `Future[…]` for the default-Future
adapter) because backup-source construction may need async setup —
opening the LSQ socket, performing an initial Blockfrost handshake,
etc. For `ChainSyncSource.Synthetic` and
`BackupSource.NoBackup`/`Custom`, the factory completes essentially
synchronously.

### Typical usage

```scala
// N2N relay + Blockfrost backup, light storage — the 90% case.
// Warm restart is on by default: engine state auto-persists to a
// file derived from appId; no extra configuration needed.
val provider = Fs2BlockchainStreamProvider.create(
    appId       = "com.mycompany.batcher",
    cardanoInfo = CardanoInfo.mainnet,
    chainSync   = ChainSyncSource.N2N("relays.cardano.org", 3001),
    backup      = BackupSource.Blockfrost(apiKey, Network.Mainnet)
).unsafeRunSync()

// Heavy indexer — drop into the full config
val heavy = Fs2BlockchainStreamProvider.create(
    StreamProviderConfig(
        appId       = "com.mycompany.indexer",
        cardanoInfo = CardanoInfo.mainnet,
        chainSync   = ChainSyncSource.N2N(...),
        backup      = BackupSource.Blockfrost(...),
        storage     = StorageProfile.Heavy,
    )
).unsafeRunSync()

// Test / one-shot — Cold restart (opt out of persistence)
val ephemeral = Fs2BlockchainStreamProvider.create(
    StreamProviderConfig(
        appId             = "test-run",
        cardanoInfo       = CardanoInfo.preview,
        chainSync         = ChainSyncSource.Synthetic,
        backup            = BackupSource.NoBackup,
        enginePersistence = EnginePersistenceStore.noop
    )
).unsafeRunSync()
```

`ox` and any future adapter expose the same factory shape — same
config object, same overloads, just a different concrete provider
type and runtime requirement (`using Ox` instead of `using
Dispatcher[IO]`).

## Snapshot methods architecture

Each `BlockchainProviderTF` method has multiple possible data sources.
The provider picks the best available given what's wired (engine
state, protocol capabilities, configured historical source). Sources
are tried in order; the first that can answer wins.

### `cardanoInfo: CardanoInfo`
- **Static** — set at construction.

### `currentSlot: F[SlotNo]` / `subscribeTip(): C[ChainPoint]`
- **Engine rollback-buffer tip** — updated on every `RollForward`. Always available once chain-sync is running. *Default and authoritative for both N2N and N2C.*
- **LSQ `GetChainPoint`** — N2C only (when `backup = LocalStateQuery(...)`). Useful for cross-checking the engine's tip against the node.
- **`backup.currentSlot`** — fallback if no chain-sync is running yet (engine cold-start, between Mithril restore and chain-sync resume). Stale relative to the actual tip.

### `fetchLatestParams: F[ProtocolParams]` / `subscribeProtocolParams(): C[ProtocolParams]`
- **Engine cell, tracked from chain** — updated when the engine observes era boundaries and on-chain parameter-update governance. Authoritative once implemented; non-trivial because it requires cardano-ledger parameter-update semantics.
- **LSQ `GetCurrentPParams`** — N2C only (when `backup = LocalStateQuery(...)`). Authoritative live answer from the node, no chain-tracking work needed.
- **`backup.fetchLatestParams`** — e.g. `Blockfrost(...)` queries `/epochs/latest/parameters`. Works on N2N. Refreshed when the cell is asked; eventual consistency.
- **Initial value = `cardanoInfo.protocolParams`** — correct at chain genesis, stale thereafter.
- **N2N reality:** without on-chain tracking work or LSQ, N2N apps must rely on `backup`. `BackupSource.NoBackup` plus stale `cardanoInfo.protocolParams` is wrong for any app that submits non-trivial transactions (fee math, ex-units cost models change between eras).

### `checkTransaction(h): F[TransactionStatus]` / `subscribeTransactionStatus(h): C[TransactionStatus]`
- **Engine tx-hash index over the rollback buffer** — `Confirmed` for transactions observed in recent blocks (within `securityParam`). Always available.
- **Engine tracked-own-submissions** — `Pending` for transactions we submitted ourselves but haven't yet seen on chain. Always available.
- **LSQ `LocalTxMonitor`** — N2C only (when `backup = LocalStateQuery(...)`). General mempool view: `Pending` for any tx in the local node's mempool, even ones we didn't submit.
- **`backup.checkTransaction`** — e.g. `Blockfrost(...)` queries `/txs/{hash}`. Works on N2N. Answers older confirmations beyond the rollback buffer; doesn't see local mempool.

### `findUtxos(q): F[Either[UtxoQueryError, Utxos]]`
Index-keyed coverage resolution. See `StorageProfile` and the "Coverage rules" section below.
- **Engine `UtxoKey`-keyed buckets** — the query decomposes into a set of `UtxoKey`s; if every key has a live bucket the engine answers from the union of those buckets, post-filtered by the full query predicate. Addr- and Asset-scoped queries plus their refinements (`MinLovelace`, `HasAsset`, `HasDatum`, etc.) all resolve locally as long as the subscriber has taken out the matching scope subscription.
- **Engine full UTxO state** — `StorageProfile.Heavy`; answers any query locally; in practice requires Mithril bootstrap on mainnet.
- **LSQ `GetUTxOByAddress` / `GetUTxOByTxIn`** — N2C only (when `backup = LocalStateQuery(...)`). Authoritative point-in-time answer for any query.
- **`backup.findUtxos`** — e.g. `Blockfrost(...)` queries `/addresses/{addr}/utxos`. Works on N2N. Trust-the-delegate.

### `submit(tx: Transaction): F[Either[SubmitError, TransactionHash]]`
- **`LocalTxSubmission` (N2C)** — typed ledger rejection errors (BadInputsUTxO, OutsideValidityInterval, etc.). Best for development and apps that need rich error feedback.
- **`TxSubmission2` (N2N)** — fire-and-forget; success = "the relay accepted the announcement and pulled the body." Acceptance into a block is implicit (observe in chain-sync) and rejection is invisible.
- **`backup.submit`** — e.g. `Blockfrost(...)` posts to `/tx/submit`. Works on either wire as a fallback. Typed errors depend on the backend's quality.
- Records the submitted tx-hash so `subscribeTransactionStatus(h)` reports `Pending` until the engine sees it confirmed.

### `pollForConfirmation`, `submitAndPoll`
- **Default impls** compose `submit` + `checkTransaction`; no per-adapter work. Once `submit` and `checkTransaction` work, these are free.

## Engine state profiles

The two `StorageProfile` choices control what the engine maintains;
the `backup` always supplies whatever local state can't answer.

**Light (default — most apps).** Engine maintains the rollback buffer
plus `UtxoKey`-keyed buckets, one per distinct index key any
active subscription touches. Memory bounded by `securityParam` ×
subscribed-scope footprint — multiple subscriptions on the same
address share one bucket. Right answer for the batcher (one
address), wallets (handful of addresses), marketplaces (a single
script). `findUtxos` queries that decompose to keys with no live
bucket fall through to `backup`.

**Heavy (opt-in via `StorageProfile.Heavy`).** Engine additionally
maintains a complete UTxO set, durably (typically via a `ChainStore`
plus Mithril bootstrap; multi-GB on mainnet). `findUtxos` answers
any query locally without touching `backup`. Right answer for indexer
infrastructure (Yaci-Store / Kupo replacements).

The third path that *doesn't* work: "track a few addresses, no
backup source." If a UTxO at your watched address was created before
your subscription started, you can't see it and have no way to find
out. The construction API forces an explicit choice — either
configure a `backup` that can answer historical queries, opt into
`StorageProfile.Heavy`, or pick `BackupSource.NoBackup` and accept
that the indexer is forever blind to pre-subscription state.

## Subscribe seeds the index

The unified interface (`BlockchainStreamProviderTF` extends
`BlockchainProviderTF`) is not just an ergonomic choice — it is
load-bearing for the engine design. Subscribing to a query is what
makes `findUtxos` for the same query cheap.

When `subscribeUtxoQuery(q, opts)` is called, the engine:

1. If `opts.includeExistingUtxos = true`, asks `backup` for UTxOs
   matching `q` as of the current tip. `backup = NoBackup` causes
   this step to fail the subscription with a typed "no source
   configured" error.
2. Seeds each [`UtxoKey`] bucket that `q` decomposes to from the
   response (buckets already shared with other subscriptions stay
   populated; fresh buckets are filled on first seed) and emits
   synthetic `UtxoEvent.Created` events to the subscriber.
3. Begins live event delivery from chain-sync, applying per-block
   deltas to the buckets and then fanning subscribers their
   filtered view.

`backup` is hit *once* per subscription start, not per `findUtxos`
call. After that the engine carries the state, and `findUtxos(q)` is
a local lookup.

### Coverage rules for `findUtxos`

When `findUtxos(q)` arrives the engine resolves it as follows:

1. **Index-keyed coverage.** The engine decomposes `q` via
   `QueryDecomposer.keys` into a set of `UtxoKey`s. If every key
   has a live bucket (refcount > 0), the engine unions those
   buckets and applies the full `q` predicate as a post-filter.
   Covers exact-match and refinement cases uniformly: a subscriber
   watching `FromAddress(A)` also enables `findUtxos` for any
   query of the form `FromAddress(A) && HasAsset(X)` (refinement),
   for `FromAddress(A)` itself (exact), and for any other shape
   whose scope atoms are all `Addr(A)`.
2. **Heavy-storage mode.** `StorageProfile.Heavy` → engine has
   full UTxO state → answer locally.
3. **No coverage.** Fall through to `backup.findUtxos`.
4. **No backup configured.** `BackupSource.NoBackup` → return
   `UtxoQueryError.NotFound` with a "no source configured" message.

A streaming indexer in light mode is *not* a general UTxO store. It
indexes what subscriptions ask it to. Apps that need arbitrary
queries either subscribe to the relevant scope first, configure a
backup source, or accept the heavy-indexer cost.

## Rollback handling

Rollbacks are a first-class concern, not an afterthought.

- The protocol client surfaces both `RollForward(block)` and
  `RollBackward(point)` from chain-sync.
- The rollback buffer holds the last *k* blocks (k ≈ `securityParam`, 2160 on
  mainnet). Anything older is immutable and can be flushed to long-term
  storage or discarded.
- On `RollBackward(to)` the buffer reverses its volatile tail, each
  `UtxoKey` bucket replays its reverse-delta log back to `to`, and
  `RolledBack(to)` is emitted to every subscriber whose emitted
  events overlap the reverted range.
- Subscribers must treat `RolledBack` as "discard everything I've told you
  past this point" and resume from the new events that follow.

Two delivery modes, selectable per subscription:

1. **Tentative** (default): events emitted as soon as seen; rollbacks
   possible.
2. **Stable**: events held until N confirmations; no rollbacks visible to the
   subscriber. Useful for batchers that must not act on reverted state.

## Storage strategy

Three distinct storage concerns, often confused but orthogonal:

### In-memory state (always)

The engine's live working set — rollback buffer, per-`UtxoKey`
buckets, tx-hash index, protocol-params cell, subscription
registries. Bounded by `securityParam × active-subscription
footprint`. Present in every configuration.

### Engine persistence (on by default, M6)

Durable snapshots of the engine's own state, written by an
`EnginePersistenceStore`. Purpose: survive process restart
without losing continuity (see *Restart semantics*).

**The default is on.** `StreamProviderConfig.enginePersistence`
defaults to `EnginePersistenceStore.fileForApp(appId)`, which
writes to a platform-appropriate directory named by the app's
reverse-DNS `appId`. This makes **every restart Warm** by
default; Cold restart is an explicit opt-out for tests and
one-shot scripts.

```scala
trait EnginePersistenceStore {
    def save(snapshot: EngineSnapshot): Future[Unit]
    def load(): Future[Option[EngineSnapshot]]
}

object EnginePersistenceStore {
    /** Platform-default path derived from the app's reverse-DNS
      * identifier. See *App identity and persistence location*.
      */
    def fileForApp(appId: String, snapshotEvery: FiniteDuration = 30.seconds): EnginePersistenceStore

    /** Explicit path, for apps that want to control the location. */
    def file(path: Path, snapshotEvery: FiniteDuration = 30.seconds): EnginePersistenceStore

    /** No-op store. Used explicitly by tests, demos, and one-shot
      * scripts that don't want to persist anything. Accepting
      * this is accepting Cold-restart semantics — see *Restart
      * semantics*.
      */
    val noop: EnginePersistenceStore
}
```

An `EnginePersistenceStore` snapshots: last `ChainTip`, protocol
params, `ownSubmissions` set, volatile-tail and bucket snapshots
for active subscriptions. **Small** — tens of KB for most apps,
not megabytes.

### App identity and persistence location

`StreamProviderConfig.appId` is a reverse-DNS string that
identifies this application instance for persistence purposes.
Two Scalus apps on the same machine must have distinct `appId`s
or they'll share (and corrupt) the same state file.

Convention: reverse-DNS, e.g. `com.mycompany.batcher`,
`org.acme.wallet`, `com.example.dapp-script-watcher`. Must be
non-empty; the factory fails fast if not.

Default path for `EnginePersistenceStore.fileForApp(appId)` per
platform:

| Platform | Default directory |
|---|---|
| JVM / Linux | `${XDG_DATA_HOME:-$HOME/.local/share}/scalus-stream/<appId>/` |
| JVM / macOS | `$HOME/Library/Application Support/scalus-stream/<appId>/` |
| JVM / Windows | `%LOCALAPPDATA%\scalus-stream\<appId>\` |
| Scala Native | same as JVM, per OS |
| Scala.js (Node) | `~/.cache/scalus-stream/<appId>/` |
| Scala.js (browser) | IndexedDB keyed by `<appId>` (separate adapter module) |

Apps that need multiple instances on the same host (staging +
prod on one machine, say) distinguish by appending an instance
suffix: `com.mycompany.batcher.staging`,
`com.mycompany.batcher.prod`. The library does nothing clever
with suffixes — they're just part of the `appId` string.

### Chain store (optional, M9 — for indexer infrastructure)

A `ChainStore` is a durable UTxO set + header chain + block body
history — the data a full-node-like indexer answers arbitrary
queries from. Populated by Mithril snapshot bootstrap or genesis
sync. Two consumers:

- `StorageProfile.Heavy` reads the UTxO set to answer any
  `findUtxos(q)` locally without touching `backup`.
- M7's `StartFrom.At(point)` replay path uses the block-body
  history when the subscriber's checkpoint is older than the
  in-memory rollback buffer.

```scala
trait ChainStore {
    def tip: Future[Option[ChainTip]]
    def utxos: Future[Utxos]
    def blocks(from: ChainPoint, to: ChainPoint): fs2.Stream[Future, Block]
    // …
}
```

**Large** — hundreds of MB to multi-GB on mainnet. `ChainStore`
is the indexer-infrastructure concern; most apps ignore it
entirely and rely on their `BackupSource` for historical queries.

### The three really are independent

| Use case | In-mem | Engine persistence | Chain store |
|---|---|---|---|
| Batcher, wallet, DApp script watcher (dev / short-lived) | ✓ | — | — |
| Long-running service with warm-restart requirement | ✓ | ✓ | — |
| Kupo / yaci-store replacement | ✓ | ✓ | ✓ (Heavy) |

The default `StreamProviderConfig` wires only the first column.
Each of the other two is an opt-in field; neither blocks on the
other.

## Restart semantics

What the app developer gets on a process restart is a *restart
contract*. **Warm is the default** — `StreamProviderConfig`'s
default `enginePersistence = EnginePersistenceStore.fileForApp(appId)`
writes snapshots on every run, and the engine restores from
them on startup. The Cold contract is an explicit opt-out
(`EnginePersistenceStore.noop`) for tests, demos and one-shot
scripts. The Checkpoint-driven contract is a stronger option
for apps that persist their own progress alongside their side
effects.

The contracts are orthogonal to `StorageProfile` and to
`ChainStore` — engine persistence and chain storage are
different concerns (see *Storage strategy*).

| | Cold (opt-out) | Warm (default) | Checkpoint-driven (opt-in, stronger) |
|---|---|---|---|
| **When to use** | tests, demos, one-shots | long-running services (default for most apps) | apps maintaining their own event log or needing at-least-once per-event side effects |
| **What persists?** | nothing (explicit `.noop`) | engine state (tip, params, `ownSubmissions`, bucket snapshots for active subscriptions) via `EnginePersistenceStore` — default file-backed | engine state *plus* app-owned checkpoints in the app's DB |
| **Restart procedure** | re-subscribe; seed-from-backup; resume chain-sync from current tip | restore engine from persistence; resume chain-sync from saved tip; subscribers re-attach with `includeExistingUtxos = false` | `subscribe(startFrom = At(checkpoint))`; engine replays blocks from the checkpoint |
| **Downtime events** | lost as discrete events (only aggregate state visible) | delivered on resume (as if slow processing) | delivered from the checkpoint forward |
| **`subscribeTransactionStatus(h)` `Pending`** | lost | preserved | preserved iff app also persisted the hash |
| **User cost** | explicit `enginePersistence = EnginePersistenceStore.noop` | nothing — just set `appId` | app-side checkpoint persistence + pass `StartFrom.At(point)` on resubscribe |
| **Library cost** | already in M2 | M6 — default-on once it lands | M7 — adds `StartFrom.At(point)` replay plumbing |

**Status (as of M2):** only the Cold path is implemented —
nothing is actually persisted yet, so every restart is Cold
regardless of `appId` configuration. The Warm default becomes
real with M6 and the Checkpoint-driven path with M7. Users
should still set `appId` now so no code changes are needed when
M6 lands.

### Consumer-side analysis: what events does a subscriber see after a restart?

**For the default config, every restart is Warm.** The table
below covers all three contracts for completeness, but most
readers should focus on the Warm row — that's what their code
sees unless they explicitly chose `EnginePersistenceStore.noop`.
The Cold row exists so test and demo writers understand why
their streams look different.

Concrete scenario — subscriber watches `FromAddress(A)`.

1. Before subscribe, chain has `{U1, U2}` live at A.
2. Subscribe with `includeExistingUtxos = true` → consumer sees
   `Created(U1), Created(U2)`.
3. Block `B1` produces `U3` → `Created(U3)`.
4. Block `B2` spends `U1` → `Spent(U1)`.
5. App crashes. Consumer's internal state: `{U2, U3}`.
6. Downtime — block `B3` produces `U4`, block `B4` spends `U3`.
7. App restarts. Live chain state at A: `{U2, U4}`.

What the consumer sees on the re-subscribed stream:

| Contract | Events delivered | Duplicates | Silent losses |
|---|---|---|---|
| **Cold** | `Created(U2)`, `Created(U4)` as fresh seed | `Created(U2)` re-delivered | `Created(U4)`'s real block context; `Spent(U3)` never signalled |
| **Warm**, `includeExistingUtxos=true` | `Created(U2)`, `Created(U3)` seed + `Created(U4) @ B3` + `Spent(U3) @ B4` | `Created(U2)`, `Created(U3)` | none |
| **Warm**, `includeExistingUtxos=false` | `Created(U4) @ B3` + `Spent(U3) @ B4` | none | none |
| **Checkpoint** (`startFrom = At(B2.point)`) | `Created(U4) @ B3` + `Spent(U3) @ B4` (replay) + live events | none if checkpoint persisted atomically with side effects | none |

**Implication for consumer design**:

- Warm (default) with `includeExistingUtxos=false` is the
  right shape for long-running services: engine durability
  covers UTxO state, consumer sees a seamless event stream
  post-restart, no silent losses, no duplicates of live
  events. **This is what you get if you just set `appId` and
  leave the rest alone.**
- Checkpoint is the stronger option for consumers that need a
  persistent, deduplicable event log on their own side (a DB
  table of every `Created`/`Spent` ever observed) or
  at-least-once per-event side effects with atomic
  checkpoint-plus-side-effect writes.
- Cold (opt-out only) is safe **only** for consumers whose
  derived state is a function of the current UTxO set and who
  can reconstruct it from the seed (tests, demos, short-lived
  scripts, UI that just re-renders from current snapshot). It
  is *not* safe for per-event side effects — `Created(U2)`
  re-delivered would send the email twice, and `Spent(U3)`
  during downtime is never signalled. If a developer finds
  themselves reaching for `EnginePersistenceStore.noop` in
  production code, something is wrong.

**Design invariant (universal across contracts).** Any `Created`
event the consumer ever receives is *real* — it corresponds to
a UTxO that was at some point live on-chain at a point `≥` the
event's `at: ChainPoint`. Nothing synthetic or speculative is
ever emitted. What differs across contracts is whether an event
is re-emitted on resubscribe and whether an on-chain event
during downtime reaches the consumer as a discrete event or
only via aggregate state.

## Bootstrap from Mithril snapshot

A first-time start (no checkpoint) on mainnet means syncing from genesis,
which is slow (hours to days depending on storage and CPU). For an
embedded data node this is unacceptable as a default user experience.

[Mithril](https://mithril.network/) is the IOG-developed snapshot
protocol that produces signed snapshots of the Cardano chain DB,
verifiable against a stake-based threshold signature. A node can
bootstrap by downloading and verifying a snapshot at some recent point,
then chain-syncing forward from there.

Plan:

- Default cold-start path: fetch the latest Mithril snapshot, verify
  the certificate, restore the local `ChainStore` from it, then resume
  chain-sync from the snapshot tip.
- Fallback: if Mithril is disabled (configuration) or unreachable,
  fall back to chain-sync from genesis (or from the genesis-shielded
  point users provide).
- Trust root: ship the Mithril genesis verification key in the module
  (or pull from a documented URL); document how users override for
  testnets / preview / preprod.

Reference implementation: txpipe's Dolos uses Mithril for bootstrap;
see [Dolos Mithril bootstrap docs](https://docs.txpipe.io/dolos/bootstrap/mithril).
Pallas exposes Mithril client primitives we can either bind or port.

Open questions:

- Snapshot freshness vs verification cost — how often do we refresh on
  warm starts? Probably never; chain-sync from checkpoint is enough.
- Snapshot size on mainnet is multi-GB — this is a meaningful download
  the first time. Document expectations clearly.

## Protocol strategy

Direct N2C / N2N integration is the main option. Users should not need a
separate process (Ogmios, cardano-db-sync, Yaci) just to use this feature.

- **N2N** (TCP, works against remote relays): **first target.** Most app
  deployments don't co-locate with a `cardano-node`, so N2N is what
  unblocks real users. Carries chain-sync and `TxSubmission2`. Submission
  is pull-based and designed for relay-to-relay mempool propagation — the
  client announces tx IDs, the peer pulls bodies, and acceptance feedback
  is implicit (the tx either appears in a block or it doesn't; no typed
  ledger error comes back). Apps that need rich rejection feedback must
  use N2C or run a local submission bridge.
- **N2C** (local Unix socket, requires `cardano-node` on the same host):
  simpler wire format and carries `LocalTxSubmission` (full ledger
  validation, detailed rejection reasons) and local-state-query. Second
  target, landed when users or contributors actually have a local node.
  Most of the N2N scaffolding (multiplexer, handshake, CBOR framing)
  carries over.
- **Initial submission path.** For the first N2N release, don't implement
  `TxSubmission2`. Instead provide a composite `BlockchainStreamProvider`
  that reads over N2N but delegates `submit` to an existing
  `BlockchainProvider` (Blockfrost today, user-supplied later). This
  unblocks the motivating batcher use case without paying for
  `TxSubmission2` up front, and it gives typed rejection reasons for free
  via the delegated provider.
- **Reference implementation.** Pallas (Rust) is the cleanest spec. We can
  either:
  - port the relevant chain-sync / local-state-query / tx-submission
    mini-protocols to Scala (long-term correct answer), or
  - bind to pallas via JNI as a stopgap (adds native-library ops burden).

### N2N is a strict subset of N2C

N2N's mini-protocols are ChainSync, BlockFetch, `TxSubmission2`,
KeepAlive, Handshake, PeerSharing. **N2N has no LocalStateQuery and
no LocalTxMonitor.** Capabilities at a glance:

| Capability                              | N2C native            | N2N native              |
|-----------------------------------------|-----------------------|-------------------------|
| Chain-sync (read blocks)                | Yes                   | Yes                     |
| Block bodies                            | Yes (in chain-sync)   | Yes (`BlockFetch`)      |
| Submit (typed errors)                   | `LocalTxSubmission`   | **No**                  |
| Submit (fire-and-forget)                | —                     | `TxSubmission2`         |
| Live protocol params                    | LSQ `GetCurrentPParams`| **No** — track from chain or use `backup`           |
| `findUtxos(arbitrary q)`                | LSQ `GetUTxOByAddress`| **No** — `StorageProfile.Heavy`, or use `backup`    |
| Mempool view (`Pending` for any tx)     | LSQ `LocalTxMonitor`  | **No** — own-submission tracking only                |

So pure-N2N apps either configure a non-`NoBackup` `BackupSource`
(`Blockfrost` answers all the gaps) or accept the limitations. The
doc's snapshot-method descriptions enumerate the per-method fallback
chain.

### Platform support

The CBOR framing and mini-protocol state machines are pure logic and run
anywhere Scalus already runs. What differs per platform is socket access:

| Platform           | N2N (TCP)        | N2C (Unix socket) |
|--------------------|------------------|-------------------|
| JVM                | yes              | yes (JDK 16+)     |
| Scala Native       | yes              | yes (POSIX)       |
| Scala.js (Node.js) | yes (`net`)      | yes (`net`)       |
| Scala.js (browser) | no               | no                |

Browser deployments cannot open raw sockets and must route through a
WebSocket bridge (Ogmios, `blockfrost-websocket-link`, or a
user-supplied proxy) — or use `ChainSyncSource.BlockfrostPolling`,
which is pure HTTPS and works everywhere including the browser.
Direct protocol integration is therefore available everywhere
*except* the browser — so "non-browser" is the real constraint, not
"JVM-only".

### Blockfrost as a chain-sync source

`ChainSyncSource.BlockfrostPolling` and
`ChainSyncSource.BlockfrostWebSocketLink` give an HTTPS-only
alternative to N2N/N2C for deployments that can't open raw sockets
(browser JS) or that already live inside a Blockfrost-based stack and
want one data source. They are not a drop-in replacement for N2N —
several design-level guarantees in the plan above relax when you pick
this path.

**Common engine path.** Both variants share one `BlockfrostChainSync`
engine. A tiny `TipNotifier` abstraction is the only thing that
differs between them: `Polling` timer-polls `/blocks/latest`;
`WebSocketLink` subscribes to `SUBSCRIBE_BLOCK` on a self-hosted
`blockfrost-websocket-link` bridge. Block bodies + tx UTxOs are
fetched from the REST API (`/blocks/{hash}`, `/blocks/{hash}/txs`,
`/txs/{hash}/utxos`) by the engine in both cases; rollbacks are
detected by walking `previous_block` hashes when the newly-observed
block's parent doesn't match the engine's current tip.

**Capability delta vs N2N.**

| Capability                  | N2N                                    | Blockfrost streaming (Polling + WS-link) |
|-----------------------------|----------------------------------------|------------------------------------------|
| Tip latency                 | ~sub-second (push)                     | ≥ `pollInterval` (15s default) — WS-link reduces notification jitter, not floor |
| Rollback notification       | Wire-native `RollBackward(point)`      | Synthesized from hash-chain walk (heuristic; correctness hinges on catching every intermediate tip) |
| Block bodies                | `BlockFetch` mini-protocol             | REST `/blocks/{hash}` + `/blocks/{hash}/txs` |
| Tx UTxO view                | Derived from block body                | REST `/txs/{hash}/utxos` — one round-trip per tx |
| Mempool / own-submission    | Tracked locally (+ N2C LSQ for general) | None — same gaps as N2N; `BackupSource.Blockfrost.submit` covers submit |
| Rate limits                 | None (peer-to-peer)                    | Blockfrost tiered: free = 10 req/s, 50K/day. High-tx blocks can approach the second-limit on mainnet; `BlockfrostPolling` is not recommended on free tier for mainnet production |
| Deployment dep              | None (TCP to a public relay)           | Polling: none. WS-link: self-hosted `blockfrost-websocket-link` process |
| Platform                    | JVM + Native (+ Node, not browser)     | Anywhere sttp supports WS/HTTPS, including browser JS |

**Rollback-detection correctness.** If the polling interval is long
enough that a short-lived fork is entirely born and rolled back
between two polls, the engine will observe the post-rollback tip
directly and may not be able to detect that anything happened. The
engine walks `previous_block` backwards from the observed tip until
it finds a hash matching something in its local rollback buffer;
anything beyond the buffer's horizon (`securityParam` blocks) fires
`ResyncRequiredException`. A shorter `pollInterval` narrows the
window but doesn't eliminate it.

**When to pick which.**

- Native / JVM apps that can open TCP → `ChainSyncSource.N2N` is
  unambiguously better (lower latency, first-class rollbacks, no
  rate limits).
- Browser JS apps (cannot open raw sockets) →
  `BlockfrostPolling` is the only in-process option. No bridge to
  deploy; pays for a tight `pollInterval` in rate-limit budget.
- Apps already operating a `blockfrost-websocket-link` bridge for
  other reasons → `BlockfrostWebSocketLink` trims tip latency to
  the bridge's polling interval (typically shorter than our
  `pollInterval`); no code-path win vs polling beyond that.
- Heavy / indexer workloads → `Blockfrost*` sources are not
  appropriate for `StorageProfile.Heavy` bootstrap; use Mithril
  + N2N.

## Module layout

- `scalus-cardano-ledger` — existing snapshot `BlockchainProvider` and
  ledger types, unchanged. Platform-neutral.
- `scalus-cardano-streaming` (new) — streaming API (`BlockchainStreamProvider`,
  `ChainPoint`, event types, `ScalusAsyncStream` typeclass), rollback
  buffer, query engine, default in-memory store, and N2C/N2N protocol
  clients plus tx submission (`LocalTxSubmission` / `TxSubmission2`).
  Cross-built for JVM and Scala.js on Node; not published for browser JS
  (no raw sockets). Scala Native: can be added once we decide on the
  async-I/O story there.
- `scalus-streaming-fs2`, `scalus-streaming-pekko`, `scalus-streaming-ox`
  (new) — `ScalusAsyncStream` instances and any stream-lib-specific
  conveniences. Cross-build per what each stream library supports
  (fs2: JVM+JS+Native; pekko: JVM; ox: JVM).
- Future: a browser-JS adapter that talks to a WebSocket bridge (Ogmios or
  equivalent) instead of opening sockets directly.
- Future: Java and Kotlin wrappers exposing the public API.

## Emulator integration

`Emulator` and `ImmutableEmulator` should implement
`BlockchainStreamProvider` so the same application code exercises both the
real and the simulated environment.

- Non-scenario mode: per-transaction emission is natural — apply tx, emit
  events to matching subscribers.
- Scenario mode (deterministic replay): streams become synchronous iterables
  over the scenario tape. Open question: whether rollback events should be
  simulable in scenario mode, or whether scenarios stay linear and apps wanting
  to test rollback paths use a different harness.

## Milestones

N2N is the first protocol target because most deployments don't co-locate
with a `cardano-node`. N2C and `LocalTxSubmission` follow when a local
node is available. The initial submission path delegates to an existing
`BlockchainProvider` (e.g. Blockfrost) instead of implementing
`TxSubmission2` up front.

1. **Done.** Uniform `ScalusAsyncStream` interface (`channel` returning
   a `ScalusAsyncSink` + native `S[A]` pair) plus native fs2 and ox
   adapter implementations validated end-to-end by a synthetic event
   source. fs2 adapter is cross-built JVM+JS (queue allocation flows
   through `Dispatcher.unsafeToFuture`, never `unsafeRunSync`); ox is
   JVM-only. `BaseStreamProvider` provides the per-subscription
   registry and channel allocation generically; adapters only supply
   their `ScalusAsyncStream[C]` instance.
   *Snapshot methods: all unimplemented (no engine state yet).*
2. **Done.** In-memory engine + light-indexer stack. Rollback-aware
   engine: rollback buffer, `UtxoKey`-keyed UTxO buckets, tx-hash
   index, protocol-params tracking cell. `StreamProviderConfig` /
   `BackupSource` / `ChainSyncSource` / `StorageProfile` ADTs
   landed, plus the two-overload `Fs2BlockchainStreamProvider.create`
   / `OxBlockchainStreamProvider.create` factories;
   `ChainSyncSource.Synthetic` and `BackupSource.Blockfrost` are
   wired (Blockfrost via the existing `BlockfrostProvider` in
   `scalus-cardano-ledger`).
   Subscribe-seeds-the-index pattern: `subscribeUtxoQuery` asks
   `backup` for matching UTxOs at subscribe time, seeds the index,
   then applies live deltas. New stream methods landed:
   `subscribeProtocolParams`, `subscribeTransactionStatus`
   (`subscribeTip` was already wired in M1). Snapshot methods light
   up from engine state: `currentSlot`, `fetchLatestParams` (cell
   value, initial = `cardanoInfo.protocolParams`), `checkTransaction`
   (recent confirmed via tx-hash index), and `findUtxos` with
   index-keyed coverage resolution (query decomposes into
   `UtxoKey`s; if every key has a live bucket the engine answers
   locally, post-filtered by the full query predicate; uncovered
   → fall through to `backup`). `submit` (and any other
   uncovered snapshot method) defaults to delegation through
   `backup`. Driven by `ChainSyncSource.Synthetic` — no protocol yet.
   *M2 internals (settled during implementation):*
   * **Single-thread encapsulation.** Engine state is mutated only
     from a private worker thread; the only mutation path is a
     private `submit[R](thunk: => R): Future[R]` helper that puts a
     `Runnable` on a `LinkedBlockingQueue`. No public executor.
     `currentTip` / `latestParams` remain lock-free via
     `AtomicReference`.
   * **Index-keyed state (not per-subscription).** UTxO state lives
     in a single `byKey: Map[UtxoKey, Bucket]` (not a per-sub
     `UtxoIndex`). `UtxoKey` decomposes from `UtxoQuery` (`Addr` /
     `Asset` / `TxOuts` / `Inputs`) and is refcounted by the
     subscriptions that need it. Multiple subscriptions on the same
     address share one bucket; refinement coverage for `findUtxos`
     falls out for free.
   * **Mailboxes for fan-out** — engine never calls a sink directly.
     `DeltaMailbox[A]` (FIFO, `Bounded(n)` overflows fail with
     `ScalusBufferOverflowException`, never silent drops) backs
     `UtxoEvent`/`TransactionEvent`/`BlockEvent` subscriptions;
     `LatestValueMailbox[A]` (size-1 newer-wins) backs `subscribeTip`
     / `subscribeProtocolParams` / `subscribeTransactionStatus`.
     `BufferPolicy` (with `DropOldest`/`DropNewest`) → `DeltaBufferPolicy`
     (`Unbounded` default, or `Bounded(n)` = fail-on-overflow).
   * **`MailboxSource[C[_]]` typeclass** replaces M1's
     `ScalusAsyncStream`. Adapters provide one given that builds the
     adapter's stream by pulling from the mailbox.
   * **`ChainTip(point, blockNo)`** — `subscribeTip` now emits
     position + height so subscribers can compute confirmations
     locally (`tip.blockNo - tx.blockNo`). Events still carry only
     `ChainPoint`.
   * **Rollback-past-horizon guard.** `RollbackBuffer.rollbackTo`
     returns `RollbackOutcome` (`Reverted` / `PastHorizon`); on
     `PastHorizon` the engine fails every subscription with
     `Engine.ResyncRequiredException` (state is no longer
     trustworthy).
   * **`TxHashIndex.ownSubmissions` is lifetime-stable** — not
     cleared on confirmation. Rollback of a confirmed tx now
     correctly reverts the subscriber to `Pending` instead of
     `NotFound`. `onRollBackward` fires status reversions to
     `subscribeTransactionStatus` subscribers.
   * **Seeded UTxOs** emit as `UtxoEvent.Created` (no `Seeded`
     variant). `SubscriptionOptions` gained
     `includeExistingUtxos: Boolean = true`.

   M2 ships only the Cold path of the restart contract — no
   engine state is persisted yet, so every restart is Cold
   regardless of `appId`. See *Restart semantics* for the
   cross-milestone contract and consumer-side implications.
3. **`StreamingEmulator` wrapper** — a `BlockchainStreamProvider`
   in `scalus-cardano-streaming` that wraps `Emulator` (from
   `scalus-cardano-ledger`), so application code can exercise the
   simulated environment through the same provider interface as
   the real one. `Emulator` itself does not implement the
   streaming trait directly: `scalus-cardano-streaming` depends
   on `scalus-cardano-ledger`, not the other way, so the wrapper
   lives on the streaming side.

   `ImmutableStreamingEmulator` — an immutable counterpart
   wrapping `ImmutableEmulator` (from `scalus-testkit`) — is a
   nice-to-have that is **fs2-only by construction** (fs2 streams
   are pure values that can describe the whole run; ox streams
   are effect-scoped and inherently mutable). Its placement is
   also non-trivial: testkit does not depend on streaming today,
   so it would need either a new module or a streaming dep from
   testkit. **Scope for M3: ship `StreamingEmulator` and a
   high-level design doc for `ImmutableStreamingEmulator`; defer
   the implementation to a later milestone.**
4. N2N transport foundation — multiplexer, handshake, CBOR framing,
   KeepAlive. End-to-end "connect to a public relay and stay
   connected."
5. N2N chain-sync — `RollForward` / `RollBackward` wired into the
   engine from (2). First real end-to-end: observe tip from a public
   relay. `ChainSyncSource.N2N(...)` becomes a real backend. The
   motivating batcher is now fully runnable in production shape —
   N2N for the read path, `BackupSource.Blockfrost(...)` for
   historical seeding and `submit`.
6. **Engine persistence (minimal) — Warm restart by default** (see
   *Restart semantics* for the consumer-facing contract; full
   design in
   `docs/local/claude/indexer/engine-persistence-minimal.md`).
   `EnginePersistenceStore` trait + a file-backed implementation
   named from `appId` (reverse-DNS; see *App identity and
   persistence location*). Persists `ChainTip`, protocol params,
   `ownSubmissions`, and per-bucket deltas as an append-only
   journal, with occasional full-rewrite snapshot compaction;
   reloads on startup and resumes chain-sync via
   `FindIntersect([savedTip])`.
   `StreamProviderConfig.enginePersistence` defaults to the
   file-backed store, so **every restart is Warm unless the user
   explicitly writes `EnginePersistenceStore.noop`**. The `appId`
   field becomes required on `StreamProviderConfig` (users
   writing M2 code today should start setting it now, even
   though M2 ignores it).
   Directly on top of (5) because `Synthetic` has nothing to
   persist; once N2N chain-sync is real, every non-warm restart
   wastes a full seed-from-backup pass per subscription, loses
   `Pending` submissions, and (more seriously) silently drops any
   `Spent` event that happened during downtime — a correctness
   footgun for any per-event side-effect consumer. Shipping M6
   closes that footgun as the default configuration.
   **Scope caveat:** this milestone ships the *minimal* half of
   the original persistence plan. Snapshot-restore reconciliation
   (what happens when a Mithril-restored ChainStore disagrees
   with a stale engine snapshot), schema migration, async
   compaction, and related durability polish are deferred to
   M14 (*advanced persistence*), which lands after M9
   (ChainStore) and M10 (Mithril) have provided the
   snapshot-restore surface to design against.
7. **`StartFrom.At(point)` end-to-end — Checkpoint-driven
   restart** (see *Restart semantics* for the consumer-facing
   contract; full design in
   `docs/local/claude/indexer/checkpoint-restart-m7.md`). No new
   public methods — the existing `SubscriptionOptions.startFrom`
   field already carries `StartFrom.At(point)`. What lands: the
   engine's per-subscription replay path. Blocks between the
   checkpoint and current tip are sourced from the rollback
   buffer (recent), a `ChainStore` (if configured — real in M9,
   plumbing-only in M7), or a second ChainSync conversation on
   the live N2N connection that re-intersects at `point`; each
   block flows through a per-subscription dry-run of
   `onRollForward` (no mutation of global engine state, no
   fan-out to other subscribers) so the replay subscriber sees a
   seamless event trail from their checkpoint forward without
   disrupting live subscribers. Cross-validated with
   `includeExistingUtxos = false`. Failure when the replay
   source can't reach `point` surfaces as
   `Engine.ReplaySourceExhausted(point)` — per-subscription, not
   fatal to the engine. Depends on (6) to make "restart with a
   few-seconds-stale checkpoint" the common case land in the
   rollback buffer.
8. `TxSubmission2` over N2N — replaces the delegated submission
   path for deployments that want to be N2N-only; returns
   success/unknown rather than rich rejection detail.
9. Pluggable `ChainStore` — durable UTxO set + header chain for
    Heavy-mode indexers. `StorageProfile.Heavy` answers arbitrary
    `findUtxos(q)` locally without touching `backup`. Scope:
    trait + a first backend (SQLite/JDBC or RocksDB — pick when
    we get there). Pairs with (10); also serves as an alternative
    block-history source for (7)'s replay path when the
    checkpoint is older than the rollback buffer.
10. **Snapshot bootstrap (Mithril-shaped, verifier-independent).**
    Cold-start path: restore `ChainStore` from a `ChainStoreSnapshot`
    — Scalus's own streaming interchange format (header + AppliedBlock
    records + UTxO-set entries) — then resume chain-sync from the
    snapshot tip via `StartFrom.At(snapshotTip)`. `SnapshotSource` ADT
    (`File`, `Url`, `Mithril(aggregator, genesisKey)`) sits in front of
    `ChainStoreRestorer`, so any trusted source can feed the store.
    The `Mithril(...)` variant is a stub in M10 — it throws
    `UnsupportedSourceException` pointing at M10b — while the
    interchange format, restore pipeline, and startup integration
    land for real. Unlocks Heavy-mode `findUtxos` on chains where a
    trusted snapshot is available (dev/test fixtures, internal
    bundles, wallets running their own mithril-client behind a proxy).
    The natural way to populate the Full store on mainnet (syncing
    UTxO state from genesis is impractical); full turn-key Mithril
    support arrives with M10b.
10b. **WASM-embedded Mithril client (primary path).** Embeds the
    upstream `@mithril-dev/mithril-client-wasm` blob (pinned via
    SHA-256 in test resources) via Chicory — a pure-JVM WebAssembly
    runtime — so the Mithril team's already-maintained Rust client
    runs inside Scalus without any JS runtime or native deps.
    **Why WASM over a Scala reimplementation:** the Rust
    `mithril-client` already contains everything the two earlier
    drafts of this milestone would have had to ship — the
    certificate chain walk, the MuSig2 verifier, the Aggregator
    HTTP client, tar/zstd extraction, the ImmutableDB chunk
    parser, AND the cardano-node LedgerState parser (the hardest
    part, tracking a CIP-0084-unspecified moving target the
    Mithril + cardano-node teams already track in Rust). Bumping
    a .wasm blob at releases is ~1 day of regression testing;
    reimplementing + tracking all of the above in Scala is
    indefinite.
    Components landing here:
    (a) Chicory runtime + WasmBindgenAbi bridge: externref table,
        UTF-8 string marshalling, JS-like predicates, object
        property accessors;
    (b) host-function adapters for the JS platform APIs the WASM
        blob expects — `fetch` over `java.net.http.HttpClient`,
        `crypto.subtle.digest` over `MessageDigest`,
        `ReadableStream` reader, `setTimeout` / `AbortController`
        over `ScheduledExecutorService`;
    (c) Scala facade MithrilClient: mirror of the TypeScript API
        (list_mithril_certificates, verify_certificate_chain,
        get_cardano_database_v2, …) returning typed Scala records;
    (d) ChainStoreRestorer integration: SnapshotSource.Mithril
        (the variant M10 reserved) resolves via this module —
        downloads, verifies, extracts, streams blocks + UTxOs into
        ChainStoreRestorer.runRestore.
    **Era scope:** whatever the Mithril client itself supports,
    including Byron genesis through Conway. Our Scalus-side
    `BlockEnvelope` constraint (Babbage+) still applies to the
    live chain-sync path but the restore path goes through WASM
    which handles early eras transparently.
    Prerequisites: none beyond M10 restore infrastructure.
    Estimated scope: 3–5 days for the ABI bridge (115 wasm-bindgen
    imports, mostly mechanical once fetch/streams/timer are done)
    plus 1–2 days for end-to-end restore wiring. Status: P1
    (scaffold + instantiation) and P2-start (6 ABI imports + driver
    test) committed; P2 finish + P3 integration ahead.

10c. **CLI shell-out fallback.** Parallel path for users on
    environments where in-process WASM is undesirable (tight
    memory, no Chicory runtime) OR as the out-of-process option
    for users who prefer operational separation. Survey of
    existing tools produces exactly one near-fit:

    - **`mithril-client tools utxo-hd snapshot-converter`** —
      ships with every Mithril release (Linux / macOS / Windows
      binaries, currently `--unstable`). Takes a Mithril-restored
      cardano-node `db/` directory and converts between UTxO-HD
      flavors (InMemory / LMDB / Legacy). Useful as the
      *download + verify + extract* half — produces a standard
      cardano-node DB layout on disk — but its output is
      cardano-node's format, not Scalus's.
    - **`cardano-cli query ledger-state dump`** — requires a
      running local node + IPC socket; whole-state CBOR dump,
      "stretches the memory limits of CBOR libraries" per
      upstream issues. Not production-grade.
    - **`cardano-db-sync` state snapshots** — portable only
      across matching db-sync versions AND matching CPU
      architectures. Too brittle as an interchange format.
    - **CIP-165** — the target standard, still in progress (5
      namespaces implemented as of Mar 2026, more to come).
      Eventually supersedes the "pick a cardano-node version
      pin" problem but not landed yet.

    So M10c's companion-tool gap is concrete: to convert
    cardano-node's `db/` layout (as produced by
    `mithril-client tools utxo-hd snapshot-converter` or by a
    raw `mithril-client download`) into our
    `ChainStoreSnapshot` format, we need new code. Two options:

    - **Upstream contribution**: propose
      `mithril-client tools utxo-hd snapshot-converter
      --output-format scalus` to the Mithril team. Upside:
      bundled into releases everyone already has; downside:
      adoption lag + upstream review cycle.
    - **Scalus-owned Rust companion**: a small
      `scalus-snapshot-dump` binary (wraps `mithril-client` as
      a lib, writes Scalus-native streaming format). ~1-2 week
      Rust project; distributed via Scalus releases as
      platform-classified artifacts or source.

    Either way the Scala side adds a ~200-line driver:
    `SnapshotSource.ExternalCli(binaryPath, aggregator,
    genesisKey, workingDir)` spawns the process, redirects its
    stdout into `SnapshotReader`, surfaces exit codes + stderr
    as typed errors. Prerequisites: M10 restore infrastructure;
    independent of M10b (can ship before or after).

    Scala-side scope: 1-2 days. Companion tool scope: separate
    project, not blocking M10b.

    Note: M10b and M10c are **not mutually exclusive** — the two
    `SnapshotSource` variants coexist, and the user picks based on
    their deployment constraints. M10b is the default
    recommendation (no external process management, no
    platform-specific binaries on the user's critical path); M10c
    is the escape hatch and the natural host-of-truth if we
    upstream the Scalus output format.

10d. **UTxO-HD V2 LSM backend support.** Extend the ledger-state
    parser (M10b.next) to recognise and decode the LSM-tree
    `tables/` layout in addition to the InMemory `tables/tvar`
    file. Currently rejected with `UnsupportedLedgerSnapshotFormat`.
    **Why deferred (i.e. why LSM isn't the path today):**
    - Upstream default for end-users is V2 InMemory, not LSM.
      `ouroboros-consensus/.../LedgerDB/Args.hs:defaultArgs` pins
      `LedgerDbBackendArgsV2` with the comment "closest thing to
      a pre-UTxO-HD node, and as such it will be the default for
      end-users." Stake-pool operators opt *into* LSM when holding
      the UTxO set in RAM becomes too expensive; regular users
      don't.
    - LSM is the newest backend and still settling. The consensus
      CHANGELOG notes "With the current (only) backend (in-memory),
      this doesn't matter, but on-disk backends (like LSM trees)
      need this" — i.e. LSM is landing but InMemory is treated as
      the production path. LSM lives in a separate sub-library
      (`ouroboros-consensus-lsm`) and the on-disk tree format
      evolves (`lsm-tree` repo is independent).
    - Mithril aggregators sign and serve whatever cardano-node
      writes, and the canonical aggregator snapshots for preview /
      mainnet today are V2 InMemory. No LSM-flavored snapshots
      are in the wild to consume.
    - Users who *do* run LSM locally can still feed a Scalus
      bootstrap via `mithril-client tools utxo-hd snapshot-converter`
      (ships with every Mithril release) to convert an InMemory
      snapshot into an LSM layout, or vice versa — so an LSM-only
      operator is not blocked by us targeting InMemory first.

    When we add LSM support: two pieces. (a) Format sniffer picks
    `tables/tvar` vs `tables/<lsm-tree-files>` from `ledger/<slot>/`.
    (b) Streaming LSM reader that walks the on-disk sorted runs
    and yields `(TransactionInput, TransactionOutput)` in
    per-block-height-independent key order. Scope: 3–5 days if the
    lsm-tree format spec is stable; re-estimate before committing
    — we should cross-check `ouroboros-consensus/src/ouroboros-consensus-lsm/`
    first.

    Prerequisites: M10b.next (InMemory parser) — share the
    `TransactionInput` / `TransactionOutput` Borer decoders and
    the `ChainStoreUtxoSet.restoreUtxoSet` call site.

11. N2C chain-sync + `LocalTxSubmission` — second protocol
    target, lands when a local node is in play. Reuses the
    handshake/mux/framing from (4); adds typed ledger rejection
    reasons to `submit`. *`submit` becomes engine-native; tracked
    submissions feed `subscribeTransactionStatus(h)` Pending
    state.* Ordered after Mithril because a local `cardano-node`
    is itself a heavy operational commitment — apps that have one
    typically want Heavy-mode indexing too, and the natural
    bootstrap for that is Mithril. Flipping the order would force
    N2C users to sync-from-genesis on their first start.
12. Local-state-query support (N2C only) — adds an LSQ-backed
    `BlockchainProviderTF` implementation, exposed as
    `BackupSource.LocalStateQuery(socketPath, network)`.
    Authoritative live variants of `fetchLatestParams` (real-time
    params instead of cached cell or Blockfrost), `findUtxos`
    (`GetUTxOByAddress` for queries no subscription indexes, no
    Blockfrost trip), and `subscribeTransactionStatus` (integrates
    `LocalTxMonitor` for general mempool visibility, not just our
    own submissions). *N2N apps that want the same capabilities
    either stick with `BackupSource.Blockfrost(...)`, or wait for
    on-chain parameter-update tracking (separate, larger work
    item) plus `StorageProfile.Heavy` with the Full ChainStore
    (M9 + M10).*
13. **Blockfrost streaming (HTTPS-only fallback).**
    `ChainSyncSource.BlockfrostPolling(apiKey, network, interval)`
    and `ChainSyncSource.BlockfrostWebSocketLink(bridgeWsUrl,
    apiKey, network)`. Shared `BlockfrostChainSync` engine: a
    `TipNotifier` abstraction plugs in either a timer-driven
    `/blocks/latest` poll (`Polling`) or an sttp-client4
    WebSocket subscription to `SUBSCRIBE_BLOCK` on a self-hosted
    `blockfrost-websocket-link` bridge (`WebSocketLink`). Block
    bodies and tx UTxO views are fetched over REST in both
    variants; rollbacks are synthesised by walking
    `previous_block` hashes against the rollback buffer and firing
    `RolledBack` / `ResyncRequiredException` as appropriate. See
    *Blockfrost as a chain-sync source* for the capability delta
    vs N2N and the rollback-detection correctness caveat.
    Unblocks browser-JS deployments (only in-process option when
    raw sockets are unavailable) and teams already operating a
    Blockfrost-only infrastructure. Independent of M4–M12; could
    land immediately after M3 (Synthetic is the only prerequisite)
    but is listed here because N2N is the recommended default
    and most development effort should go there first.
14. **Engine persistence (advanced)** — second half of the
    original M6 plan, deferred until M9 and M10 land so the
    design can react to how snapshot restore actually works.
    Adds: ChainStore/Mithril reconciliation policy (stale
    engine snapshot vs. restored UTxO set), schema migration
    infrastructure for format changes between library
    releases, and whatever compaction / atomicity work the
    snapshot-restore flow implies. On-disk format introduced
    in M6 is expected to remain stable — this milestone adds
    records and policies, doesn't rewrite the wire.
15. Java/Kotlin wrappers.

## References

- IntersectMBO sources (local): `/Users/rssh/packages/IntersectMBO/`
  - cardano-node: `/Users/rssh/packages/IntersectMBO/cardano-node`
  - ouroboros-network: `/Users/rssh/packages/IntersectMBO/ouroboros-network`
    — authoritative mini-protocol state machines (ChainSync,
    BlockFetch, TxSubmission2, Handshake, KeepAlive, PeerSharing),
    multiplexer/framing code, and CDDL wire-format grammars under
    `ouroboros-network-*/cddl/`. Start here when porting N2N/N2C to
    Scala.
  - ouroboros-consensus: /Users/rssh/packages/IntersectMBO/ouroboros-consensus
     Define format for snapshot storage.
- yaci-store: `/Users/rssh/packages/bloxbean/yaci-store`
- stretto: https://github.com/clawdano/stretto
- pallas: https://github.com/txpipe/pallas — cleanest Rust
  reimplementation of the mini-protocols; useful cross-reference
  when the Haskell code is hard to follow
- dolos (Mithril bootstrap reference): https://docs.txpipe.io/dolos/bootstrap/mithril
- dolos sources locally: /Users/rssh/packages/txpipe/dolos
- mithril: https://mithril.network/
  local sources:  /Users/rssh/packages/input-output-hk/mithril
- amaru: /Users/rssh/packages/pragma-org/amaru
    rust node implementation, constains code for reading mitril snapshot

