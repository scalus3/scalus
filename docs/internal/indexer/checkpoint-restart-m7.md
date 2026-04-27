# `StartFrom.At(point)` end-to-end — Checkpoint-driven restart (M7)

**Status.** All three phases implemented. Phase 1 delivered the
rollback-buffer replay and single-submit sync path; Phase 2a
added the pluggable fallback-source trait plus ChainStore
passthrough; Phase 2b wires peer replay via a short-lived
separate N2N connection and reintroduces per-subscription
`ReplayPending` state for the async-prefetch window. M6
(`engine-persistence-minimal.md`) delivered the *connection-level*
warm restart — the engine resumes from its last persisted tip and
replays downtime blocks through the normal `onRollForward` path.
M7 adds the *per-subscription* replay path so a subscriber can
anchor on its own app-side checkpoint and receive a contiguous
event trail from that point forward.

**Implementation note — sync vs async replay paths.** Synchronous
sources (rollback buffer, ChainStore) drive the whole replay
inside one `Engine.submit` block. Because the engine worker is
single-threaded, no live `onRollForward` interleaves — live
events simply queue behind the replay on the worker queue, so
the sync path needs no per-subscription buffering. The
`ReplaySource.iterate` contract is therefore synchronous
(returning `Either[ReplaySourceExhausted, Iterator[AppliedBlock]]`).
Peer replay needs network I/O, so [[AsyncReplaySource]] extends
`ReplaySource` with a separate `prefetch(from, to)(using EC):
Future[Either[ReplayError, Seq[AppliedBlock]]]` method. The
engine runs a two-phase flow for async sources: Phase A pre-
fetches off the worker while live events arriving at the
subscription land in a per-sub `replayBuffer`; Phase B is one
worker task that applies the prefetched blocks, drains the
buffer, and flips the sub back to live fan-out.

## Scope

M7 delivers:

1. Per-subscription honouring of `SubscriptionOptions.startFrom` in
   `registerUtxoSubscription` / `registerTransactionSubscription` /
   `registerBlockSubscription`. `Tip` stays the default; `At(point)`
   becomes real; `Origin` remains rejected on mainnet (Byron
   decoding is still deferred to M11 / ChainStore territory).
2. An engine-level **replay coordinator** that, on
   `register*Subscription(..., startFrom = At(point))`, streams the
   blocks between `point` and `currentTip` through the subscriber's
   mailbox as synthetic per-subscription events, then slots the
   subscriber into the live fan-out at `currentTip` without gap or
   duplicate.
3. A **block source precedence** policy that resolves the replay
   window in this order:
   1. The engine's in-memory rollback buffer (`volatileTail`).
   2. A configured [`ChainStore`] (if present — M10 substrate; M7
      compiles and ships with `ChainStore = None` on the config and
      fails with `ReplaySourceExhausted` when the checkpoint pre-dates
      the rollback buffer). Plumbing-only in M7; real reads light up
      once M9 (renumbered; the old M10) lands.
   3. A second chain-sync intersect at `At(point)` — a *separate*
      short-lived ChainSync conversation, not a reset of the live one,
      so live fan-out keeps flowing while replay catches up.
4. `noRollback` / `confirmations` compatibility on replay: synthetic
   replay events respect the same depth-gate the live path uses.
   Replayed `Created`/`Spent` older than the confirmation depth pass
   through immediately; newer ones queue alongside live events and
   emit together once sealed.
5. Error model extensions. `ReplaySourceExhausted(point)` when no
   source can reach the checkpoint; `ReplayInterrupted(cause)` when
   the replay's ChainSync intersect fails mid-stream. Both are
   per-subscription: the owning mailbox fails with the typed cause,
   and the engine's global live fan-out is unaffected.
6. Wiring in `Fs2BlockchainStreamProvider` and
   `OxBlockchainStreamProvider`: the provider threads a
   `ReplaySource` capability (derived from the live
   `NodeToNodeConnection` plus any configured `ChainStore`) into the
   engine constructor. Purely additive; non-replaying subscribers
   see no change.
7. Test coverage for the "subscribe, run, crash, persist our own
   checkpoint, restart, re-subscribe with `At(checkpoint)`"
   round-trip — the canonical Checkpoint-driven flow the restart
   table in `indexer-node.md` advertises.

M7 does **not** deliver:

- **`StartFrom.Origin` on mainnet.** Byron decoding is still out of
  scope; `Origin` remains practical only on dev/test chains (yaci,
  preview from Conway, preprod from Shelley). Documented; the
  applier already fails fast on Byron eras.
- **ChainStore backend.** The `ReplaySource.ChainStore` branch is a
  trait surface plus a `None` default. A pluggable backend arrives in
  M9 (renumbered — see the updated milestone list in
  `indexer-node.md`); until then the second replay source is always
  unavailable.
- **Mithril-backed deep replay.** Lands with M10 (Mithril
  bootstrap, renumbered).
- **Durable per-subscription state.** M7 does *not* persist
  subscriber identity or their last-delivered point. The app is the
  source of truth for its own checkpoint (that's the whole point of
  the contract); the engine only replays when asked.
- **At-least-once delivery guarantees across a crash mid-replay.** A
  restart in the middle of a replay means the app re-subscribes with
  the same checkpoint; the engine re-runs the replay. Idempotence is
  the app's responsibility (e.g. DB upserts keyed on
  `(TransactionInput, ChainPoint)`).
- **Cross-subscriber replay reuse.** Two subscribers asking for the
  same checkpoint at the same time each pay the cost. A shared
  replay cache is M7b if we ever see it in profiling.
- **`StartFrom.At(point)` on `subscribeTip` / `subscribeProtocolParams`
  / `subscribeTransactionStatus`.** Latest-value streams are always
  `At(currentTip)` by nature; the type system already reflects that
  (those methods take no options). No work.

## Why now

M6 closed the correctness footgun for the *default* configuration:
warm restart, no silent event loss during process downtime. What
M6 does not solve is the consumer who already maintains their own
event log or checkpoint on the side — a DB table keyed on
`(TransactionInput, ChainPoint)`, a Kafka topic, a custom wallet
persistence — and needs the engine to pick up exactly where their
app-side persistence left off. The `Restart semantics` table in
`indexer-node.md` promises that Checkpoint contract; M7 makes it
real.

The implementation unlocks three practical patterns out of the box:

- **Per-event side effects with at-least-once delivery.** App
  commits `(checkpoint, side-effect)` atomically in its DB; after a
  crash it re-subscribes with `At(checkpoint)` and re-runs every
  event past that point. Duplicates are bounded and idempotence is
  trivial (`INSERT … ON CONFLICT DO NOTHING`).
- **Historical backfill on subscribe.** A new subscriber wants the
  last 24h of matching events to initialise a dashboard. With M7,
  calling `subscribe(..., startFrom = At(point_24h_ago))` is the
  whole API — no bespoke backfill path, no off-engine Blockfrost
  query, no reconciliation against live events.
- **Cross-subscription consistency after restart.** A multi-stream
  app (batcher + dashboard + audit log) that keeps three separate
  checkpoints can resubscribe each to its own `At(point)` and see
  every stream deliver a coherent, seamless event trail starting
  from its last-seen position. No cross-stream ordering surprises
  because all three flow through the same engine worker.

M6 is a strict prerequisite: without warm restart, the rollback
buffer after a crash starts empty, so *every* `At(point)` more
than a few seconds stale would fall through to the (not-yet-real)
ChainStore path and fail. With M6's saved `volatileTail`, the
rollback-buffer source covers the overwhelmingly common case of
"restart the app after a few seconds or a few minutes and resume".

## Architecture at a glance

```
  Subscriber
     │ subscribe(..., startFrom = At(point))
     ▼
  Provider.registerXxxSubscription
     │
     ▼
  Engine.registerXxxSubscription(..., startFrom)
     │
     ├─ register mailbox, acquire bucket(s)  (unchanged from M6)
     │
     ▼
  Engine.replayFor(subId, point) -- NEW
     │ pickSource(point) ⇒ ReplaySource
     ├─ if point in volatileTail          → RollbackBufferSource
     ├─ else if chainStore covers point   → ChainStoreSource   (M9)
     ├─ else if peer can re-intersect     → PeerReplaySource
     └─ else                              → fail with ReplaySourceExhausted
     │
     ▼
  ReplaySource.iterate(point, engine.currentTip) ⇒ Iterator[AppliedBlock]
     │
     ▼
  For each block in the replay window:
     apply block through a per-subscription "dry-run" of onRollForward
     that only touches the one subscriber's mailbox (no byKey mutation,
     no tip update, no global fan-out)
     │
     ▼
  Once the iterator reaches a block whose tip == engine.currentTip:
     ├─ atomically swap the subscriber from "replay mode" to "live mode"
     │   on the engine worker thread
     └─ subscriber now receives the live fan-out stream
```

Key invariant: the engine's **live** state (buckets, rollback
buffer, protocol params, fan-out) is never mutated by the replay
machinery. Replay only reads. Per-subscription replay produces
events tailored to one mailbox by re-running the `matches` filter
over the replayed block's transactions — the engine is already
doing exactly this in `Engine.onRollForward` (the inner `utxoSubs`
loop), so the code path is a controlled re-use.

## Replay source precedence

```scala
trait ReplaySource {
    /** Synchronous; AsyncReplaySource implementations return
      * Left(ReplaySourceExhausted) here and answer via prefetch.
      */
    def iterate(from: ChainPoint, to: ChainPoint)
        : Either[ReplayError.ReplaySourceExhausted, Iterator[AppliedBlock]]
}

trait AsyncReplaySource extends ReplaySource {
    /** Off-worker prefetch. See *Phase 2b peer replay* for the
      * full contract (from == to short-circuits, failures map to
      * ReplayInterrupted, exhaustion cascades).
      */
    def prefetch(from: ChainPoint, to: ChainPoint)(using ExecutionContext)
        : Future[Either[ReplayError, Seq[AppliedBlock]]]
}
```

Implementations (all shipped in M7):

- `RollbackBufferReplaySource(snapshot: Seq[AppliedBlock])` — in
  memory, always-sync. Covers the
  `checkpoint-less-than-securityParam-blocks-old` window. This is
  the only source implemented in Phase 1.
- `ChainStoreReplaySource(store: ChainStore)` — delegates to a
  pluggable `ChainStore` that answers
  `blocks(from, to): Iterator[AppliedBlock]`. Stub trait lives in
  streaming-core; real backends land with M9.
- `PeerReplaySource(factory: PeerReplayConnectionFactory)` —
  async source that opens a transient separate N2N connection per
  prefetch, intersects at `from`, drives ChainSync + BlockFetch
  up to `to`, closes. See *Phase 2b peer replay* below.

The engine tries sources in order: rollback buffer first, then
each sync fallback (ChainStore) via `iterate`, then each async
fallback (Peer) via `prefetch`. The first covering source wins.
Async exhaustion (`Left(ReplaySourceExhausted)` from `prefetch`)
cascades to the next async source; non-exhaustion errors
(`ReplayInterrupted`) fail the subscription immediately without
cascading. The final failure path is `ReplaySourceExhausted`.

Selection is sticky — once a source is chosen, the replay
doesn't dynamically upgrade or downgrade; if the rollback buffer
ages out during a slow replay, the subscription fails with
`ReplaySourceExhausted` (the same error that fires for an
out-of-horizon checkpoint at registration time, just delayed).
The app's recovery is to re-checkpoint forward and re-subscribe.

### Phase 2b peer replay — a separate N2N connection

**Correction on the original design.** An earlier draft of this
doc proposed opening a *second ChainSync channel* on the same mux
for peer replay. That's not possible with Ouroboros
Node-to-Node: each mini-protocol has exactly one state machine
per connection (`MiniProtocolId.ChainSync` has wire ID 2,
singular). Two concurrent `FindIntersect` / `RequestNext` sequences
over one mux would violate the peer's state machine.

Phase 2b therefore takes the other option: **open a short-lived
separate N2N connection** (same host and port as the live
applier's connection, or a user-supplied replay peer) for the
duration of the replay. Sequence:

1. Caller provides a `PeerReplaySource` backed by a
   `PeerReplayConnectionFactory` — a zero-arg
   `open()(using EC): Future[NodeToNodeConnection]` that builds
   on the same `(host, port, networkMagic, config)` tuple the
   live applier used, or an override.
2. On `ReplaySource.iterate(from, to)` — called on the Engine
   worker — the peer source returns
   `Left(ReplaySourceExhausted)` immediately (it's an
   `AsyncReplaySource`, so `iterate` is a no-op by contract).
   The engine's cascade advances to `prefetch`.
3. The engine's `registerUtxoAtCheckpoint` runs a two-phase flow
   for async sources:
   - Phase A (off-worker): call `AsyncReplaySource.prefetch(from,
     to)`. `PeerReplaySource` opens a transient connection, runs
     `IntersectSeeker.seek(StartFrom.At(from))` (a `NoIntersection`
     reply maps to `Left(ReplaySourceExhausted)` so the cascade
     moves on), absorbs the peer's initial `MsgRollBackward` ack,
     drives the ChainSync + BlockFetch loop to collect every
     block up to `to`, closes the connection, and returns
     `Future[Either[ReplayError, Seq[AppliedBlock]]]`.
   - Phase B (worker submit): apply the prefetched blocks to the
     subscription in a single worker task, drain any live events
     captured by the `replayBuffer` during Phase A, flip the sub
     back to live fan-out.
4. During Phase A the subscription is already in the registry so
   live events can fan out to it — but the mailbox must deliver
   *replay* events before *live* events. That's what requires
   reintroducing the per-subscription
   `replayBuffer: mutable.ArrayDeque[UtxoEvent]` + `ReplayPending`
   state that the sync-only Phase 1 didn't need (and the
   corresponding drain-on-transition step). The extra complexity
   is justified because peer replay inherently has an async I/O
   phase.

### Phase 2a — sync fallback plumbing

Phase 2a landed the fallback machinery without a real peer
source:

- `Engine` constructor accepts
  `fallbackReplaySources: List[ReplaySource] = Nil`.
- `registerUtxoAtCheckpoint` iterates them after the rollback
  buffer via `iterate`.
- `ChainStoreReplaySource` (stub) is wired so a non-`None`
  `chainStore` in the config becomes a real source entry.
- Both adapters (`Fs2` / `Ox`) thread `chainStore` through.

### Phase 2b — peer replay

Phase 2b layered the async-prefetch path on top:

- `AsyncReplaySource` trait adds
  `prefetch(from, to)(using EC): Future[Either[ReplayError,
  Seq[AppliedBlock]]]`; its `iterate` is a sealed no-op that
  always returns `Left(ReplaySourceExhausted)`.
- `Engine.registerUtxoAtCheckpoint` gains the two-phase flow:
  sync sources first via `iterate`, then the async cascade via
  `prefetch` with per-subscription `replayPending` buffering of
  live events during Phase A. `onRollForward` /
  `onRollBackward` route events for pending subs into that
  buffer; Phase B drains it.
- `PeerReplaySource` (in `scalus-cardano-network`) opens a
  transient separate N2N connection per prefetch, intersects,
  pulls blocks, closes.
- Both adapters (`Fs2` / `Ox`) append a `PeerReplaySource`
  backed by the live endpoint's `(host, port, magic)` tuple
  whenever `ChainSyncSource` is N2N.

## Engine surface additions

```scala
// Engine.scala — additions

/** Extended version of registerUtxoSubscription that honours
  * `startFrom`. Keeping the existing signature for back-compat;
  * the new overload (or default parameter) is the preferred path.
  */
def registerUtxoSubscription(
    id: Long,
    query: UtxoQuery,
    includeExistingUtxos: Boolean,
    mailbox: Mailbox[UtxoEvent],
    startFrom: StartFrom = StartFrom.Tip           // NEW
): Future[Unit]

/** Symmetric additions for transaction/block subscriptions
  * (both of which already exist in M5's engine but currently
  * default to Tip semantics).
  */
def registerTransactionSubscription(
    id: Long,
    query: TransactionEventQuery,
    mailbox: Mailbox[TransactionEvent],
    startFrom: StartFrom = StartFrom.Tip
): Future[Unit]

def registerBlockSubscription(
    id: Long,
    query: BlockQuery,
    mailbox: Mailbox[BlockEvent],
    startFrom: StartFrom = StartFrom.Tip
): Future[Unit]
```

Registration semantics (`At(point)` case), per-source type:

**Sync path** — rollback buffer or ChainStore covers the window:

1. Inside the engine worker: create the subscription record,
   acquire buckets, snapshot `currentTip`.
2. Try every sync source's `iterate(point, currentTip)` in order.
   The first that returns `Right(iterator)` wins.
3. Still on the worker, drive the iterator: for each replayed
   `AppliedBlock`, compute the subscription's per-query diff and
   offer events directly to the mailbox. Because the engine
   worker is single-threaded, no live `onRollForward` interleaves
   — queued live events wait for this task to finish.
4. Return `Future.unit` to the caller. Live fan-out to this
   subscription flows through the normal `onRollForward` path
   from the next block onwards.

**Async path** — no sync source covers, at least one
`AsyncReplaySource` is configured:

1. Inside the engine worker: create the subscription record,
   acquire buckets, install an entry in `replayPending` keyed on
   the subscription id with `(snapshotTip, empty buffer)`. Live
   `onRollForward` / `onRollBackward` now route events for this
   sub into the buffer instead of the mailbox.
2. Off-worker: call `AsyncReplaySource.prefetch(point,
   snapshotTip)` on the first async source. If it returns
   `Left(ReplaySourceExhausted)`, recurse on the remaining async
   sources. If it returns `Left(ReplayInterrupted)` or a failed
   Future, stop cascading and fail the subscription.
3. On success, submit a single worker task (Phase B) that:
   - applies the prefetched blocks to the subscription's mailbox
     in chain order;
   - removes the subscription from `replayPending` and drains its
     captured buffer into the mailbox (these are live events that
     landed during Phase A — strictly after `snapshotTip`, so
     they continue the stream seamlessly);
   - returns `Future.unit` to the caller (the original
     `registerXxxSubscription` future completes here).
4. From this point on, live `onRollForward` fan-out delivers to
   this subscription's mailbox directly — no more buffering.

The `replayPending` buffer is the only place the engine holds
per-subscription queued events, and it is bounded by the replay
window (blocks between `point` and `currentTip` × per-block
fan-out density). The `DeltaBufferPolicy.Bounded(n)` option
applies to the mailbox as usual; if the live-event queue
saturates mid-replay the mailbox fails just like the live case.

### Why not use `onRollForward` directly

`onRollForward` updates global state (`tipRef`, `byKey` buckets,
rollback buffer) and fans out to *every* subscriber. Using it for
replay would corrupt the engine's view of the chain and flood
every other subscriber with duplicate events. A per-subscription
dry-run variant is the smallest correct primitive.

The dry-run reuses `QueryDecomposer.matches` verbatim — we do not
copy the matching logic, we call it.

## Interaction with M5 chain-sync

M5's applier is unchanged. The live `ChainApplier` owns *its*
ChainSync + BlockFetch channels, managed by its `applierScope`.
M7 introduces a second, orthogonal scope for each replay:
`ReplayScope` is a cancel-source linked to the subscription's
unregister path — when the subscriber goes away, the replay
cancels, its ChainSync route tears down, and any pending BlockFetch
requests on the shared channel are aborted.

The applier's `StartFrom` parameter (passed at startup via the
warm-restart tip logic from M6) continues to drive the *live*
intersect. That's the single engine-wide anchor; per-subscription
`StartFrom.At(point)` never changes it.

## Interaction with M6 persistence

The engine does not persist subscription identity (see *Out of
scope*). A checkpointed subscriber on restart must:

1. Read its own persisted checkpoint from the app's DB.
2. Construct the provider as usual (M6 handles warm restart from
   the engine's tip).
3. Call `subscribe(..., startFrom = At(checkpoint))`.

The engine then replays from `checkpoint` to the warm-restored
tip, then switches to live fan-out. No configuration on the
persistence store; no opinions about where the app stores its
checkpoint.

Corner case — the app's checkpoint is *ahead* of the engine's
warm-restored tip. This can happen if the engine persistence
file was reset between crashes but the app's DB was not. The
engine detects `point > currentTip`, fails the subscription with
`ReplayCheckpointAhead(point, currentTip)`, and the caller's
recovery is its business (wipe DB, rebuild from backup, whatever
the app's policy is).

## Provider wiring

### Fs2

```scala
// Fs2BlockchainStreamProvider.connectN2N — M7 Phase 2b:
for {
    backup    <- buildBackup(config.backup)
    fallbacks  = config.fallbackReplaySources :+ buildPeerReplaySource(n)
    engine    <- buildEngine(config, backup, persistence, fallbacks)
    conn      <- NodeToNodeClient.connect(n.host, n.port, ...)
    ...
} yield ...
```

The engine gains one optional constructor parameter:
`fallbackReplaySources: List[ReplaySource] = Nil`. The provider
assembles this list:

- `config.chainStore` (if `Some`) wraps into a
  `ChainStoreReplaySource`;
- on the N2N chain-sync path, a `PeerReplaySource` backed by a
  factory that calls `NodeToNodeClient.connect(host, port, magic,
  config)` each time is appended after the sync sources.

Without any configured fallback the engine only has its
in-memory rollback buffer, which suffices for short downtimes
(checkpoint within `securityParam` blocks of tip).

`PeerReplayConnectionFactory` is a one-method trait:

```scala
trait PeerReplayConnectionFactory {
    /** Produce a freshly-handshaken connection ready for
      * ChainSync + BlockFetch. PeerReplaySource owns the returned
      * connection and closes it at the end of every prefetch.
      */
    def open()(using ExecutionContext): Future[NodeToNodeConnection]
}
```

Each prefetch opens a new TCP socket, runs the handshake +
keep-alive boilerplate via `NodeToNodeClient.connect`, drives
the intersect + pull loop, closes. The cost is one TCP + one
Ouroboros handshake per replay, negligible against the
BlockFetch cost for any meaningful window. No changes to
`NodeToNodeConnection`; no second ChainSync route on the live
mux.

### Ox

Parallel changes in `OxBlockchainStreamProvider.create`; the
subscription lifecycle is managed inside the ox scope's structured
concurrency, so `ReplayScope` fits naturally as a child scope.

## Restart semantics, revisited

The *Restart semantics* table in `indexer-node.md` now reads all
three columns real:

| | Cold (opt-out) | Warm (M6) | Checkpoint-driven (M7) |
|---|---|---|---|
| **What persists** | nothing | tip, params, `ownSubmissions`, buckets | engine state *plus* app-owned checkpoints |
| **Downtime events** | lost as discrete events | delivered on resume | delivered from the checkpoint forward |
| **`includeExistingUtxos=false` needed?** | n/a | recommended, avoids re-seed | not relevant (replay supersedes seed) |
| **Library cost** | M2 | M6 | M7 |
| **User cost** | `.noop` opt-out | set `appId` | persist own checkpoint + `At(point)` resubscribe |

The Checkpoint row now becomes the go-to pattern for apps with
at-least-once per-event requirements. The doc's existing guidance
— "Checkpoint is the stronger option for consumers that need a
persistent, deduplicable event log on their own side" — becomes
actionable code instead of a plan.

## Error model

Adds to M6's table.

| Failure | Where | Cause | Observers see |
|---|---|---|---|
| Checkpoint older than rollback buffer, no ChainStore, no peer | `registerXxxSubscription` | `Engine.ReplaySourceExhausted(point)` | subscription future fails; mailbox fails |
| Checkpoint ahead of `currentTip` | `registerXxxSubscription` | `Engine.ReplayCheckpointAhead(point, tip)` | subscription future fails |
| PeerReplaySource `FindIntersect` returns `NotFound` | replay driver | `Engine.ReplaySourceExhausted(point)` (caught from `ChainSyncError.NoIntersection`) | subscription future fails mid-replay; mailbox fails; live subscriptions unaffected |
| Replay ChainSync mid-stream transport failure | replay driver | `Engine.ReplayInterrupted(cause)` | subscription future fails; mailbox fails; live unaffected |
| Rollback buffer ages the checkpoint out during a slow replay | replay worker | `Engine.ReplaySourceExhausted(point)` | subscription fails, caller can retry with a newer checkpoint |
| Subscription unregistered during replay | replay scope | replay cancelled, no event to observer | clean cancellation |
| App registers `At(point)` immediately after engine boot before tip is known | `registerXxxSubscription` | `Engine.EngineNotReady` | subscription future fails (transient) |

`ReplaySourceExhausted` carries the checkpoint the app passed so
its recovery handler can log it verbatim and choose whether to
wipe-and-restart from tip.

## Test strategy

Five tiers, building on M2/M5/M6.

### 1. Unit tests — shared

- **`ReplaySource.RollbackBufferSource` iteration** against fixed
  volatile tails: all combinations of `from` inclusive/exclusive of
  each block, `to == tip`, `from == tip` (empty replay), `from` not
  in buffer (exhausted).
- **Replay-pending event queueing**: fabricate a subscription in
  replay-pending state, apply live `onRollForward`s, assert events
  are queued on the subscription's internal buffer and not yet
  visible on the mailbox.
- **Replay-to-live transition**: simulate completion of replay,
  assert queued events drain to the mailbox in order, the
  replay buffer is released, and subsequent live events flow
  directly.
- **`ReplayCheckpointAhead` detection**: `point.slot` strictly
  greater than `currentTip.slot`.

### 2. Engine integration — shared

Uses `EnginePersistenceStore.inMemory()` plus a synthetic applier.

- **Round-trip**: seed 20 blocks into the engine, persist, stop,
  restart, re-subscribe with `At(block_5.point)`; assert the
  subscriber receives the events from blocks 6..20 exactly once,
  in order, each with the correct `ChainPoint`.
- **Concurrent live + replay**: subscriber A live, subscriber B
  registers with `At(old_point)`; run 50 more blocks while B's
  replay is simulated-slow; assert A sees live fan-out without
  stalls, B sees replay-then-live without gaps or duplicates.
- **Rollback during replay**: subscriber in mid-replay, engine
  receives a `onRollBackward(to)` where `to` is inside the
  replayed range. Assert B's mailbox receives the `RolledBack(to)`
  after the replay transition (not during — consistent with the
  live contract that replay events are "real" past events).
- **Multi-subscription same checkpoint**: three subscribers
  register with the same `At(point)` simultaneously; assert each
  runs its own replay (M7 makes no sharing promise) and all three
  complete.

### 3. Replay-source selection — shared

- **`pickSource` precedence**: checkpoint inside buffer →
  `RollbackBufferSource`; inside ChainStore (stubbed) → stub
  returned; inside peer horizon → `PeerReplaySource`; past all →
  `ReplaySourceExhausted`.
- **Degradation with time**: start replay via `RollbackBufferSource`,
  advance the engine's tip enough to age the starting point out,
  assert the replay completes successfully if it was fast enough
  (the iterator captured the blocks up front) and fails with
  `ReplaySourceExhausted` if the iterator races buffer eviction
  (deterministic via a manual-advance test harness).

### 4. Peer replay — JVM, yaci

Extends `scalusCardanoNetworkIt` (`YaciPeerReplaySuite`).

- **Transient N2N connection against yaci**: live `Fs2` provider
  observes 3 tips to capture a recent point, then a
  `PeerReplaySource` opens a separate short-lived connection and
  `prefetch(origin, capturedTip)` returns the expected
  `Seq[AppliedBlock]`. Asserts the last block's point equals the
  target.
- **Simultaneous replay + live**: same yaci instance. Live tip
  subscription runs in the background while a concurrent
  `PeerReplaySource.prefetch` executes against the same
  endpoint; assert the live tip advances during prefetch —
  proving the transient connection does not stall the live mux.
- **Replay over unreachable slot**: `prefetch` against an
  obviously-future slot returns
  `Left(ReplaySourceExhausted)` via the `NoIntersection`
  mapping. (A full peer-restart-during-replay test is a natural
  extension but requires Docker control of the yaci container's
  lifecycle; current scope stops short of that.)

### 5. End-to-end batcher with app-side checkpoints — JVM, yaci

- **Canonical Checkpoint pattern**: batcher test harness that
  persists `(last_processed_point, last_utxo_digest)` to an
  in-memory map after every processed event. Kill the process;
  restart; subscribe with `At(last_processed_point)`; assert the
  post-restart event trail reaches the same final state the
  no-crash run produces for the same yaci block sequence.

## Out of scope for M7 — explicit list

- `StartFrom.Origin` from mainnet (Byron decoding — M11+).
- ChainStore backend implementation (M9).
- Durable per-subscription state (app owns it).
- Shared replay cache across subscribers.
- `StartFrom.At(point)` on latest-value streams (type-level no-op).
- Back-pressure on the replay ChainSync route beyond mailbox
  bounded policy — we rely on the driver's per-request
  pipelining, not a global window policy.
- Automatic "downgrade to seed-from-backup" when all replay
  sources are exhausted — caller's recovery path, not the engine's.

## Open questions

- **`PeerReplaySource` concurrency with BlockFetch.** First cut
  shares a single BlockFetch route fairly-queued between live
  and replay. If that serialises too aggressively on mainnet-scale
  replay windows, allocate a second BlockFetch channel. Measure
  first; the yaci IT will surface the shape of this quickly.
- **Replay-buffer overflow policy.** The per-subscription
  live-events buffer during replay is currently unbounded. If a
  slow replay + fast live chain-sync floods a subscriber's
  buffer, the process OOMs silently. Options: reuse the existing
  `DeltaBufferPolicy` (treat overflow as mailbox failure); add a
  separate `ReplayBufferPolicy`; cap at some large constant and
  log-warn before failing. Default to "treat as mailbox failure"
  — consistent with existing overflow semantics — and revisit if
  a real app triggers it.
- **Replay ordering within a block.** A block's transactions
  have a natural order. The live fan-out preserves it. Replay
  must preserve the same order — verified by the engine tests,
  but worth calling out as an invariant.
- **`includeExistingUtxos = true` + `At(point)`.** Two possible
  semantics: (a) seed from backup at the *checkpoint* (gives a
  point-in-time accurate view at `point`); (b) seed from backup at
  the *current tip*, then replay events from `point` (standard
  Checkpoint pattern). Option (b) is simpler and matches the
  doc's existing implication; option (a) requires backup that
  supports point-in-time queries (not every backup does).
  Default to (b); reject (a) with a typed error if
  `includeExistingUtxos = true` and `startFrom = At(point)` (or
  treat as b, and document). Settle during implementation.
- **`subscribeBlock` replay semantics.** Block subscriptions
  currently emit on every block. Replaying them is literal. But
  at high block rate the mailbox could flood more easily than
  UTxO subscriptions do. No new machinery; just observe and
  document the memory envelope.
- **Replay metrics.** A per-subscription replay should surface
  blocks-remaining / eta for UI feedback. Add a
  `SubscriptionDiagnostics` handle to the stream's companion
  object — shape TBD, small, read-only.

## References

- `docs/local/claude/indexer/indexer-node.md` — milestone 7 row;
  *Restart semantics* (all three columns); *Subscribe seeds the
  index*; *Start point* convention.
- `docs/local/claude/indexer/engine-persistence-minimal.md` — M6
  substrate; warm-restart tip handling informs how the replay's
  target `currentTip` is established.
- `docs/local/claude/indexer/cardano-network-chainsync.md` — M5
  ChainSync driver; `IntersectSeeker.seek`; the same code drives
  the short-lived replay conversation.
- `docs/local/claude/indexer/cardano-network-transport.md` — M4
  mux; the basis for allocating a second ChainSync channel on
  the same connection.
- `scalus-embedded-node/scalus-streaming-core/.../engine/Engine.scala`
  — existing `onRollForward` fan-out; the per-subscription
  dry-run reuses `QueryDecomposer.matches` and the subscription
  registry.
