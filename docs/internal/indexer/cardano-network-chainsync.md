# N2N chain-sync — design doc

**Status.** Design doc for milestone M5 of
`docs/local/claude/indexer/indexer-node.md`. Not yet implemented.
Covers the ChainSync and BlockFetch mini-protocols, header / block
decoding into `AppliedBlock`, start-point handling (`StartFrom.Tip`
/ `Origin` / `At(point)`), and the wiring that makes
`ChainSyncSource.N2N(host, port)` a real backend on both the fs2
and ox stream providers. M4's transport
(`docs/local/claude/indexer/cardano-network-transport.md`) is the substrate;
M5 is the first milestone that turns transport frames into engine
events.

## Scope

M5 delivers:

1. A **ChainSync** (mini-protocol 2) state machine driving
   `MsgFindIntersect` / `MsgRequestNext` / `MsgRollForward` /
   `MsgRollBackward` against a peer.
2. A **BlockFetch** (mini-protocol 3) state machine driving
   `MsgRequestRange` / batch response cycles for per-block body
   fetches.
3. Era-aware decoding of the `[era, header]` / `[era, block]`
   envelopes carried on the wire, producing
   `scalus.cardano.ledger.BlockHeader` / `Block` and projecting
   those into the engine's `AppliedBlock`.
4. A **chain applier** that composes ChainSync + BlockFetch into
   the engine-visible event stream: `onRollForward(AppliedBlock)`
   on every block, `onRollBackward(ChainPoint)` on every
   rollback, a single teardown on unrecoverable wire failure.
5. A **start-point resolver** that maps
   `SubscriptionOptions.startFrom` (`Tip` / `Origin` / `At(p)`)
   onto the peer via `MsgFindIntersect` and surfaces a typed
   failure if the peer has no usable intersection.
6. Wiring of `ChainSyncSource.N2N(host, port)` in
   `Fs2BlockchainStreamProvider.create` and
   `OxBlockchainStreamProvider.create` — the factories connect,
   install the chain applier against the engine, and return a
   live provider. This closes the motivating batcher use case:
   N2N read, Blockfrost seeding and `submit`.
7. Diagnostic exposure of the N2N connection's `rtt` and
   `negotiatedVersion` via a provider-level diagnostic handle
   (shape TBD — scoped here to resolve the M4 open question).
8. Teardown story: connection-root cancel on the underlying
   `NodeToNodeConnection` translates to
   `Engine.closeAllSubscribers()` and fails every live stream
   with the stored cause.

M5 does **not** deliver:

- **TxSubmission2** — M9. Submit stays delegated through
  `BackupSource.Blockfrost(...)` until then.
- **N2C chain-sync / LocalTxSubmission / LocalStateQuery** —
  M8 / M12. Separate concerns; will reuse the same
  `ChainSyncDriver` state machine verbatim over an N2C mux.
- **Block-fetch pipelining beyond a single in-flight range
  request.** See *BlockFetch pipelining policy*. Throughput
  optimisations are M5+ work once the single-range path is
  proven.
- **Byron-era block decoding.** Byron headers and bodies have a
  different CBOR shape than Shelley+; scalus-cardano-ledger
  only covers Shelley+ (`BlockHeader` + `Block`). For M5 we
  defend-decode the era tag and **fail fast** on Byron blocks
  with a typed exception so users understand why syncing from
  Origin on mainnet doesn't work end-to-end until M11 (Mithril
  bootstrap) lets them skip the Byron era entirely. Preview /
  preprod / yaci-devkit / "sync from tip on mainnet" all work
  because they land in Shelley+ eras.
- **On-chain protocol-parameter tracking.** The engine still
  uses `cardanoInfo.protocolParams` as the initial
  `paramsRef` and relies on `backup.fetchLatestParams` for
  refreshes. Era-boundary / update-proposal tracking is a
  separate, larger work item outside the N2N scope.
- **Peer-sharing** — wire-protocol handshake already advertises
  `peerSharing = 0`; M5 doesn't change that.
- **Multi-peer / connection pooling.** One peer, one connection.
  Adequate for the motivating use cases. A wallet-grade
  multi-relay topology is a separate work item.
- **Warm restart persistence.** M5 stays on the M2 Cold path
  — engine state is not persisted. `StartFrom.At(checkpoint)`
  replay is M7; durable engine snapshot is M6. Both build on
  the drivers M5 ships but add their own layers.

## Why now

M4 landed the transport: a connected, handshake-complete,
keep-alive-running `NodeToNodeConnection` that exposes
`MiniProtocolBytes` handles for the not-yet-consumed
mini-protocols. The engine side (M2) has been driven solely by
`ChainSyncSource.Synthetic` since then — exercising the
subscribe / fan-out / rollback plumbing but never seeing a real
block.

M5 is the join point. It writes the two small state machines
that turn wire bytes into `AppliedBlock` / `RollBackward`
signals, teaches the providers how to stand a chain-sync loop up
against a real relay, and fixes the edges (rollback, teardown,
startup resume, diagnostic surface) so that `ChainSyncSource.N2N`
stops being a `return IO.raiseError(UnsupportedSourceException)`
in the factory.

Once M5 is in, the motivating batcher of the indexer-node doc is
fully runnable against a public network. Everything after this
(M6 warm restart, M7 checkpoint replay, M8 N2C, M9 TxSubmission2,
M10+ Heavy / Mithril) adds durability, protocol reach, and
operational envelope — but the read path is proven end-to-end.

## Architecture at a glance

```
  Application
      │
      ▼
  Fs2BlockchainStreamProvider / OxBlockchainStreamProvider
      │         (M2 — already landed; M5 wires the N2N branch)
      ▼
  Engine + EngineDriver            ◄── AppliedBlock stream
      │         (M2 — already landed)
      ▲
      │   chain applier calls onRollForward / onRollBackward
      │
  ChainApplier                     (M5 — this doc)
      ▲            ▲
      │            │
      │  headers   │  bodies
      │            │
  ChainSyncDriver  BlockFetchDriver (M5 — this doc)
      │            │
      ▼            ▼
  CborMessageStream[ChainSyncMsg] / CborMessageStream[BlockFetchMsg]
                 │
                 ▼
             MiniProtocolBytes(2) / MiniProtocolBytes(3)
                 │
  ───────────────┴─────────────────────────────── M4 / M5 boundary
                 │
             Multiplexer
             AsyncByteChannel
                 │
                 ▼
             OS socket
```

The M4/M5 boundary is exactly where M4's doc drew it: "the mux
delivers bytes for protocol P" ⇄ "the state machine for protocol P
consumes those bytes and emits application-level events." M5 adds
one more layer on top — the chain applier — that composes two
co-operating state machines into the single input stream the
engine expects (`AppliedBlock` + `RollBackward`).

## Module layout

M5 extends the existing `scalus-embedded-node/scalus-cardano-network/`
module (renamed from `scalus-n2n` just before M5) rather than
spinning up a new subproject. Rationale:

- Handshake and KeepAlive are already mini-protocols living in
  this module; ChainSync / BlockFetch are the same kind of thing.
- Matches pallas's single-module layout for the mini-protocol
  stack — transport + state machines in one place.
- N2C (M8) reuses the state machines verbatim over an N2C-flavoured
  transport; keeping them in `scalus-cardano-network` is the shortest
  path to that reuse.

The module's initial M4 "transport is lean, no cardano-ledger
dependency" framing is **relaxed** starting with M5: chain-sync
needs block decoding, so the module picks up `scalus-cardano-ledger`
as a dependency. That's fine — M4's framing was about the initial
slice, not a frozen contract.

```
scalus-embedded-node/scalus-cardano-network/
    shared/src/main/scala/scalus/cardano/network/
        <existing M4 files: AsyncByteChannel, Multiplexer, …>
        handshake/                         ← existing (M4)
        keepalive/                         ← existing (M4)
        chainsync/                         ← NEW (M5)
            ChainSyncMessage.scala         // wire ADT + borer codec
            ChainSyncDriver.scala          // state machine
            IntersectSeeker.scala          // StartFrom.* → MsgFindIntersect
        blockfetch/                        ← NEW (M5)
            BlockFetchMessage.scala        // wire ADT + borer codec
            BlockFetchDriver.scala         // state machine
        BlockEnvelope.scala                // [era, header|block] decoder
        ChainApplier.scala                 // composes the two drivers + engine
        ChainSyncError.scala               // typed failures exposed on the provider
    shared/src/test/scala/scalus/cardano/network/…
    jvm/src/test/scala/scalus/cardano/network/…
```

Existing sbt projects — no new subprojects:

| sbt val                    | artifact name             | targets |
|----------------------------|---------------------------|---------|
| `scalusCardanoNetworkJVM`  | `scalus-cardano-network`  | JVM     |
| `scalusCardanoNetworkJS`   | `scalus-cardano-network`  | JS (later, alongside a Node `AsyncByteChannel`) |

Dependencies change from M4 state:

- `scalus-streaming-core` — already present (for `Mailbox`,
  `DeltaBufferPolicy`, `CancelToken`/`CancelSource` in
  `scalus.cardano.infra`). No change.
- `scalus-cardano-ledger` — **new in M5**, needed by the
  `BlockEnvelope` decoder and the `ChainApplier` projection.

`scalus-streaming-core` gains one call site into
`scalus-cardano-network`: the providers call
`ChainApplier.spawn(connection, engine, startFrom)` from their
`create` factories when `ChainSyncSource.N2N(...)` is selected.
No streaming-core code knows what a `MsgRollForward` is — the
applier is the only boundary-crossing object.

## ChainSync state machine

### Wire (from `cardano-diffusion/protocols/cddl/specs/chain-sync.cddl`)

```
chainSyncMessage
    = msgRequestNext
    / msgAwaitReply
    / msgRollForward
    / msgRollBackward
    / msgFindIntersect
    / msgIntersectFound
    / msgIntersectNotFound
    / chainSyncMsgDone

msgRequestNext         = [0]
msgAwaitReply          = [1]
msgRollForward         = [2, base.header, base.tip]
msgRollBackward        = [3, base.point, base.tip]
msgFindIntersect       = [4, base.points]
msgIntersectFound      = [5, base.point, base.tip]
msgIntersectNotFound   = [6, base.tip]
chainSyncMsgDone       = [7]
```

The `header` carried by `MsgRollForward` is era-tagged: `[era,
headerBody]` where `era ∈ {1..7}` (Byron, Shelley, Allegra,
Mary, Alonzo, Babbage, Conway). M5 handles Shelley+ (eras 2–7)
and fails fast on Byron (era 1) with
`ChainSyncError.ByronEra` — see *Header & block decoding*.

### Agency and the driver's states

ChainSync is a two-agency protocol (client agency on state
`Idle`; server agency on `CanAwait`, `MustReply`, `Intersect`).
The driver tracks agency implicitly by which message it just
sent and what response shapes it will accept.

```
                     initial
                        │
                        │  driver constructed, no intersect yet
                        ▼
                  ┌──────────────┐
                  │ SeekIntersect│  ← send MsgFindIntersect(points)
                  └──────┬───────┘
                         │
           ┌─────────────┴─────────────┐
           │                           │
  MsgIntersectFound(p,t)      MsgIntersectNotFound(t)
           │                           │
           ▼                           ▼
    record point = p             ChainSyncError.NoIntersection
    record tip = t                (fails the provider future)
           │
           ▼
   ┌──────────────┐   MsgRequestNext     ┌──────────────┐
   │    Idle      │────────────────────► │  AwaitReply  │
   └──────────────┘                      └──────┬───────┘
         ▲                                      │
         │     ┌────────────────────────────────┼──────────────┐
         │     │                                │              │
         │     │     MsgAwaitReply              │              │
         │     │                                │              │
         │     ▼                                ▼              ▼
         │  ┌──────────────┐         MsgRollForward(h,t)  MsgRollBackward(p,t)
         │  │  MustReply   │                │                  │
         │  └──────┬───────┘                │                  │
         │         │                        │                  │
         │         └────────────────┐       │                  │
         │                          │       │                  │
         │     MsgRollForward(h,t)  │       │                  │
         │     MsgRollBackward(p,t) │       │                  │
         │                          ▼       ▼                  ▼
         │                    emit Forward(h,t)     emit Backward(p,t)
         │                          │                          │
         └──────────────────────────┴──────────────────────────┘
```

The driver exposes a single pull-style method:

```scala
sealed trait ChainSyncEvent
object ChainSyncEvent {
    final case class Forward(header: BlockHeader, tip: ChainTip)  extends ChainSyncEvent
    final case class Backward(to: ChainPoint,     tip: ChainTip)  extends ChainSyncEvent
}

final class ChainSyncDriver private[protocols] (
    stream: CborMessageStream[ChainSyncMessage],
    scope: CancelToken
) {
    /** Negotiate start point. Completes once the peer accepts
      * an intersection or fails with [[ChainSyncError.NoIntersection]].
      * Callers invoke exactly once, before the first [[next]] call.
      */
    def findIntersect(points: Seq[ChainPoint]): Future[ChainPoint]

    /** Pull the next event. Issues [[MsgRequestNext]] internally;
      * handles the optional [[MsgAwaitReply]] interstitial
      * transparently (stays parked on the wire rather than
      * returning a "waiting" marker to the caller).
      */
    def next(): Future[ChainSyncEvent]

    /** Send [[MsgDone]] and drive the route to the mux's Draining
      * state. Used on clean shutdown only. */
    def close(): Future[Unit]
}
```

### Why no pipelining in M5

Ouroboros-network's chain-sync client pipelines aggressively:
it can have many `MsgRequestNext` in flight and only blocks when
the pipeline depth is hit. For our initial deployment that's
premature:

- Latency vs throughput: our consumer is the engine, a single
  worker thread processing `AppliedBlock` sequentially. The
  block-fetch round trip is the throughput bottleneck, not
  chain-sync depth.
- Rollback handling is subtle under pipelining (the client may
  have requested headers past a rollback point; they need to be
  discarded correctly). The straight request-response shape is
  provably correct and lets us land the feature.

Revisit once we have a measured shortfall — probably when M10's
Heavy-mode indexer wants to sync hundreds of megabytes per
minute from genesis (and by then we have Mithril for the cold
start anyway).

### `MsgAwaitReply`

Server sends `MsgAwaitReply` when we've caught up with its tip
and the next block hasn't been produced yet. The driver's
response is to keep the receive future pending — the next
message we care about is a `MsgRollForward` or `MsgRollBackward`
arriving on the same stream. No per-call timeout; the caller's
scope token is the only cancellation signal.

This deviates from a naive reading of the protocol (where
`MsgAwaitReply` is a distinct state the client can observe) but
matches how consumers actually want to use the driver: "block
until there's a real event" rather than "tell me when you're
parked."

## BlockFetch state machine

### Wire (from `cardano-diffusion/protocols/cddl/specs/block-fetch.cddl`)

```
blockFetchMessage
     = msgRequestRange
     / msgClientDone
     / msgStartBatch
     / msgNoBlocks
     / msgBlock
     / msgBatchDone

msgRequestRange = [0, base.point, base.point]
msgClientDone   = [1]
msgStartBatch   = [2]
msgNoBlocks     = [3]
msgBlock        = [4, base.block]
msgBatchDone    = [5]
```

Like `base.header`, `base.block` is era-tagged `[era, block]`
on the wire. Shelley+ only in M5.

### States

```
  ┌──────┐  MsgRequestRange(p1, p2)  ┌────────────────┐
  │ Idle │──────────────────────────►│ AwaitBatchStart│
  └──────┘                           └───────┬────────┘
    ▲                                        │
    │                  ┌────────────┬────────┴─────────┐
    │                  │            │                  │
    │          MsgStartBatch   MsgNoBlocks       other/decode-err
    │                  │            │                  │
    │                  ▼            ▼                  ▼
    │          ┌──────────────┐  (range empty)   ChainSyncError.Decode
    │          │ StreamingBlks│                       │
    │          └──────┬───────┘                   root cancel
    │                 │
    │     ┌───────────┴──────────┐
    │     │                      │
    │ MsgBlock(b)           MsgBatchDone
    │     │                      │
    │     │                      ▼
    │     │                   ┌──────┐
    │     ▼                   │ Idle │  (back to top)
    │ emit b; stay            └──────┘
    │ in StreamingBlks
    └─────────┘
```

### Pipelining policy

One in-flight `MsgRequestRange` at a time for M5. The driver's
interface is:

```scala
final class BlockFetchDriver private[protocols] (
    stream: CborMessageStream[BlockFetchMessage],
    scope: CancelToken
) {
    /** Fetch a single block at `point`. Completes when the peer's
      * MsgBatchDone has arrived, or fails with
      * [[ChainSyncError.MissingBlock]] on MsgNoBlocks. */
    def fetchOne(point: ChainPoint): Future[Block]

    /** Send [[MsgClientDone]] and drive to Draining. Clean shutdown. */
    def close(): Future[Unit]
}
```

Range requests fetch `[point, point]` to avoid walking the
multi-block batch path in M5. Even a single-element range uses
the `MsgStartBatch → MsgBlock → MsgBatchDone` envelope per the
CDDL; `MsgNoBlocks` handling stays correct for the "peer rolled
back between ChainSync delivering the header and BlockFetch
asking for it" race.

### Why fetch one block per ChainSync event

The chain applier consumes ChainSync events serially (matching
the engine's serial ingestion path). Fetching bodies one at a
time keeps the applier straightforward: "here's a new header →
fetch its body → apply." A throughput win from "batch N pending
headers into one range request" is real but deferred; it
interacts with rollback semantics in ways we don't want to
re-derive for an initial ship.

## Header & block decoding

### Wire envelope

Both `base.header` (ChainSync) and `base.block` (BlockFetch) are
**era-tagged** on the wire, but with different wrapping shapes
AND different era-numbering conventions — verified against
pallas during M5 IT work:

```
base.header (ChainSync)   =  [ era_u8 , tag24(headerCbor) ]
                               ^^ HardFork 0-based (Byron=0..Conway=6)

base.block  (BlockFetch)  =  tag24( [ era_u16 , blockCbor ] )
                                      ^^ cardano-ledger-spec 1-based
                                         (Byron=1..Conway=7)
```

- ChainSync: era sits **outside** the tag24, u8, 0-based.
- BlockFetch: era sits **inside** the tag24, u16, 1-based.

Our BlockFetch codec normalises the 1-based era to the HardFork
0-based scheme before handing it to the applier, so every other
layer sees a single era-numbering convention.

```scala
object BlockEnvelope {
    sealed trait Era
    object Era {
        case object Byron   extends Era  // 1
        case object Shelley extends Era  // 2
        case object Allegra extends Era  // 3
        case object Mary    extends Era  // 4
        case object Alonzo  extends Era  // 5
        case object Babbage extends Era  // 6
        case object Conway  extends Era  // 7
    }

    def decodeHeader(bytes: ByteString): Either[ChainSyncError, (Era, BlockHeader)]
    def decodeBlock (bytes: ByteString): Either[ChainSyncError, (Era, Block)]
}
```

Shelley+ eras (2–7) all use the same `BlockHeader` /
`Block` types from `scalus-core` — the structural differences
between them (MA asset support, Alonzo Plutus, Babbage
inline-datums, Conway governance) are all inside
`TransactionBody` / `BlockHeaderBody`, already handled by the
existing decoders.

Byron (era 1) uses a separate CBOR shape not modelled in
`scalus-core`. M5 returns `ChainSyncError.ByronEra(slot)` for
it and escalates the connection root. The practical consequence:

- Sync from **tip** on mainnet, preview, preprod, yaci-devkit
  → always lands in Shelley+ → works.
- Sync from **origin** on mainnet → hits Byron blocks → fails.
  Users who want this today either use Mithril (M11) or
  `BlockfrostPolling` (M13).

Documented behaviour; not a "bug" to retrofit.

### `AppliedBlock` projection

From `(header, block)` the applier builds:

```scala
def toAppliedBlock(header: BlockHeader, block: Block, bytes: Array[Byte]): AppliedBlock = {
    given OriginalCborByteArray = OriginalCborByteArray(bytes)
    val point = ChainPoint(
        SlotNo(header.slot),
        BlockHash(header.headerBody.blockBodyHash.bytes) // TBD: header hash vs body hash
    )
    val tip = ChainTip(point, header.blockNumber)
    val txs = block.transactions.map { tx =>
        AppliedTransaction(
            id      = tx.id,
            inputs  = tx.body.value.inputs.toSet,
            outputs = tx.body.value.outputs.map(_.value).toIndexedSeq
        )
    }
    AppliedBlock(tip, txs)
}
```

Open question flagged here: `ChainPoint` uses a **block hash**,
and Cardano has both a "header body hash" (what the peer's
`MsgRollForward` point refers to) and the hash of the
serialised header as a whole. The existing `Block.hash` returns
`headerBody.blockBodyHash`, which is **not** the identifier
ouroboros-network uses for `base.point`. The correct value is
`Blake2b_256(serialized header bytes)`. Cross-checked against
pallas before implementation; noted in *Open questions* so the
projection above gets fixed to pass the real hash through.

## Chain applier

The applier is the single orchestrator that owns the two
drivers, composes them, and feeds the engine.

```scala
final class ChainApplier private (
    chainSync:  ChainSyncDriver,
    blockFetch: BlockFetchDriver,
    engine:     Engine,
    scope:      CancelToken
) {

    /** Drive the sync loop until the scope cancels. Installs itself
      * as the engine's chain-sync source; completes normally on
      * clean tear-down, fails on the stored cause otherwise.
      */
    private def run(startFrom: StartFrom): Future[Unit] = for {
        intersect <- chainSync.findIntersect(pointsFor(startFrom))
        _         <- loop()
    } yield ()

    private def loop(): Future[Unit] =
        chainSync.next().flatMap {
            case ChainSyncEvent.Forward(header, _tip) =>
                val point = pointOf(header)
                blockFetch.fetchOne(point).flatMap { block =>
                    engine.onRollForward(toAppliedBlock(header, block, block.raw))
                }.flatMap(_ => loop())
            case ChainSyncEvent.Backward(to, _tip) =>
                engine.onRollBackward(to).flatMap(_ => loop())
        }
}

object ChainApplier {
    /** Wires drivers from a live NodeToNodeConnection and starts the
      * loop in the background. Returns a handle holding the applier
      * scope; cancelling the handle stops the loop. The connection
      * itself outlives the handle — close the connection through the
      * provider's teardown, not through this handle. */
    def spawn(
        conn:      NodeToNodeConnection,
        engine:    Engine,
        startFrom: StartFrom,
        logger:    scribe.Logger
    )(using ExecutionContext): ChainApplierHandle
}

trait ChainApplierHandle {
    def cancel(): Future[Unit]
    def done:    Future[Unit]   // normal or failure
}
```

### Scope nesting

Built on the M4 scope tree:

```
connectionRoot : CancelSource                 ← owned by N2N client
    ├── handshake                             ← settled pre-M5
    ├── keepAlive                             ← runs for lifetime
    ├── chainSync  : linkedTo(connectionRoot) ← owned by ChainApplier
    ├── blockFetch : linkedTo(connectionRoot) ← owned by ChainApplier
    └── applier    : linkedTo(connectionRoot) ← owned by ChainApplier
```

- `applier.cancel()` ⇒ stop the loop, drain the drivers with
  `MsgDone`, leave the connection alive. (Useful for a future
  provider `restart()` — not in M5.)
- `connectionRoot.cancel()` (M4 semantics — peer EOF, keep-alive
  timeout, socket error, user close) ⇒ fan-out via the linked
  scopes ⇒ applier loop's next read fails ⇒ applier's own cause
  propagates to `done`.
- Applier sees a decode error / header-era failure / missing
  block ⇒ escalates via `RoutingOps.escalateRoot(cause)` from
  M4 — we've lost protocol state, the wire is no longer
  trustworthy.

### Engine teardown

On `applier.done` completing with a failure:

```scala
handle.done.onComplete {
    case Failure(cause) =>
        // Fail everyone: subscribers see the typed cause, not a
        // generic CancelledException.
        engine.closeAllSubscribers()  // M2 — already exists
    case Success(_) =>
        () // applier finished cleanly; engine stays usable if
           // another applier spawns (M5 single-connection: not used)
}
```

Users observe the failure on their subscribed streams (the
mailbox's `fail` path M2 already implements) and can inspect the
cause.

## Start-point resolution

`SubscriptionOptions.startFrom` is currently `Tip`, `Origin`, or
`At(ChainPoint)`. The applier asks the peer for an intersect:

```
Tip        → MsgFindIntersect([peer-reported-tip])
             (two-step: first FindIntersect([Origin]) to learn the tip, then
              FindIntersect([tipJustLearned]) to commit)
Origin     → MsgFindIntersect([Origin])
At(point)  → MsgFindIntersect([point])   -- opaque single-candidate
```

**`Tip` is the default**, and the two-step dance is necessary
because ChainSync has no "start from whatever tip you have"
message. The first `MsgFindIntersect([Origin])` returns
`MsgIntersectFound(Origin, tip)` — we keep `tip` and immediately
send `MsgFindIntersect([tip])` to commit. Server accepts; first
`MsgRequestNext` returns an `AwaitReply` until a new block is
produced. Net: the app misses no block and receives no past ones.

**`Origin`** asks literally for genesis — only usable on dev
chains (yaci) because of the Byron-era limitation.

**`At(point)`** is the checkpoint case. Either the peer has our
checkpoint in its header store and we resume, or it doesn't and
we get `MsgIntersectNotFound(tip)` — we surface
`ChainSyncError.NoIntersection(tip)` and the provider's
construction future fails. M7 layers richer recovery on top
(multi-candidate `FindIntersect`, ChainStore replay) but in M5
a missing-checkpoint peer fails the startup.

The resolver lives in `IntersectSeeker` and is unit-testable
without wire I/O.

## Provider wiring

### Fs2

```scala
// Fs2BlockchainStreamProvider.create, the M2-shaped factory:
config.chainSync match
    case ChainSyncSource.Synthetic => /* M2 */
    case ChainSyncSource.N2N(host, port) =>
        for
            connection <- IO.fromFuture(IO(
                NodeToNodeClient.connect(host, port, cardanoInfo.networkMagic)
            ))
            engine     = new Engine(config.cardanoInfo, backup, Engine.DefaultSecurityParam)
            handle     = ChainApplier.spawn(connection, engine, StartFrom.Tip, logger)
            provider   = new Fs2BlockchainStreamProvider(engine)
            _          = bindTeardown(connection, handle, provider)
        yield provider
    case ChainSyncSource.N2C(_) =>
        IO.raiseError(UnsupportedSourceException("…M8"))
```

`bindTeardown` wires three things:

- `connection.rootToken.onCancel ⇒ handle.cancel() ⇒ engine.closeAllSubscribers()`.
  Users observe the cause via the same streams that were open;
  no silent termination.
- `provider.close() ⇒ connection.close()`. The provider's
  owning-side close cascades down to the socket.
- `handle.done` failure ⇒ `connection.close()`. A decode /
  invariant failure inside the applier takes the whole
  connection down rather than leaving a chain-sync-less
  zombie.

### Ox

Identical wiring, different surface: `Future` from the N2N
client becomes a `Try[...]` inside an `Ox` scope, and the
applier's `scope` is tied to the Ox scope rather than a
manually-managed `CancelSource`. The applier itself is
effect-polymorphic by virtue of returning `Future` — the fs2
adapter wraps in `IO.fromFuture`, the ox adapter awaits on
the Ox runtime.

### What `StartFrom` the provider passes

**M5: always `StartFrom.Tip`.** The provider has no
subscription-scoped `startFrom` yet — subscriptions read from
`tipRef` as it advances, and the two-step `Tip` dance on
startup ensures they don't miss blocks.

`SubscriptionOptions.startFrom` carrying `At(point)` and
`Origin` for **per-subscription** replay is M7 territory; M5
leaves the field on the subscription options untouched but
ignored at the connection layer. The field's presence doesn't
hurt — the engine simply doesn't re-drive the connection's
start-point from it.

### Provider-level diagnostic handle

M4 left an open question: "do we expose the negotiated version
on the public `BlockchainStreamProvider`?" M5's answer:

```scala
// Added to BlockchainStreamProviderTF as an extension method or a
// narrow diagnostic trait — not on the main API surface.
trait N2nDiagnostics {
    def negotiatedVersion: Int          // 14 or 16
    def rtt:               Option[FiniteDuration]
    def peerHost:          String
    def peerPort:          Int
}
```

The provider exposes it via a `maybeN2nDiagnostics: Option[N2nDiagnostics]`
method (None for `Synthetic` providers). Keeps the diagnostic
details out of the main streaming API where they would not make
sense on an emulator / polling source.

## Error model and lifetime

All failures collapse to typed causes on the appropriate scope,
following M4's pattern:

| Failure mode | Scope fired | Cause stored | Observers see |
|---|---|---|---|
| ChainSync decode error | `connectionRoot` (escalated) | `ChainSyncError.Decode(...)` | subscribers fail; `provider.close()` ok |
| BlockFetch decode error | `connectionRoot` (escalated) | `ChainSyncError.Decode(...)` | same |
| `MsgIntersectNotFound` at startup | provider future fails; no connection teardown (we never entered the loop) | `ChainSyncError.NoIntersection(tip)` | `create` returns `IO.raiseError(...)` |
| `MsgNoBlocks` for a header we just saw | retry once, then `connectionRoot` | `ChainSyncError.MissingBlock(point)` | (rare: a same-cycle rollback — still fatal to our state) |
| Byron-era block encountered | `connectionRoot` | `ChainSyncError.ByronEra(slot)` | subscribers fail |
| Engine `ResyncRequiredException` (rollback past horizon — M2 path) | engine fails subscribers directly; connection stays up | existing M2 cause | subscribers fail; connection may keep running (future resubscribe) |
| Applier scope cancelled (internal API, M5 doesn't trigger) | `applier` only | `ProtocolClosingException` | connection untouched |
| `NodeToNodeConnection.closed` fires (peer EOF, keepalive, etc.) | already fired by M4 | M4 stored cause | subscribers fail with same cause |

Engine subscribers receive failures through the existing M2
`Mailbox.fail` path. No new fan-out mechanism.

## Out of scope for M5 — explicit list

- Block-fetch pipelining (> 1 in-flight range).
- ChainSync pipelining (send multiple `MsgRequestNext` before
  reading the reply).
- Multi-candidate `FindIntersect` for opportunistic checkpoint
  resume — M7.
- Warm restart of engine state — M6.
- Byron-era block decoding.
- On-chain protocol-parameter tracking.
- TxSubmission2 — M9.
- Multi-peer / connection pooling.
- Peer-sharing responses.
- Per-subscription `startFrom=At(...)` replay path — M7.

## Test strategy

Five tiers, mirroring the M4 test plan.

### 1. Unit tests — shared, JVM + JS (when JS lands)

- **ChainSync message codec round-trip**: every variant,
  every era of `header` the decoder must accept (Shelley +
  Allegra + Mary + Alonzo + Babbage + Conway golden vectors
  sourced from `cardano-diffusion/protocols/cddl/specs/` test
  traces and from the existing `scalus-core` block fixtures).
- **BlockFetch message codec round-trip**: same shape.
- **`BlockEnvelope.decodeHeader` / `decodeBlock`**: each era
  tag 1..7 — eras 2..7 decode to a `BlockHeader` / `Block`,
  era 1 returns `ChainSyncError.ByronEra`, era 0 and 8+
  return `ChainSyncError.Decode`.
- **ChainSync state machine**: table-driven transitions. Rows:
  happy-path Forward, Forward+Backward interleave,
  `MsgAwaitReply` stays parked, `MsgIntersectNotFound` fails,
  peer sends `MsgRollForward` before `MsgRequestNext` (protocol
  violation → escalate), `MsgDone` mid-flight.
- **BlockFetch state machine**: happy-path `Start → Block →
  Done`, `NoBlocks`, `Block` without `StartBatch` (violation),
  `BatchDone` without `StartBatch` (violation).
- **`IntersectSeeker`**: given `StartFrom.Tip`, produces the
  two-step sequence `([Origin], [tipFromFirstReply])`; given
  `StartFrom.Origin`, single-step `[Origin]`; given
  `StartFrom.At(p)`, single-step `[p]`.
- **`ChainApplier.toAppliedBlock`**: deterministic
  point/tip extraction and tx projection from a fixture
  Shelley+ block.

### 2. Loopback tests — shared, no real socket

Reuse M4's `PipeAsyncByteChannel`. Stand up a test peer that
serves a canned sequence of `RollForward`s and `RollBackward`s.

- **Happy path**: canned 10-block run, assert the engine
  records 10 `AppliedBlock`s with the right slots.
- **Mid-stream rollback**: peer emits 5 forwards, then
  `RollBackward` to block 3, then 4 more forwards; assert
  engine sees the rollback and re-applies forward from block 3.
- **Tip change without block**: peer sends `MsgAwaitReply`
  and stays silent; applier does not emit anything; next
  pulled event (after an unblocking forward) is that
  forward.
- **Checkpoint not found**: peer replies
  `MsgIntersectNotFound`; assert `provider.create` fails with
  `ChainSyncError.NoIntersection`.
- **Byron block delivered**: peer includes an era-1-tagged
  block; assert escalation to `connectionRoot` with
  `ChainSyncError.ByronEra`.
- **Block-fetch rollback race**: peer advertises block B via
  chain-sync, responds `MsgNoBlocks` on the fetch; assert
  escalation.

### 3. Mock-responder tests — JVM, non-network

Extend M4's `StubN2NResponder` with chain-sync and
block-fetch minimal mini-responders that can serve a
canned chain.

- **Real-socket chain-sync smoke**: connect through
  `JvmAsyncByteChannel`, fetch 5 blocks, assert engine state.
- **Close-mid-sync**: mid-stream, stub drops the socket;
  assert engine subscribers fail with
  `ChannelClosedException`.

### 4. Yaci-DevKit integration tests — JVM, `sbtn it` only

Extends the existing `scalusCardanoNetworkIt` project. Yaci-devkit
gives us a deterministic chain from Byron genesis into
Babbage/Conway — we can ask it to start at Babbage directly
via its config, skipping the era-1 Byron decoder issue.

- **Sync from tip**: connect, sync 10 blocks (wait ~200 slots
  on devnet), verify engine's `tipRef` advances
  monotonically and block slots are sequential.
- **Rollback smoke**: use yaci's `reset`/`forkAt` API to
  cause a rollback on the chain side, verify the engine sees
  `RolledBack` and re-applies forward.
- **Seed a known tx via BackupSource**: configure
  `BackupSource.Custom(immutableEmulatorAsBackup)` with a
  fixture UTxO, subscribe; assert the seed event arrives
  before live chain events.
- **Clean shutdown mid-sync**: call `provider.close()` 5
  blocks into the sync; assert socket is reaped, engine
  worker shut down, subscribers observed the close.

### 5. Preview-relay smoke — JVM, env-gated

Same env-gate as M4 (`SCALUS_N2N_PREVIEW_IT=1`), one or
two tests only.

- **Sync 50 blocks from tip against a preview relay**:
  connect, subscribe, let the engine run until the engine
  has seen 50 forward blocks or 5 minutes elapse, assert
  every applied block has `slot > previous.slot` and
  `tip.blockNo == previous.blockNo + 1` when no rollback
  occurred.

Flakiness budget remains as in M4 — these run before
releases, not on every PR.

## Open questions

- **Point hash semantics.** *Resolved during M5 IT verification
  against yaci.* The peer's `base.point = [slotNo, blockHash]`
  uses `Blake2b_256` of the **full** BlockHeader CBOR bytes —
  the wire value of `[header_body, body_signature]` as
  received — not just the HeaderBody sub-field, and not
  `headerBody.blockBodyHash` (which hashes transaction bodies).
  Cross-referenced against pallas
  (`pallas-traverse/src/hashes.rs`,
  `impl OriginalHash for KeepRaw<'_, babbage::Header>`).
  Implemented in `ChainApplier.pointOf` — kept there because the
  applier already has the raw header bytes on hand; a general
  `BlockHeader.headerHash` accessor is a nice-to-have for
  scalus-core but not needed to close M5.
- **BlockFetch `base.block` envelope.** *Resolved during M5 IT
  verification against yaci.* Unlike ChainSync's `base.header`,
  which wraps era + block bytes as `[era, tag24(...)]`,
  BlockFetch's `base.block` is a bare `tag24(blockCbor)` with
  no era prefix. Cross-referenced against pallas's
  `BlockContent` codec. Our wire codec now matches;
  `BlockFetchDriver.FetchedBlock` threads era through from the
  matching ChainSync header at the applier layer rather than
  re-reading it from the wire.
- **Header hash needs raw bytes.** The header hash is over the
  serialised header CBOR bytes, not a re-encoded form. The
  `BlockHeader` decoder needs to capture the original input
  slice (via `KeepRaw[BlockHeader]` or a companion `def
  rawHeader: ByteString`) for `ChainApplier.toAppliedBlock`
  to compute the point. Check whether `scalus-core` already
  exposes this; add it if not.
- **Two-step `StartFrom.Tip` behaviour when the chain is
  producing blocks rapidly.** Between "learn tip" and "commit
  to tip", a new block may land on the peer. The peer accepts
  `MsgFindIntersect([oldTip])` because `oldTip` is still in
  its header store; the next `MsgRequestNext` returns a real
  forward for the block produced in the window. So no event
  is missed. Document the behaviour, test it (loopback tier
  2).
- **`ChainSyncSource.N2N` network magic.** The N2N connection
  needs the network magic at handshake. M5 threads it through
  from `config.cardanoInfo.networkMagic` — confirm this field
  exists on `CardanoInfo`. If not, add it. Relates directly
  to M4's "yaci's network magic is configurable" open
  question.
- **Single-block BlockFetch vs larger ranges.** Even a
  single-element range allocates a full `MsgStartBatch →
  MsgBlock → MsgBatchDone` cycle. For typical mainnet block
  cadence (~20s between blocks) the overhead is negligible.
  For cold-starting from a checkpoint hours in the past
  (M7) it's a problem. Note here, resolve in M7.
- **Graceful `MsgDone` on clean shutdown.** The
  transport's `Draining` state (M4) exists specifically
  for this — chain-sync sends `MsgDone` to the peer, the
  route transitions to Draining, the peer acknowledges with
  its own `MsgDone`. M5 should exercise this on
  `provider.close()` rather than racing the socket close.
- **Applier backpressure.** `engine.onRollForward` returns a
  `Future[Unit]` that completes when the worker processes
  the block. The applier awaits that Future before the next
  chain-sync pull — natural backpressure. Verify in test that
  a slow subscriber (mailbox at capacity) stalls the applier
  instead of dropping frames; critical regression guard.
- **BlockFetch failure modes we haven't considered.** If the
  peer rolls back on its end between our `MsgRollForward`
  receipt and our `MsgRequestRange`, we get `MsgNoBlocks`.
  Re-fetching the same point is wrong — the chain has moved.
  Correct recovery is "drop the header we were going to apply,
  continue with chain-sync's next event (which will be the
  rollback)." M5 models this as a typed
  `BlockFetchError.RollbackRaced`; applier drops the header
  and continues rather than escalating. Needs explicit test
  coverage.
- **Where does `StartFrom.At(point)` go in M5?** Answered
  above (engine-level: M7; connection-level: not yet
  exposed). Leaving the open question here because the UX
  is unclear: when an M7 user subscribes with `At(p)`, does
  the engine pull an additional ChainSync session just for
  that subscriber, or does it replay from a persistent
  block store? M7 decides; M5's structure must not make
  either path harder.
- **Connection reconnection / restart.** If
  `connection.rootToken` fires mid-sync (peer went away),
  M5 terminates the provider. There's no reconnect loop.
  Deferred to a dedicated "resilience" milestone; callers
  wanting auto-reconnect restart the provider themselves.
  Decision: document, don't wrap.
- **Tip handling during rollback.** `MsgRollBackward`
  includes both the rollback `point` and the peer's current
  `tip`. Right now the applier only passes `point` into
  `engine.onRollBackward`. The `tip` is useful for
  diagnostics (how far have we actually fallen behind?) but
  not currently persisted anywhere. Document that we drop
  it; revisit if M7's replay needs it.

## References

- `docs/local/claude/indexer/indexer-node.md` — milestone 5
  row. Links to this doc.
- `docs/local/claude/indexer/cardano-network-transport.md` — M4
  substrate; shares scope/cancellation patterns with this
  doc.
- `cardano-diffusion/protocols/cddl/specs/chain-sync.cddl`
- `cardano-diffusion/protocols/cddl/specs/block-fetch.cddl`
- `ouroboros-network/ouroboros-network-protocols/` —
  authoritative Haskell state machines and pipelining
  logic.
- pallas `src/miniprotocols/chainsync/` and
  `src/miniprotocols/blockfetch/` — cleanest non-Haskell
  reference.
