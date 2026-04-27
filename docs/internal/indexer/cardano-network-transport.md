# N2N transport foundation — design doc

**Status.** Design doc for milestone M4 of
`docs/local/claude/indexer/indexer-node.md`. Not yet implemented.
Covers the mini-protocol multiplexer, the N2N handshake, CBOR framing,
and KeepAlive — end-to-end "connect to a public relay and stay
connected." Chain-sync and other mini-protocols layer on top in
subsequent milestones; the transport must be designed to host them
without churn.

## Scope

M4 delivers:

1. A `Future`-based async byte-channel abstraction, with a JVM impl
   over `AsynchronousSocketChannel` (NIO2). JS (Node `net`) deferred.
2. An ouroboros-network multiplexer that frames and demultiplexes
   the wire into per-(protocol, direction) byte streams.
3. The handshake mini-protocol (ID 0), negotiating Node-to-Node v14
   or v16.
4. The keep-alive mini-protocol (ID 8), with a periodic heartbeat,
   RTT measurement, and timeout-driven connection close.
5. A `NodeToNodeClient` factory returning a live connection object
   that exposes (not-yet-consumed) byte-channel handles for the other
   mini-protocols so M5+ can plug in without reshaping the core.

M4 does **not** deliver:

- Chain-sync, block-fetch, tx-submission2, peer-sharing state
  machines — these are M5 / M8 / M9 / later.
- Any new `BlockchainStreamProvider` wiring. `ChainSyncSource.N2N(...)`
  remains unimplemented from the engine's point of view until M5
  consumes the chain-sync channel. The engine stays on `Synthetic`.
- N2C (local Unix socket). M8 reuses the multiplexer and framing
  verbatim; only the `AsyncByteChannel` impl and handshake version
  table change.
- JS cross-build of the transport. Types stay in `shared/`; the
  actual `AsyncByteChannel` impl for Node ships when there's a
  consumer.
- Virtual-thread / Loom-specific fast paths on JVM. The blocking-NIO
  path we land should profile well enough for M5.

## Why now

M2 landed the engine with `ChainSyncSource.Synthetic` as the only
live input. M3 wraps the emulator through the same provider shape.
Both validate that the engine interface is right. What's missing is
real chain data — for that we need to speak ouroboros-network
mini-protocols to a running node. M4 is the transport layer: the
thing that connects and stays connected. It produces no end-user
value on its own, but without it M5 has nowhere to land.

Placing the transport in its own module (separate from the engine
and the stream adapters) also keeps M8 honest: N2C reuses the
entire multiplexer, CBOR framing, and handshake state machine
unchanged; only a different `AsyncByteChannel` impl and a different
version table.

## Architecture at a glance

```
  Application
      │
      ▼
  Fs2BlockchainStreamProvider / OxBlockchainStreamProvider
      │          (M2 — already landed)
      ▼
  Engine + EngineDriver     ◄── AppliedBlock stream
      │          (M2 — already landed)
      ▼
  ChainSync state machine           ◄── M5 (not M4)
  BlockFetch state machine          ◄── M5 (not M4)
  TxSubmission2 state machine       ◄── M9 (not M4)
  ─────────────────────────────────────────────
  NodeToNodeClient       (M4 — this doc)
      │
      │  per-mini-protocol byte streams
      ▼
  Multiplexer            (M4)
      │  SDU frames
      ▼
  AsyncByteChannel       (M4 — trait in shared/, JVM impl on NIO2)
      │  raw bytes
      ▼
  OS socket
```

The boundary between M4 and M5 is exactly the line between
"multiplexer delivers bytes for protocol P" and "state machine for
protocol P consumes those bytes and emits application events."

## Module layout

New subproject: `scalus-embedded-node/scalus-cardano-network/` with the
cross-build shape used by the rest of embedded-node.

```
scalus-embedded-node/scalus-cardano-network/
    shared/src/main/scala/scalus/cardano/network/
        AsyncByteChannel.scala          // Future-based byte channel
        Multiplexer.scala               // frame codec + demux/mux
        MiniProtocolId.scala            // ADT over protocol numbers
        MiniProtocolBytes.scala         // per-protocol byte-stream handle
        CborMessageStream.scala         // typed msg stream over MiniProtocolBytes
        ProtocolScope.scala             // CancelSource + Timer bundle for a protocol
        RoutingOps.scala                // mux-facing interface handed to each protocol
        handshake/                      // handshake state machine + codecs
        keepalive/                      // keep-alive state machine + codecs
        NodeToNodeClient.scala          // public factory + connection object
    jvm/src/main/scala/scalus/cardano/network/jvm/
        JvmAsyncByteChannel.scala       // NIO2 AsynchronousSocketChannel
    shared/src/test/scala/...
    jvm/src/test/scala/...
```

sbt projects (respecting `feedback_module_names_match_dirs.md`):

| sbt val           | artifact name       | targets |
|-------------------|---------------------|---------|
| `scalusCardanoNetworkJVM`    | `scalus-cardano-network`        | JVM     |
| `scalusCardanoNetworkJS`     | `scalus-cardano-network`        | JS (later) |

Dependencies — **transport module is lean on purpose for M4**
(relaxed in M5 once chain-sync needs block decoding):

- `scalus-core` (for `ByteString`, `SlotNo`, CBOR primitives via
  `io.bullet.borer`).
- Nothing from `scalus-cardano-ledger` **in M4**; added in M5 for
  block / header decoding alongside the chain-sync state machine.
- Nothing from `scalus-streaming-core`. The engine consumes the
  transport, not the other way.
- No cats-effect, no ox, no fs2. Future-based end-to-end.
- Platform-specific deps: JVM brings only the JDK (NIO2 is stdlib);
  JS (future) brings scala-js-dom or direct facades to Node's `net`.

`scalus-streaming-core` gains an optional dependency on `scalus-cardano-network`
once M5 lands and `ChainSyncSource.N2N` becomes real. M4 does not
touch streaming-core.

## Cancellation

`Future` has no first-class cancellation, and for a protocol stack
that's actually a feature: it forces us to carry cancellation on an
explicit side-channel, which in turn lets us *separate capability
from observation*. Anyone given a Future can wait for it — but only
the holder of the cancel *source* can abort the underlying
operation. Consumers of a sub-channel observe cancellation (their
reads fail with `CancelledException`); they cannot escalate to
socket close. This is the standard `CancellationToken` /
`context.Context` / ox-scope pattern.

```scala
/** Observable side of a cancellation. Consumers receive one of
  * these and can inspect it or register cleanup actions; they
  * cannot trigger cancellation themselves.
  */
trait CancelToken {
    def isCancelled: Boolean
    /** Register a callback. Fires exactly once — immediately if
      * already cancelled, else on the first `cancel()`.
      */
    def onCancel(action: () => Unit): Unit
}

/** Controlling side. Only the owner of the source has the capability
  * to cancel. `token` is what gets handed to consumers.
  */
trait CancelSource {
    def token: CancelToken
    def cancel(): Unit
}

object CancelSource {
    def apply(): CancelSource
    /** Linked source: cancelling the parent cancels this one too.
      * Cancelling this one does NOT cancel the parent.
      */
    def linkedTo(parent: CancelToken): CancelSource
}

object CancelToken {
    val never: CancelToken   // useful for unit tests and non-cancellable call sites
}

final class CancelledException(reason: String) extends RuntimeException(reason)
```

### Scope hierarchy

A live `NodeToNodeConnection` owns a three-level scope tree:

```
  connectionRoot : CancelSource          ← owned by NodeToNodeClient
        │
        ├── handshake : linkedTo(connectionRoot.token)     ← short-lived
        │
        ├── keepAlive : linkedTo(connectionRoot.token)     ← lifetime of connection
        │
        └── <per mini-protocol> : linkedTo(connectionRoot.token)
              │
              └── <per-call> : linkedTo(mini-protocol.token)   ← optional
```

- **Connection root** cancels on any of: explicit `close()`, socket
  error observed by the reader loop, handshake refusal, keep-alive
  timeout, mailbox overflow. One event — "this connection is gone"
  — regardless of cause, so subscribers see a uniform failure.
- **Mini-protocol scope** cancels on protocol-local `close()` or
  when the root cancels. Holder of `MiniProtocolBytes` has the
  *token* (observe) but not the *source* (trigger); that stays
  with the mux.
- **Per-call scope** is the optional narrow token a state machine
  passes into an individual `receive()` / `send()` for request-level
  timeout (M5's chain-sync `FindIntersect` round-trip, M9's tx
  submission). M4 itself doesn't use this layer.

### Where cancellation has teeth, and where it doesn't

- **Read not yet issued.** Token cancel completes the pending Future
  with `CancelledException` before the OS is ever called.
- **Read in flight at the NIO layer.** The OS syscall cannot be
  interrupted, but the completion handler checks the token and
  fails the Future; the bytes land in the refill buffer and are
  discarded on the next real read. Cost: at most one `refillSize`
  of wasted I/O per cancelled read. Bounded, small.
- **Write not yet pulled from the send queue.** Clean cancel.
- **Write already handed to the OS.** Cancellation is ignored for
  the rest of that SDU — a half-sent frame would corrupt the
  demux on the peer. The Future may complete successfully even
  though the token fired mid-flight. Documented as: cancellation
  is best-effort for writes already dequeued. State machines that
  need strict abort semantics must avoid re-using the connection
  after the owning scope cancels.

All platform-specific I/O is funneled through a single trait. The
multiplexer and everything above it is pure platform-neutral code in
`shared/`. The surface speaks immutable `ByteString` — no
`ByteBuffer` leakage into `shared/`, no ownership puzzle for
callers. The implementation is free to hold a reusable internal
buffer and refill it from the OS without the caller seeing.

```scala
/** Platform-neutral async byte channel.
  *
  * `readExactly(n, cancel)` completes with exactly `n` bytes, or
  * `None` if EOF arrives before `n` bytes have been received.
  * Shaping the primitive this way matches what the mux actually
  * wants (read an 8-byte header, then read `length` bytes) and
  * lets the impl own a single reusable internal buffer — we do
  * not materialize a fresh `ByteString` on every OS-level chunk.
  *
  * The `cancel` token fires `CancelledException` on the pending
  * Future when triggered — before the syscall if we haven't
  * reached the OS yet, or on completion-handler entry if we have.
  * See the *Cancellation* section for the exact contract.
  *
  * Back-pressure is the completion of the returned Future: a slow
  * consumer that never calls `readExactly` again cannot be forced
  * to buffer beyond the impl's internal refill size.
  *
  * `write` completes when the bytes have been handed to the OS.
  * Cancellation is best-effort for writes already mid-flight (see
  * *Cancellation* section).
  *
  * `close` is idempotent and completes pending reads/writes with
  * `ChannelClosedException`. It does NOT substitute for
  * cancellation — it is the transport owner's "tear everything
  * down" escape hatch.
  *
  * Concurrency contract: at most one `readExactly` and at most one
  * `write` may be in flight at a time. The mux enforces this by
  * construction (single reader loop, single send queue).
  */
trait AsyncByteChannel {
    def readExactly(n: Int, cancel: CancelToken): Future[Option[ByteString]]
    def write(bytes: ByteString, cancel: CancelToken): Future[Unit]
    def close(): Future[Unit]
}

object AsyncByteChannel {
    final class ChannelClosedException(message: String) extends RuntimeException(message)
    final class UnexpectedEofException(wanted: Int, got: Int)
        extends RuntimeException(s"EOF after $got of $wanted bytes")
}
```

### JVM implementation sketch

`JvmAsyncByteChannel` wraps `AsynchronousSocketChannel` and holds a
single reusable `ByteBuffer` as its internal refill buffer. The
`readExactly` loop issues as many underlying reads as needed to fill
`n` bytes, then slices out a `ByteString`:

```scala
private val refill = ByteBuffer.allocateDirect(refillSize) // reused across reads
// invariant: on entry/exit, `refill` is in *read mode* — position..limit = buffered bytes

def readExactly(n: Int, cancel: CancelToken): Future[Option[ByteString]] = {
    if cancel.isCancelled then return Future.failed(CancelledException("pre-read"))
    val out = new Array[Byte](n)
    def loop(filled: Int): Future[Option[ByteString]] =
        if cancel.isCancelled then
            Future.failed(CancelledException(s"after $filled/$n bytes"))
        else if filled == n then
            Future.successful(Some(ByteString.unsafeFromArray(out)))
        else if refill.hasRemaining then {
            val take = math.min(n - filled, refill.remaining())
            refill.get(out, filled, take)
            loop(filled + take)
        } else refillFromSocket(cancel).flatMap {
            case 0 if filled == 0 => Future.successful(None)           // clean EOF
            case 0                => Future.failed(UnexpectedEofException(n, filled))
            case _                => loop(filled)
        }
    loop(0)
}

private def refillFromSocket(cancel: CancelToken): Future[Int] = {
    val p = Promise[Int]()
    refill.clear()
    ch.read(refill, null, new CompletionHandler[Integer, Null] {
        def completed(r: Integer, a: Null): Unit = {
            refill.flip()
            // Token may have fired while the syscall was in flight;
            // discard the data and fail the Future rather than
            // propagate bytes the caller is no longer interested in.
            if cancel.isCancelled then {
                refill.position(refill.limit()) // drop buffered bytes
                p.failure(CancelledException("mid-syscall"))
            } else {
                p.success(if r < 0 then 0 else r.intValue)
            }
        }
        def failed(t: Throwable, a: Null): Unit = p.failure(t)
    })
    p.future
}
```

No locks, no threads created by us; the OS's async I/O thread-pool
drives the `CompletionHandler`. The single-reader contract makes the
`refill` buffer's mutable state safe without synchronization.

Direct (off-heap) buffer for `refill` lets NIO skip pinning heap
memory during the syscall. This is the one place where the
`ByteBuffer`-flavoured win actually lands — and we get it without
exposing `ByteBuffer` in the API.

### JS implementation sketch (future)

`NodeAsyncByteChannel` wraps `net.Socket`. Data arrives via
`on('data', buf)` as a Node `Buffer`; we accumulate into an internal
byte-array ring buffer and complete pending `readExactly` promises
as enough bytes arrive. Write uses `socket.write(buf, cb)`; the
callback completes the write future.

Not landing in M4 — the types live in `shared/`, the impl waits
until there's a JS user. Same trait shape; only the refill
primitive differs.

## Multiplexer

### Wire frame (SDU)

8-byte header + payload, all big-endian:

```
offset  size  field
0       4     timestamp (u32, microseconds, lower 32 bits of monotonic clock)
4       2     protocol-id + direction-bit (high bit = direction;
                                            0 = initiator, 1 = responder;
                                            low 15 bits = protocol number)
6       2     payload length (u16, bytes)
8       N     payload (0..12288 bytes)
```

This matches ouroboros-network's `network-mux/src/Network/Mux/Types.hs`
`SDUHeader` encoding verbatim. Max SDU payload is 12288 bytes
(ouroboros-network default `sduTrailingEdge`); larger mini-protocol
messages are split across multiple SDUs transparently by the mux.

### Protocol IDs

```scala
enum MiniProtocolId(val wire: Int):
    case Handshake     extends MiniProtocolId(0)
    case ChainSync     extends MiniProtocolId(2)
    case BlockFetch    extends MiniProtocolId(3)
    case TxSubmission  extends MiniProtocolId(4)
    case KeepAlive     extends MiniProtocolId(8)
    case PeerSharing   extends MiniProtocolId(10)
```

M4 implements only Handshake and KeepAlive end-to-end. The others
exist as enum variants so the mux can route frames for them into
their byte-stream mailboxes even though the state machines aren't
written yet — useful for debugging ("yes we're receiving chain-sync
frames; we just don't decode them") and for M5 integration.

### Demux (inbound)

Single reader loop, one per connection. Scoped to the connection
root token so `connectionRoot.cancel()` aborts the loop promptly;
falls out of the `readExactly` primitive as a two-call chain, no
accumulator in the mux:

```scala
def step(): Future[Unit] =
    channel.readExactly(8, connectionRoot.token).flatMap {
        case None         => fail(CleanEof); Future.unit
        case Some(header) =>
            val (protoNum, dir, length) = Sdu.parseHeader(header)
            channel.readExactly(length, connectionRoot.token).flatMap {
                case None          => fail(UnexpectedEof); Future.unit
                case Some(payload) =>
                    mailbox(protoNum, dir).offer(payload)
                    step()
            }
    }
// `fail(reason)` triggers `connectionRoot.cancel()` (fans out through
// every linked scope) and then closes the underlying AsyncByteChannel.
```

`mailbox(p, d)` is a `DeltaMailbox[ByteString]` keyed by
`(MiniProtocolId, Direction)`. Reusing the `DeltaMailbox` from M2 —
same backpressure semantics (`Bounded(n)` overflow ⇒ trigger the
root cancel with a typed exception; we do not silently drop frames).

Reader-loop fatal errors (EOF mid-frame, framing error, mailbox
overflow) collapse to one code path: fire the connection root.
Linked scopes fan out; every pending `receive` / `send` completes
with `CancelledException` (or the specific cause stored on the
connection). The socket is closed last, after consumers have
observed the cancel.

### Mux (outbound)

Sends are serialized through a single `Promise`-chained queue on
the mux, not per-sub-channel. Rationale: interleaving SDUs from
multiple protocols is fine on the wire, but the OS-level write
itself must be atomic per frame or the demux on the peer gets
confused. A single queue gives us that for free without per-frame
locking.

```scala
private val sendQueue = new LinkedBlockingQueue[() => Future[Unit]]()
// one dedicated single-Future chain drains it, FIFO
```

Timestamp field is filled in at send-time from a monotonic clock
(`System.nanoTime / 1000` on JVM, `performance.now() * 1000` on JS),
matching ouroboros-network's `RemoteClockModel`. Only the low 32
bits are used; the peer uses it for RTT and for its own diagnostics,
we ignore the field on the inbound path.

### CBOR framing across SDUs

Each mini-protocol sees a byte *stream*, not a message stream. SDU
boundaries are a wire-level concern and do not align with CBOR
message boundaries: a single chain-sync `MsgRollForward` can easily
exceed 12 KB and span multiple SDUs, while a short handshake reply
fits in part of one SDU.

The sub-channel handle therefore exposes byte-stream semantics.
Each `MiniProtocolBytes` carries its own protocol-level
`CancelToken` (`scope`) linked to the connection root; callers use
it by default, or pass a narrower linked token for per-call
cancellation (request-level timeout, explicit user abort). Holders
of the handle do not get the *source* — only the mux can trigger
the protocol-level cancel.

```scala
trait MiniProtocolBytes {
    /** Protocol-level scope. Cancelled when: the mux tears this
      * protocol down, the connection root cancels, or the state
      * machine owning the protocol scope triggers its source.
      */
    def scope: CancelToken

    /** Next chunk of bytes received on this mini-protocol. None = peer
      * sent `MsgDone` or the connection closed. Chunks follow SDU
      * boundaries but callers MUST NOT depend on that.
      */
    def receive(cancel: CancelToken = scope): Future[Option[ByteString]]

    /** Send one CBOR-encoded message. The mux splits into SDUs as
      * needed.
      */
    def send(message: ByteString, cancel: CancelToken = scope): Future[Unit]
}
```

Note the handle has no `close()` — closing a protocol is a
capability on its owning `CancelSource`, not on the observable
handle. Connection-level tear-down is `NodeToNodeConnection.close()`.

Each state machine layers an incremental CBOR decoder on top.
`io.bullet.borer` supports incremental parsing via its `Reader`
API — we maintain an accumulator buffer, attempt `decode` on every
new chunk, and commit/advance on success. Handshake and keep-alive
messages are trivially small (< 100 bytes) so in M4 the naive
"accumulate until decode succeeds" path is sufficient; the same
pattern scales to chain-sync in M5 without a design change.

## Handshake (mini-protocol 0)

### Wire (v14 CDDL shape, from `handshake-node-to-node-v14.cddl`)

```
handshakeMessage     = msgProposeVersions / msgAcceptVersion / msgRefuse / msgQueryReply
msgProposeVersions   = [0, {* versionNumber => versionData}]
msgAcceptVersion     = [1, versionNumber, versionData]
msgRefuse            = [2, refuseReason]
msgQueryReply        = [3, {* versionNumber => versionData}]

; v14 versionData
versionDataV14       = [networkMagic, initiatorOnlyDiffusionMode, peerSharing, query]

; v16 versionData (extends v14 with perasSupport)
versionDataV16       = [networkMagic, initiatorOnlyDiffusionMode, peerSharing, query, perasSupport]

refuseReason         = versionMismatch / handshakeDecodeError / refused
```

### State machine

```
    ┌─────────┐ send MsgProposeVersions ┌───────────┐
    │ Propose │ ──────────────────────► │ AwaitReply│
    └─────────┘                          └────┬──────┘
                                              │
                  ┌──────────────┬────────────┴────────────┬──────────────┐
                  │              │                         │              │
          MsgAcceptVersion  MsgRefuse                MsgQueryReply    other/decode-err
                  │              │                         │              │
                  ▼              ▼                         ▼              ▼
              ┌──────┐    HandshakeRefused          ┌────────────┐   HandshakeDecodeError
              │ Done │                              │ QueryDone  │
              └──────┘                              └────────────┘
```

The client always sends `MsgProposeVersions` first. `MsgQueryReply`
is the "query bit" path where the peer only advertises its supported
versions without committing to one — we propose with `query = false`
so we won't see it, but we handle it defensively.

### Version table

For M4 we propose the latest two stable versions:

| Version | Fields | Why include |
|---------|--------|-------------|
| v14 | magic, initiatorOnly, peerSharing, query | Broadly deployed; minimum supported by current mainnet relays |
| v16 | v14 fields + perasSupport | Latest stable; Peras support (era signatures) |

No v15 — it was a transient version between v14 and v16.

We deliberately do not propose v11/v12/v13 (obsolete per
`cardano-diffusion/protocols/cddl/specs/obsolete/`). If a peer only
understands an obsolete version, handshake fails with
`HandshakeRefused(VersionMismatch)`; that's correct behavior.

### Version-data values for M4

| Field | Value | Rationale |
|-------|-------|-----------|
| `networkMagic` | `cardanoInfo.networkMagic` | Mainnet 764824073, Preview 2, Preprod 1. |
| `initiatorOnlyDiffusionMode` | `true` | We are a client, not a relay — we do not want the peer opening its initiator-side mini-protocols against us. |
| `peerSharing` | `0` (disabled) | Peer sharing is not on our roadmap yet. |
| `query` | `false` | We commit to a version, we do not browse. |
| `perasSupport` (v16 only) | `false` | Peras era-signature support is a separate work item. |

### Timeouts and errors

- **Handshake timeout**: 30 seconds from socket connect to handshake
  completion. Implemented via a `handshakeScope = CancelSource.linkedTo(connectionRoot.token)`
  and `timer.schedule(30.seconds)(handshakeScope.cancel())`. The
  handshake driver's reads observe `handshakeScope.token`; cancel
  fires `CancelledException`, which the driver translates to
  `HandshakeTimeoutException` before completing the handshake
  future. Crucially, the handshake timeout does *not* take down
  the connection root — if we wanted to recover and retry a
  handshake (we don't, but structurally we could), the rest of
  the scope tree is unaffected.
- **Handshake refusal** (peer sends `MsgRefuse`) does fire the
  connection root — we have no route to recovery without renegotiating
  with a different peer, and the socket is useless.
- **Typed errors** (all extending `sealed trait HandshakeError`):
  - `VersionMismatch(proposed: Set[Int], accepted: Option[Int])`
  - `DecodeError(payload: ByteString, cause: Throwable)`
  - `Refused(text: String)`
  - `HandshakeTimeoutException`

## KeepAlive (mini-protocol 8)

### Wire (CDDL)

```
keepAliveMessage = msgKeepAlive / msgKeepAliveResponse / msgDone
msgKeepAlive         = [0, word16]   ; cookie
msgKeepAliveResponse = [1, word16]   ; echo
msgDone              = [2]
```

### Behaviour

A heartbeat loop runs for the lifetime of the connection:

1. On handshake completion, the keep-alive driver starts in state
   `Idle` with `keepAliveScope = CancelSource.linkedTo(connectionRoot.token)`.
2. Every `keepAliveInterval` (default 30s), send
   `MsgKeepAlive(nextCookie)` and enter state `AwaitingResponse(cookie, sentAt)`.
   Arm a per-beat timeout: `beatScope = CancelSource.linkedTo(keepAliveScope.token)`
   plus `timer.schedule(keepAliveTimeout)(beatScope.cancel())`. The
   pending `receive(beatScope.token)` on the keep-alive protocol
   channel will fail with `CancelledException` if the response
   doesn't arrive in time.
3. On `MsgKeepAliveResponse(echo)`:
   - If `echo == cookie`: RTT = `now - sentAt`, publish to the
     connection's `AtomicReference[Option[Duration]]`, return to
     `Idle`, schedule next beat.
   - Else: trigger `connectionRoot.cancel()` with
     `KeepAliveCookieMismatch` — wire-protocol violation, the
     peer is misbehaving.
4. If the per-beat timeout fires, the driver sees `CancelledException`,
   translates it to `KeepAliveTimeoutException`, and triggers
   `connectionRoot.cancel()` with that cause. The per-beat scope
   isolates *which* beat timed out; connection-root cancel is the
   right response because the peer is unresponsive.
5. On clean shutdown (`connectionRoot.token.onCancel(...)` fires),
   send `MsgDone` best-effort, no response expected.

Cookies are a simple 16-bit counter — not a nonce, no anti-replay
requirement because the mini-protocol is ordered and one-shot per
beat.

The RTT estimate is exposed on the `NodeToNodeConnection` object
for diagnostics; no application consumes it yet but it's useful in
M5+ when we decide how aggressively to pipeline chain-sync.

## Protocol wind-down — graceful vs. unrecoverable

A protocol scope can fire for two qualitatively different reasons,
and the mux's response differs:

- **Graceful wind-down.** Per-call timeout, user unsubscribed, the
  state machine chose to stop. The protocol itself is still
  semantically healthy — we've lost interest, but the peer hasn't.
  The peer will keep sending replies until it sees our
  `MsgDone`.
- **Unrecoverable failure.** CBOR decode error, state-machine
  invariant violated, peer sent a message that doesn't fit any
  state transition. We have lost our model of the conversation.
  Anything we send next is wire-level undefined behavior.

### Per-protocol routing states

Each `(MiniProtocolId, Direction)` routing slot has three states:

- **`Live`** — normal. Inbound frames land in the mailbox;
  subscriber pulls them.
- **`Draining`** — we've sent `MsgDone`, peer hasn't yet. Inbound
  frames are still pulled off the wire (otherwise the demux
  stalls), but they're discarded with a debug counter. Not
  charged against mailbox overflow.
- **`Closed`** — peer's `MsgDone` observed (or we decided the
  protocol is unrecoverable and triggered root). Mailbox is
  decommissioned; future frames for this route are a wire-level
  error (`UnexpectedFrameException` → root cancel) because a
  well-behaved peer would not send after `MsgDone`.

### Graceful wind-down flow

1. State machine catches its scope cancel (per-call timeout,
   user abort, etc.).
2. Drive the state machine to a state with agency, then send
   `MsgDone`.
3. Mux transitions the outbound side of the route to `Draining`.
4. Peer's `MsgDone` eventually arrives; the state machine's
   decoder sees it and signals the mux to transition the route
   to `Closed`. Mailbox is freed.
5. The rest of the connection continues untouched.

### Unrecoverable-failure flow

1. State machine sees a decode error, invariant violation, or
   wire-level surprise.
2. Trigger `connectionRoot.cancel()` with the typed cause. No
   attempt to send `MsgDone` — we can't be sure the peer will
   accept it in our current state.
3. Mux fans out, closes the socket.

### What M4 actually needs

Both M4 protocols are degenerate:

- **Handshake** is one-shot and terminates naturally; there is no
  "ongoing stream" window in which wind-down matters.
- **KeepAlive** has no "no longer interested" state — it runs for
  the lifetime of the connection. Every failure is
  connection-fatal (root cancel). KeepAlive's `MsgDone` is sent
  from the `connectionRoot.onCancel` handler on clean shutdown,
  best-effort; no `Draining` state — we're about to close the
  socket anyway.

So the `Draining` state exists in the routing-state enum from M4
onward (so M5's chain-sync can use it without API churn), but no
M4 code path ever drives a route into it. First real use is M5
chain-sync unsubscribe.

### Implementation notes for the mailbox

On scope cancel that intends graceful wind-down, the state
machine calls a mux method `mux.beginDraining(proto, dir)`. That
method:

- Marks the route `Draining`.
- Fails any pending `receive()` that was still suspended on the
  mailbox with a typed `ProtocolClosingException` (so the state
  machine's own cleanup loop exits).
- Returns a Future that completes when the peer's `MsgDone` has
  been observed *or* the connection root fires, whichever comes
  first.

The state machine's own `receive`-and-decode path is responsible
for recognizing `MsgDone` on the wire and calling
`mux.finishClose(proto, dir)` after. The mux does not parse
protocol-level messages on its own — that's intentional, keeps the
CBOR-decoder dependency out of the transport module.

## Protocol runner — considered, deferred

A natural impulse is to mirror ouroboros-network's `Peer`/`runPeer`
shape: a `MiniProtocolDriver[S, M, R]` type describing
state/agency/encode/decode/transition, plus a single generic
`ProtocolRunner.run(driver, channel, scope, client)` that handles
every mini-protocol uniformly. One runner, many drivers.

We are **not** doing this for M4. Reasons:

- **Shapes diverge.** ChainSync and TxSubmission2 are pipelined
  (client issues multiple requests before waiting); TxSubmission2
  is additionally server-led pull. A single agency/state-transition
  model either grows pipeline-aware type parameters until it's
  noise, or lowers everything to raw channel ops and loses the
  safety the framework was supposed to give.
- **Type encoding in Scala is heavier than in Haskell.** Haskell's
  `Peer` leans on GADTs and rank-N types. The Scala 3 equivalent
  is match-types + existentials (readable by few) or a
  sealed-ADT-per-protocol (hand-rolling what Haskell does for
  free). Either way the framework's *implementation* dominates
  its *use*.
- **M4 doesn't exercise it.** Handshake is one-shot; KeepAlive is
  a forever-loop with no agency question. Neither protocol needs
  agency tracking, draining, or pipelining. Designing a framework
  around them is designing for M5+ based on reading specs, not
  writing code.

What we do extract are three small utilities that genuinely repeat
across every mini-protocol:

- **`CborMessageStream[M]`** — wraps `MiniProtocolBytes` with an
  incremental CBOR decoder over borer's `Reader` API (accumulate
  chunks, attempt decode, commit on success) plus a symmetric
  encoder. Exposes `next(cancel): Future[Option[M]]` and
  `send(m, cancel): Future[Unit]`. ~50 lines. Every state
  machine uses it.
- **`ProtocolScope`** — bundles `CancelSource` + `Timer` +
  ergonomic helpers (`linkedTo(parent)`, `schedule(d)(cancel)`,
  `onClose(action)`). ~30 lines. Factored so per-protocol code
  reads as intent, not cancellation plumbing.
- **`RoutingOps`** — the mux-facing interface handed to each
  protocol at construction time: `beginDraining(p, d)`,
  `finishClose(p, d)`, `escalateRoot(cause)`. ~20 lines. Keeps
  state machines from poking mux internals and lets us mock the
  mux in unit tests.

With these in place, each mini-protocol stays a bespoke state
machine file with its own state ADT and transition logic, but
without boilerplate around CBOR incremental decode, cancel
sources, or mux-route bookkeeping. Estimated size for M4:
handshake ~180 lines, keep-alive ~120 lines, including tests
inline — total smaller than a framework would be.

**When to revisit.** After M5 lands ChainSync we will have three
protocols. If `ProtocolRunner.run` can be extracted such that
each driver is genuinely just a state-transition table and the
pipelining cases are naturally expressed, do it then. If we
find ourselves adding `PipelineDepth` type parameters or
`ServerLedVariant` flags, the framework isn't the right shape —
stay with bespoke state machines and keep the utilities.

## Error model and lifetime

All errors collapse to a single mechanism: the appropriate
`CancelSource` fires. The *scope* of the source determines the
blast radius; the *cause* is stored on the source (or the
connection) so consumers see a typed exception instead of a generic
`CancelledException`.

| Failure mode | Scope fired | Cause stored | Observers see |
|---|---|---|---|
| Socket error (read/write) | connectionRoot | `ChannelClosedException` | `CancelledException` chained to cause, on every pending op |
| Frame-decode error | connectionRoot | `FrameDecodeException` | same |
| Mailbox overflow | connectionRoot | `ScalusBufferOverflowException` | same |
| Handshake refused by peer | connectionRoot | `HandshakeError.Refused(...)` | handshake future fails with typed error; everything else sees cancel |
| Handshake timeout | handshakeScope only | `HandshakeTimeoutException` | handshake future fails; connection root stays live (unused in M4) |
| Keep-alive cookie mismatch | connectionRoot | `KeepAliveCookieMismatch` | cancel everywhere |
| Keep-alive beat timeout | connectionRoot | `KeepAliveTimeoutException` | cancel everywhere |
| Explicit `NodeToNodeConnection.close()` | connectionRoot | `ChannelClosedException("user close")` | cancel everywhere, then socket close |
| Graceful protocol wind-down (M5+, state machine sent `MsgDone`) | that protocol's scope | `ProtocolClosingException` | only that protocol's pending ops; route goes `Live → Draining → Closed` |
| Unrecoverable protocol failure (M5+, decode error / invariant violation) | connectionRoot | protocol-specific (e.g. `ChainSyncDecodeException`) | cancel everywhere — we've lost state sync with the peer |
| Peer sends frame for a `Closed` route | connectionRoot | `UnexpectedFrameException` | cancel everywhere; peer is misbehaving |

The socket is closed *after* the root cancel fan-out has fired, so
consumers observe the cancel (and can run their `onCancel`
cleanup) before the raw transport disappears.

`NodeToNodeConnection.closed: Future[Unit]` completes once the
connection-root cancel has fully propagated and the socket is
closed. Normal completion on clean shutdown; failure with the
stored cause otherwise.

### Timers

Timeouts are implemented by scheduling `source.cancel()`, not
`channel.close()`. A small `Timer` trait in `shared/` abstracts the
platform timer:

```scala
trait Timer {
    def schedule(delay: FiniteDuration)(action: => Unit): Cancellable
}
trait Cancellable { def cancel(): Unit }
```

JVM backs with `Executors.newScheduledThreadPool(1)`; JS with
`js.timers.setTimeout`. No external deps. Typical call site:

```scala
val beatScope = CancelSource.linkedTo(keepAliveScope.token)
val handle    = timer.schedule(keepAliveTimeout)(beatScope.cancel())
// on successful response:
handle.cancel()   // don't fire the timeout-cancel if we're done
```

## The public surface of M4

```scala
object NodeToNodeClient {

    /** Connect to a public relay, perform the handshake, start the
      * keep-alive loop. Future completes when the connection is
      * fully established (post-handshake), or fails if any step
      * before that fails.
      */
    def connect(
        host: String,
        port: Int,
        networkMagic: NetworkMagic,
        config: ClientConfig = ClientConfig.default
    )(using ExecutionContext): Future[NodeToNodeConnection]
}

trait NodeToNodeConnection {
    def negotiatedVersion: NodeToNodeVersion

    /** Per-mini-protocol byte-stream handles. M4 users consume
      * keep-alive internally; the others are handed off to M5+.
      */
    def channel(p: MiniProtocolId): MiniProtocolBytes

    /** Observable root token — cancelled on any connection-fatal
      * event (peer EOF, socket error, keep-alive timeout, explicit
      * close). Consumers registering `onCancel` see a single event
      * regardless of cause; the cause is available via [[closed]].
      */
    def rootToken: CancelToken

    /** Observable RTT from the keep-alive loop. `None` until the
      * first keep-alive round trip completes.
      */
    def rtt: Option[FiniteDuration]

    /** Completes when the connection closes, normally or on fault.
      * Fault paths complete with the stored cause (see *Error
      * model and lifetime*), not a generic `CancelledException`.
      */
    def closed: Future[Unit]

    /** Tear-down capability — only the client owner holds this
      * reference. Equivalent to firing the internal root
      * `CancelSource` with `ChannelClosedException("user close")`.
      */
    def close(): Future[Unit]
}

case class ClientConfig(
    handshakeTimeout: FiniteDuration = 30.seconds,
    keepAliveInterval: FiniteDuration = 30.seconds,
    keepAliveTimeout: FiniteDuration = 60.seconds,
    sduMaxPayload: Int = 12288,
    refillSize: Int = 64 * 1024,
    mailboxCapacity: DeltaBufferPolicy = DeltaBufferPolicy.Bounded(256)
)
```

This is the exact surface M5's chain-sync state machine builds on:
it asks for `connection.channel(MiniProtocolId.ChainSync)` and
drives the state machine from the returned byte stream.

## Test strategy

Five tiers, roughly in order of cheapness. The whole point of
making `AsyncByteChannel` the only platform-specific piece is that
everything above it is testable without real sockets.

### 1. Unit tests — shared, JVM + JS

All pure logic, no I/O:

- **Frame codec round-trip**: random `(protoNum, dir, timestamp, payload)`
  4-tuples (ScalaCheck), encode → decode → assert equal. Include
  payloads of length 0, 1, 12287, 12288.
- **Frame golden vectors**: fixed byte sequences captured from
  `ouroboros-network` test traces. Assert decode yields the
  expected tuple.
- **Handshake CBOR codecs**: golden encodings for
  `MsgProposeVersions`, `MsgAcceptVersion`, `MsgRefuse(*)`, each
  for v14 and v16. Vectors pulled from the CDDL examples in
  `cardano-diffusion/protocols/cddl/specs/handshake-node-to-node-v14.cddl`.
- **Handshake state machine**: table-driven. Inputs: sequences of
  incoming messages. Assert state transitions and terminal
  `Future[Either[HandshakeError, NegotiatedVersion]]` values.
  One row per CDDL message variant, plus malformed-bytes rows.
- **KeepAlive state machine**: same shape. Rows for
  `happy path`, `cookie mismatch`, `no response → timeout`,
  `MsgDone mid-flight`. Timeout row drives a fake `Timer` so
  test wall-clock stays fast and asserts that cancel fan-out
  reaches the connection root.
- **Cancellation fan-out**: wire a `CancelSource` tree by hand,
  assert that cancelling the parent triggers every linked child
  exactly once; cancelling a child leaves siblings and parent
  untouched; `onCancel` handlers registered after cancel fire
  synchronously.
- **CBOR incremental decode**: feed a serialized handshake message
  split into N chunks for N = 1..payload.size; assert decode
  completes exactly once, at the chunk that straddles the last
  byte.

### 2. Loopback tests — shared, no real socket

A test-only `PipeAsyncByteChannel` where `write` on endpoint A
appends to the read buffer of endpoint B and vice versa. Exposes:

- **Full handshake + keep-alive interop between two of our own
  muxes.** Stand up both endpoints, let them handshake, let
  keep-alive run for 5 simulated seconds of test time, close
  cleanly. Asserts frame ordering, timestamp population, cookie
  matching.
- **Large-message splitting.** Send a 100 KB mini-protocol message
  through the mux; assert it's split into 9 SDUs of ≤ 12288 bytes
  and reassembled on the other side.
- **Close propagation.** Call `close()` on endpoint A mid-flight;
  assert endpoint B's pending `receive` on every mini-protocol
  fails with `CancelledException` chained to
  `ChannelClosedException`, and every `onCancel` listener on the
  root token fires exactly once.
- **Scope isolation.** Cancel a mini-protocol's scope and assert
  only that protocol's pending ops fail — root and other
  protocols stay live.
- **Per-call cancellation.** Submit a `receive(narrowToken)`;
  cancel `narrowToken`; assert the Future fails, the protocol
  scope is unaffected, the next `receive()` on the same protocol
  succeeds normally.
- **Graceful wind-down (`Draining`).** Use a dummy two-protocol
  setup (the second protocol's state machine is a trivial
  echo-and-done). On endpoint A, call `mux.beginDraining(p, dir)`;
  have endpoint B continue to send N frames for `p` *before*
  finally sending its own `MsgDone`. Assert: (a) endpoint A pulls
  all N frames off the wire (demux doesn't stall), (b) the drained
  frames never reach any mailbox, (c) endpoint A's `beginDraining`
  Future completes when B's `MsgDone` arrives, (d) the route then
  becomes `Closed`, (e) the *other* protocol on the same mux
  kept delivering frames normally throughout. Critical regression
  guard: a drained protocol must not backpressure the connection.
- **Frame for a closed route.** After a protocol closes, have a
  test peer send one more frame for that route; assert connection
  root fires with `UnexpectedFrameException`.
- **Mailbox overflow.** Fill a mini-protocol mailbox past capacity;
  assert the connection root cancels with the typed overflow
  exception (not a silent drop).

### 3. Mock-responder tests — JVM, non-network

A `StubN2NResponder` implementing just enough of the N2N wire to
negotiate a handshake and echo keep-alive messages. Listens on
`127.0.0.1:0` (ephemeral port). Fast (<1s per run), in the default
test suite.

- **Real-socket handshake**: connect through `JvmAsyncByteChannel`,
  complete handshake, verify negotiated version.
- **Keep-alive cadence**: connect, wait for ~3 beats, verify RTT
  is populated and in a sensible range (< 100 ms on loopback).
- **Unexpected close from peer**: stub drops the socket mid-frame;
  assert we get `ChannelClosedException`, not a hang.
- **Malformed framing from peer**: stub sends `length = 65535` and
  then fewer bytes; assert `FrameDecodeException` and clean close.

### 4. Yaci-DevKit integration tests — JVM, `sbtn it` only

Primary integration target. Matches the existing
`scalusCardanoLedgerIt` pattern: separate sbt project
(`scalusCardanoNetworkIt`, directory `scalus-embedded-node/scalus-cardano-network-it/`),
Docker-backed via testcontainers, runs in `sbtn it` but not in
`sbtn ci`.

Yaci-DevKit (`bloxbean/yaci-cli` image, already a dep of
`scalusCardanoLedgerIt`) spins up a real `cardano-node` in
devnet mode and exposes its N2N TCP port. That's exactly the
kind of peer we need to exercise handshake + keep-alive
end-to-end against the real wire — with the advantage over a
public relay that it's deterministic, offline-capable,
contained, and its network magic is known and stable.

- **Handshake against yaci-devkit**: connect to the container's
  exposed N2N port, assert negotiated version is `v14` or `v16`,
  assert `networkMagic` round-trip (devnet magic = 42 by
  default; whatever yaci reports via its config).
- **Stay-alive**: connect, hold the connection open across
  `≥ 3 × keepAliveInterval`, assert RTT is populated and
  `keepAliveTimeout` is never reached.
- **Clean shutdown from client**: call `close()`, assert the
  `closed` future completes normally, socket is reaped.
- **Clean shutdown from peer**: stop the yaci container while
  the client is connected; assert the client's root cancels
  with `ChannelClosedException`, every pending op fails with
  a chained cause, no hang.
- **Draining smoke test (M4-shaped, M5-prefactor)**: once we
  can drive chain-sync in M5 this exercises graceful wind-down
  against a real peer. For M4, a placeholder that connects and
  closes cleanly is enough.

Container lifecycle is per-test-class via
`TestContainerForAll`, matching the existing IT suite style.
Single shared container across cases in one class — handshake +
keep-alive are cheap, so test runtime is dominated by
container startup (~10-15s).

### 5. Preview-relay smoke — JVM, env-gated, 1-2 tests only

Kept intentionally small. The goal is to answer one question —
"does our transport speak to a real public node on a real
network?" — not to exhaustively retest logic we already covered
against yaci.

Lives in the same `scalusCardanoNetworkIt` project; gated behind
`SCALUS_N2N_PREVIEW_IT=1` so it runs only when a maintainer
explicitly asks. Not invoked by `sbtn it` or `sbtn ci`.

Scope:

- **Handshake against a preview relay**: connect, assert
  negotiated version is `v14` or `v16`, asserts mainnet-shape
  `networkMagic` round-trip against preview magic (2).
- **Stay-alive for 90 seconds**: connect, observe `≥ 2`
  keep-alive beats, assert RTT stays reasonable (< 500 ms
  from a random CI runner).

That's it — no more than two tests. Anything we'd want to
verify in more depth, we verify against yaci because yaci is
deterministic.

Relay host: single canonical name, currently
`preview-node.play.dev.cardano.org:3001` (IOG-hosted play
node). If it goes away, the env flag lets us skip instead of
chasing alternative hosts.

Flakiness budget: these tests *can* fail on network weather.
They are the thing a maintainer runs before cutting a release,
not a gate on everyday development.

### 5. Property + fuzz — deferred

Noted for completeness; not blocking M4:

- ScalaCheck over the frame decoder with arbitrary byte streams;
  assert every input either decodes into something valid or
  surfaces as a typed exception (no uncaught `Throwable`).
- Corpus-based fuzzing over the handshake and keep-alive decoders
  using a random byte generator, same invariant.

## Out of scope for M4 — explicit list

- Chain-sync state machine (M5).
- Block-fetch state machine (M5).
- `TxSubmission2` (M9).
- Peer-sharing (unscheduled).
- N2C / LocalStateQuery / LocalTxSubmission (M8, M12).
- Connection pooling / multi-relay (single connection is enough
  for M5's read path; a wallet-grade multi-peer setup is a
  separate work item).
- TLS on the N2N wire (ouroboros-network runs raw TCP on a trusted
  relay address; if we ever need TLS it's the peer's problem, not
  the transport's).
- Peras era signatures (we advertise `perasSupport = false`).

## Open questions

- **Yaci-DevKit's N2N network magic.** Default is 42 for the
  devnet, but it's configurable via yaci's `.env`. The IT suite
  should read the magic from the container rather than hardcode
  it, so a yaci version bump doesn't silently break tests.
  Probably read from `yaci-cli info` or the container's
  `node-config.json`.
- **Yaci-DevKit chain-sync pre-state.** Devnet starts at the
  Byron genesis and immediately rolls forward into Babbage/Conway
  per yaci's config. For M4 we only exercise handshake +
  keep-alive, so this doesn't matter, but M5 will want a known
  cold-start block sequence. Worth noting the fixture for when
  M5 lands.
- **Preview relay host longevity.** `preview-node.play.dev.cardano.org:3001`
  is the current IOG-hosted target; if it disappears, the
  `SCALUS_N2N_PREVIEW_IT` env flag lets us skip rather than
  chase a replacement. Revisit only if the smoke suite starts
  failing consistently.
- **Do we expose the negotiated version on the public M2
  `BlockchainStreamProvider`?** Not in M4 (which doesn't touch the
  provider), but M5 will have to surface it somewhere — likely on
  a diagnostic handle, not the main API.
- **Should `MiniProtocolBytes.receive` deliver whole mini-protocol
  messages instead of raw byte chunks?** Tempting, but it forces a
  CBOR decoder into the transport module. Decision for M4: stay at
  bytes; every state machine carries its own decoder. Revisit if
  M5 finds the pattern repetitive.
- **Timer backpressure on JS.** `setTimeout` fires on the event
  loop; if the loop is saturated (`fs2.Stream` computations),
  keep-alive may miss its window. Mitigation is M5+: move timers
  to a dedicated runner. Not a problem for JVM.
- **Virtual threads.** A follow-up could swap `JvmAsyncByteChannel`
  for a blocking-I/O-on-loom implementation and drop `Promise`
  bookkeeping. Pure performance optimization; API unchanged.
- **`onCancel` execution context.** Listeners registered on a
  `CancelToken` — where do they run? Options: synchronously on
  the `cancel()` caller's thread (simple, risk of handler work
  blocking cancel propagation), or via the client's
  `ExecutionContext` (decoupled, adds a hop). Leaning
  synchronous for M4 because listeners are all small
  (`Promise.tryFailure`), but worth re-examining when M5+ state
  machines register richer cleanup.
- **Non-cancelled op cleanup ordering.** When the connection root
  fires, what's the exact order of: (a) cancel fan-out to linked
  scopes, (b) socket close, (c) `closed` Future completion? Draft
  says (a) before (b) before (c), so consumers observe the cancel
  (and run their `onCancel`) while the socket is still technically
  open. Needs to be spelled out in the `NodeToNodeConnection`
  scaladoc when implemented.
