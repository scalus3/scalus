# ImmutableStreamingEmulator — design doc

**Status.** Deferred implementation. Design doc is load-bearing — it
informs the M3 `StreamingEmulator` design so the two remain aligned.

## Why we need this

Scalus already has a `Scenario` monad
(`scalus-testkit/shared/src/main/scala/scalus/testing/Scenario.scala:38-554`)
— a state-threading, branching DSL for writing deterministic transaction
scenarios against `ImmutableEmulator`. Users write:

```scala
async[Scenario] {
    val amount = Scenario.choices(10, 50, 100).await
    val result = Scenario.submit(tx).await
    Scenario.check(balance >= 0).await
}
```

and get a deterministic exploration of all branches with final states
and an action log.

**What `Scenario` cannot do today:** produce an event stream. A scenario
yields a final state (and an `actionLog`), but there is no way to write:

> "this scenario should, when run, emit the following sequence of
> `UtxoEvent`s in this order against `FromAddress(A)`"

— or, more importantly, run the same test logic that a real DApp uses
(subscribe, react, submit) against the simulation. Everything that
operates on `BlockchainStreamReader` subscriptions is invisible to
`Scenario`.

`ImmutableStreamingEmulator` closes that gap: **a streaming-enabled
version of the scenario monad**. Same declarative style, same state
threading, same branching — *plus* a live `BlockchainStreamReader`
surface so subscription-based code runs unchanged against simulated
scenarios.

### Concrete use cases

1. **Test UI / frontend code** that subscribes to chain events — drive
   it from a scripted scenario, assert on the event trace.
2. **Property-test DApp backends** that react to `Created`/`Spent`
   events — same scenario can be run across thousands of seeds, with
   `Scenario.choices` exploring alternative histories.
3. **Golden-file tests over event streams** — dump the deterministic
   event sequence and diff against expected output.
4. **Replay bug reports** — user reports "at this state, after this
   submit, I saw this wrong event" → encode as a scenario, replay
   deterministically.

The mutable `StreamingEmulator` (M3, live, in-process-shared) is the
right tool for in-process integration tests and demos. The immutable
version is for the purity/determinism/scenario use cases above — the
two coexist.

## Design review: M3 `StreamingEmulator` from this perspective

Before getting into the immutable design, does M3 already fit a future
immutable sibling?

### ✅ What's aligned

1. **Engine-driven architecture.** Both variants drive the same M2
   `Engine` via `onRollForward(AppliedBlock)`. One event-distribution
   mechanism serves both; same `QueryDecomposer.matches`, same
   mailbox fan-out, same `subscribe seeds the index`. A subscriber
   can't tell whether the blocks it observes were produced by a
   mutable `Emulator.submitSync` or by a replayed scenario step.

2. **`AppliedBlock` construction.**
   `StreamingEmulatorOps.buildAppliedBlock` is already a pure function
   from `(blockNo, txs)` to an `AppliedBlock`. Both variants use it
   the same way.

3. **Streaming-local block/slot numbering.** The counter in
   `StreamingEmulatorOps` is monotonic per wrapper instance —
   independent of the wrapped emulator's internal slot. For the
   scenario version the same idea holds: blockNo is scenario-local,
   unrelated to whatever slot the scenario's `ImmutableEmulator` is
   at.

4. **`newEmptyBlock()` primitive.** Maps naturally to scenario's
   `sleep(n)` / `advance` — advance the tip without a transaction.

### ⚠️ Proposed small refactor to improve alignment

The current `StreamingEmulatorOps` bakes "run `submitSync`, then drive
engine" into a trait tied to a mutable `Emulator`. For the scenario
case we need the same "drive engine" step, but the `submit` is not
`submitSync` — it's a scenario-threaded computation.

**Proposal:** extract a reusable `EngineDriver` helper. Both variants
consume it.

```scala
// scalus-streaming-core: new file
final class EngineDriver(engine: Engine):
    private val blockNoCounter = new java.util.concurrent.atomic.AtomicLong(0L)

    /** On a successful submit (ledger-validated upstream): notify Pending, then
      * enqueue an `AppliedBlock` with the given tx. */
    def applySubmit(tx: Transaction, hash: TransactionHash): Future[Unit]

    /** Empty block — tip advance, no transaction events. */
    def emptyBlock(): Future[Unit]
```

`StreamingEmulatorOps` becomes:

```scala
trait StreamingEmulatorOps[F[_], C[_]] extends BaseStreamProvider[F, C]:
    protected def emulator: Emulator
    protected lazy val driver = new EngineDriver(engine)

    override def submit(tx: Transaction): F[Either[SubmitError, TransactionHash]] =
        liftFuture {
            emulator.submitSync(tx) match
                case Left(e) => Future.successful(Left(e))
                case Right(h) => driver.applySubmit(tx, h).map(_ => Right(h))
        }

    def newEmptyBlock(): F[Unit] = liftFuture(driver.emptyBlock())
```

`ImmutableStreamingEmulator` reuses the same `EngineDriver`:

```scala
// scalus-testkit-streaming-fs2: scenario → engine bridge
private def applyStep(step: ScenarioStep, state: ScenarioState, driver: EngineDriver): Scenario[Unit] =
    step match
        case Submit(tx) =>
            state.emulator.submit(tx) match
                case Right((hash, newEmu)) =>
                    // drive the live engine AND thread the new emulator into state
                    Scenario.fromFuture(driver.applySubmit(tx, hash)).map(_ => newEmu)
                case Left(e) => Scenario.raise(...)
        case EmptyBlock =>
            Scenario.fromFuture(driver.emptyBlock())
        ...
```

The refactor is tiny (~50 LOC moved) and should land as part of M3.
Cost: small. Payoff: the "drive the engine" logic has a single home,
and the future immutable variant is a thin adapter layer on top.

### ⚠️ Widen `Engine.backup` type

Currently `Engine.backup: Option[BlockchainProvider]`. The engine only
calls read-side methods (`findUtxos`, `fetchLatestParams`). The
scenario version's backup is a read-only projection of
`ScenarioState.emulator`; there is no meaningful `submit`.

**Proposal:** widen to `Option[BlockchainReader]`. Small type change,
zero runtime impact, removes the need for the scenario version to
fake a `submit` method that throws.

This change is independent of M3 but wanted for the immutable version.
Can land in M3 prep or with the immutable module.

### ⚠️ Engine shutdown / per-run isolation

Scenario runs create and tear down an ephemeral `Engine` per `run(…)`
call. The engine's `closeAllSubscribers()` should be idempotent and
finalizer-safe (so it slots into fs2 `Resource.release`). Worth
auditing in M3 prep.

## Placement

`scalus-embedded-node/scalus-testkit-streaming-fs2/` (sbt:
`scalus-testkit-streaming-fs2`; val: `scalusTestkitStreamingFs2`).

- Depends on: `scalus-testkit` (for `Scenario`, `ImmutableEmulator`)
  and `scalus-streaming-fs2` (for `Engine`, `EngineDriver`,
  `Fs2BlockchainStreamProvider` shell).
- Platform: JVM-only in the first cut.
- Publish scope: yes.

Why a bridge module: testkit doesn't depend on streaming today, and
streaming depends on ledger but not testkit. The bridge module is the
only direction-compatible place.

## Why fs2-only

`ImmutableEmulator.submit` returns
`Either[SubmitError, (TransactionHash, ImmutableEmulator)]` — a value-threaded
transition. `Scenario` already exposes this shape. fs2 is
pull-based and can express state-threading natively
(`unfoldEval`, `scan`, `Pull.stateful`).

Ox is effect-scoped and inherently mutable — a `Flow` cannot express
"hand me the emulator, return new emulator + events" without reaching
for an `AtomicReference` (at which point the purity is gone and you
may as well use mutable `OxStreamingEmulator`).

Users under ox who want simulation-backed streaming use
`OxStreamingEmulator` (M3). Users under fs2 who want *scenario-style*
simulation use `ImmutableStreamingEmulator` (this doc). Users under
fs2 who want simple live simulation use `Fs2StreamingEmulator` (M3).

## API sketch

```scala
package scalus.testing.streaming.fs2

import cats.effect.IO
import fs2.Stream
import scalus.cardano.node.stream.BlockchainStreamReaderTF
import scalus.testing.{ImmutableEmulator, Scenario, ScenarioState}

/** A scripted step consumed by [[ImmutableStreamingEmulator]]. Mirrors
  * scenario primitives but is declarative — all state transitions are
  * applied in a single pass, producing a deterministic event trace.
  */
enum StreamingStep:
    case Submit(tx: Transaction)
    case Advance(slots: Long)
    case EmptyBlock
    case Check(condition: ScenarioState => Boolean, message: String = "")

final case class RunResult(
    finalState: ScenarioState,
    /** Read-only stream provider. Subscribing after `run` returns the deterministic
      * event sequence that corresponds to the `StreamingStep` script. Any
      * subsequent `submit` call returns `Left(error)` — submissions are declared
      * in the script. */
    reader: BlockchainStreamReaderTF[IO, Stream[IO, *]]
)

object ImmutableStreamingEmulator:

    /** Run a scripted sequence against an initial emulator. Referentially
      * transparent: same `(initial, script)` input → same `RunResult`.
      *
      * For scenario-style branching, use [[runScenario]] which takes a
      * `Scenario[Seq[StreamingStep]]` and returns one `RunResult` per branch.
      */
    def run(
        initial: ImmutableEmulator,
        script: Stream[IO, StreamingStep]
    ): IO[RunResult]

    /** Run a scenario that yields a streaming script. Explores all branches;
      * each branch gets its own isolated engine and event trace. */
    def runScenario(
        initial: ScenarioState,
        scenario: Scenario[Seq[StreamingStep]]
    ): IO[IndexedSeq[RunResult]]
```

Usage — scenario style, which is the headline use case:

```scala
val scenario = async[Scenario] {
    val amount = Scenario.choices(10, 50, 100).await
    val tx1 = buildTransfer(Alice, Bob, amount)
    val tx2 = buildTransfer(Bob, Charlie, amount / 2)
    Seq(StreamingStep.Submit(tx1), StreamingStep.Advance(5), StreamingStep.Submit(tx2))
}

val results = ImmutableStreamingEmulator.runScenario(initial, scenario).unsafeRunSync()

results.foreach { case RunResult(finalState, reader) =>
    val events = reader
        .subscribeUtxoQuery(FromAddress(Bob.address))
        .take(2)
        .compile
        .toList
        .unsafeRunSync()

    assert(events.head.isInstanceOf[UtxoEvent.Created])
    assert(finalState.actionLog.size == 2)
}
```

## Internal shape — Shape A (engine reuse, ephemeral per run)

Per `run` invocation:

1. Build a fresh `EngineDriver` over a per-run `Engine(cardanoInfo, backup = Some(initial.asReader))`.
2. fs2 pipeline: fold the script `(emu, _) × step → (emu', events)` via `Pull` / `scan`.
3. Each `Submit(tx)`:
   - `emu.submit(tx) match { Right((hash, emu')) => driver.applySubmit(tx, hash); state.copy(emulator = emu') }`.
   - `Left(e)` — record in action log, no event driven.
4. Each `EmptyBlock`: `driver.emptyBlock()`.
5. Each `Advance(slots)`: emulator-internal slot bump, optionally `emptyBlock()`.
6. On script completion, return `RunResult(finalState, reader)`.
7. Reader lifetime: subscribers created *after* `run` returns replay the buffered event sequence (engine holds it in the rollback buffer). For live-during-run subscribers, see "extensions" below.
8. `engine.closeAllSubscribers()` on `RunResult` finalizer.

Shape B (pure-fs2 bypass, no engine) considered and rejected — would
reimplement `QueryDecomposer.matches`, tx-hash-index, rollback-buffer
logic. Too much duplication for negligible purity benefit.

## What blocks implementation today

1. `scalus-embedded-node/scalus-testkit-streaming-fs2/` module doesn't exist.
2. `Engine.backup` type needs widening from `BlockchainProvider` to
   `BlockchainReader` (mentioned above).
3. `Engine` finalizer safety audit (mentioned above).
4. **`EngineDriver` extraction from `StreamingEmulatorOps` — see
   *Design review* above.** Proposed to land as a small M3 follow-up
   so both variants share it.
5. `ImmutableEmulator` may need small helpers (currently only
   `.submit(tx)` — may also need `.setSlot`, `.cardanoInfo` parity
   with mutable `Emulator`).
6. Scenario-native support: either extend `Scenario` with a
   `Scenario.streamingStep(step)` primitive, or accept raw
   `Scenario[Seq[StreamingStep]]` and let the runner drive — latter
   is simpler.

## Sketch implementation path (once unblocked)

1. Land **`EngineDriver` extraction** (small M3 follow-up commit).
2. Widen `Engine.backup` to `BlockchainReader`.
3. Add the `scalus-testkit-streaming-fs2` module to `build.sbt`, wire
   into `jvm` alias.
4. Define `StreamingStep` ADT + `RunResult` case class.
5. Implement `ImmutableStreamingEmulator.run` as an fs2 pipeline using
   `EngineDriver`.
6. Implement `runScenario` by running `Scenario.runAll` on the
   scenario, then `run` for each branch with an isolated engine.
7. Read-only `BlockchainStreamReaderTF` wrapper — delegates subscribes
   to the engine; `submit` returns `Left(error)`.
8. Tests (fs2-only):
    - Determinism: same `(initial, script)` → same event sequence.
    - Branching: `Scenario.choices` produces independent traces per
      branch.
    - Ledger-rule violation: `Submit(doubleSpend)` — no event, logged
      in `actionLog`.
    - Seed-from-backup on `includeExistingUtxos=true`.
    - Scenario round-trip: a realistic DApp test (subscribe to Bob,
      assert `Created` events after each transfer).

## Extensions (nice-to-have, later)

- **Live-during-run subscriptions.** Today's sketch is "subscribe
  after run". Alternatively, allow scenario code to subscribe mid-way
  and consume events live — useful for testing reactive code.
  Requires integrating scenario's continuation with fs2's pull.
- **Golden-file mode.** `runToGolden(path)` — runs the scenario and
  writes the event trace to a file; on CI, diff against committed
  expectation.
- **Branching event traces.** For `runScenario`, label each branch's
  trace by the `choices` decisions that led to it.

## Status

- **M3 follow-up (small refactor):** extract `EngineDriver` from
  `StreamingEmulatorOps`. ~50 LOC move, no behaviour change.
- **M3 follow-up (type widening):** `Engine.backup: Option[BlockchainReader]`.
- **Future milestone (deferred):** full `ImmutableStreamingEmulator`
  implementation per the sketch above, once the above two land.

## References

- `scalus-testkit/shared/src/main/scala/scalus/testing/Scenario.scala:38-554`
  — the scenario DSL this doc extends.
- `scalus-testkit/shared/src/main/scala/scalus/testing/ImmutableEmulator.scala:10-52`
  — the value-threaded emulator this wraps.
- `docs/internal/testing/ledger-logical-monad.md` — background on
  `Scenario`'s `CpsConcurrentLogicMonad`.
- M3 `StreamingEmulator` implementation (shipped in the same
  `scalus-embedded-node/` tree).
