# Logical Monad for Blockchain Scenario Testing


Status: **partially implemented** — ImmutableEmulator, Scenario monad (sealed trait ADT), and core DSL are implemented with tests passing. TxVariations, ContractStepVariations, and ScenarioExplorer are planned but not yet implemented.

## Overview

Add to testkit the ability to test blockchain scenarios with logical monad, enabling exploration of multiple execution paths (branching) in a single test scenario.

Logical monad from dotty-cps-async:
- Implementation: `/Users/rssh/work/oss/dotty-cps/dotty-cps-async/logic`
- Documentation: `/Users/rssh/work/oss/dotty-cps/dotty-cps-async/docs/logic/LogicUsage.rst`
- Notes: https://github.com/rssh/notes/blob/master/2024_01_30_logic-monad-1.md

## Use Cases

1. **Race condition testing** - Multiple actors making different bid choices, explore all combinations
2. **Time-dependent behavior** - Fork into "before timeout" vs "after timeout" scenarios
3. **Attack simulation** - Test all orderings of attacker actions
4. **Property-based scenarios** - Verify invariants hold across all execution paths

## Design Decisions

### Monad Encoding: Sealed Trait ADT

The Scenario monad uses a sealed trait ADT encoding rather than a function-based encoding. This was chosen because:

1. **Independent mplus branches** — `mplus` branches can have completely different initial states (they are independent). A function-based encoding `ScenarioState => LogicStreamT[Future, (ScenarioState, A)]` would force both branches to start from the same state passed to the function.

2. **Working fsplit** — `fsplit` returns `Future[Option[(Try[A], Scenario[A])]]` which requires evaluating the scenario tree without a state parameter. The ADT encoding carries state inside each node, so `eval` can interpret the tree to `LogicStreamT` without external state.

3. **State injection via sentinel** — `flatMap(Done(s, a))(f)` calls `injectState(s, f(a))` which only overrides nodes bound to the `ScenarioState.empty` sentinel (checked by reference equality via `eq`). Nodes already bound to a real state (e.g., from independent mplus branches) are left unchanged.

```scala
sealed trait Scenario[A]

object Scenario {
    case class Done[A](state: ScenarioState, value: A) extends Scenario[A]
    case class Leaf[A](state: ScenarioState, run: ScenarioState => Scenario[A]) extends Scenario[A]
    case class Branches[A](branches: LogicStreamT[Future, Scenario[A]]) extends Scenario[A]
    case class WaitFuture[A](future: Future[Scenario[A]]) extends Scenario[A]
    case class ScenarioError[A](state: ScenarioState, error: Throwable) extends Scenario[A]
    case class FromStream[A](stream: LogicStreamT[Future, (ScenarioState, A)]) extends Scenario[A]
}

case class ScenarioState(
    emulator: ImmutableEmulator,
    rng: Seed
)
```

**ADT cases:**

| Case | Purpose |
|------|---------|
| `Done(state, value)` | Terminal: holds final state and value |
| `Leaf(state, run)` | State-dependent continuation — `run(state)` produces next Scenario |
| `Branches(streams)` | Non-deterministic branching — each branch is an independent Scenario |
| `WaitFuture(future)` | Async suspension — wait for Future, then continue |
| `ScenarioError(state, error)` | Error with state for rollback in flatMapTry |
| `FromStream(stream)` | Already-evaluated stream (from fsplit/msplit tails) |

No separate `slot` field - the emulator already tracks the current slot internally via `env.slot`.

### State Threading: injectState

The key mechanism for state threading in flatMap:

```scala
private def injectState[A](s: ScenarioState, scenario: Scenario[A]): Scenario[A] =
    scenario match
        case Done(existing, a) =>
            if existing eq ScenarioState.empty then Done(s, a) else scenario
        case Leaf(existing, run) =>
            if existing eq ScenarioState.empty then Leaf(s, run) else scenario
        case Branches(streams) =>
            Branches(streams.map(branch => injectState(s, branch)))
        case WaitFuture(fut) =>
            WaitFuture(fut.map(sc => injectState(s, sc)))
        case ScenarioError(existing, e) =>
            if existing eq ScenarioState.empty then ScenarioError(s, e) else scenario
        case FromStream(_) => scenario // stream states are already bound
```

Uses `eq` (reference equality) against `ScenarioState.empty` to distinguish unbound nodes (created by DSL methods like `pure`, `choices`) from nodes already bound to a real state (from independent mplus branches). Only unbound nodes get the injected state.

### Evaluation: eval

`eval` interprets a Scenario ADT tree into `LogicStreamT[Future, (ScenarioState, A)]` for observation:

```scala
def eval[A](scenario: Scenario[A]): LogicStreamT[Future, (ScenarioState, A)] =
    scenario match
        case Done(s, a)          => LogicStreamT.Pure((s, a))
        case Leaf(s, run)        => eval(run(s))
        case Branches(streams)   => streams.flatMap(branch => eval(branch))
        case WaitFuture(fut)     => LogicStreamT.WaitF(fut.map(sc => eval(sc)))
        case ScenarioError(_, e) => LogicStreamT.Error(e)
        case FromStream(stream)  => stream
```

### Typeclass: CpsLogicMonad

```scala
given scenarioLogicMonad: CpsLogicMonad[Scenario] with CpsLogicMonadInstanceContext[Scenario]
```

Hierarchy:
```
CpsMonad
    ↓
CpsTryMonad (fromTry, restore, flatMapTry)
    ↓
CpsLogicMonad (mzero, mplus, msplit, fsplit, guard, once, interleave, ...)
```

Since `CpsLogicMonad` extends `CpsTryMonad`, we get exception handling (`try`/`catch` in direct style) automatically.

### Sequential Branching

Branching is sequential (depth-first via `mplus`). Since we have `adoptCallbackStyle`, `parOr`/`interleave` can be built, but we choose not to expose them - sequential exploration is sufficient for scenario testing and simpler to reason about.

### Error Handling via flatMapTry

`flatMapTry` handles state rollback on failure:

```scala
override def flatMapTry[A, B](fa: Scenario[A])(f: Try[A] => Scenario[B]): Scenario[B] =
    fa match
        case Done(s, a)          => injectState(s, f(Success(a)))
        case ScenarioError(s, e) => injectState(s, f(Failure(e)))
        case Leaf(s, run)        => Leaf(s, state => flatMapTry(run(state))(f))
        case Branches(streams)   => Branches(streams.map(branch => flatMapTry(branch)(f)))
        case WaitFuture(fut)     => WaitFuture(fut.map(sc => flatMapTry(sc)(f)))
        case FromStream(stream)  =>
            FromStream(stream.flatMapTry {
                case Success((s, a)) => eval(injectState(s, f(Success(a))))
                case Failure(e)      => eval(f(Failure(e)))
            })
```

On `ScenarioError(s, e)`, the error's state `s` is preserved and injected into `f(Failure(e))`, enabling rollback to the state before the error occurred.

### Two Kinds of Failure

| Failure Type | Meaning | Handling |
|--------------|---------|----------|
| Logical (`mzero`) | No solutions, try next branch | `guard`, `choices`, backtracking |
| Exception (`Throwable`) | Real error | `try`/`catch`, `restore`, `flatMapTry` |

### mplus Optimization

When the first argument is already a `Branches`, its underlying `LogicStreamT` is flattened into the combined stream rather than double-wrapping:

```scala
override def mplus[A](a: Scenario[A], b: => Scenario[A]): Scenario[A] =
    val aStream = a match
        case Branches(streams) => streams
        case other => LogicStreamT.Pure[Future, Scenario[A]](other)
    Branches(aStream.mplus(LogicStreamT.Pure[Future, Scenario[A]](b)))
```

### fsplit and msplit

`fsplit` evaluates the scenario to a LogicStreamT stream, then delegates to `logicStreamMonad.fsplit`. The tail is wrapped as `FromStream`:

```scala
override def fsplit[A](sa: Scenario[A]): Future[Option[(Try[A], Scenario[A])]] =
    logicStreamMonad.fsplit(eval(sa)).map {
        case None => None
        case Some((tryPair, restStream)) =>
            val tail: Scenario[A] = FromStream(restStream)
            tryPair match
                case Success((s2, a)) => Some((Success(a), tail))
                case Failure(e)       => Some((Failure(e), tail))
    }
```

`msplit` works similarly but operates within the Scenario monad (returns `Scenario[Option[(Try[A], Scenario[A])]]`), using a `Leaf` to capture state and `FromStream` for the tail.

## Implementation

### ImmutableEmulator (✅ Implemented)

File: `scalus-testkit/shared/src/main/scala/scalus/testing/ImmutableEmulator.scala`

Immutable emulator for Cardano transactions. Every state-changing operation returns a new instance.

```scala
case class ImmutableEmulator(
    state: State,
    env: UtxoEnv,
    slotConfig: SlotConfig = SlotConfig.mainnet,
    evaluatorMode: EvaluatorMode = EvaluatorMode.Validate,
    validators: Iterable[STS.Validator] = Emulator.defaultValidators,
    mutators: Iterable[STS.Mutator] = Emulator.defaultMutators
) {
    def utxos: Utxos = state.utxos
    def currentSlot: SlotNo = env.slot
    def protocolParams: ProtocolParams = env.params
    def cardanoInfo: CardanoInfo = CardanoInfo(env.params, env.network, slotConfig)

    def submit(tx: Transaction): Either[SubmitError, (TransactionHash, ImmutableEmulator)]
    def advanceSlot(n: Long): ImmutableEmulator
    def setSlot(slot: SlotNo): ImmutableEmulator
    def findUtxos(query: UtxoQuery): Either[UtxoQueryError, Utxos]

    def asProvider: BlockchainProvider   // read-only snapshot provider
    def toEmulator: Emulator             // convert to mutable Emulator
}

object ImmutableEmulator {
    def empty: ImmutableEmulator                    // no UTxOs, test-mainnet params
    def fromEmulator(emulator: EmulatorBase): ImmutableEmulator
    def withAddresses(addresses: Seq[Address], initialValue: Value = Value.ada(10_000L)): ImmutableEmulator
}
```

Query evaluation is shared with the mutable Emulator via static `EmulatorBase.evalQuery(utxos, query)`.

### Scenario Monad (✅ Implemented)

File: `scalus-testkit/shared/src/main/scala/scalus/testing/Scenario.scala`

CpsLogicMonad instance with all required operations:

```scala
given scenarioLogicMonad: CpsLogicMonad[Scenario] with CpsLogicMonadInstanceContext[Scenario] with {

    override type Observer[A] = Future[A]
    override val observerCpsMonad: CpsTryMonad[Future] = futureMonad

    override def pure[A](a: A): Scenario[A] = Done(ScenarioState.empty, a)
    override def map[A, B](fa: Scenario[A])(f: A => B): Scenario[B]
    override def flatMap[A, B](fa: Scenario[A])(f: A => Scenario[B]): Scenario[B]
    override def flatMapTry[A, B](fa: Scenario[A])(f: Try[A] => Scenario[B]): Scenario[B]
    override def error[A](e: Throwable): Scenario[A]
    override def mzero[A]: Scenario[A]
    override def mplus[A](a: Scenario[A], b: => Scenario[A]): Scenario[A]
    override def msplit[A](sa: Scenario[A]): Scenario[Option[(Try[A], Scenario[A])]]
    override def fsplit[A](sa: Scenario[A]): Future[Option[(Try[A], Scenario[A])]]
    override def withMsplit[A, B](c: Scenario[A])(f: ...): Scenario[B]
    override def flattenObserver[A](fsa: Future[Scenario[A]]): Scenario[A]
}
```

### Scenario DSL (✅ Implemented)

```scala
object Scenario {
    // --- Internal state primitives (private[testing]) ---
    private[testing] def get: Scenario[ScenarioState]
    private[testing] def modify(f: ScenarioState => ScenarioState): Scenario[Unit]
    private[testing] def modifyAndReturn[A](f: ScenarioState => (ScenarioState, A)): Scenario[A]

    // --- Public DSL ---

    // Logical branching
    def choices[A](xs: A*): Scenario[A]
    def fromCollection[A](xs: Iterable[A]): Scenario[A]
    def guard(cond: Boolean): Scenario[Unit]
    def fail[A]: Scenario[A]
    def error[A](e: Throwable): Scenario[A]

    // Time
    def now: Scenario[SlotNo]
    def sleep(slots: Long): Scenario[Unit]

    // Emulator operations
    def currentEmulator: Scenario[ImmutableEmulator]
    def submit(tx: Transaction): Scenario[Either[SubmitError, TransactionHash]]

    // Provider — BlockchainProviderTF[Scenario] for endpoint code (reads latest state per call)
    def provider: Scenario[BlockchainProviderTF[Scenario]]

    // Provider — BlockchainProvider snapshot for Gen construction (reads state once)
    def snapshotProvider: Scenario[BlockchainProvider]

    // Runners
    def run[A](initial: ScenarioState)(s: Scenario[A]): LogicStreamT[Future, (ScenarioState, A)]
    def runAll[A](initial: ScenarioState, maxResults: Int = 1000)(s: Scenario[A]): Future[IndexedSeq[(ScenarioState, A)]]
    def runFirst[A](initial: ScenarioState)(s: Scenario[A]): Future[Option[(ScenarioState, A)]]
}
```

**Internal leaf helper** — DSL methods use `leaf(run)` which creates `Leaf(ScenarioState.empty, run)`. The empty sentinel is overridden by `injectState` during flatMap state threading.

**Two provider flavors:**

- `provider` returns `BlockchainProviderTF[Scenario]` — each call (findUtxos, submit, etc.) reads/writes the *latest* Scenario state. Used by endpoint code (`AuctionEndpoints[Scenario](provider)`).
- `snapshotProvider` returns `BlockchainProvider` (Future-based) — a read-only snapshot of the current emulator state. Used for `TxBuilder.complete` and Gen construction.

### BlockchainProvider Integration (✅ Implemented)

- `BlockchainProviderTF[F[_]]` tagless-final trait
- `BlockchainProvider extends BlockchainProviderTF[Future]` with captured EC
- `Scenario.provider` returning `BlockchainProviderTF[Scenario]`
- `Scenario.snapshotProvider` returning `BlockchainProvider`

### Tests (✅ Implemented)

File: `scalus-testkit/jvm/src/test/scala/scalus/testing/ScenarioTest.scala`

26 tests covering:
- Pure/map/flatMap monad operations
- Non-deterministic branching: choices, fromCollection, guard, mzero
- State operations: sleep, per-branch state independence, currentEmulator
- mplus of independent branches (with different initial states)
- fsplit and msplit
- Error handling: error propagation, flatMapTry rollback
- Runners: runAll, runFirst, maxResults
- ImmutableEmulator: empty, withAddresses, advanceSlot, fromEmulator, toEmulator
- Integration: TxBuilder + submit valid transaction

## Endpoint Pattern

Endpoints use `BlockchainProviderTF[F]` directly:

```scala
class AuctionEndpoints[F[_]: CpsTryMonad](provider: BlockchainProviderTF[F]) {

    def startAuction(
        seller: Address,
        item: String,
        minBid: Coin,
        endTime: SlotNo,
        signer: TransactionSigner
    ): F[TransactionHash] = async[F] {
        val utxos = provider.findUtxos(FromAddress(seller)).await.getOrThrow
        val tx = TxBuilder()
            .addInput(utxos.head)
            .addOutput(auctionScriptAddress, auctionDatum(item, minBid, endTime, seller))
            .addCollateral(utxos)
            .build()
        provider.submit(signer.sign(tx)).await.getOrThrow
    }
}
```

**Usage:**
```scala
// Test - with branching
async[Scenario] {
    val provider = Scenario.provider.await
    val endpoints = AuctionEndpoints[Scenario](provider)

    endpoints.startAuction(...).await
    val amount = choices(10, 50, 100).await  // explore branches
    val now = Scenario.now.await
    endpoints.bid(..., amount, now, ...).await
}

// Production
async[Future] {
    val provider = BlockfrostProvider.mainnet(apiKey).await
    val endpoints = AuctionEndpoints[Future](provider)

    endpoints.startAuction(...).await
    endpoints.bid(..., 100, currentSlot, ...).await
}
```

### Time Operations

Time operations (`now`, `sleep`) are Scenario-specific DSL that delegate to the emulator's internal slot:

```scala
object Scenario {
    def now: Scenario[SlotNo] =
        leaf(state => Done(state, state.emulator.currentSlot))

    def sleep(slots: Long): Scenario[Unit] =
        modify(s => s.copy(emulator = s.emulator.advanceSlot(slots)))
}
```

In production, current slot comes from external source (chain tip query).

## Examples

### Auction with Multiple Bidders

Illustrates testing different bid amounts from multiple parties:

```scala
val scenario = async[Scenario] {

    val provider = Scenario.provider.await
    val endpoints = AuctionEndpoints(provider)

    val endTime = Scenario.now.await + 100

    endpoints.startAuction(Alice.address, "myItem", 100, endTime, Alice.signer).await

    Scenario.sleep(1).await

    val bobBid = Try {
        val amount = choices(10, 101, 120).await
        endpoints.bid(Bob.address, "myItem", amount, Scenario.now.await, Bob.signer).await
    }

    Scenario.sleep(2).await

    val charlyBid = Try {
        val amount = choices(103, 120).await
        endpoints.bid(Charly.address, "myItem", amount, Scenario.now.await, Charly.signer).await
    }

    Scenario.sleep(89).await

    val winner = endpoints.findTopBidder("myItem").await
    assert(winner != Jon.address)

    endpoints.endAuction("myItem", winner, Alice.signer).await
}

Scenario.runAll(initialState)(scenario)
```

### HTLC Time-Dependent Behavior

Testing unlock before vs after timeout:

```scala
async[Scenario] {

    val provider = Scenario.provider.await
    val htlcTxs = HtlcTransactions(provider, contract)

    htlcTxs.lock(
        value = Value.ada(10),
        sponsor = Alice.address,
        committer = Alice.addrKeyHash,
        receiver = Bob.addrKeyHash,
        image = image,
        timeout = timeout,
        signer = Alice.signer
    ).await

    choices(
        {
            Scenario.sleep(1).await
            // unlock before timeout - should fail
            val result = Try(htlcTxs.unlock(...).await)
            assert(result.isFailure)
        },
        {
            Scenario.sleep(100).await
            // unlock after timeout - should succeed
            val result = Try(htlcTxs.unlock(...).await)
            assert(result.isSuccess)
        }
    ).await
}
```

### Race Condition Testing

Testing concurrent UTxO access:

```scala
async[Scenario] {

    val provider = Scenario.provider.await

    // Alice and Bob both try to spend the same UTxO
    val sharedUtxo = provider.findUtxos(contractAddress).await.getOrThrow.head

    val order = choices("alice-first", "bob-first").await

    order match {
        case "alice-first" =>
            val aliceResult = Try(spendUtxo(provider, sharedUtxo, Alice.signer).await)
            Scenario.sleep(1).await
            val bobResult = Try(spendUtxo(provider, sharedUtxo, Bob.signer).await)
            assert(aliceResult.isSuccess)
            assert(bobResult.isFailure)  // UTxO already spent

        case "bob-first" =>
            val bobResult = Try(spendUtxo(provider, sharedUtxo, Bob.signer).await)
            Scenario.sleep(1).await
            val aliceResult = Try(spendUtxo(provider, sharedUtxo, Alice.signer).await)
            assert(bobResult.isSuccess)
            assert(aliceResult.isFailure)  // UTxO already spent
    }
}
```

## Planned: TxVariations + Contract Testing Framework

### Design Principle: Seq[TxBuilder] as Universal Currency

All TxBuilder methods except `complete` are synchronous. With sponsor and signer as builder steps, the entire transaction *construction* is synchronous and can live inside `Seq[TxBuilder]` (via `enumerate`) or `Gen[TxBuilder]` (derived). Only `signAndComplete(provider)` requires the provider.

`Seq[TxBuilder]` (from `TxVariations.enumerate`) is the universal intermediate form:
- Scenario uses it directly via `allVariationsEnum` → `choices(...)` (exhaustive branching)
- ScalaCheck uses `Gen.oneOf(seq)` via `allVariationsGen` (random sampling with shrinking)
- `TxSamplingVariations` inverts this for fuzz testing: `gen` is primary, `enumerate` samples from it

### TxBuilder: Sponsor as Builder Step (⬚ Not yet implemented)

`TxBuilder.complete(provider, sponsor)` currently takes both provider and sponsor address. We split this: sponsor becomes a builder step, so `complete` only needs the provider.

```scala
def sponsor(address: Address): TxBuilder = copy(sponsorAddress = Some(address))
def signer(s: TransactionSigner): TxBuilder = copy(txSigner = Some(s))

def complete(provider: BlockchainProvider): Future[TxBuilder]
def signAndComplete(provider: BlockchainProvider): Future[Transaction]
```

### Core Abstractions (⬚ Not yet implemented)

#### TxVariations[S]: Transaction Variation Generator

Two flavors — differing in which method is primary:

```scala
/** Enumerable variations (boundary testing). Primary method: enumerate. */
trait TxVariations[S] {
    def enumerate(provider: BlockchainProvider, state: S, base: TxBuilder): Seq[TxBuilder]
    def gen(provider: BlockchainProvider, state: S, base: TxBuilder): Gen[TxBuilder] =
        Gen.oneOf(enumerate(provider, state, base))
    def ++(other: TxVariations[S]): TxVariations[S]
}

/** Sampling-based variations (fuzz testing). Primary method: gen. */
trait TxSamplingVariations[S] extends TxVariations[S] {
    override def gen(provider: BlockchainProvider, state: S, base: TxBuilder): Gen[TxBuilder]
    override def enumerate(provider: BlockchainProvider, state: S, base: TxBuilder): Seq[TxBuilder] =
        List.fill(sampleSize)(gen(provider, state, base).sample).flatten
    def sampleSize: Int = 20
}
```

#### ContractStepVariations[S]: Bundled Step Description

```scala
trait ContractStepVariations[S] {
    def extractState(provider: BlockchainProvider): Future[S]
    def baseTx(info: CardanoInfo, state: S): TxBuilder
    def variations(state: S): TxVariations[S]
    def allVariationsEnum(provider: BlockchainProvider, state: S): Seq[TxBuilder]
    def allVariationsGen(provider: BlockchainProvider, state: S): Gen[TxBuilder]
}
```

### Consuming ContractStepVariations: Three Modes (⬚ Not yet implemented)

```scala
// Mode 1: ScalaCheck forAll — random sampling with shrinking
given Arbitrary[TxBuilder] = Arbitrary(AuctionStep.allVariationsGen(provider, state))
forAll { (incompleteTx: TxBuilder) => ... }

// Mode 2: ScalaCheck Commands — random per-command, stateful
ContractScalaCheckCommands(initialEmulator, AuctionStep).property()

// Mode 3: Scenario — exhaustive branching over domain
ScenarioExplorer.explore(maxDepth = 4, AuctionStep)
```

### Gen in Scenario (⬚ Not yet implemented)

ScenarioState carries an RNG for deterministic replay across branches:

```scala
object Scenario {
    def sample[A](gen: Gen[A]): Scenario[A]
}

given CpsMonadConversion[Gen, Scenario] with {
    def apply[A](ga: Gen[A]): Scenario[A] = Scenario.sample(ga)
}
```

### Complexity

With `C` categories per dimension, `D` dimensions per action, and depth `N`:
- Per step: `C^D` paths (e.g. 3×3×3 = 27)
- Multi-step: up to `(C^D)^N` total paths

Mitigation: keep categories at 2-4 per dimension, use `guard` to prune impossible branches, use `once` to stop at first violation.

## Implementation Status

| Component | Status | Files |
|-----------|--------|-------|
| EmulatorBase.evalQuery extraction | ✅ Done | `scalus-cardano-ledger/shared/.../EmulatorBase.scala` |
| ImmutableEmulator | ✅ Done | `scalus-testkit/shared/.../testing/ImmutableEmulator.scala` |
| Scenario monad (sealed trait ADT) | ✅ Done | `scalus-testkit/shared/.../testing/Scenario.scala` |
| ScenarioState | ✅ Done | `scalus-testkit/shared/.../testing/Scenario.scala` |
| CpsLogicMonad[Scenario] instance | ✅ Done | `scalus-testkit/shared/.../testing/Scenario.scala` |
| Scenario DSL (choices, guard, sleep, submit, etc.) | ✅ Done | `scalus-testkit/shared/.../testing/Scenario.scala` |
| Scenario.provider (BlockchainProviderTF[Scenario]) | ✅ Done | `scalus-testkit/shared/.../testing/Scenario.scala` |
| Scenario.snapshotProvider (BlockchainProvider) | ✅ Done | `scalus-testkit/shared/.../testing/Scenario.scala` |
| Runners (run, runAll, runFirst) | ✅ Done | `scalus-testkit/shared/.../testing/Scenario.scala` |
| ScenarioTest (26 tests) | ✅ Done | `scalus-testkit/jvm/.../testing/ScenarioTest.scala` |
| BlockchainProviderTF[F] | ✅ Done | `scalus-cardano-ledger/shared/.../node/BlockchainProviderTF.scala` |
| dotty-cps-async deps in build.sbt | ✅ Done | `build.sbt` |
| TxBuilder sponsor/signer as builder steps | ⬚ Planned | — |
| Scenario.sample(gen) | ⬚ Planned | — |
| TxVariations[S] | ⬚ Planned | — |
| TxSamplingVariations[S] | ⬚ Planned | — |
| ContractStepVariations[S] | ⬚ Planned | — |
| BoundaryValues helpers | ⬚ Planned | — |
| ContractScalaCheckCommands adapter | ⬚ Planned | — |
| ScenarioExplorer.explore adapter | ⬚ Planned | — |
| FutureInScenario convenience layer | ⬚ Planned (optional) | — |

## Open Questions

1. ~~**Provider approach**~~ - ✅ Resolved: Tagless-final chosen, `BlockchainProviderTF[F[_]]` trait created as first step

2. **FutureInScenario convenience layer** - Should we add a convenience layer that allows existing `Future`-based endpoint code to work in Scenario tests without modification? This would use runtime type detection but provide a smoother migration path for existing code.

3. ~~**Gen.domain for exhaustive enumeration**~~ - ✅ Resolved: `TxVariations` uses `enumerate` (returns `Seq`) as primary method, `gen` is derived. `TxSamplingVariations` inverts this for large/continuous domains (gen-first, enumerate samples N values). No need to extract domain from `Gen`.

4. ~~**Monad encoding**~~ - ✅ Resolved: Sealed trait ADT chosen over function-based encoding. Supports independent mplus branches, working fsplit, and state injection via sentinel pattern.
