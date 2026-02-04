# Logical Monad for Blockchain Scenario Testing


Status: **implemented** — ImmutableEmulator, Scenario monad (sealed trait ADT), core DSL, Scenario.sample/sampleN, Scenario.check (macro), ScenarioExplorer, TxTemplate, TxVariations, TxSamplingVariations, ContractStepVariations, StandardTxVariations, BlockchainReader, and ContractScalaCheckCommands adapter are all implemented with tests passing.

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

### Typeclass: CpsConcurrentLogicMonad

```scala
given scenarioLogicMonad: CpsConcurrentLogicMonad[Scenario, Future]
    with CpsConcurrentLogicMonadInstanceContext[Scenario, Future]
```

Hierarchy:
```
CpsMonad
    ↓
CpsTryMonad (fromTry, restore, flatMapTry)
    ↓
CpsLogicMonad (mzero, mplus, msplit, fsplit, guard, once, interleave, ...)
    ↓
CpsConcurrentLogicMonad (toAsyncList, toStream, parOr)
```

Since `CpsLogicMonad` extends `CpsTryMonad`, we get exception handling (`try`/`catch` in direct style) automatically. `CpsConcurrentLogicMonad` adds streaming and parallel operations.

### Two Kinds of Failure

| Failure Type | Meaning | Handling |
|--------------|---------|----------|
| Logical (`mzero`) | No solutions, try next branch | `guard`, `choices`, backtracking |
| Exception (`Throwable`) | Real error | `try`/`catch`, `restore`, `flatMapTry` with state rollback |

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

    def asReader: BlockchainReader       // read-only snapshot (preferred)
    @deprecated def asProvider: BlockchainProvider  // deprecated, use asReader
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

Implements `CpsConcurrentLogicMonad[Scenario, Future]` with all required operations: `pure`, `map`, `flatMap`, `flatMapTry`, `error`, `mzero`, `mplus`, `msplit`, `fsplit`, `withMsplit`, `flattenObserver`.

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

    // Reader — BlockchainReader snapshot for TxBuilder and Gen construction (reads state once)
    def snapshotReader: Scenario[BlockchainReader]
    @deprecated def snapshotProvider: Scenario[BlockchainProvider]  // use snapshotReader

    // Runners
    def run[A](initial: ScenarioState)(s: Scenario[A]): LogicStreamT[Future, (ScenarioState, A)]
    def runAll[A](initial: ScenarioState, maxResults: Int = 1000)(s: Scenario[A]): Future[IndexedSeq[(ScenarioState, A)]]
    def runFirst[A](initial: ScenarioState)(s: Scenario[A]): Future[Option[(ScenarioState, A)]]
}
```

**Internal leaf helper** — DSL methods use `leaf(run)` which creates `Leaf(ScenarioState.empty, run)`. The empty sentinel is overridden by `injectState` during flatMap state threading.

**Two provider flavors:**

- `provider` returns `BlockchainProviderTF[Scenario]` — each call (findUtxos, submit, etc.) reads/writes the *latest* Scenario state. Used by endpoint code (`AuctionEndpoints[Scenario](provider)`).
- `snapshotReader` returns `BlockchainReader` (Future-based) — a read-only snapshot of the current emulator state. Used for `TxBuilder.complete` and Gen construction. (`snapshotProvider` is deprecated.)

### BlockchainProvider Integration (✅ Implemented)

- `BlockchainReaderTF[F[_]]` tagless-final trait for read-only operations
- `BlockchainProviderTF[F[_]] extends BlockchainReaderTF[F[_]]` adds `submit`
- `BlockchainReader extends BlockchainReaderTF[Future]` with captured EC
- `BlockchainProvider extends BlockchainProviderTF[Future] with BlockchainReader`
- `Scenario.provider` returning `BlockchainProviderTF[Scenario]`
- `Scenario.snapshotReader` returning `BlockchainReader` (preferred)
- `Scenario.snapshotProvider` returning `BlockchainProvider` (deprecated)

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

### Design Principle: Future[Seq[Transaction]] as Universal Currency

`TxVariations.enumerate` returns `Future[Seq[Transaction]]` — fully completed and signed transactions, ready to submit.

- **Scenario**: uses it directly — `txs.await` then `choices(txs*).await` (exhaustive branching)
- **ScalaCheck**: `ContractScalaCheckCommands` handles `Await.result` internally (JVM-only detail, hidden from user)

### Core Abstractions (✅ Implemented)

#### TxVariations[S]: Transaction Variations (shared source)

The main trait is async, returning `Future[Seq[Transaction]]`. Uses `TxTemplate` which bundles builder with sponsor and signer:

```scala
/** Transaction variations generator. */
trait TxVariations[S] {
    /**
     * Generate all transaction variations for the given state.
     * TxTemplate provides sponsor and signer.
     */
    def enumerate(
        reader: BlockchainReader,
        state: S,
        txTemplate: TxTemplate
    )(using ExecutionContext): Future[Seq[Transaction]]

    def ++(other: TxVariations[S]): TxVariations[S]
}

/** Sampling-based variations (fuzz testing). */
trait TxSamplingVariations[S] extends TxVariations[S] {
    def gen(
        reader: BlockchainReader,
        state: S,
        txTemplate: TxTemplate
    ): Gen[Future[Transaction]]

    override def enumerate(
        reader: BlockchainReader,
        state: S,
        txTemplate: TxTemplate
    )(using ExecutionContext): Future[Seq[Transaction]] = {
        val futures = List.fill(sampleSize)(gen(reader, state, txTemplate).sample).flatten
        Future.sequence(futures)
    }

    def sampleSize: Int = 20
}
```

**Implementation pattern** for `enumerate`:

```scala
def enumerate(
    reader: BlockchainReader,
    state: S,
    txTemplate: TxTemplate
)(using ExecutionContext): Future[Seq[Transaction]] = {
    val env = reader.cardanoInfo
    TxBuilder(env)
        .spend(utxo, redeemer, script)
        .payTo(...)
        .complete(reader, txTemplate.sponsor)
        .map(b => Seq(b.sign(txTemplate.signer).transaction))
}
```

The sync conversion for ScalaCheck is handled internally by `ContractScalaCheckCommands` — no sync wrapper exposed to users.

#### StandardTxVariations: Common Attack Patterns (✅ Implemented)

`StandardTxVariations` provides factory methods for common attack patterns. Each method returns a `TxVariations[S]` or `TxSamplingVariations[S]`.

```scala
object StandardTxVariations {

    // Default combined variations (most critical attacks)
    def default[S](
        extractUtxo: S => Utxo,
        extractDatum: S => Data,
        redeemer: S => Data,
        script: PlutusScript
    ): TxVariations[S]

    // Extended with datum/address corruption
    def defaultExtended[S](
        extractUtxo: S => Utxo,
        extractDatum: S => Data,
        redeemer: S => Data,
        script: PlutusScript,
        corruptedDatums: S => Gen[Data],
        alternativeAddresses: S => Gen[Address]
    ): TxVariations[S]

    // Individual variations
    def removeContractOutput[S](...)   // Steal attack
    def stealPartialValue[S](...)      // Partial theft (sampling)
    def corruptDatum[S](...)           // Wrong datum (sampling)
    def wrongOutputAddress[S](...)     // Wrong recipient (sampling)
    def duplicateOutput[S](...)        // Double output attack
    def unauthorizedMint[S](...)       // Unauthorized minting (sampling)
    def mintExtra[S](...)              // Over-minting (sampling)
    def aroundDeadline[S](...)         // Timing boundary (sampling)
    def noValidityRange[S](...)        // Missing time constraints
    def wideValidityRange[S](...)      // Overly permissive time
    def wrongRedeemer[S](...)          // Incorrect redeemer (sampling)
    def aroundThreshold[S](...)        // Value boundary (sampling)
    def removeReferenceInput[S](...)   // Missing reference
    def wrongReferenceInput[S](...)    // Stale reference (sampling)
    def doubleSatisfaction[S](...)     // Double satisfaction attack

    // Boundary generators (helpers)
    def valuesAround(threshold: Coin): Gen[Coin]
    def slotsAround(deadline: Long): Gen[Long]
}
```

**Design notes:**
- Output address is automatically taken from the UTXO being spent (`utxo.output.address`)
- No need to pass `scriptAddress` separately — it's derived from the spent UTXO
- Accessible via `TxVariations.standard` for discoverability

**Usage:**
```scala
case class AuctionState(utxo: Utxo)

// Simple usage with default attacks
val variations = TxVariations.standard.default[AuctionState](
    extractUtxo = _.utxo,
    extractDatum = s => s.utxo.output.requireInlineDatum,
    redeemer = _ => BidRedeemer.toData,
    script = auctionScript
)

// Extended with custom generators
val extended = TxVariations.standard.defaultExtended[AuctionState](
    extractUtxo = _.utxo,
    extractDatum = s => s.utxo.output.requireInlineDatum,
    redeemer = _ => BidRedeemer.toData,
    script = auctionScript,
    corruptedDatums = _ => Gen.const(Data.I(0)),
    alternativeAddresses = _ => Gen.const(attackerAddress)
)

// Combine individual variations
val custom = TxVariations.standard.removeContractOutput(...) ++
    TxVariations.standard.duplicateOutput(...)
```

#### ContractStepVariations[S]: Bundled Step Description (shared source)

```scala
trait ContractStepVariations[S] {
    def extractState(reader: BlockchainReader)(using ExecutionContext): Future[S]
    def makeBaseTx(reader: BlockchainReader, state: S)(using ExecutionContext): Future[TxTemplate]
    def variations: TxVariations[S]

    def allVariations(
        reader: BlockchainReader,
        state: S
    )(using ExecutionContext): Future[Seq[Transaction]] =
        makeBaseTx(reader, state).flatMap { txTemplate =>
            variations.enumerate(reader, state, txTemplate)
        }
}
```

`BlockchainReader` gives access to `cardanoInfo` (protocol params, network, slot config), so no need to pass it separately. The `TxTemplate` returned by `makeBaseTx` bundles sponsor and signer.

For ScalaCheck, `ContractScalaCheckCommands` handles `Await.result` internally — users don't see sync wrappers.

### Consuming ContractStepVariations: Two Modes (✅ Implemented)

```scala
// Mode 1: ScalaCheck Commands — JVM-only, stateful property testing
// Await.result handled internally
ContractScalaCheckCommands(initialEmulator, AuctionStep).property()

// Mode 2: Scenario — cross-platform, exhaustive branching
async[Scenario] {
    val reader = Scenario.snapshotReader.await
    val state = step.extractState(reader).await
    val txs = step.allVariations(reader, state).await
    val tx = choices(txs*).await
    Scenario.submit(tx).await
}
```

### Multi-Actor Variations

Since sponsor/signer are bundled in `TxTemplate`, test multiple actors by creating templates for each:

```scala
val actors = Seq(
    TxTemplate(baseBuilder, Alice.address, Alice.signer),
    TxTemplate(baseBuilder, Bob.address, Bob.signer),
    TxTemplate(baseBuilder, Attacker.address, Attacker.signer)
)

val allTxs: Future[Seq[Transaction]] = Future.sequence(
    actors.map(template => variations.enumerate(reader, state, template))
).map(_.flatten)

forAll(Gen.oneOf(allTxs.await)) { tx => ... }
```

### Gen in Scenario (✅ Implemented)

ScenarioState carries an RNG for deterministic replay across branches:

```scala
object Scenario {
    // Sample single value, mzero if generator fails
    def sample[A](gen: Gen[A]): Scenario[A]

    // Sample N values, creating N branches (each with different RNG state)
    def sampleN[A](gen: Gen[A], n: Int = 10): Scenario[A]
}

given CpsMonadConversion[Gen, Scenario] with {
    def apply[A](ga: Gen[A]): Scenario[A] = Scenario.sample(ga)
}
```

### Scenario.check (✅ Implemented)

Inline macro for invariant checking that captures predicate expression and source location:

```scala
// In Scenario object
inline def check(inline condition: Boolean, inline message: String = ""): Scenario[Unit]

// Throws CheckFailure with predicate, message, and SourceLocation on failure
case class CheckFailure(predicate: String, message: String, location: SourceLocation) extends Exception
case class SourceLocation(file: String, line: Int)
```

Usage:
```scala
async[Scenario] {
    val state = extractState(provider).await
    Scenario.check(state.balance >= 0, "balance must be non-negative").await
    Scenario.check(state.owner != Address.empty).await  // message optional
}
```

### ScenarioExplorer (✅ Implemented)

Explores contract state space by applying transaction variations at each step:

```scala
case class Violation(
    predicate: String,      // failed check expression
    message: String,        // check message
    location: SourceLocation,
    path: Seq[Transaction]  // transactions that led to this state
)

object ScenarioExplorer {
    def explore(maxDepth: Int)(
        genTransactions: BlockchainReader => Scenario[Seq[Transaction]]
    ): Scenario[Option[Violation]]
}
```

Usage:
```scala
val results = Scenario.runAll(initial)(
    ScenarioExplorer.explore(maxDepth = 3) { reader =>
        async[Scenario] {
            val state = extractState(reader).await
            Scenario.check(state.balance >= 0).await  // invariant
            buildTransactionVariations(reader, state)
        }
    }
)
val violations = results.flatMap(_._2)  // collect all check failures
```

### TxTemplate (✅ Implemented)

Bundles transaction builder with sponsor and signer:

```scala
case class TxTemplate(
    builder: TxBuilder,
    sponsor: Address,
    signer: TransactionSigner
) {
    def complete(reader: BlockchainReader)(using ExecutionContext): Future[Transaction]
    def mapBuilder(f: TxBuilder => TxBuilder): TxTemplate
    def withSponsor(newSponsor: Address): TxTemplate
    def withSigner(newSigner: TransactionSigner): TxTemplate
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
| CpsConcurrentLogicMonad[Scenario, Future] instance | ✅ Done | `scalus-testkit/shared/.../testing/Scenario.scala` |
| Scenario DSL (choices, guard, sleep, submit, etc.) | ✅ Done | `scalus-testkit/shared/.../testing/Scenario.scala` |
| Scenario.provider (BlockchainProviderTF[Scenario]) | ✅ Done | `scalus-testkit/shared/.../testing/Scenario.scala` |
| Scenario.snapshotReader (BlockchainReader) | ✅ Done | `scalus-testkit/shared/.../testing/Scenario.scala` |
| Scenario.snapshotProvider (deprecated) | ✅ Done | `scalus-testkit/shared/.../testing/Scenario.scala` |
| Runners (run, runAll, runFirst) | ✅ Done | `scalus-testkit/shared/.../testing/Scenario.scala` |
| BlockchainReaderTF[F], BlockchainReader | ✅ Done | `scalus-cardano-ledger/shared/.../node/BlockchainProvider.scala` |
| BlockchainProviderTF[F], BlockchainProvider | ✅ Done | `scalus-cardano-ledger/shared/.../node/BlockchainProvider.scala` |
| dotty-cps-async deps in build.sbt | ✅ Done | `build.sbt` |
| Scenario.sample(gen) | ✅ Done | `scalus-testkit/shared/.../testing/Scenario.scala` |
| Scenario.sampleN(gen, n) | ✅ Done | `scalus-testkit/shared/.../testing/Scenario.scala` |
| Scenario.check (macro) | ✅ Done | `scalus-testkit/shared/.../testing/ScenarioCheck.scala` |
| CheckFailure, SourceLocation | ✅ Done | `scalus-testkit/shared/.../testing/ScenarioCheck.scala` |
| ScenarioExplorer.explore | ✅ Done | `scalus-testkit/shared/.../testing/ScenarioExplorer.scala` |
| Violation | ✅ Done | `scalus-testkit/shared/.../testing/ScenarioExplorer.scala` |
| TxTemplate | ✅ Done | `scalus-testkit/shared/.../testing/TxTemplate.scala` |
| TxVariations[S] | ✅ Done | `scalus-testkit/shared/.../testing/TxVariations.scala` |
| TxSamplingVariations[S] | ✅ Done | `scalus-testkit/shared/.../testing/TxVariations.scala` |
| ContractStepVariations[S] | ✅ Done | `scalus-testkit/shared/.../testing/TxVariations.scala` |
| StandardTxVariations | ✅ Done | `scalus-testkit/shared/.../testing/StandardTxVariations.scala` |
| Boundary generators (valuesAround, slotsAround) | ✅ Done | `scalus-testkit/shared/.../testing/StandardTxVariations.scala` |
| ContractScalaCheckCommands adapter | ✅ Done | `scalus-testkit/jvm/.../testing/ContractScalaCheckCommands.scala` |
| FutureInScenario convenience layer | ⬚ Planned (optional) | — |

## Open Questions

1. ~~**Provider approach**~~ - ✅ Resolved: Tagless-final chosen, `BlockchainProviderTF[F[_]]` trait created as first step

2. **FutureInScenario convenience layer** - Should we add a convenience layer that allows existing `Future`-based endpoint code to work in Scenario tests without modification? This would use runtime type detection but provide a smoother migration path for existing code.

3. ~~**Gen.domain for exhaustive enumeration**~~ - ✅ Resolved: `TxVariations` uses `enumerate` (returns `Seq`) as primary method, `gen` is derived. `TxSamplingVariations` inverts this for large/continuous domains (gen-first, enumerate samples N values). No need to extract domain from `Gen`.

4. ~~**Monad encoding**~~ - ✅ Resolved: Sealed trait ADT chosen over function-based encoding. Supports independent mplus branches, working fsplit, and state injection via sentinel pattern.

5. ~~**TxVariations API refactoring**~~ - ✅ Resolved: API now uses `TxTemplate` which bundles sponsor and signer. `enumerate` takes `(reader, state, txTemplate)` instead of separate sponsor/signer parameters. `BlockchainReader` introduced for read-only operations (preferred over `BlockchainProvider` for APIs that don't need `submit`).
