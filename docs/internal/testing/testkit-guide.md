# Testkit User Guide

## Overview

The Scalus testkit provides a framework for testing Cardano smart contracts by exploring transaction variations around boundary values. You implement a `ContractStepVariations[S]` trait:

1. **`extractState`** — what to read from the blockchain
2. **`baseTx`** — the "happy path" transaction
3. **`variations`** — boundary mutations to explore, depends on current state (see [tx-variations.md](tx-variations.md))

Then run with ScalaCheck (`forAll`, `Commands`) or Scenario (exhaustive exploration), adding consumer-specific assertions.

## Quick Start

### 1. Define Contract State

Define a case class for your contract's on-chain state:

```scala
case class AuctionState(
    currentBid: Coin,
    deadline: SlotNo,
    topBidder: PubKeyHash,
    auctionUtxo: Utxo
)
```

### 2. Define Variations

`TxVariations[S]` enumerates boundary-value transactions. The primary method is `enumerate`, returning `Seq[TxBuilder]`. `gen` (for ScalaCheck) is derived automatically.

```scala
import scalus.testing.{TxVariations, BoundaryValues}

val bidVariations: TxVariations[AuctionState] = new TxVariations[AuctionState] {
    def enumerate(provider, state, base) = for
        amount <- BoundaryValues.aroundValue(state.currentBid)
        actor  <- BoundaryValues.fundedActors(provider, allActors, minBalance = 100)
        timing <- BoundaryValues.aroundDeadline(state.deadline)
    yield TxBuilder(provider.cardanoInfo)
        .spend(state.auctionUtxo, BidRedeemer(amount))
        .payTo(auctionScriptAddress, updatedDatum(amount, actor))
        .payTo(previousBidder(state), refundValue(state))
        .validFrom(timing)
        .sponsor(actor.address)
        .signer(actor.signer)
}
```

The `base` parameter is available to read from (what UTxO is being spent, what the default structure looks like). Variations typically rebuild the transaction with modified fields.

Variations compose via `++`:

```scala
val allVariations = bidVariations ++ claimVariations ++ valueFuzz
```

### 3. Implement ContractStepVariations

Bundle state extraction, base transaction, and variations into one trait:

```scala
object AuctionStep extends ContractStepVariations[AuctionState] {

    def extractState(provider: BlockchainProvider): Future[AuctionState] =
        provider.findUtxos(auctionScriptAddress).map { result =>
            val utxo = result.getOrThrow.head
            val datum = utxo._2.datum
            AuctionState(
                currentBid = datum.field("topBid").toCoin,
                deadline = datum.field("deadline").toSlot,
                topBidder = datum.field("topBidder").toPubKeyHash,
                auctionUtxo = utxo
            )
        }(provider.executionContext)

    def baseTx(info: CardanoInfo, state: AuctionState): TxBuilder =
        TxBuilder(info)
            .spend(state.auctionUtxo, BidRedeemer(state.currentBid + 1))
            .payTo(auctionScriptAddress, updatedDatum(state.currentBid + 1, Alice))
            .payTo(previousBidder(state), refundValue(state))
            .validFrom(state.deadline - 10)
            .sponsor(Alice.address)
            .signer(Alice.signer)

    def variations(state: AuctionState): TxVariations[AuctionState] =
        if currentSlot < state.deadline then bidVariations
        else claimVariations
}
```

Note: `baseTx` takes `CardanoInfo` (not `BlockchainProvider`) — it only needs protocol parameters for TxBuilder construction. All TxBuilder methods except `complete` are synchronous.

### 4. Run Tests

#### ScalaCheck forAll

Simplest mode — random sampling with shrinking against a fixed state.

```scala
test("bid boundary values") {
    val provider: BlockchainProvider = emulator
    val state = AuctionStep.extractState(provider).await()

    given Arbitrary[TxBuilder] = Arbitrary(AuctionStep.allVariationsGen(provider, state))

    forAll { (incompleteTx: TxBuilder) =>
        val result = Try(incompleteTx.signAndComplete(emulator).await())
        // test-specific assertions
        val amount = extractAmount(incompleteTx)
        if amount > state.currentBid then assert(result.isSuccess)
        else assert(result.isFailure)
    }
}
```

#### ScalaCheck Commands

Stateful property testing — generates sequences of commands, with shrinking.

```scala
test("auction command sequence") {
    ContractScalaCheckCommands(initialEmulator, AuctionStep,
        postcondition = (state, result) => {
            val s = AuctionStep.extractState(state.asProvider).value.get.get
            Prop(s.currentBid >= 0)
        }
    ).property(threadCount = 2).check()
}
```

#### Scenario — Exhaustive Exploration

Explores all combinations of boundary values at bounded depth using a logic monad.

```scala
test("auction exhaustive boundaries") {
    val scenario = async[Scenario] {
        setupAuction(Alice, minBid = 100, deadline = 200).await

        ScenarioExplorer.explore(maxDepth = 4, AuctionStep,
            check = (_, state, depth) =>
                assert(state.currentBid >= 0, s"negative bid at depth $depth")
        ).await
    }

    Scenario.runAll(initialState)(scenario)
}
```

## Defining Variations

### TxVariations — Enumerate-First (Boundary Testing)

The primary method is `enumerate` returning `Seq[TxBuilder]`. `gen` is derived as `Gen.oneOf(enumerate(...))`.

```scala
trait TxVariations[S] {
    def enumerate(provider: BlockchainProvider, state: S, base: TxBuilder): Seq[TxBuilder]
    def gen(provider: BlockchainProvider, state: S, base: TxBuilder): Gen[TxBuilder] // derived
    def ++(other: TxVariations[S]): TxVariations[S]  // compose
}
```

- **provider** — query blockchain state (UTxOs, slot, params). Synchronous on Emulator.
- **state** — contract-specific state `S` extracted by `extractState`.
- **base** — the "happy path" TxBuilder to vary around.

### TxSamplingVariations — Gen-First (Fuzz Testing)

For large/continuous domains that can't be enumerated (e.g., arbitrary `Value` with random token bundles):

```scala
val valueFuzz: TxSamplingVariations[AuctionState] = new TxSamplingVariations[AuctionState] {
    def sampleSize = 30
    def gen(provider, state, base) = for
        adaAmount <- Gen.oneOf(Coin(0), minUtxo, state.currentBid - 1, state.currentBid + 1)
        extraTokens <- Gen.someOf(knownPolicies)
        tokenAmount <- Gen.choose(0L, 1_000_000L)
    yield TxBuilder(provider.cardanoInfo)
        .spend(state.auctionUtxo, BidRedeemer(adaAmount))
        .payTo(auctionScriptAddress, Value(adaAmount, extraTokens.map(_ -> tokenAmount)))
        .payTo(previousBidder(state), refundValue(state))
        .validFrom(state.deadline - 10)
        .sponsor(Alice.address)
        .signer(Alice.signer)
}
```

`enumerate` samples N values from `gen` for bounded exploration in Scenario mode.

### Using BoundaryValues Helpers

```scala
object BoundaryValues {
    def aroundValue(current: Coin): Seq[Coin]              // below, equal, above
    def aroundValueExtended(current: Coin, minUtxo: Coin): Seq[Coin]  // zero, minUTxO, below, equal, above
    def aroundDeadline(deadline: SlotNo): Seq[SlotNo]      // before, at, after
    def fundedActors(provider: BlockchainProvider, actors: Seq[TestActor], minBalance: Coin): Seq[TestActor]
    def signersAround(authorized: Seq[TestActor], outsiders: Seq[TestActor]): Seq[TestActor]
}
```

### Composing Multiple Dimensions

Use Seq for-comprehension to compose boundary values across multiple dimensions:

```scala
val variations: TxVariations[MyState] = new TxVariations[MyState] {
    def enumerate(provider, state, base) = for
        amount <- BoundaryValues.aroundValue(state.threshold)
        timing <- BoundaryValues.aroundDeadline(state.deadline)
        actor  <- BoundaryValues.signersAround(state.authorized, Seq(outsider))
    yield TxBuilder(provider.cardanoInfo)
        .spend(state.scriptUtxo, MyRedeemer(amount))
        .payTo(scriptAddress, updatedDatum(amount))
        .validFrom(timing)
        .sponsor(actor.address)
        .signer(actor.signer)
}
```

Each dimension has ~3 values (below/at/above), so 3 dimensions = 27 combinations.

### Composing Variations

Combine multiple `TxVariations` with `++`:

```scala
// State-dependent composition
def variations(state: AuctionState): TxVariations[AuctionState] =
    if currentSlot < state.deadline then bidVariations ++ valueFuzz
    else claimVariations

// Add malicious variation via refinement
new AuctionStep {
    override def variations(state: AuctionState) =
        super.variations(state) ++ maliciousBid
}
```

### Standard Variations

See [tx-variations.md](tx-variations.md) for a catalog of standard attack/boundary variations:
- Input manipulation (add/remove/duplicate UTxOs)
- Output manipulation (steal funds, corrupt datum)
- Minting manipulation (extra mint, unauthorized mint)
- Timing manipulation (expired, too early)
- Signer manipulation (wrong signer, missing signer)

## ContractStepVariations

A trait that bundles the three things that travel together:

```scala
trait ContractStepVariations[S] {
    def extractState(provider: BlockchainProvider): Future[S]
    def baseTx(info: CardanoInfo, state: S): TxBuilder
    def variations(state: S): TxVariations[S]

    // derived
    def allVariationsEnum(provider: BlockchainProvider, state: S): Seq[TxBuilder]
    def allVariationsGen(provider: BlockchainProvider, state: S): Gen[TxBuilder]
}
```

Implement as an `object` for your contract:

```scala
object HtlcStep extends ContractStepVariations[HtlcState] {
    def extractState(provider: BlockchainProvider) = ...
    def baseTx(info: CardanoInfo, state: HtlcState) = ...
    def variations(state: HtlcState) = unlockVariations ++ timeoutVariations
}
```

Override individual methods via anonymous refinement:

```scala
// Bid-only testing
new AuctionStep { override def variations(s: AuctionState) = bidVariations }

// Add malicious variation
new AuctionStep {
    override def variations(s: AuctionState) =
        super.variations(s) ++ stealFunds
}
```

## Complexity Control

With `C` categories per dimension, `D` dimensions, and depth `N`:
- Per step: `C^D` combinations (e.g., 3×3×3 = 27)
- Multi-step: up to `(C^D)^N` total paths

Keep categories at 2–4 per dimension. Use `guard` to prune impossible branches and `once` to stop at first violation in Scenario mode.

## Testing Patterns

### Pattern: Single Action Boundaries

Test one contract action with all boundary combinations:

```scala
test("bid boundaries") {
    val provider = emulator
    val state = AuctionStep.extractState(provider).await()
    given Arbitrary[TxBuilder] = Arbitrary(AuctionStep.allVariationsGen(provider, state))
    forAll { (tx: TxBuilder) => ... }
}
```

### Pattern: Multi-Step State Exploration

Test sequences of actions where each step changes the state:

```scala
val scenario = async[Scenario] {
    setupContract(...).await
    ScenarioExplorer.explore(maxDepth = 3, MyStep,
        check = (_, state, _) => assert(invariant(state)))
    .await
}
```

### Pattern: Attack Simulation

Add malicious variations alongside normal ones:

```scala
val stealFunds: TxVariations[MyState] = new TxVariations[MyState] {
    def enumerate(provider, state, base) = Seq(
        TxBuilder(provider.cardanoInfo)
            .spend(state.scriptUtxo, MyRedeemer.claim)
            .payTo(attacker.address, state.scriptUtxo.value)  // steal everything
            .sponsor(attacker.address)
            .signer(attacker.signer)
    )
}

ScenarioExplorer.explore(maxDepth = 2,
    new MyStep {
        override def variations(s: MyState) = super.variations(s) ++ stealFunds
    },
    check = (_, state, _) => assert(state.value > 0, "funds stolen!"))
```

### Pattern: Time-Dependent Behavior

Test behavior before and after deadlines:

```scala
val scenario = async[Scenario] {
    setupHtlc(...).await

    // Try claiming before timeout — should fail
    Scenario.sleep(1).await
    val earlyResult = Try {
        val provider = Scenario.snapshotProvider.await
        val state = HtlcStep.extractState(provider).await
        val tx = HtlcStep.baseTx(provider.cardanoInfo, state)
            .signAndComplete(provider).value.get.get
        Scenario.submit(tx).await
    }
    assert(earlyResult.isFailure)

    // Try claiming after timeout — should succeed
    Scenario.sleep(100).await
    val lateResult = Try { ... }
    assert(lateResult.isSuccess)
}
```
