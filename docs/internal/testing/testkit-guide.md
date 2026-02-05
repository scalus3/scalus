# Testkit User Guide

## Overview

The Scalus testkit provides a framework for testing Cardano smart contracts by exploring transaction variations around boundary values. You implement a `ContractStepVariations[S]` trait:

1. **`extractState`** — what to read from the blockchain
2. **`makeBaseTx`** — the "happy path" transaction template (builder + sponsor + signer)
3. **`variations`** — boundary mutations to explore, depends on current state (see [tx-variations.md](tx-variations.md))

Then run with ScalaCheck (`forAll`, `Commands`) or Scenario (exhaustive exploration), adding consumer-specific assertions.

Optionally:
4. **`slotDelays`** — slot delays to explore (e.g., `Seq(10L, 100L)`) for time-dependent testing

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

`TxVariations[S]` enumerates transaction variations. The easiest approach is to use standard variations:

```scala
import scalus.testing.TxVariations

// Use pre-built attack patterns
val bidVariations = TxVariations.standard.default[AuctionState](
    extractUtxo = _.auctionUtxo,
    extractDatum = s => updatedDatum(s.currentBid + 1),
    redeemer = _ => BidRedeemer.toData,
    script = auctionScript
)
```

For custom variations, implement the trait directly:

```scala
val customVariation: TxVariations[AuctionState] = new TxVariations[AuctionState] {
    override def enumerate(reader, state, txTemplate)(using ExecutionContext) = {
        val txs = for amount <- TxVariations.standard.valuesAround(state.currentBid) yield
            TxBuilder(reader.cardanoInfo)
                .spend(state.auctionUtxo, BidRedeemer(amount).toData, auctionScript)
                .payToScript(auctionScriptAddress, updatedDatum(amount), auctionValue)
                .payTo(previousBidder(state), refundValue(state))
                .validFrom(state.deadline - 10)

        Future.sequence(txs.map(_.complete(reader, txTemplate.sponsor)
            .map(_.sign(txTemplate.signer).transaction)))
    }
}
```

Variations compose via `++`:

```scala
val allVariations = bidVariations ++ claimVariations ++ customVariation
```

### 3. Implement ContractStepVariations

Bundle state extraction, base transaction template, and variations into one trait:

```scala
object AuctionStep extends ContractStepVariations[AuctionState] {

    def extractState(reader: BlockchainReader)(using ExecutionContext): Future[AuctionState] =
        reader.findUtxos(auctionScriptAddress).map { result =>
            val utxo = result.getOrThrow.head
            val datum = AuctionDatum.fromData(utxo.output.requireInlineDatum)
            AuctionState(
                currentBid = datum.topBid,
                deadline = datum.deadline,
                topBidder = datum.topBidder,
                auctionUtxo = utxo
            )
        }

    def makeBaseTx(reader: BlockchainReader, state: AuctionState)(using ExecutionContext): Future[TxTemplate] =
        Future.successful(
            TxTemplate(
                builder = TxBuilder(reader.cardanoInfo)
                    .spend(state.auctionUtxo, BidRedeemer(state.currentBid + 1).toData, auctionScript)
                    .payToScript(auctionScriptAddress, updatedDatum(state.currentBid + 1, Alice), auctionValue)
                    .payTo(previousBidder(state), refundValue(state))
                    .validFrom(state.deadline - 10),
                sponsor = Alice.address,
                signer = Alice.signer
            )
        )

    def variations: TxVariations[AuctionState] =
        TxVariations.standard.default[AuctionState](
            extractUtxo = _.auctionUtxo,
            extractDatum = s => updatedDatum(s.currentBid + 1, Alice),
            redeemer = _ => BidRedeemer.toData,
            script = auctionScript
        )
}
```

Note: `makeBaseTx` returns `Future[TxTemplate]` which bundles the builder with sponsor and signer. `variations` returns a value (not a method taking state) — state is passed to individual variation methods at enumerate time.

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

Stateful property testing — generates sequences of commands, with shrinking. Uses `step.allActions` to generate both transaction submissions and slot advancements.

```scala
test("auction command sequence") {
    val commands = ContractScalaCheckCommands(initialEmulator, AuctionStep) {
        (reader, state) => Future.successful(Prop(state.currentBid >= 0))
    }
    commands.property().check()
}
```

Slot advancement is controlled by overriding `slotDelays` on the step — no need to pass a slot generator to `ContractScalaCheckCommands`.

#### Scenario — Exhaustive Exploration

Explores all combinations of boundary values at bounded depth using a logic monad. The step function receives a `BlockchainReader` and performs one interaction using normal Scenario operations. Actions (`submit`, `sleep`) are automatically logged.

```scala
test("auction exhaustive boundaries") {
    val scenario = async[Scenario] {
        setupAuction(Alice, minBid = 100, deadline = 200).await

        val violation = Scenario.explore(maxDepth = 4) { reader =>
            async[Scenario] {
                val state = AuctionStep.extractState(reader).await
                Scenario.check(state.currentBid >= 0, "negative bid").await
                val txs = AuctionStep.allVariations(reader, state).await
                val tx = Scenario.fromCollection(txs).await
                val result = Scenario.submit(tx).await
                result match
                    case Right(_) => ()
                    case Left(_) => Scenario.fail[Unit].await
            }
        }.await
        violation.foreach(v => fail(s"Violation: ${v.message} at ${v.location}, path: ${v.path}"))
    }

    Scenario.runAll(initialState)(scenario)
}
```

If a `Scenario.check` fails, `ScenarioExplorer` returns a `Violation` containing the action path (all `StepAction.Submit` and `StepAction.Wait` entries) that led to the failure.

## Defining Variations

### TxVariations — Enumerate-First (Boundary Testing)

The primary method is `enumerate` returning `Future[Seq[Transaction]]` — fully completed, signed transactions ready to submit.

```scala
trait TxVariations[S] {
    def enumerate(
        reader: BlockchainReader,
        state: S,
        txTemplate: TxTemplate
    )(using ExecutionContext): Future[Seq[Transaction]]

    def ++(other: TxVariations[S]): TxVariations[S]  // compose
}
```

- **reader** — query blockchain state (UTxOs, slot, params) — read-only, no submit
- **state** — contract-specific state `S` extracted by `extractState`
- **txTemplate** — bundles sponsor (pays fees/collateral) and signer

### TxSamplingVariations — Gen-First (Fuzz Testing)

For large/continuous domains that can't be enumerated (e.g., arbitrary `Value` with random token bundles):

```scala
trait TxSamplingVariations[S] extends TxVariations[S] {
    def gen(
        reader: BlockchainReader,
        state: S,
        txTemplate: TxTemplate
    ): Gen[Future[Transaction]]

    def sampleSize: Int = 20  // samples for enumerate
}
```

Example implementation:

```scala
val valueFuzz: TxSamplingVariations[AuctionState] = new TxSamplingVariations[AuctionState] {
    override def sampleSize = 30

    def gen(reader, state, txTemplate) = for
        adaAmount <- Gen.oneOf(Coin(0), minUtxo, state.currentBid - 1, state.currentBid + 1)
        extraTokens <- Gen.someOf(knownPolicies)
        tokenAmount <- Gen.choose(0L, 1_000_000L)
    yield {
        given ExecutionContext = reader.executionContext
        TxBuilder(reader.cardanoInfo)
            .spend(state.auctionUtxo, BidRedeemer(adaAmount), auctionScript)
            .payToScript(auctionScriptAddress, updatedDatum, Value(adaAmount, ...))
            .complete(reader, txTemplate.sponsor)
            .map(_.sign(txTemplate.signer).transaction)
    }
}
```

`enumerate` samples N values from `gen` for bounded exploration in Scenario mode.

### Using Boundary Generators

`StandardTxVariations` provides helper generators for boundary testing:

```scala
// Generate values around a threshold (below, equal, above)
TxVariations.standard.valuesAround(threshold: Coin): Gen[Coin]

// Generate slots around a deadline (before, at, after)
TxVariations.standard.slotsAround(deadline: Long): Gen[Long]
```

### Composing Multiple Dimensions

Use for-comprehension to compose boundary values across multiple dimensions:

```scala
val variations: TxVariations[MyState] = new TxVariations[MyState] {
    override def enumerate(reader, state, txTemplate)(using ExecutionContext) = {
        val amounts = TxVariations.standard.valuesAround(state.threshold).sample.toSeq
        val timings = TxVariations.standard.slotsAround(state.deadline).sample.toSeq

        val txBuilders = for
            amount <- amounts
            timing <- timings
        yield TxBuilder(reader.cardanoInfo)
            .spend(state.scriptUtxo, MyRedeemer(amount).toData, myScript)
            .payToScript(scriptAddress, updatedDatum(amount), outputValue)
            .validFrom(timing)

        Future.sequence(txBuilders.map(_.complete(reader, txTemplate.sponsor)
            .map(_.sign(txTemplate.signer).transaction)))
    }
}
```

Each dimension has ~3 values (below/at/above), so 3 dimensions = 27 combinations.

### Composing Variations

Combine multiple `TxVariations` with `++`:

```scala
// Combine standard attacks with custom variations
val allVariations = TxVariations.standard.default(...) ++ customVariation

// Extend via refinement
new MyStep {
    override def variations = super.variations ++ additionalAttacks
}
```

### Standard Variations

Use `TxVariations.standard` to access pre-built attack patterns. The `default` method combines common attack vectors with minimal configuration:

```scala
import scalus.testing.TxVariations

case class ContractState(utxo: Utxo)

// Minimal setup - covers steal, duplicate output, partial theft
val defaultVariations = TxVariations.standard.default[ContractState](
    extractUtxo = _.utxo,
    extractDatum = s => s.utxo.output.requireInlineDatum,
    redeemer = _ => MyRedeemer.toData,
    script = myScript
)

// Extended - adds corrupted datum and wrong address testing
val extendedVariations = TxVariations.standard.defaultExtended[ContractState](
    extractUtxo = _.utxo,
    extractDatum = s => s.utxo.output.requireInlineDatum,
    redeemer = _ => MyRedeemer.toData,
    script = myScript,
    corruptedDatums = _ => Gen.const(Data.I(BigInt(-1))),  // invalid datum
    alternativeAddresses = _ => Gen.const(attackerAddress)
)
```

Individual variations available via `TxVariations.standard`:
- `removeContractOutput` — steal attack (no output)
- `stealPartialValue` — return less value than expected
- `corruptDatum` — wrong datum in output
- `wrongOutputAddress` — send to wrong recipient
- `duplicateOutput` — split into two outputs
- `unauthorizedMint` — mint without authorization
- `mintExtra` — mint more than allowed
- `aroundDeadline` — test timing boundaries
- `wrongRedeemer` — use invalid redeemer
- `doubleSatisfaction` — satisfy one validator, steal from another

See [tx-variations.md](tx-variations.md) for detailed documentation of each variation.

## ContractStepVariations

A trait that bundles the things that travel together:

```scala
trait ContractStepVariations[S] {
    def extractState(reader: BlockchainReader)(using ExecutionContext): Future[S]
    def makeBaseTx(reader: BlockchainReader, state: S)(using ExecutionContext): Future[TxTemplate]
    def variations: TxVariations[S]

    // convenience — builds template and enumerates variations
    def allVariations(reader: BlockchainReader, state: S)(using ExecutionContext): Future[Seq[Transaction]]

    // slot delays to explore at this step (default: empty)
    def slotDelays(state: S): Seq[Long] = Seq.empty

    // all actions — combines Submit(tx) for each variation + Wait(slots) for each delay
    def allActions(reader: BlockchainReader, state: S)(using ExecutionContext): Future[Seq[StepAction]]
}
```

### StepAction

`StepAction` is the universal step currency consumed by both `ScenarioExplorer` and `ContractScalaCheckCommands`:

```scala
sealed trait StepAction
object StepAction {
    case class Submit(tx: Transaction) extends StepAction
    case class Wait(slots: Long) extends StepAction
}
```

`allActions` returns `Submit` for each transaction variation plus `Wait` for each slot delay. This means time-dependent testing is configured on the step, not on the consumer.

### Implementing a Step

```scala
object HtlcStep extends ContractStepVariations[HtlcState] {
    def extractState(reader: BlockchainReader)(using ExecutionContext) = ...
    def makeBaseTx(reader: BlockchainReader, state: HtlcState)(using ExecutionContext) = ...
    def variations = TxVariations.standard.default[HtlcState](
        extractUtxo = _.utxo,
        extractDatum = s => s.utxo.output.requireInlineDatum,
        redeemer = _ => HtlcRedeemer.Unlock.toData,
        script = htlcScript
    )

    // Time-dependent: explore advancing 10 and 100 slots
    override def slotDelays(state: HtlcState) = Seq(10L, 100L)
}
```

Override individual methods via anonymous refinement:

```scala
// Extend with custom attack variation
new HtlcStep {
    override def variations =
        super.variations ++ customAttackVariation
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
    Scenario.explore(maxDepth = 3) { reader =>
        async[Scenario] {
            val state = MyStep.extractState(reader).await
            Scenario.check(invariant(state)).await
            val txs = MyStep.allVariations(reader, state).await
            val tx = Scenario.fromCollection(txs).await
            val result = Scenario.submit(tx).await
            result match
                case Right(_) => ()
                case Left(_) => Scenario.fail[Unit].await
        }
    }.await
}
```

### Pattern: Attack Simulation

Add malicious variations alongside standard ones:

```scala
// Use standard steal variation
val standardAttacks = TxVariations.standard.default[MyState](
    extractUtxo = _.scriptUtxo,
    extractDatum = s => s.scriptUtxo.output.requireInlineDatum,
    redeemer = _ => MyRedeemer.claim.toData,
    script = myScript
)

// Or create custom attack
val customAttack: TxVariations[MyState] = new TxVariations[MyState] {
    override def enumerate(reader, state, txTemplate)(using ExecutionContext) = {
        TxBuilder(reader.cardanoInfo)
            .spend(state.scriptUtxo, MyRedeemer.claim.toData, myScript)
            .payTo(attackerAddress, state.scriptUtxo.output.value)  // steal to attacker
            .complete(reader, txTemplate.sponsor)
            .map(b => Seq(b.sign(txTemplate.signer).transaction))
    }
}

// Combine and test
val allAttacks = standardAttacks ++ customAttack
```

### Pattern: Multi-UTXO State (Double Satisfaction Testing)

When testing contracts that may have multiple UTXOs at the same address, model state as a collection of all open UTXOs. This enables testing double satisfaction attacks where one transaction tries to spend multiple UTXOs while only satisfying one validator:

```scala
// State includes all open UTXOs at the contract address
case class MultiUtxoState(
    openUtxos: Seq[Utxo],
    datums: Seq[MyDatum]  // parsed datums for each UTXO
)

object MultiUtxoStep extends ContractStepVariations[MultiUtxoState] {

    def extractState(reader: BlockchainReader)(using ExecutionContext): Future[MultiUtxoState] =
        reader.findUtxos(scriptAddress).map { result =>
            val utxos = result.getOrThrow
            val datums = utxos.map(u => MyDatum.fromData(u.output.requireInlineDatum))
            MultiUtxoState(utxos, datums)
        }

    def makeBaseTx(reader: BlockchainReader, state: MultiUtxoState)(using ExecutionContext) = {
        // Normal case: spend first UTXO only
        val utxo = state.openUtxos.head
        Future.successful(
            TxTemplate(
                builder = TxBuilder(reader.cardanoInfo)
                    .spend(utxo, myRedeemer, script)
                    .payToScript(scriptAddress, state.datums.head.toData, utxo.output.value),
                sponsor = Alice.address,
                signer = Alice.signer
            )
        )
    }

    def variations: TxVariations[MultiUtxoState] =
        TxVariations.standard.default[MultiUtxoState](
            extractUtxo = _.openUtxos.head,
            extractDatum = s => s.datums.head.toData,
            redeemer = _ => MyRedeemer.toData,
            script = myScript
        ) ++ doubleSatisfactionVariation
}

// Test spending multiple UTXOs in one transaction
val doubleSatisfactionVariation: TxVariations[MultiUtxoState] = new TxVariations[MultiUtxoState] {
    override def enumerate(
        reader: BlockchainReader,
        state: MultiUtxoState,
        txTemplate: TxTemplate
    )(using ExecutionContext): Future[Seq[Transaction]] = {
        if state.openUtxos.size < 2 then Future.successful(Seq.empty)
        else {
            // Spend two UTXOs, but only create output satisfying one
            val (utxo1, utxo2) = (state.openUtxos(0), state.openUtxos(1))
            val totalValue = utxo1.output.value + utxo2.output.value

            val tx = TxBuilder(reader.cardanoInfo)
                .spend(utxo1, myRedeemer, script)
                .spend(utxo2, myRedeemer, script)
                // Only one output - steals from second UTXO
                .payToScript(scriptAddress, state.datums.head.toData, utxo1.output.value)
                .payTo(attackerAddress, utxo2.output.value)  // steal second

            tx.complete(reader, txTemplate.sponsor).map { completedTx =>
                Seq(completedTx.sign(txTemplate.signer).transaction)
            }
        }
    }
}
```

This pattern tests that contracts correctly enforce independent validation of each UTXO spend, even when multiple UTXOs are consumed in one transaction.

### Pattern: Time-Dependent Behavior

Test behavior before and after deadlines:

```scala
val scenario = async[Scenario] {
    setupHtlc(...).await

    // Try claiming before timeout — should fail
    Scenario.sleep(1).await
    val earlyResult = Try {
        val reader = Scenario.snapshotReader.await
        val state = HtlcStep.extractState(reader).await
        val txTemplate = HtlcStep.makeBaseTx(reader, state).await
        val tx = txTemplate.complete(reader).await
        Scenario.submit(tx).await
    }
    assert(earlyResult.isFailure)

    // Try claiming after timeout — should succeed
    Scenario.sleep(100).await
    val lateResult = Try { ... }
    assert(lateResult.isSuccess)
}
```
