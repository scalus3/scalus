Source: https://scalus.org/docs/testing/emulator

# Cardano Emulator for Fast Smart Contract Development

The `Emulator` is an in-memory Cardano node that **validates transactions** and **executes Plutus scripts** — just like a real node, but instantly. No Docker, no network, no waiting.

It performs both **Phase 1 validation** (transaction structure, signatures, fees, value conservation) and **Phase 2 validation** (Plutus script execution with cost tracking).

## Quick Start

```scala
import scalus.cardano.node.Emulator
import scalus.cardano.ledger.rules.Context

// Create emulator with pre-funded addresses
val emulator = Emulator.withAddresses(Seq(Alice.address, Bob.address))

// Or with custom initial UTxOs
val emulator = Emulator(
  initialUtxos = Map(
    input(0) -> Output(Alice.address, Value.ada(1000)),
    input(1) -> Output(Bob.address, Value.ada(500))
  )
)
```

Use it with `TxBuilder` just like any other `Provider`:

```scala
val tx = TxBuilder(testEnv)
  .payTo(Bob.address, Value.ada(10))
  .complete(emulator, Alice.address)
  .await()
  .sign(Alice.signer)
  .transaction

emulator.submit(tx).await() match {
  case Right(txHash) => println(s"Success: $txHash")
  case Left(error)   => println(s"Failed: $error")
}
```

## What It Does

The Emulator provides **real transaction validation** and **real script execution**:

| Feature | Description |
|---------|-------------|
| **Validates Transactions** | Runs 20+ Cardano ledger rules — fees, signatures, value conservation, execution limits |
| **Executes Plutus Scripts** | Runs V1, V2, V3 scripts via the Scalus UPLC interpreter with full cost model evaluation |
| **Tracks Execution Costs** | Reports CPU and memory usage against protocol limits |
| **Manages UTxO State** | Updates inputs/outputs atomically on successful transactions |
| **Handles Collateral** | Processes collateral correctly when scripts fail (isValid=false) |
| **Validates Native Scripts** | Checks multisig and timelock native scripts |

### Ledger Rules

The Emulator uses the [Scalus Ledger Rules Framework](/docs/ledger/ledger-rules) — the same validators and mutators that implement Cardano's UTXOW state transition rules.

  Rules are auto-discovered at startup. You can customize which rules run by passing custom `validators` and `mutators` to the constructor.

## API Reference

### Constructor

```scala
class Emulator(
    initialUtxos: Utxos = Map.empty,
    initialContext: Context = Context.testMainnet(),
    val validators: Iterable[STS.Validator] = Emulator.defaultValidators,
    val mutators: Iterable[STS.Mutator] = Emulator.defaultMutators,
    initialCertState: CertState = CertState.empty,
    initialDatums: Map[DataHash, Data] = Map.empty,
    initialAppliedTxLog: Vector[AppliedTx] = Vector.empty
) extends EmulatorBase // which extends BlockchainProvider
```

This constructor builds the default [`ChainConfig.instant`](#blocks-mempool-and-forks) preset.
`initialAppliedTxLog` is deprecated: the log has no blocks, so `txStatus` and `checkTransaction`
report its transactions `NotFound`. Use `snapshot()` to copy an emulator with its blocks. The
constructor that takes a `ChainConfig` (in place of the log) has no default arguments – Java and
JavaScript cannot use Scala defaults – so a Scala caller either gives every argument or uses a
factory such as `Emulator.withAddresses(addresses, value, chainConfig)` below.

### Factory Methods

```scala
// Quick setup with funded addresses (10,000 ADA each by default)
val emulator = Emulator.withAddresses(Seq(addr1, addr2))

// Custom initial value per address
val emulator = Emulator.withAddresses(Seq(addr1, addr2), Value.ada(50_000))
```

### Methods

| Method | Description |
|--------|-------------|
| `submit(tx)` | Submit transaction, returns `Future[Either[SubmitError, TransactionHash]]` |
| `findUtxo(input)` | Look up single UTxO by transaction input |
| `findUtxos(address, ...)` | Query UTxOs by address with optional filters |
| `setSlot(slot)` | Advance the current slot (for time-based validation) |
| `snapshot()` | Create a point-in-time copy of the emulator |
| `utxos` | Get current UTxO set |
| `produceBlock()` | Forge a block from the mempool; see [Blocks, mempool and forks](#blocks-mempool-and-forks) |
| `rollback(n)` | Remove the last `n` blocks of the selected chain |
| `txStatus(hash)` | Where a transaction stands: `Pending`, `Confirmed`, `Dropped`, `RolledBack` or `NotFound` |
| `tip` | The `ChainTip` of the selected chain, or `ChainTip.origin` before the first block |
| `findUtxos(query, view)` | Query a specific `ChainView`: `Tip`, `Mempool` or `Stable(depth)` |

### Error Handling

When a transaction fails validation, you get detailed error information:

```scala
emulator.submit(tx).await() match {
  case Right(txHash) =>
    println(s"Transaction submitted: $txHash")
  case Left(error: NodeSubmitError) =>
    println(s"Rejected by ledger rules: ${error.message}")
  case Left(error) =>
    println(s"Error: ${error.message}")
}
```

## Working with Time

Use `setSlot()` to test time-dependent validators:

```scala
// Set slot to test validity intervals
emulator.setSlot(1000L)

// Transaction with validity range [500, 1500] will pass
val tx = TxBuilder(testEnv)
  .validFrom(500L)
  .validTo(1500L)
  .payTo(Bob.address, Value.ada(10))
  .complete(emulator, Alice.address)
  .await()
  .transaction

// Transaction with validity range [2000, 3000] will fail
val invalidTx = TxBuilder(testEnv)
  .validFrom(2000L)
  .validTo(3000L)
  // ...
```

## Blocks, mempool and forks

The `Emulator` behaves like a small Cardano node: a submitted transaction waits in a mempool, a
block makes it visible, and a fork can roll it back. `ChainConfig.blockProduction` picks the
preset:

```scala
import scalus.cardano.node.Emulator
import scalus.cardano.node.emulator.ChainConfig

// The default: every submit forges a block at once, so every example above still holds.
val emulator = Emulator.withAddresses(Seq(Alice.address, Bob.address), Value.ada(1000), ChainConfig.instant)

// Only an explicit produceBlock() forges a block; a submitted tx waits in the mempool.
// Only Alice is funded, so Bob's address shows a payment only once a block holds it.
val manual = Emulator.withAddresses(Seq(Alice.address), Value.ada(1000), ChainConfig.manual)
```

`ChainConfig`'s constructor is private: build one from `ChainConfig.instant` or
`ChainConfig.manual` and change a field with its `with` method, for example
`ChainConfig.manual.withSecurityParam(10)`, `withAwaitTx(...)` or `withOrphanTxs(...)`. Pass it to
a factory such as `Emulator.withAddresses(addresses, value, chainConfig)` or
`Emulator.withState(initState, context, chainConfig)`.

In `Manual`, `setSlot` below the tip slot throws `IllegalArgumentException`: a forge moves the
current slot to the block's slot, so a test cannot go back before it.

### The delay case

With `ChainConfig.manual`, a query keeps showing the old state until `produceBlock()` makes the
submitted transaction's outputs real:

```scala
val tx = TxBuilder(testEnv)
  .payTo(Bob.address, Value.ada(10))
  .complete(manual, Alice.address)
  .await()
  .sign(Alice.signer)
  .transaction

manual.submitSync(tx)                                            // Right(tx.id)
manual.findUtxosSync(UtxoQuery(UtxoSource.FromAddress(Bob.address))) // still empty
manual.txStatus(tx.id)                                            // TxStatus.Pending

val point = manual.produceBlock()
manual.findUtxosSync(UtxoQuery(UtxoSource.FromAddress(Bob.address))) // the payment is there
manual.txStatus(tx.id)                                            // TxStatus.Confirmed(1, point)
```

This is the behaviour a real provider has: Blockfrost, Koios and Kupo show only block-confirmed
state, so dApp code that reads right after `submit` must cope with the old state still showing.

### Tx status

`txStatus(hash): TxStatus` answers where a transaction stands:

| Case | Meaning |
|---|---|
| `Pending` | in the mempool |
| `Confirmed(depth, point)` | in a block on the selected chain; the tip is depth 1 |
| `Dropped(reason)` | removed from the mempool; `reason` is `Expired`, `InputsSpent` or `Invalid(error)` |
| `RolledBack` | was in a block that left the chain, under the `Drop` orphan policy |
| `NotFound` | in neither the mempool nor a block |

`checkTransaction`, the method off-the-shelf dApp code already calls, maps `Pending` and
`Confirmed` to themselves and every other case to `NotFound`. A transaction known only from an
applied-tx log the Emulator was built with has no block, so both report it `NotFound`; `hasTx` and
`getTransaction` read the log and still find it.

### Views

A query normally reads the tip – the selected chain's last block, what a real provider shows.
`findUtxos(query, view)` (and its synchronous twin `findUtxosSync`) reads something else instead:

```scala
manual.findUtxosSync(query, ChainView.Tip)       // the chain; same as findUtxosSync(query)
manual.findUtxosSync(query, ChainView.Mempool)   // the chain plus the pending txs
manual.findUtxosSync(query, ChainView.Stable(2)) // the block two deep; the tip is depth 1
```

`Stable(d)` returns `Left(ChainViewError.DepthOutOfRange(...))` for a `d` below 1 or past genesis,
and `Left(ChainViewError.SnapshotDropped(d))` once a rollback or the chain's growth has pruned that
block's snapshot past `securityParam`.

### TTL drop

Revalidation removes a mempool transaction once it is no longer valid – most often because its
validity interval ended:

```scala
manual.setSlot(99)
val tx = TxBuilder(testEnv)
  .payTo(Bob.address, Value.ada(10))
  .validTo(Instant.ofEpochMilli(testEnv.slotConfig.slotToTime(100)))
  .complete(manual, Alice.address)
  .await()
  .sign(Alice.signer)
  .transaction

manual.submitSync(tx)      // Right(tx.id)
manual.setSlot(100)
manual.txStatus(tx.id)     // TxStatus.Dropped(DropReason.Expired)
```

### A rival double spend

`produceBlock(on, txs)` forges a block anywhere in the block tree, not from the mempool – this is
how a test builds a rival chain. Once a branch grows past the current chain, the Emulator switches
to it, and the transactions the switch orphans follow `ChainConfig.orphanTxs`:

```scala
// paymentFrom and doubleSpendOf are test helpers: each builds and signs a payment that spends
// the given input. input0 and input1 are two UTxOs of Alice's.
val p1 = manual.produceBlock()
val txA = paymentFrom(input0)
val txB = paymentFrom(input1)
manual.submitSync(txA)
manual.submitSync(txB)
val p2 = manual.produceBlock()          // A and B are confirmed in p2

// A rival that double-spends B's input, forged on the branch at p1 instead of the tip.
val rival = doubleSpendOf(input1)
val Right(r2) = manual.produceBlock(p1, Seq(rival))
manual.tip.point // still p2: the rival branch (height 2) only ties the current chain

val Right(r3) = manual.produceBlock(r2, Seq.empty) // the rival branch is now longer
manual.tip.point // r3: the Emulator switched

// With the default ReturnToMempool:
manual.txStatus(txB.id)     // TxStatus.Dropped(DropReason.InputsSpent): B's input is now rival's
manual.txStatus(txA.id)     // TxStatus.Pending: A is back in the mempool
manual.txStatus(rival.id)   // TxStatus.Confirmed(2, r2)
```

With `ChainConfig.manual.withOrphanTxs(OrphanTxPolicy.Drop)` instead, A and B end up
`TxStatus.RolledBack` rather than returning to the mempool.

### rollback(n)

```scala
manual.rollback(1)     // removes the last block; its txs follow orphanTxs
manual.rollback(0)     // Left(RollbackError.NotPositive(0))
manual.rollback(10_000) // Left(RollbackError.TooDeep(10_000, securityParam))
```

`rollback` never moves the current slot, and a switch that follows it can make the tip a block
that was never the old tip. The Emulator keeps the last `securityParam` (`k`, default 2160)
blocks' snapshots plus one more as the anchor, the same bound a real node's LedgerDB has, so a
rollback past that point fails with `TooDeep`.

In `Instant`, with the default `ReturnToMempool`, `rollback(1)` at an unchanged slot changes
nothing you can see: the removed block's transactions return to the mempool, and `Instant` forges
them at once into the same block, with the same hash. To see a rollback, use `ChainConfig.manual`,
use `withOrphanTxs(OrphanTxPolicy.Drop)`, or move the slot first:

```scala
val instant = Emulator.withAddresses(Seq(Alice.address), Value.ada(1000), ChainConfig.instant)
instant.submitSync(tx)
val before = instant.tip
instant.rollback(1)    // Right(before.point): the same block again
instant.tick(1)
instant.rollback(1)    // Right(p): a new block, one slot later, with the same transactions
```

### awaitTx and pollForConfirmation

`pollForConfirmation` and `submitAndPoll` never sleep – the Emulator has virtual time – so
off-the-shelf dApp code that already calls them keeps working against `ChainConfig.manual`.
`ChainConfig.awaitTx` controls what they do while a transaction is pending:

- `AwaitTxPolicy.ProduceBlock` (the default): forge a block, up to `maxAttempts` times, until the
  transaction is no longer `Pending`.
- `AwaitTxPolicy.WaitOnly`: report the status as it is and forge nothing – for a test that drives
  blocks itself and wants to see a transaction sit in the mempool.

### Watching the chain

`StreamingEmulator` turns the Emulator's transitions into ChainSync-style events: a block forged
on the tip is a `RollForward` carrying the real `Block`; a fork switch or a `rollback` is a
`RollBackward` to the common ancestor, then a `RollForward` per block of the new chain.
`EmulatorBase.diff(from, to)` computes the same events between any two tips directly, without a
subscription.

`StreamingEmulator(emulator, n)` advertises `Some(n)` (default `Some(0)`) as its rollback
horizon, so a `noRollback` subscriber waits `n` blocks. It still keeps up to the Emulator's k
(`ChainConfig.securityParam`, default 2160) blocks, so a `rollback` or a fork within that depth
sends `RolledBack` to every subscription that got an event past the rollback point, `noRollback`
subscriptions included.

### Documented deviations from a real node

- In `Instant`, two blocks can share a slot: several `submit` calls at the same slot each get
  their own block, so a test does not need to tick between them.
- In `Instant`, moving the slot backward with `setSlot` before an epoch boundary no block has
  crossed restores the donations that boundary moved into the treasury – the boundary never
  happened on the stored chain. A `setSlot` back below the tip slot, followed by a `submit`, still
  forges the next block at the tip's later slot, so that block's recorded `AppliedTx.slot` can be
  earlier than the block itself, and in a different epoch.
- A forged block carries fixed placeholder bytes for the consensus fields a node checks
  (`issuerVkey`, `vrfVkey`, `vrfResult`, `operationalCert`, `bodySignature`); nothing in the
  Emulator verifies them.
- When several branches are longer than the current chain, chain selection picks the tallest, then
  the lowest block hash. A real node breaks that tie by VRF, which the Emulator does not model.

## Thread Safety

The `Emulator` is thread-safe using `AtomicReference` for state management. Concurrent transaction submissions use compare-and-swap for atomic updates:

```scala
// Safe to use from multiple threads
val futures = (1 to 10).map { i =>
  Future {
    val tx = buildTransaction(i)
    emulator.submit(tx).await()
  }
}
```

## Customizing Rules

Run with a subset of validators for specific test scenarios:

```scala
import scalus.cardano.ledger.rules.*

// Only run Plutus script execution, skip other validations
val minimalEmulator = Emulator(
  initialUtxos = myUtxos,
  validators = Set.empty,  // No validators
  mutators = Set(PlutusScriptsTransactionMutator)  // Only script execution
)

// Add custom validators
val customEmulator = Emulator(
  validators = Emulator.defaultValidators + MyCustomValidator,
  mutators = Emulator.defaultMutators
)
```

## Using the Emulator from Java

The Emulator has a Java-friendly surface: factories take `java.util.List`, state accessors return
Java collections, lookups return `null` instead of `Option`, and submission reports a
`SubmitResult` instead of `Either` — so expected failures need no try/catch. `CompletableFuture`
variants (`submitAsync`, `findUtxosForAddressAsync`) mirror the asynchronous
`BlockchainProvider` shape; for the Emulator they complete immediately.

```java
import scalus.cardano.address.Address;
import scalus.cardano.ledger.*;
import scalus.cardano.node.*;
import scalus.cardano.node.emulator.*;
import java.util.List;
import java.util.Map;

// Create an emulator with two funded addresses (10,000 ADA each)
Emulator emulator = Emulator.withAddresses(List.of(alice, bob));

// Or build the initial state explicitly
EmulatorInitialState state = EmulatorInitialState.builder()
        .putUtxo(input, output)
        .addStakeRegistration(EmulatorStakeRegistration.of(credential, Coin.zero()))
        .build();
Emulator emulator2 = Emulator.withState(state);

// Submit and inspect the outcome — no Either, no exceptions for rejections
SubmitResult result = emulator.trySubmit(signedTx);
if (result.isSuccess()) {
    System.out.println("Applied: " + result.getTxHashOrNull().toHex());
} else {
    System.out.println("Rejected: " + result.getErrorMessageOrNull());
}

// Query state with Java collections
Map<TransactionInput, TransactionOutput> utxos = emulator.getUtxos();
List<Utxo> aliceUtxos = emulator.findUtxosForAddress(alice);
Transaction applied = emulator.getTransactionOrNull(result.getTxHashOrNull());

// Control time
emulator.setSlot(1000L);
emulator.tick(5);
long slot = emulator.getCurrentSlot();

// CompletableFuture variant — same shape as asynchronous providers
emulator.submitAsync(anotherTx).thenAccept(r ->
    System.out.println("async result: " + r.isSuccess()));
```

### Blocks and forks from Java

Java builds a `ChainConfig` from the static `ChainConfig.instant()` or `ChainConfig.manual()` and
its `with` methods. Java sees a case of a Scala 3 enum only as a static field of its module class,
so the enums of the chain add static methods for their cases: `ChainView.tip()`, `mempool()` and
`stable(depth)`; `TxStatus.pending()`, `rolledBack()` and `notFound()`; `DropReason.expired()` and
`inputsSpent()`. The methods whose Scala form returns an `Either` or takes a `Seq` have a Java form
that returns an `EmulatorResult` or takes a `java.util.List`:

```java
ChainConfig config = ChainConfig.manual().withSecurityParam(10L);
Emulator manual = Emulator.withAddresses(List.of(alice, bob), Value.ada(1000), config);

ChainPoint point = manual.produceBlock();
EmulatorResult<ForgeError, ChainPoint> onBranch = manual.tryProduceBlock(point, List.of(rivalTx));
EmulatorResult<RollbackError, ChainPoint> rolledBack = manual.tryRollback(1);

List<Transaction> pending = manual.getMempoolTxs();
List<ChainEvent> events = manual.getDiff(ChainTip.origin(), manual.tip());

boolean isPending = manual.txStatus(txHash) == TxStatus.pending();
TxStatus status = manual.txStatus(txHash);
boolean expired = status instanceof TxStatus.Dropped
        && ((TxStatus.Dropped) status).reason() == DropReason.expired();
EmulatorResult<ChainViewError, List<Utxo>> atDepth =
    manual.findUtxosForAddress(alice, ChainView.stable(2));
```

## When to Use Emulator

Both Emulator and Yaci DevKit **validate transactions** and **execute Plutus scripts**. The difference is implementation:

| Scenario | Emulator | Yaci DevKit |
|----------|:--------:|:-----------:|
| Transaction validation | ✓ | ✓ |
| Plutus script execution | ✓ | ✓ |
| Unit tests | ✓ | |
| Rapid development iteration | ✓ | |
| CI/CD (speed matters) | ✓ | |
| Real Haskell Cardano node | | ✓ |
| Complete ledger rule set | | ✓ |
| Pre-deployment confidence | | ✓ |

**Use Emulator** for fast feedback during development. Instant script execution, real validation, no setup overhead.

**Use [Local Devnet](/docs/testing/local-devnet)** when you need the actual Haskell Cardano node for final validation before deployment.

## Example: Testing a Minting Policy

```scala
import scalus.compiler.compile
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilder

test("minting policy validates token name") {
  val emulator = Emulator.withAddresses(Seq(Alice.address))

  // Compile minting policy
  val policy = compile { (redeemer: Data, ctx: Data) =>
    val sc = ctx.to[ScriptContext]
    val tokenName = redeemer.to[TokenName]
    // Validate only "MyToken" can be minted
    require(tokenName == TokenName.fromString("MyToken"))
  }
  val script = Script.PlutusV3(policy.toUplc().plutusV3.cborByteString)

  // Valid mint - should succeed
  val validTx = TxBuilder(testEnv)
    .mint(script, Map(AssetName.fromString("MyToken") -> 100L), TokenName.fromString("MyToken"))
    .payTo(Alice.address, Value.asset(script.scriptHash, AssetName.fromString("MyToken"), 100))
    .complete(emulator, Alice.address)
    .await()
    .sign(Alice.signer)
    .transaction

  emulator.submit(validTx).await() shouldBe a[Right[_, _]]

  // Invalid mint - should fail
  val invalidTx = TxBuilder(testEnv)
    .mint(script, Map(AssetName.fromString("WrongToken") -> 100L), TokenName.fromString("WrongToken"))
    // ...

  emulator.submit(invalidTx).await() shouldBe a[Left[_, _]]
}
```

## Pre-initializing UTxOs from JSON

You can initialize the emulator with UTxOs declared in a JSON file using `Preconfiguration`. Address keys can be test party names (`"alice"`, `"bob"`) or raw bech32 addresses:

```json
{
  "utxo": {
    "alice": [
      { "ada": 10000 },
      { "ada": 5, "datum": { "int": 42 } }
    ],
    "bob": [{ "ada": 5000, "datum_hash": "abcd..." }],
    "addr_test1qz...": [{ "ada": 100, "tx_id": "abcd...", "idx": 0 }]
  }
}
```

Each UTxO entry supports:
- `ada` (required) — amount in ADA
- `datum` — inline datum as a Plutus JSON Data value
- `datum_cbor` — inline datum as a CBOR hex string
- `datum_hash` — datum hash hex (produces a datum-hash reference, not inline)
- `tx_id`, `idx` — explicit transaction input; if absent, a genesis hash and auto-index are used

```scala
import scalus.testing.{ImmutableEmulator, Preconfiguration}

// From a JSON string
val emulator = ImmutableEmulator.fromJson(json)

// Or parse and resolve separately
val config = Preconfiguration.fromJson(json)
val utxos = Preconfiguration.resolveUtxos(config)
val emulator = Emulator(initialUtxos = utxos)
```

## See Also

- [JS/TS Emulator](/docs/testing/js-emulator) — Emulator for JavaScript and TypeScript
- [Local Devnet](/docs/testing/local-devnet) — Docker-based devnet for integration testing
- [Provider](/docs/ledger/provider) — The Provider interface
- [Ledger Rules](/docs/ledger/ledger-rules) — Transaction validation rules
- [Transaction Builder](/docs/transactions) — Building transactions
