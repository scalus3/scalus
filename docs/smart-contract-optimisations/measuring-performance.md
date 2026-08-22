Source: https://scalus.org/docs/smart-contract-optimisations/measuring-performance

# Measuring Performance

Always measure before and after optimization.

## Measuring script execution directly

For quick comparisons of script variants, you can evaluate the UPLC directly and get CPU steps, memory, and flat-encoded size:

```scala mdoc:compile-only copy showLineNumbers
import scalus.*, scalus.compiler.{compile, Options}
import scalus.uplc.*, scalus.uplc.eval.*
import scalus.cardano.ledger.{ExUnitPrices, ExUnits, NonNegativeInterval}

given Options = Options.release
given PlutusVM = PlutusVM.makePlutusV3VM()

val sir = compile { /* your validator */ ??? }
val program = sir.toUplcOptimized().plutusV3

// Apply arguments and evaluate
val applied = program // $ datum $ redeemer $ ctxData
val result = applied.deBruijnedProgram.evaluateDebug

result match
    case Result.Success(_, budget, _, _) =>
        val flatSize = applied.flatEncoded.length
        println(s"Flat size: $flatSize bytes")
        println(s"CPU steps: ${budget.steps}")
        println(s"Memory:    ${budget.memory}")
        // Compute execution fee with mainnet prices
        val exPrices = ExUnitPrices(
          priceMemory = NonNegativeInterval(0.0577, precision = 15),
          priceSteps = NonNegativeInterval(0.0000721, precision = 15)
        )
        val execFee = ExUnits(budget.memory, budget.steps).fee(exPrices)
        println(s"Exec fee:  ${execFee.value} lovelace")
        // Approximate total: exec fee + size fee (44 lovelace/byte)
        println(s"Size fee:  ${flatSize * 44} lovelace")
    case Result.Failure(err, _, _, logs) =>
        println(s"Failed: ${err.getMessage}")
```

However, measuring script execution alone can be misleading. The actual transaction fee includes a size component (44 lovelace per byte of transaction), so a script that saves CPU but grows in size may cost more overall. If you have a working smart contract, the most accurate way to measure is to build a complete transaction via the Emulator and check the final fee.

## Measuring via the Emulator

Build a real transaction with `TxBuilder` and the `Emulator`, then inspect the fee and execution units:

```scala mdoc:compile-only copy showLineNumbers
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilder
import scalus.uplc.PlutusV3
import scalus.compiler.Options
import scalus.utils.await

given CardanoInfo = CardanoInfo.mainnet
given Options = Options.release

val compiled = PlutusV3.compile(MyValidator.validate)
val emulator = Emulator(initialUtxos)

// Build and complete the transaction
val tx = TxBuilder(summon[CardanoInfo])
    .spend(scriptUtxo, redeemer, compiled)
    .payTo(recipientAddress, outputValue)
    .complete(emulator, changeAddress)
    .await()
    .sign(signer)
    .transaction

// The transaction fee includes both size and execution costs
val fee: Coin = tx.body.value.fee
println(s"Transaction fee: ${fee.value} lovelace")

// Inspect execution units per redeemer
val redeemers = tx.witnessSet.redeemers.toSeq.flatMap(_.value.toSeq)
redeemers.foreach { r =>
    println(s"  ${r.tag}: CPU=${r.exUnits.steps}, mem=${r.exUnits.memory}")
}
```

See the [Emulator](/docs/testing/emulator) page for full setup details.

## Compiler Improvements Since 1.0.0

Recent compiler work moved most budgets down without any source changes, so re-measure after
upgrading Scalus:

- **Self-application recursion** replaced the Z combinator in the recursion encoding. Measured:
  the Knights benchmark dropped 19.8% memory and 16.1% CPU, the CAPE `fibonacci_25` fee fell
  23%, and typical validators gained 4-12% CPU.
- **Static-argument transformation** (part of `optimizeUplc = true`) stops re-passing loop
  arguments that never change. Across the example corpus: 7.5% memory and 5.2% CPU on average,
  with recursive folds over constant context improving far more.
- **`Value` operations** lower to CIP-153 builtins at PV11, 13-75x cheaper per operation. See
  [Value Builtins](/docs/smart-contract-optimisations/value-builtins).

Because the generated UPLC changes between Scalus versions, script hashes and pinned budgets
change too: tests that assert exact `ExUnits` or script hashes must be re-measured on upgrade,
and deployed scripts keep the hash of the compiler that produced them (use
`Options.plomin`-style pinned targets when you need to reproduce old output).

## What's Next?

- **[Algorithmic Optimisations](/docs/smart-contract-optimisations/algorithmic-optimisations)** — start here for the biggest wins; design patterns that reduce on-chain work asymptotically.
- **[Scala Metaprogramming](/docs/smart-contract-optimisations/scala-metaprogramming)** — once you've measured a hot path, use compile-time evaluation to eliminate it.
