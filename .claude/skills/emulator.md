---
name: Local on-chain development with Emulator
description: Information on leveraging Emulator and TxBuilder to develop and test validators and other on-chain code 
globs: scalus-cardano-ledger/**/Emulator.scala
---

## Overview

When developing on-chain code, one of the effective tools to do so is to use the Emulator, which is 
a local, single-node no-consensus implementation of the Cardano ledger. It includes phase 1 & 2 testing.

Create a provider:

```scala
private def createProvider(): Emulator = {
        // Create multiple UTXOs per player so they have funds after initiate tx
        val initialUtxos = Map(
          
          TransactionInput(genesisHash, 0) -> TransactionOutput(Alice.address, Value.ada(5000)),
          TransactionInput(genesisHash, 1) -> TransactionOutput(Alice.address, Value.ada(5000)),
          
          TransactionInput(genesisHash, 2) -> TransactionOutput(Bob.address, Value.ada(5000)),
          TransactionInput(genesisHash, 3) -> TransactionOutput(Bob.address, Value.ada(5000)),
          
          TransactionInput(genesisHash, 4) -> TransactionOutput(Eve.address, Value.ada(10000))
        )
        Emulator(
          initialUtxos = initialUtxos,
          initialContext = Context.testMainnet(),
        )
    }
```

Submit a transaction:
```scala
val emulator = createProvider()
// Create a transaction to spend some utxos, potentially triggering a script to test
val myTransaction = createTestSpecificTransaction(emulator)
// Submit the transaction, altering the local ledger UTxO state
provider.submit(myTransaction).await() match {
  case Left(value) => fail(s"Couldn't the transaction, error: $value")
  case Right(_)    => ()
}
// Find the UTxOs after the transaction
val utxos = provider.findUtxos(Alice.address).await().toOption.get
```

Important trick -- when creating transactions using the TxBuilder, if you want to test negative cases, i.e. the validator
retuning `false` in expected conditions, override the `PlutusScriptEvaluator`. Otherwise, you won't be able to create a transaction, 
since TxBuilder has to run the validator and expects it _not_ to fail. Do it like so:
```scala
// some instance of CardanoEnv most likely available at call site, use `constMaxBudget`.
val evaluator = PlutusScriptEvaluator.constMaxBudget(cardanoEnv)

// use the evaluator 
TxBuilder(env, evaluator)
  ... // assemble the rest of the transaction
            .complete // or `build`
            .transaction
```
