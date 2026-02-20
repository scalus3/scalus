---
name: local-development-loop
description: Use when developing or testing Scalus smart contracts with the local Emulator and TxBuilder. Covers how to create an Emulator provider, submit transactions, and test negative validator cases.
---

## Overview

When developing on-chain code, the Emulator is a local, single-node no-consensus implementation of
the Cardano ledger. It includes phase 1 & 2 validation.

## File structure for testing a validator

Three files are typically involved:

- **Validator** — the on-chain logic (e.g. `MyValidator.scala`)
- **Contract** — compiles the validator into a `PlutusV3` value (e.g. `MyContract.scala`)
- **Transactions** — uses `TxBuilder` to create validator-specific transactions (e.g. `MyTransactions.scala`)
- **Test** — wires everything together using the Emulator (e.g. `MyValidatorTest.scala`)

### Contract file

```scala
private given Options = Options.release
lazy val MyContract = PlutusV3.compile(MyValidator.validate)
```

For parameterized validators, apply the parameter after compilation:

```scala
val applied = MyContract.withErrorTraces(myParam)   // applies param + enables traces
val script  = Script.PlutusV3(applied.program.cborByteString)
val scriptAddress = applied.address(network)
```

### Transactions file

`CardanoInfo` (aliased as `env`) carries network/protocol config and is provided by `ScalusTest`
via `TestUtil.testEnvironment`. Pass `evaluator = PlutusScriptEvaluator.constMaxBudget(env)` when
the `TxBuilder` needs to run scripts (i.e. for spending transactions).

```scala
case class MyTransactions(
    env: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    contract: PlutusV3[Data => Unit]  
) {
    def script: Script.PlutusV3 = contract.script
    val scriptAddress: Address  = contract.address(env.network)

    // Necessary to have funds locked behind the contract under test
    def sendToScript(utxos: Utxos, datum: MyDatum, sponsor: Address, signer: TransactionSigner): Transaction =
        TxBuilder(env)
            .spend(Utxo(utxos.head))
            .payTo(scriptAddress, Value.ada(10), datum) // or pass the ada amount in a parameter
            .complete(availableUtxos = utxos, sponsor = sponsor)
            .sign(signer)
            .transaction

    def spendFromScript(
        utxos: Utxos,
        scriptUtxo: Utxo,
        redeemer: MyRedeemer,
        signerPkh: AddrKeyHash,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction =
        TxBuilder(env, evaluator)
            .spend(scriptUtxo, redeemer, script)
            .spend(Utxo(utxos.head))
            .payTo(sponsor, scriptUtxo.output.value)
            .complete(availableUtxos = utxos, sponsor = sponsor)
            .sign(signer)
            .transaction
}
```

### Test file

```scala
// ScalusTest provides: PlutusVM, Alice/Bob/Eve/Charles/Dave parties (via Party.*),
//                       cardanoEnv, genesisHash, assertScriptFail, and ArbitraryInstances
class MyValidatorTest extends AnyFunSuite, ScalusTest {
    import MyValidatorTest.{*, given}

    test("valid redeemer succeeds") {
        val provider = createProvider()
        // ... build and submit transactions
    }
}

object MyValidatorTest extends ScalusTest {
    private given env: CardanoInfo = TestUtil.testEnvironment
    private val compiledContract   = MyContract.withErrorTraces
    private val scriptAddress      = compiledContract.address(env.network)

    private val txCreator = MyTransactions(
        env       = env,
        evaluator = PlutusScriptEvaluator.constMaxBudget(env),
        contract  = compiledContract
    )

    private def createProvider(): Emulator = {
        // genesisHash is a zero-hash sentinel used as the "genesis" input
        val initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> TransactionOutput(Alice.address, Value.ada(5000)),
            TransactionInput(genesisHash, 1) -> TransactionOutput(Alice.address, Value.ada(5000)),
            TransactionInput(genesisHash, 2) -> TransactionOutput(Bob.address,   Value.ada(5000)),
        )
        Emulator(initialUtxos = initialUtxos, initialContext = Context.testMainnet())
    }
}
```

## Submitting a transaction

```scala
provider.submit(tx).await() match {
    case Left(value) => fail(s"Transaction failed: $value")
    case Right(_)    => ()
}
```

## Testing negative cases (validator expected to fail)

Override `PlutusScriptEvaluator` so `TxBuilder` won't reject the transaction during construction:

```scala
val evaluator = PlutusScriptEvaluator.constMaxBudget(env)

TxBuilder(env, evaluator)
    .spend(scriptUtxo, badRedeemer, script, Set(signerPkh))
    ...
    .complete(...) // or `build`
    .sign(signer)
    .transaction
// Then submit to the Emulator and assert Left(SubmitError.ScriptFailure(...))
```
