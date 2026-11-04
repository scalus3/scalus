---
name: local-development
description: Use when developing or testing Scalus smart contracts with the local Emulator and TxBuilder. Covers how to create an Emulator provider, submit transactions, test negative validator cases, and test confirmation delay and rollbacks.
---

## Overview

When developing on-chain code, the Emulator is a local, single-node no-consensus implementation of
the Cardano ledger. It includes phase 1 & 2 validation.

## File structure for testing a validator

Four files are typically involved:

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
via `TestUtil.testEnvironment`. Spending transactions need an evaluator.
`evaluator = PlutusScriptEvaluator.constMaxBudget(env)` skips script evaluation while the builder
balances the transaction. The Emulator still runs the scripts when you submit.

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
            Input(genesisHash, 0) -> Output(Alice.address, Value.ada(5000)),
            Input(genesisHash, 1) -> Output(Alice.address, Value.ada(5000)),
            Input(genesisHash, 2) -> Output(Bob.address,   Value.ada(5000)),
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

## Testing delay and rollbacks with the Manual preset (Scalus 1.5.0 or newer)

By default the Emulator forges a block on every `submit`, so a test never sees a transaction
`Pending`. Use `ChainConfig.manual` to test code that depends on confirmation delay, or on
surviving a fork (a rollback that returns a transaction to the mempool, or drops it):

```scala
import scalus.cardano.node.Emulator
import scalus.cardano.node.emulator.ChainConfig

val provider = Emulator.withAddresses(addresses, value, ChainConfig.manual)
provider.submit(tx).await()                 // Right(txHash); not yet visible
provider.txStatus(tx.id)                    // TxStatus.Pending
provider.produceBlock()                     // forges a block from the mempool
provider.txStatus(tx.id)                    // TxStatus.Confirmed(1, point)
```

Use it when the thing under test is the *absence* of a confirmed transaction (a provider that
polls, a UI that shows "pending", a retry path) or a rollback (`provider.rollback(n)`, or a rival
block via `provider.produceBlock(on, txs)`). For everything else, `ChainConfig.instant` (the
default) is simpler and is what every other example on this page uses.

Pitfalls:
- Calling `produceBlock()` after every `submit` gives one block per tx, like `ChainConfig.instant`,
  but at a new slot each time: Manual forges at `max(current slot, tip slot + 1)` and moves the
  current slot there; Instant never moves the slot. Submit everything that should land in one
  block first, then call `produceBlock()` once.
- In Manual, `setSlot` below the tip slot throws `IllegalArgumentException`. Set the slot before
  you forge, not after.
- `txStatus(hash)` is the one safe assertion for "still pending" / "confirmed" / "dropped" /
  "rolled back"; `findUtxos` alone cannot tell "pending" apart from "never submitted".
- Off-the-shelf code that calls `pollForConfirmation` or `submitAndPoll` still works under
  `Manual`: with the default `AwaitTxPolicy.ProduceBlock` it forges blocks until the transaction
  leaves the mempool, up to `maxAttempts` times.
- Under `ChainConfig.instant`, `rollback(1)` at an unchanged slot returns the txs to the mempool
  and forges them at once into the same block, with the same hash, so nothing changes. Test a
  rollback under `Manual`, with `withOrphanTxs(OrphanTxPolicy.Drop)`, or after `tick(1)`.
- The default `securityParam` (`k = 2160`) is already far deeper than any test builds; `rollback(n)`
  and a branch `produceBlock(on, txs)` need no larger value. Lower it
  (`ChainConfig.manual.withSecurityParam(2)`) only to test the `TooDeep` boundary on purpose.

## Ledger rules the Emulator checks (Scalus 1.4.0 or newer)

The Emulator rejects what a node rejects. A test that sets up stake or governance state hits these:

- A withdrawal from a key-hash reward account needs a DRep delegation first, from protocol
  version 10: `TxBuilder.delegateVoteToDRep(stakeAddress, DRep.AlwaysAbstain)`. Script-hash
  accounts are exempt, so the withdraw-zero pattern needs no DRep.
- A delegation needs a registered pool or DRep. Register it in an earlier transaction first.
- A withdrawal subtracts from the balance; a drained account stays registered with balance 0.
- A UTxO added with `provider.addUtxo(input, output)` that holds only a datum hash: call
  `provider.addDatum(datum)` so the Emulator finds the datum.

From Scalus 1.5.0:

- To assert *why* a transaction fails, use `provider.ledgerFailures(tx)`: every ledger-rule
  failure, in the ledger's order. `failures.flatMap(TransactionException.ledgerNames)` gives the cardano-ledger names,
  such as `MissingRedeemers`. `submit` reports only the first failure.
- Crossing an epoch boundary (`setSlot`, `tick`) runs the epoch rules: rewards, pool retirement,
  proposal expiry, ratification and enactment. A test that crosses two or more boundaries and
  asserts an absolute treasury, reserves or reward balance must account for them.
- A PlutusV3 transaction that spends a UTxO and also references it fails, as on the ledger.
