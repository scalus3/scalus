---
name: contract-test
description: Guide for testing Scalus smart contracts. Use when writing unit, property, budget, or Emulator tests for Cardano validators.
---

# Scalus Smart Contract Testing

Before writing test code from memory, fetch `https://scalus.org/llms-api.txt` and check the testkit signatures you plan to use.

## Documentation

In the Scalus repo, read `scalus-site/content/<path>.mdx`; in any other project, fetch `https://scalus.org/docs/<path>.md`.

**Testing fundamentals:**
- `testing/unit-testing` - ScalusTest trait, property-based testing
- `smart-contracts/evaluating-script` - Script evaluation patterns
- `testing/debugging` - Debugging techniques

**Integration testing:**
- `testing/emulator` - in-memory Emulator testing
- `testing/local-devnet` - Yaci DevKit integration tests
- Use the `local-development` skill for the full Emulator + TxBuilder loop.

## Test Examples

Study existing tests before writing new ones:
- Fetch `https://scalus.org/llms-examples.txt` - validators with their tests; `HtlcTest.scala` is the reference style.
- In the Scalus repo: `scalus-examples/jvm/src/test/scala/scalus/examples/`.

## Testing Patterns

**Emulator test with ScalusTest** (the standard pattern; see HtlcTest):

```scala
class MyValidatorTest extends AnyFunSuite, ScalusTest {
    private given env: CardanoInfo = TestUtil.testEnvironment
    private val contract = MyContract.compiled.withErrorTraces

    test("valid spend succeeds") {
        val provider = Emulator.withAddresses(Seq(Alice.address, Bob.address))
        val utxos = provider.findUtxos(address = Alice.address).await().toOption.get
        // build a transaction with txBuilder, then:
        assert(provider.submit(tx).await().isRight)
    }
}
```

**Assertions from `scalus.testing.kit`:**
- `assertScriptFail(expectedError) { buildTx }` - expect a validator failure
- `assertEvalResult(expected)(program)` - check an evaluation result

**Budget assertions from `scalus.testing.dsl.EvalTestDsl`:**
- `.assertBudgetWithin(memory, steps)` - upper bound
- `.assertBudgetEquals(memory, steps)` - exact pin (baseline tests)

**Pin convention:**
- Every new prelude/stdlib operation gets a `budget:` test in `scalus-core` that pins mem, cpu
  and the mainnet fee: `assertEvalWithBudgetAndFee(code, arg, expected, ExUnits(memory = m, steps = s), Coin(fee))`
  from `scalus.testing.kit.EvalTestKit`. Pass the input as `arg` (it goes in as `Data`) so constant
  folding cannot remove the work being measured.
- Example validator tests pin the exact `ExUnits` of every transaction, and the fee where the test
  already does (`result.budget.fee == Coin(2398)` in `HtlcTest`). Wrap a pin in
  `ScalaCompilerVersion.baseline(pre38 = ..., since38 = ...)` only when the two compiler generations
  differ; otherwise write the bare value.
- Format: `ExUnits(memory = 27127, steps = 11_541882)`, named parameters, `_` at the million boundary.

**Property-based testing:** ScalusTest provides generators (e.g. `genByteStringOfN`); use ScalaCheck `forAll` for on-chain function properties.

**Negative cases:** three idioms; pick one per test, do not mix them:
- Builder path (`HtlcTest`): keep the default evaluator and wrap the build in
  `assertScriptFail(expectedError) { buildTx }`. The builder runs the script while balancing,
  throws, and the helper matches `expectedError` against the script logs, so pass the
  validator's own error constant.
- Emulator path (`EditableNftValidatorTest`): build with
  `TxBuilder(env, PlutusScriptEvaluator.constMaxBudget(env))` so the builder skips script
  evaluation, then `provider.submit(tx).await()` and `assert(result.isLeft)`. Use it when the
  transaction must get past the builder (for example, to reach the ledger's own checks).
- Unit path (`VestingValidatorTest`): build a `ScriptContext` by hand, run
  `compiled.runScript(ctx)` and `assert(result.isFailure, "...")`. For an on-chain helper
  evaluated directly, `assertEvalFailsWithMessage[OnchainError](msg)(expr)` checks the message.

```scala
assertScriptFail(HtlcValidator.InvalidReceiverPreimage) {
    txCreator.reveal(utxos = utxos, lockedUtxo = lockedUtxo, preimage = wrongPreimage, ...)
}
```

**Negative-test convention:** every safety operation the validator uses gets one `Fail: ...` test
where the UNSAFE input fails. The transaction is valid apart from the one change. Minimum set:
1. `validFromOrFail` / `validToOrFail`: an unbounded validity range (`Interval.always`) fails.
2. `hasSameTokensAndAtLeastAda` and exact value checks: an output whose value is below the expected
   amount fails; native tokens stripped from the continuing output fails.
3. `findContinuingOutputOrFail`: an output with the script payment credential but a different
   staking credential fails; a second output to the script address fails.
4. `findUniqueOrFail` on inputs: a second own input in the same transaction fails (each script run
   must fail, see "double satisfaction" in `VestingValidatorTest`).
5. `mint.hasOnly` / `onlyBurnsUnder`: an extra token name under the policy fails; a burn action that
   mints nothing fails.
6. `hasNft` / `hasPaidTagged`: quantity 2 of the beacon fails; one tagged output claimed by two
   script instances fails.

Shape of one case (unit path, `VestingValidatorTest` "Fail: continuing output redirects the staking credential"):

```scala
val hijacked = Address(
  ScriptCredential(contractHash),
  Option.Some(StakingCredential.StakingHash(Credential.PubKeyCredential(attackerPKH)))
)
val outputs = List(TxOut(hijacked, Value.lovelace(remaining), OutputDatum.OutputDatum(datum.toData)))
val result = compiled.runScript(ScriptContext(txInfo = txInfo(outputs), redeemer = redeemer.toData, scriptInfo = spending))
assert(result.isFailure, "Redirecting the staking credential must fail")
```

## Running Tests

In a user project: `sbt test` or `scala-cli test .`.
In the Scalus repo:

```bash
sbtn scalusJVM/test              # Core tests
sbtn scalusExamplesJVM/test      # Example tests
sbtn scalusCardanoLedgerJVM/test # Ledger tests
sbtn scalusCardanoLedgerIt/test  # Integration tests (requires Docker)
```

## Test Organization

- Tests mirror the source structure.
- Use ScalaTest (`AnyFunSuite`) with ScalaCheck for property-based tests.
- Fork the JVM for integration tests (`Test/fork := true`).
