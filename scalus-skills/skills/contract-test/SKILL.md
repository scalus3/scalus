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

**Property-based testing:** ScalusTest provides generators (e.g. `genByteStringOfN`); use ScalaCheck `forAll` for on-chain function properties.

**Negative cases:** build the failing transaction with `PlutusScriptEvaluator.constMaxBudget(env)` so `TxBuilder` does not reject it during construction, submit to the Emulator, and assert `Left(...)`.

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
