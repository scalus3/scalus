---
description: Guide for testing Scalus smart contracts
---

When helping with smart contract testing, read the relevant documentation first.

## Documentation Paths

**Testing fundamentals:**
- `scalus-site/content/smart-contract/testing.mdx` - ScalusTest trait, property-based testing
- `scalus-site/content/smart-contract/evaluating-script.mdx` - Script evaluation patterns
- `scalus-site/content/smart-contract/debugging.mdx` - Debugging techniques

**Integration testing:**
- `scalus-site/content/ledger/emulator.mdx` - Emulator testing
- `scalus-site/content/ledger/yaci-devkit.mdx` - YaciDevKit integration tests

## Test Examples

Study existing tests before writing new ones:
- `scalus-core/shared/src/test/scala/scalus/` - Core tests
- `scalus-examples/shared/src/test/scala/scalus/` - Example contract tests
- `scalus-cardano-ledger-it/src/test/scala/scalus/` - Integration tests

## Testing Patterns

**Unit testing with ScalusTest:**
```scala
class MyValidatorTest extends AnyFunSuite with ScalusTest {
    test("validator accepts valid input") {
        assertEval(MyValidator.validate(validInput))
    }
}
```

**Evaluation assertions:**
- `assertEval(expression)` - verify boolean evaluates to true
- `assertEvalEq(code, expected)` - verify equality
- `assertEvalThrows(code)` - verify failure

**Property-based testing:**
```scala
test("property holds for all inputs") {
    forAll { (value: BigInt) =>
        assertEval(someProperty(value))
    }
}
```

**Budget analysis with EvalTestKit:**
```scala
class BudgetTest extends AnyFunSuite with EvalTestKit {
    test("budget within limits") {
        val result = evaluateDebug(compiled, args)
        assert(result.budget.memory < limit)
    }
}
```

**Emulator testing:**
- Use `Emulator` for transaction-level testing
- Use `YaciDevKit` for full integration tests with local node

## Running Tests

```bash
sbtn scalusJVM/test              # Core tests
sbtn scalusExamplesJVM/test      # Example tests
sbtn scalusCardanoLedgerJVM/test # Ledger tests
sbtn scalusCardanoLedgerIt/test  # Integration tests (requires Docker)
```

## Test Organization

- Tests mirror source structure in `shared/src/test/scala/`
- Use ScalaTest with ScalaCheck for property-based tests
- Fork JVM for integration tests (`Test/fork := true`)
