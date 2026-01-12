# Assert Functions Analysis and Simplification Plan

## Executive Summary

Analysis of custom `assert*` functions in Scalus test codebase reveals **27+ custom functions** across multiple modules with significant duplication and inconsistent placement. This plan proposes consolidating them into the `scalus-testkit` module.

## User Preferences (Confirmed)

1. **StdlibTestKit**: Keep in `scalus-core` tests (copied to scalus-testkit at build time)
2. **Transaction Assertions**: Create separate `TxTestKit` trait in `scalus-testkit/jvm`
3. **Expected Enum**: Unified enum with all cases: `SuccessAny`, `SuccessSame`, `Success(term)`, `Failure(msg)`

---

## Classification of Assert Functions

### Category 1: Inline Compilation Assertions (StdlibTestKit)
**Location:** `scalus-core/shared/src/test/scala/scalus/prelude/StdlibTestKit.scala`

These use `Compiler.compileInline` to test Scala code compiled to UPLC:

| Function | Lines | Description |
|----------|-------|-------------|
| `assertEval(code: Boolean)` | 227-232 | Asserts boolean code is true in both Scala and UPLC |
| `assertEvalEq[T](code, expected)` | 213-214 | Asserts equality in Scala and UPLC |
| `assertEvalEqBudget[T](code, expected, budget)` | 175-211 | Equality with budget tracking |
| `assertEvalNotEq[T](code, expected)` | 216-225 | Asserts inequality |
| `assertEvalFails[E](code)` | 106-107 | Asserts exception is thrown |
| `assertEvalFails[E](cpu, memory)(code)` | 54-56 | Exception with budget limits |
| `assertEvalBudgetFails[E](code, budget)` | 58-104 | Exception with budget verification |
| `assertEvalFailsWithMessage[E](msg)(code)` | 109-151 | Exception with message verification |
| `assertEvalSuccess(code)` | 153-164 | Asserts no exception thrown |
| `assertEvalCompile(code)` | 234-236 | Asserts code compiles and evaluates |
| `evalEq` (extension) | 168-173 | Infix syntax: `code evalEq expected` |
| `checkEval[A1,...](f)` | 238-448 | Property-based testing with ScalaCheck |

**Usage:** ~750+ calls across 15+ test files

---

### Category 2: UPLC Program/Term Assertions

#### 2a. BaseValidatorTest (DUPLICATED in 2 locations)

**scalus-core/jvm** (`scalus-core/jvm/src/test/scala/scalus/BaseValidatorTest.scala`):
- `assertSameResult(expected)(program)` - Compares UplcCli AND CEK evaluation
- `assertUplcEvalResult(expected)(program)` - UplcCli only
- Uses `Expected.SuccessSame | Success(term) | Failure(description)`

**scalus-examples/shared** (`scalus-examples/shared/src/test/scala/scalus/BaseValidatorTest.scala`):
- `assertSameResult(expected)(program)` - CEK evaluation only (no UplcCli)
- `assertUplcEvalResult(expected)(program)` - UplcCli only
- Uses `Expected.SuccessAny | Success(term) | Failure(description)`

#### 2b. CekBuiltinsTest
**Location:** `scalus-core/shared/src/test/scala/scalus/uplc/CekBuiltinsTest.scala`

| Function | Lines | Description |
|----------|-------|-------------|
| `assertEvalEq(a: Term, b: Term)` | 28-29 | Asserts two Terms evaluate to same result |
| `assertEvalThrows[A](term)` | 31-32 | Asserts Term evaluation throws exception |

**Usage:** ~188 calls in CekBuiltinsTest

---

### Category 3: Transaction/Integration Assertions

**HtlcTest** (`scalus-examples/jvm/src/test/scala/scalus/examples/htlc/HtlcTest.scala`):
- `assertSuccess(provider, tx)` - Validates tx execution succeeded with budget checks
- `assertFail(expectedError)(buildTx)` - Validates tx building fails with expected error

**ScalusTest** (`scalus-testkit/shared/src/main/scala/scalus/testing/kit/ScalusTest.scala`):
- `checkResult(expected, actual)` - Validates Result with expected outcome

---

### Category 4: Local/One-off Definitions (Duplication)

| Location | Function | Notes |
|----------|----------|-------|
| `scalus-core/jvm/.../PreludeTest.scala:46` | `assertEvalFails` | Private, duplicates StdlibTestKit |
| `scalus-core/jvm/.../PreludeTest.scala:53` | `assertEvalEq` | Private, duplicates StdlibTestKit |
| `scalus-core/jvm/.../BLS12_381_G1Test.scala:68` | `assertEval` | Private, duplicates StdlibTestKit |
| `scalus-core/jvm/.../BLS12_381_G2Test.scala:68` | `assertEval` | Private, duplicates StdlibTestKit |
| `scalus-core/shared/.../ContextTest.scala:60` | `assertEval` | SIR to Term comparison |

---

## Issues Identified

### 1. Duplicate BaseValidatorTest Classes
Two nearly identical classes with subtle differences:
- `scalus-core/jvm` version: compares UplcCli + CEK, uses `SuccessSame`
- `scalus-examples/shared` version: CEK only, uses `SuccessAny`

### 2. Local Assert Definitions
Several tests define private versions of methods already in StdlibTestKit.

### 3. Inconsistent Expected Enum
Two different `Expected` enums with different cases.

### 4. StdlibTestKit Not Exported
StdlibTestKit is in scalus-core tests but not available to downstream modules via scalus-testkit.

---

## Project Dependency Analysis

```
scalus-core (base)
    ↓
scalus-cardano-ledger (depends on scalus-core)
    ↓
scalus-testkit (depends on scalus-core, scalus-cardano-ledger)
    ↓
scalus-examples (depends on scalus-testkit)
    ↓
scalus-bloxbean-cardano-client-lib (depends on scalus-core, scalus-cardano-ledger)
```

**Key insight from build.sbt (lines 436-464):** `scalus-testkit` already copies certain test files from `scalus-core/shared/src/test/scala` during build, including `StdlibTestKit.scala`.

---

## Implementation Plan

### Step 1: Create Unified Expected Enum and BaseValidatorTest

**New file:** `scalus-testkit/shared/src/main/scala/scalus/testing/kit/BaseValidatorTest.scala`

```scala
package scalus.testing.kit

import scalus.uplc.*
import scalus.uplc.eval.*
import scala.util.{Try, Success, Failure}

enum Expected:
    case SuccessAny                    // Any successful evaluation
    case SuccessSame                   // Both evaluators return same result
    case Success(term: Term)           // Specific term expected
    case Failure(description: String)  // Expected failure with description

trait BaseValidatorTest extends ScalusTest {
    /** Assert program evaluation matches expected result (CEK only) */
    protected def assertSameResult(expected: Expected)(program: Program): Unit = {
        val result = Try(program.deBruijnedProgram.evaluate)
        (expected, result) match
            case (Expected.SuccessAny, Success(_)) => ()
            case (Expected.Success(termExpected), Success(term)) =>
                assert(termExpected == term, s"Expected $termExpected but got $term")
            case (Expected.Failure(_), Failure(_)) => ()
            case _ => fail(s"Expected $expected, but got $result")
    }

    /** Assert UPLC CLI evaluation matches expected result */
    protected def assertUplcEvalResult(expected: Expected)(program: Program): Unit
}
```

**Delete:**
- `scalus-core/jvm/src/test/scala/scalus/BaseValidatorTest.scala`
- `scalus-examples/shared/src/test/scala/scalus/BaseValidatorTest.scala`

---

### Step 2: Add Term-Level Assertions to ScalusTest

**Modify:** `scalus-testkit/shared/src/main/scala/scalus/testing/kit/ScalusTest.scala`

Add to ScalusTest trait:
```scala
import scala.reflect.ClassTag

// Term-level assertions (for CekBuiltinsTest pattern)
protected def assertTermEvalEq(a: Term, b: Term): Unit =
    assert(a.evaluate == b, s"$a evaluated != $b")

protected def assertTermEvalThrows[E <: Throwable: ClassTag](term: Term): Unit =
    assertThrows[E](term.evaluate)
```

**Update:** `scalus-core/shared/src/test/scala/scalus/uplc/CekBuiltinsTest.scala`
- Remove local `assertEvalEq` and `assertEvalThrows` definitions
- Extend `ScalusTest` and use inherited methods (rename to `assertTermEvalEq`, `assertTermEvalThrows`)

---

### Step 3: Create TxTestKit Trait (JVM-only)

**New file:** `scalus-testkit/jvm/src/main/scala/scalus/testing/kit/TxTestKit.scala`

```scala
package scalus.testing.kit

import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilderException

trait TxTestKit extends ScalusTest {
    /** Assert transaction execution succeeds with valid execution units */
    protected def assertTxSuccess(
        provider: Emulator,
        tx: Transaction
    )(using env: CardanoInfo): Unit = {
        val totalExUnits = tx.witnessSet.redeemers
            .map(_.value.totalExUnits).getOrElse(ExUnits.zero)
        assert(totalExUnits.memory > 0, "ExUnits memory should be positive")
        assert(totalExUnits.steps > 0, "ExUnits steps should be positive")
        assert(
          totalExUnits.memory < env.protocolParams.maxTxExecutionUnits.memory,
          s"ExUnits memory exceeds max"
        )
        assert(
          totalExUnits.steps < env.protocolParams.maxTxExecutionUnits.steps,
          s"ExUnits steps exceeds max"
        )
        // Submit and verify
        import scalus.utils.await
        import scala.concurrent.ExecutionContext.Implicits.global
        val result = provider.submit(tx).await()
        assert(result.isRight, s"Submission failed: $result")
    }

    /** Assert transaction building fails with expected error message */
    protected def assertTxFail(expectedError: String)(buildTx: => Transaction): Unit = {
        try
            val tx = buildTx
            fail(s"Expected failure but succeeded: $tx")
        catch
            case e: TxBuilderException.BalancingException =>
                val logs = e.scriptLogs.getOrElse(Seq.empty)
                assert(
                  logs.exists(_.contains(expectedError)),
                  s"Expected '$expectedError' in logs: ${logs.mkString("\n")}"
                )
            case e: Throwable =>
                assert(
                  e.getMessage.contains(expectedError),
                  s"Expected '$expectedError' but got: ${e.getMessage}"
                )
    }
}
```

**Update:** `scalus-examples/jvm/src/test/scala/scalus/examples/htlc/HtlcTest.scala`
- Extend `TxTestKit` instead of just `ScalusTest`
- Remove local `assertSuccess` and `assertFail` definitions
- Use inherited `assertTxSuccess` and `assertTxFail`

---

### Step 4: Remove Local Duplicates

**Modify:** `scalus-core/jvm/src/test/scala/scalus/prelude/PreludeTest.scala`
- Remove private `assertEvalFails` (lines 46-51) and `assertEvalEq` (lines 53-60)
- Extend `StdlibTestKit` instead of `AnyFunSuite`

**Modify:** `scalus-core/jvm/src/test/scala/scalus/prelude/crypto/bls12_381/BLS12_381_G1Test.scala`
- Remove private `assertEval` (line 68)
- Extend `StdlibTestKit` instead of `AnyFunSuite`

**Modify:** `scalus-core/jvm/src/test/scala/scalus/prelude/crypto/bls12_381/BLS12_381_G2Test.scala`
- Remove private `assertEval` (line 68)
- Extend `StdlibTestKit` instead of `AnyFunSuite`

**Keep as-is:** `scalus-core/shared/src/test/scala/scalus/ledger/api/v1/ContextTest.scala`
- Has unique signature `assertEval(sir: SIR, expected: Term)` - different from StdlibTestKit

---

## Files Summary

### New Files (2)
| File | Purpose |
|------|---------|
| `scalus-testkit/shared/src/main/scala/scalus/testing/kit/BaseValidatorTest.scala` | Unified Expected enum + base validator assertions |
| `scalus-testkit/jvm/src/main/scala/scalus/testing/kit/TxTestKit.scala` | Transaction testing assertions (JVM-only) |

### Modified Files (7)
| File | Changes |
|------|---------|
| `scalus-testkit/shared/src/main/scala/scalus/testing/kit/ScalusTest.scala` | Add `assertTermEvalEq`, `assertTermEvalThrows` |
| `scalus-core/shared/src/test/scala/scalus/uplc/CekBuiltinsTest.scala` | Extend ScalusTest, use inherited term assertions |
| `scalus-core/jvm/src/test/scala/scalus/prelude/PreludeTest.scala` | Extend StdlibTestKit, remove local asserts |
| `scalus-core/jvm/src/test/scala/scalus/prelude/crypto/bls12_381/BLS12_381_G1Test.scala` | Extend StdlibTestKit, remove local assert |
| `scalus-core/jvm/src/test/scala/scalus/prelude/crypto/bls12_381/BLS12_381_G2Test.scala` | Extend StdlibTestKit, remove local assert |
| `scalus-examples/jvm/src/test/scala/scalus/examples/htlc/HtlcTest.scala` | Extend TxTestKit, use inherited tx assertions |
| Tests using BaseValidatorTest | Update imports to use `scalus.testing.kit.Expected` |

### Deleted Files (2)
| File | Reason |
|------|--------|
| `scalus-core/jvm/src/test/scala/scalus/BaseValidatorTest.scala` | Replaced by scalus-testkit version |
| `scalus-examples/shared/src/test/scala/scalus/BaseValidatorTest.scala` | Replaced by scalus-testkit version |

---

## Estimated Impact

| Metric | Value |
|--------|-------|
| Lines removed | ~150 (duplicate code) |
| Lines added | ~120 (consolidated + TxTestKit) |
| Test files updated | 7-9 files |
| New files | 2 |
| Deleted files | 2 |

---

## Verification

After implementation, run:
```bash
sbtn quick  # Format, compile, test
```
