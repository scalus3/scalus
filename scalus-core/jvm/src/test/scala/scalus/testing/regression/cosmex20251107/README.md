# Cosmex Shadowing Bug - Minimization Complete

## Summary

Successfully minimized the cosmex shadowing bug from **953 lines** (ContractFull.scala) to **~180 lines** (ContractStep4.scala).

## Test Status

| Test | Lines | Status | Purpose |
|------|-------|--------|---------|
| `CompileCosmexFullTest` | 952 | ❌ FAILS | Original full contract with bug |
| `CompileCosmexStep4Test` | 119 | ❌ FAILS | **Minimal reproduction** ✅ |

Note: Intermediate passing tests (Step1-3, Minimal) were removed to keep only the failing tests that reproduce the bug.

## The Bug Pattern

The bug occurs with this specific pattern:

```scala
// 1. Enum with parameter to be shadowed
enum Action:
    case Close(party: Party, signedSnapshot: SignedSnapshot)

// 2. Extract from enum
action match
    case Close(party, signedSnapshot) =>
        handleClose(..., signedSnapshot, ...)  // Pass as newSignedSnapshot parameter

// 3. Inline function receives it
inline def handleClose(..., newSignedSnapshot: SignedSnapshot, ...) = {
    // 4. Pattern match shadows a field name
    newSignedSnapshot match
        case SignedSnapshot(signedSnapshot, ...) =>  // BUG: signedSnapshot field shadows
            // 5. Both variables used in same scope
            balancedSnapshot(signedSnapshot.snapshotTradingState, ...)  // Inner (Snapshot)
            && validSignedSnapshot(newSignedSnapshot, ...)  // Outer (SignedSnapshot)
            //                     ^^^^^^^^^^^^^^^^^
            //                     Compiler uses WRONG variable here!
}
```

The compiler incorrectly resolves the outer `newSignedSnapshot: SignedSnapshot` to the inner `signedSnapshot: Snapshot` from the pattern match.

## Root Cause

During SIR lowering to UPLC, the compiler creates an intermediate variable for the pattern match extraction and incorrectly uses it when the outer parameter is referenced.

Error: `Cannot calculate apply type for ... SignedSnapshot -> ... to Snapshot`

## Files

### Test Files (Reproducing the Bug)
- **`ContractStep4.scala` - Minimal reproduction (119 lines)** ⭐
- **`CompileCosmexStep4Test.scala` - Test for minimal case (fails)** ⭐
- `ContractFull.scala` - Original full contract (952 lines)
- `CompileCosmexFullTest.scala` - Test for full contract (fails)

### Documentation
- `MINIMIZATION_PLAN.md` - Initial strategy
- `ANALYSIS.md` - Bug analysis notes
- `NEXT_STEPS.md` - Process documentation
- **`MINIMIZATION_RESULT.md` - Final results** ⭐

## Usage

To verify the bug still exists:
```bash
sbtn "jvm/testOnly scalus.testing.regression.cosmex20251107.CompileCosmexStep4Test"
# Should FAIL with: CaclulateApplyTypeException
```

To verify the bug is fixed:
```bash
sbtn "jvm/testOnly scalus.testing.regression.cosmex20251107.CompileCosmexStep4Test"
# Should PASS
sbtn "jvm/testOnly scalus.testing.regression.cosmex20251107.CompileCosmexFullTest"  
# Should PASS
```

## Next Steps

1. Use `ContractStep4.scala` to understand the bug in the compiler
2. Fix the variable resolution in SIR lowering
3. Verify both Step4 and Full tests pass
4. Consider keeping Step4 as a permanent regression test

## Key Learnings

The minimization process revealed that the bug requires:
- Enum pattern matching that extracts a parameter
- Inline functions
- Multiple levels of nested pattern matches
- A case class field name that matches a variable name from outer scope
- Both the shadowed and shadowing variables used in the same expression

Simple shadowing alone is NOT enough to trigger the bug.
