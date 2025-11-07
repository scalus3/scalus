# Cosmex Shadowing Bug - Minimization Summary

## Mission Accomplished ✅

Successfully minimized the cosmex shadowing bug test case from **952 lines** to **119 lines** (87.5% reduction).

## Minimization Journey

```
ContractFull.scala       952 lines  ❌ FAILS (original)
                           ↓
     [tried various intermediate steps...]
                           ↓
ContractStep4.scala      119 lines  ❌ FAILS (minimal!) ⭐

Reduction: 87.5% - from 952 to 119 lines
```

## The Critical Difference

What makes Step4 reproduce the bug while Step3 doesn't?

**Adding the Action enum with Close case!**

```scala
// Step3: Direct call
handleClose(params, state, newSignedSnapshot, ...)  // No bug

// Step4: Through enum match  
action match
    case Close(party, signedSnapshot) =>  // Extracts signedSnapshot
        handleClose(params, state, signedSnapshot, ...)  // BUG TRIGGERS!
        //                        ^^^^^^^^^^^^^^^^^
        //                        This becomes newSignedSnapshot parameter
```

The bug requires the parameter to flow through an enum pattern match before entering the function where shadowing occurs.

## The Bug in Detail

```scala
// 1. Action enum extracts SignedSnapshot
case Close(party, signedSnapshot: SignedSnapshot) =>
    
// 2. Pass to function (becomes newSignedSnapshot parameter)
    handleClose(..., signedSnapshot, ...)
    
// 3. Function receives it
    inline def handleClose(..., newSignedSnapshot: SignedSnapshot, ...) =
        
// 4. Pattern match extracts field (shadows!)
        newSignedSnapshot match
            case SignedSnapshot(signedSnapshot: Snapshot, ...) =>
            
// 5. Use both in same scope
                balancedSnapshot(signedSnapshot.snapshotTradingState, ...)  // Inner
                && validSignedSnapshot(newSignedSnapshot, ...)  // Outer - BUG!
                //                     ^^^^^^^^^^^^^^^^^
                //                     Compiler thinks this is signedSnapshot: Snapshot
```

**Error:** `Cannot calculate apply type for SignedSnapshot -> ... to Snapshot`

## Test Matrix

| Test | Lines | Status | Description |
|------|-------|--------|-------------|
| Full | 952 | ❌ FAIL | Complete cosmex contract |
| **Step4** | **119** | **❌ FAIL** | **Minimal reproduction** ⭐ |

87.5% reduction from original!

## Quick Start

Test the minimal reproduction:
```bash
sbtn "jvm/testOnly scalus.testing.regression.cosmex20251107.CompileCosmexStep4Test"
```

Expected output:
```
[info] - step 4 - Action enum with Close case containing SignedSnapshot *** FAILED ***
[info]   scalus.sir.SIRType$CaclulateApplyTypeException: Cannot calculate apply type for 
        ... SignedSnapshot -> ... to ... Snapshot, function type does not match argument type.
```

## Files Reference

### Must Review ⭐
- **`ContractStep4.scala`** - The minimal reproduction (119 lines)
- **`CompileCosmexStep4Test.scala`** - Test case that fails
- **`README.md`** - Complete documentation

### Supporting
- `ContractFull.scala` / `CompileCosmexFullTest.scala` - Original test
- `MINIMIZATION_RESULT.md` - Detailed analysis
- `MINIMIZATION_PLAN.md` - Strategy document
- `ANALYSIS.md` - Bug analysis notes

(Intermediate passing tests Step1-3 and Minimal were removed.)

## Key Insights

1. **Simple shadowing is not enough** - Steps 1-3 all have shadowing but don't trigger the bug
2. **Enum extraction is critical** - The bug requires data to flow through enum pattern matching
3. **Inline functions matter** - All handlers are marked `inline`
4. **Multiple nesting levels** - Bug needs specific depth of pattern match nesting
5. **Type complexity** - Requires case classes with same-named fields at different types

## What's Next

1. Use ContractStep4.scala to debug the compiler
2. Focus on SIR lowering, specifically variable resolution in nested matches
3. Check how enum case extraction variables are tracked through inline expansion
4. Fix the variable shadowing resolution
5. Verify both Step4 and Full tests pass after fix

## Success Criteria

After the bug is fixed:
```bash
sbtn "jvm/testOnly scalus.testing.regression.cosmex20251107.*Test"
# All tests should PASS
```
