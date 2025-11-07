# Test Case Files and Error Types

## Overview

We have multiple test files that trigger the same underlying bug but at different compilation phases:

## Files

### 1. ContractFull.scala (952 lines)
- **Package:** `cosmex20251107.full`
- **Test:** `CompileCosmexFullTest`
- **Error Type:** `CaclulateApplyTypeException`
- **Error Message:** 
  ```
  Cannot calculate apply type for SignedSnapshot -> ... to Snapshot
  Failed unification SignedSnapshot and Snapshot
  ```
- **Status:** ✅ Original error - this is what we want to reproduce minimally

### 2. ContractStep4.scala (112 lines currently)
- **Package:** `cosmex20251107.step4`
- **Test:** `CompileCosmexStep4Test`
- **Error Type:** TBD - work in progress
- **Goal:** Minimize to trigger the SAME error as ContractFull
- **Status:** 🔨 Work in progress

### 3. ContractStep4Minimized.scala (96 lines)
- **Package:** `cosmex20251107.minimized`
- **Test:** `CompileCosmexStep4MinimizedTest`
- **Error Type:** `LoweringException`
- **Error Message:**
  ```
  Unsupported representation ProdDataConstr for match expression
  ```
- **Status:** ⚠️ Over-minimized - triggers bug earlier in compilation

## The Bug

All three files demonstrate the same underlying bug: **variable shadowing in nested pattern matches from inline functions**. However, they trigger the bug at different compilation phases:

1. **Full version:** Reaches type application phase, clearly shows SignedSnapshot vs Snapshot mismatch
2. **Step4 (WIP):** Need to find sweet spot that triggers original error with minimal code
3. **Minimized version:** Fails during match lowering before type checking

## Lesson Learned

**Test-Driven Minimization:** Always verify that:
- ✅ Test fails
- ✅ **Error message matches original** ← We missed this!

## Next Steps

Continue working on ContractStep4.scala to find the minimal code that triggers the `CaclulateApplyTypeException` error.
