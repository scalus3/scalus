# Minimization Error Discovered

## Problem

The minimized test case (96 lines) reproduces *a* failure, but NOT the same error as the original:

- **Original Error (952 lines):**
  ```
  CaclulateApplyTypeException: Cannot calculate apply type for 
  SignedSnapshot -> ... to Snapshot
  Failed unification SignedSnapshot and Snapshot
  ```

- **Minimized Error (96 lines):**
  ```
  LoweringException: Unsupported representation ProdDataConstr 
  for match expression
  ```

## Root Cause

During minimization, we removed code that allowed the compiler to progress
further into type checking. The minimized version fails earlier during
match expression lowering, before reaching the type application phase.

## Key Insight (From User)

**"Why didn't we detect error change during minimizing?"**

We should have been verifying at EACH minimization step that:
1. ✅ The test still fails
2. ✅ The error message is THE SAME

We only checked #1, not #2!

## Correct Minimization Process

1. Run full test, capture error message
2. Make minimization change
3. Run test again
4. **Verify error message matches original** ⚠️  
5. If different, revert and try different minimization
6. If same, keep change and continue

## Next Steps

Need to find what was removed that caused error to change:
- Likely: removing parameters from `validSignedSnapshot`
- Likely: simplifying the function body
- Likely: removing `Party` enum parameter

Must restore minimal code needed to trigger the type unification error.

## Lesson Learned

**Test-Driven Minimization:** Don't just check if test fails,
check that it fails in the SAME WAY!

