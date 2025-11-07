# Minimization Process - Next Steps

## Current Status
- Full test: **FAILS** (reproduces bug) ✅
- Minimal test: **PASSES** (doesn't reproduce bug) ❌
- Step1, Step2, Step3: All **PASS** (don't reproduce bug) ❌

## What We Know
1. Bug involves parameter shadowing in nested pattern matches
2. Error: `_ContractFull.scala_275_match_signedSnapshot_1` typed as `Snapshot` instead of `SignedSnapshot`
3. Occurs when calling `validSignedSnapshot(newSignedSnapshot, ...)` on line 292
4. The pattern match on line 277 extracts `signedSnapshot: Snapshot` from `newSignedSnapshot: SignedSnapshot`

## Why Our Minimizations Don't Reproduce the Bug

Looking at the error trace, the bug manifests during lowering from SIR to UPLC, specifically in the `Action` enum handling. The full contract has:
```scala
inline def handleOpenState(action: Action, ...) 
   action match
       case Close(party, signedSnapshot) =>  // This is newSignedSnapshot
           handleClose(..., signedSnapshot, ...)
```

And inside `handleClose`:
```scala
inline def handleClose(..., newSignedSnapshot: SignedSnapshot, ...)
    state match
        case OnChainState(...) =>
            newSignedSnapshot match
                case SignedSnapshot(signedSnapshot, ...) =>  // Shadow here!
                    ...
                    validSignedSnapshot(newSignedSnapshot, ...)  // Uses wrong variable
```

## Hypothesis
The bug requires:
1. ✅ Inline functions
2. ✅ Nested pattern matches
3. ❌ **Enum pattern match higher up the call stack** (Action.Close case)
4. ❌ **The enum case extracts the parameter that will be shadowed**
5. ❌ **Complex enough state/context to trigger the lowering bug**

## Next Steps to Reproduce

### Step 4: Add Action enum match
Create a version with:
- An Action enum with a Close case that contains SignedSnapshot
- Match on action to extract the signedSnapshot
- Pass it to handleClose where the shadowing occurs

### Step 5: Bisect the full contract
Since our simplified versions pass, do a binary search on the full contract:
1. Copy ContractFull.scala
2. Remove second half of functions/types
3. Test - if fails, bug is in first half; if passes, bug is in second half
4. Repeat until minimal

### Step 6: Focus on handleClose and its callers
The error trace shows the problem path goes through:
- handleOpenState -> handleClose -> validSignedSnapshot

Remove all other Action handlers and simplify until only this path remains.

## Recommended Approach

**Start with Step 4**: Add the Action enum to step3 and see if that triggers the bug.

If that doesn't work, **do Step 5**: Binary search on the full contract by removing large chunks.

The key is to keep the bug reproducing while removing as much code as possible.
