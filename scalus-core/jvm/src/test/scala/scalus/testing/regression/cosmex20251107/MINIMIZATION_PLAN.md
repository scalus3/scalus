# Step-by-Step Minimization Plan for Cosmex Bug

## Current Status
- **Full test**: FAILS with CaclulateApplyTypeException ✓ (reproduces bug)
- **Minimal test**: PASSES ✗ (doesn't reproduce bug)

The error occurs when trying to unify `SignedSnapshot` with `Snapshot` during type calculation.

## Minimization Strategy

We'll use a binary search approach to find the minimal code that reproduces the bug:

### Phase 1: Identify Critical Components
1. Start with ContractFull.scala (953 lines) 
2. The error mentions: `validSignedSnapshot` function and `SignedSnapshot`/`Snapshot` types
3. Key suspect: The shadowing issue with nested match patterns

### Phase 2: Remove Components Systematically

#### Step 1: Remove unrelated enums and functions
- Keep: ExchangeParams, SignedSnapshot, Snapshot, validSignedSnapshot
- Remove: Action, Party, Trade, etc. (one at a time)
- Test after each removal

#### Step 2: Simplify data structures
- Reduce fields in case classes
- Remove unused type aliases
- Simplify nested structures

#### Step 3: Simplify the validator logic
- Keep only the code path that triggers the error
- Remove unrelated inline functions
- Simplify pattern matching

#### Step 4: Focus on the bug pattern
- The bug likely involves:
  - Pattern matching with shadowing (e.g., `signedSnapshot` appears in both the case class and the match pattern)
  - Nested destructuring
  - Function parameters with same name as extracted values

### Phase 3: Binary Search Within Functions

For each large function:
1. Comment out second half → test
2. If passes, bug is in first half; otherwise in second half
3. Repeat until minimal

## Detailed Steps

### Step 1: Test removing Action variants one by one
- [ ] Remove `Transfer` action handling
- [ ] Remove `Payout` action handling
- [ ] Remove `Trades` action handling
- [ ] Remove `Timeout` action handling
- [ ] Keep only `Close` and minimal logic

### Step 2: Test removing handler functions
- [ ] Remove `handlePayoutTransfer`
- [ ] Remove `handlePayoutPayout`
- [ ] Remove `handleContestTrades`
- [ ] Remove `handleTradesContestTimeout`
- [ ] Keep only functions related to `validSignedSnapshot`

### Step 3: Simplify state management
- [ ] Remove `OnChainChannelState` variants
- [ ] Simplify `OnChainState`
- [ ] Remove `TradingState` complexity

### Step 4: Focus on the exact bug
- [ ] Isolate `validSignedSnapshot` function
- [ ] Simplify its call site
- [ ] Minimize the pattern match that causes shadowing

## Expected Minimal Reproduction

The minimal bug should look something like:

```scala
case class Snapshot(data: Int)
case class SignedSnapshot(signedSnapshot: Snapshot, sig: ByteString)

def validSignedSnapshot(signedSnapshot: SignedSnapshot): Boolean = {
    signedSnapshot match
        case SignedSnapshot(signedSnapshot, sig) =>  // BUG: shadowing parameter name
            // use signedSnapshot here - compiler gets confused which one
            true
}
```

## Testing Protocol

After each change:
1. Save the file
2. Run: `sbtn "jvm/testOnly scalus.testing.regression.cosmex20251107.CompileCosmexFullTest" 2>&1 | tee test-step-N.log`
3. Check if test FAILS (good - bug still present) or PASSES (bad - went too far, revert last change)
4. Document what was removed in this file
