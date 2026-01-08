# TxBuilder Performance Analysis and Improvement Plan

## Executive Summary

This document analyzes the transaction balancing approaches used by various Cardano transaction builder libraries and proposes improvements to Scalus TxBuilder based on the findings.

**Key Metrics (Current Scalus Implementation):**

- **Balancing Iterations**: 3 iterations for simple script transaction
- **Transaction Time**: 1-4ms (after JIT warmup)
- **Simple ADA Transfer**: 0-2ms

## 1. Background: CIP-2 Coin Selection Algorithms

[CIP-2](https://cips.cardano.org/cip/CIP-2) is the Cardano Improvement Proposal that defines standard coin selection algorithms. It addresses three core challenges in UTxO-based blockchains:

1. **Transaction Size Limits** - Each transaction has a maximum size, limiting input count
2. **Dust Accumulation** - Naively minimizing change creates tiny, unusable outputs
3. **Concurrency Constraints** - Single change outputs reduce parallel transaction capability

### 1.1 CIP-2 Algorithms

#### Largest-First Algorithm

A straightforward greedy approach:

1. Sort available UTxOs from largest to smallest by value
2. Repeatedly select the largest remaining UTxO until total value >= payment amount
3. Generate single change output for excess

**Strength:** Deterministic and simple.
**Weakness:** Tends to accumulate dust over time.

#### Random-Improve Algorithm (Preferred)

A two-phase algorithm that balances coverage with change quality:

**Phase 1 (Random Selection):**

- Iterate through requested outputs (largest to smallest)
- For each output, randomly select UTxO entries until that output's value is covered
- Remove selected entries from the available pool

**Phase 2 (Improvement):**

- Process outputs in ascending order
- For each output, attempt to expand its UTxO selection toward a target range of `(v, 2v, 3v)`, where `v` = output value
- Stop improving when selection moves closer to the ideal value `2v` but stays under maximum `3v`

This creates change outputs approximating payment values, enabling the wallet to self-organize useful denominations.

### 1.2 CIP-2 Key Properties

All conforming implementations must satisfy:

- **Coverage:** Selected inputs >= requested outputs
- **Correctness:** Selected inputs = requested outputs + change
- **Conservation:** Each UTxO either gets selected or remains available
- **Output Preservation:** Requested outputs appear unchanged in results

### 1.3 CIP-2 Scope Limitation

**Important:** CIP-2 explicitly does NOT cover fee calculation or balancing. It only addresses UTXO selection. Fee adjustment requires additional logic beyond CIP-2.

### 1.4 Scalus UTXO Selection Algorithm (UtxoPool)

Scalus implements a custom UTXO selection algorithm in `UtxoPool.scala` that differs from CIP-2.

#### selectForValue: Input Selection

The algorithm uses a **Token-First + Smallest-Sufficient** strategy:

**Phase 1 - Token Selection (Mandatory):**

- Iterate through all available UTxOs
- Select any UTxO containing required tokens (tokens are constrained to specific UTxOs)
- Subtract selected UTxO's ADA from remaining requirement

**Phase 2 - ADA Selection:**

- Sort remaining UTxOs by ADA amount (ascending)
- Find the **smallest sufficient** single UTxO that covers remaining ADA
- If found: select only that UTxO (minimizes inputs)
- If not found: select multiple UTxOs using **Largest-First** until covered

```text
selectForValue(required):
  1. For each UTxO with required tokens → select it
  2. If remainingAda > 0:
     a. Sort unselected by ADA ascending
     b. If smallest >= remainingAda exists → select it
     c. Else: select largest-first until covered
```

#### selectForCollateral: Collateral Selection

A 4-tier strategy optimizing for minimal transaction overhead:

1. **Optimal UTxO**: Find ADA-only UTxO where `excess < minAda` (avoids collateralReturn output)
2. **Smallest Sufficient**: Find smallest ADA-only UTxO that covers requirement
3. **Multiple ADA-Only**: Select multiple ADA-only UTxOs (Largest-First)
4. **Token Fallback**: Use UTxOs with tokens if ADA-only insufficient

The "optimal" strategy is clever: when `collateral - required < minAda`, no collateralReturn output is needed, saving transaction space.

#### Comparison with CIP-2

| Aspect           | CIP-2 Largest-First | CIP-2 Random-Improve | Scalus UtxoPool     |
| ---------------- | ------------------- | -------------------- | ------------------- |
| Primary Strategy | Largest first       | Random + improve     | Smallest sufficient |
| Fallback         | N/A                 | N/A                  | Largest-first       |
| Token Handling   | Not specified       | Not specified        | Token-first         |
| Determinism      | Deterministic       | Non-deterministic    | Deterministic       |
| Input Count      | May use many        | Moderate             | Minimized           |
| Dust Prevention  | Poor                | Good                 | Moderate            |
| Change Quality   | Poor                | Good (2v target)     | Not optimized       |

#### Strengths

- **Minimizes input count**: Smallest-sufficient selection reduces transaction size
- **Preserves large UTxOs**: Doesn't consume large UTxOs unnecessarily
- **Smart collateral**: Avoids collateralReturn overhead when possible
- **Token-aware**: Correctly handles multi-asset selection
- **Deterministic**: Same inputs always produce same outputs (good for testing)

#### Weaknesses

- **Dust accumulation**: Small UTxOs tend to remain unused, accumulating over time
- **Single large change**: Creates one change output instead of useful denominations
- **Not CIP-2 compliant**: Though CIP-2 was designed for ADA-only (pre-Mary)

#### Potential Improvements

1. **Dust consolidation**: Optionally include small UTxOs when selecting for larger amounts
2. **Change splitting**: Split large change into multiple outputs matching common payment sizes
3. **Configurable strategy**: Allow users to choose between strategies (minimal-inputs vs dust-reduction)
4. **Random-Improve option**: Implement CIP-2 Random-Improve for users who want better change distribution

## 2. Research: Transaction Balancing Approaches

### 2.1 Lucid Evolution (TypeScript)

**Approach**: Two-phase iterative approach

1. **First Round**: Calculates base fees without script execution costs
2. **Second Round**: Recalculates including script execution budgets

**Key Features:**

- Uses exactly **2 iterations** for transactions with Plutus scripts
- Single iteration for non-script transactions
- Delta calculation for coin selection
- Recursive selection for minAda requirements

**Strengths:** Simple, predictable iteration count.
**Weaknesses:** May over-estimate fees in first pass.

### 2.2 MeshJS (TypeScript)

**Approach**: Dynamic fee calculation with 3-iteration minLovelace loop

**Key Features:**

- Uses `calculateFee()` with: `minFee = (minFeeA * txSize) + minFeeB + refScriptFee + redeemersFee`
- **3 iterations** for minimum lovelace calculation until stable
- Delegates to `IInputSelector` interface for UTXO selection
- Includes token bundle validation against `maxValSize`

**Strengths:** Comprehensive fee components, configurable selectors.
**Weaknesses:** More complex, may iterate more than necessary.

### 2.3 Elm Cardano (Elm)

**Approach**: CIP-2 inspired with Largest-First algorithm

**Key Features:**

- Follows CIP-2 coin selection specifications
- Ada selection first, then token selection
- Input limits enforcement (10-16 inputs)
- Three-tier ranking for collateral selection

**Strengths:** Standards-compliant, clear separation of concerns.
**Weaknesses:** May create suboptimal selections for complex cases.

### 2.4 Haskell cardano-api

**Approach**: Single-pass with placeholder values

**Key Features:**

- **Single pass** with no iterative refinement
- Uses maximum placeholder values (`2^32-1` for fees) to estimate CBOR size
- Calculates execution units once
- Deterministic: given inputs, produces single output

**Strengths:** Simple, fast, no iteration overhead.
**Weaknesses:** May overestimate by a few bytes; requires pre-selected inputs.

### 2.5 Cooked-Validators (Haskell)

**Approach**: Dichotomic (binary) search

**Key Features:**

- Binary search between minFee and maxFee
- **Logarithmic iterations**: approximately log2(interval_size)
- Recursive UTXO enumeration with O(2^n) worst case
- Pruned by maximum element limits

**Strengths:** Optimal fee finding.
**Weaknesses:** Complex, potentially slow for large UTXO sets.

## 3. Comparison and Ranking

| Library           | Iterations | Complexity | Fee Precision | UTXO Selection |
| ----------------- | ---------- | ---------- | ------------- | -------------- |
| cardano-api       | 1          | Low        | Conservative  | Pre-selected   |
| Lucid Evolution   | 2          | Medium     | Good          | Greedy         |
| MeshJS            | 3+         | High       | Good          | Configurable   |
| Scalus (current)  | 3          | Medium     | Good          | Greedy         |
| Cooked-Validators | log2(N)    | High       | Optimal       | Exhaustive     |

### Ranking by Simplicity and Practicality

1. **cardano-api** - Simplest, but requires pre-selected inputs
2. **Lucid Evolution** - Good balance of simplicity and precision
3. **Scalus** - Similar to Lucid, could be improved
4. **MeshJS** - Flexible but complex
5. **Cooked-Validators** - Most precise but complex

### Best Overall Approach

The recommended approach is a **two-pass strategy like Lucid Evolution**.

The ideal approach combines:

1. **Initial fee estimation** using maximum expected size (with dummy signatures)
2. **Single refinement pass** after script evaluation
3. **Early termination** when fee stabilizes

## 4. Current Scalus Implementation Analysis

### 4.1 Current Flow (TxBuilder.scala)

```text
TxBuilder.complete()
  |-> completeLoop(maxIterations=10)
       |-> TransactionBuilder.build()
       |-> finalizeContext()
            |-> balance()
                 |-> balanceFeeAndChangeWithTokens() [up to 20 iterations]
                      |-> loop:
                           - computeScriptsWitness()
                           - ensureMinFee()
                           - calculateChangeValue()
                           - diffHandler()
                           - repeat until tx == trialTx
```

### 4.2 Identified Issues

1. **Double Iteration Loop**: Both `completeLoop` and `balanceFeeAndChangeWithTokens` iterate
2. **Script Re-evaluation**: Scripts are evaluated on every balancing iteration
3. **No Initial Fee Estimate**: Fee starts from transaction's current fee, not estimated

### 4.3 Why 3 Iterations?

Based on the test output:

1. **Iteration 1**: Initial attempt, compute script costs, set initial fee
2. **Iteration 2**: Adjust for fee change affecting change output
3. **Iteration 3**: Verify convergence (tx unchanged)

This is reasonable but could be reduced to 2 with better initial estimation.

## 5. Improvement Recommendations

### 5.1 High Priority: Reduce Balancing Iterations

**Goal**: Reduce from 3 to 2 iterations

**Key Insight**: At estimation time, we already know:

- Exact number of redeemers (from transaction building steps)
- Exact redeemer data (the `Data` values are defined)
- Redeemer tags and indices (determined by transaction structure)

We only don't know the **ExUnits** (requires script evaluation). However, ExUnits are essentially fixed-size in CBOR serialization (two 64-bit integers), so we can use `maxTxExecutionUnits` as a conservative placeholder.

**Approach:**

1. **Pre-estimate fee** using max ExUnits as placeholders (like cardano-api):

   ```scala
   val txWithMaxExUnits = setRedeemersExUnits(tx, protocolParams.maxTxExecutionUnits)
   val estimatedFee = MinTransactionFee.calculate(txWithMaxExUnits, protocolParams)
   ```

2. **Early termination** when fee is stable within tolerance:

   ```scala
   if math.abs(newFee.value - oldFee.value) < TOLERANCE then Right(tx)
   ```

**Implementation:**

```scala
def estimateMinFee(tx: Transaction, protocolParams: ProtocolParams): Coin = {
    // Replace all redeemer ExUnits with max values (conservative placeholder)
    // This gives us an accurate upper bound on the serialized transaction size
    val txWithMaxExUnits = tx.copy(
        witnessSet = tx.witnessSet.copy(
            redeemers = tx.witnessSet.redeemers.map { redeemers =>
                KeepRaw(redeemers.value.map { r =>
                    r.copy(exUnits = protocolParams.maxTxExecutionUnits)
                })
            }
        )
    )

    // Calculate fee based on actual serialized size (accurate, not estimated)
    MinTransactionFee.calculate(txWithMaxExUnits, protocolParams)
}
```

**Why This Works:**

| Component                                                 | How We Handle It                                     |
| --------------------------------------------------------- | ---------------------------------------------------- |
| Size-based fee (`minFeeA * size + minFeeB`)               | Accurate - we serialize with max ExUnits placeholder |
| ExUnits-based fee (`priceMem * mem + priceSteps * steps`) | Must wait for script evaluation                      |

The size-based component is the one that causes iteration oscillation. By using max ExUnits placeholders, we get an accurate (slightly conservative) size estimate on the first pass.

### 5.2 ~~Medium Priority: Cache Script Evaluation Results~~ (Not Recommended)

**Original Idea**: Cache ExUnits from first evaluation and reuse when only fee changes.

**Why This Is Problematic:**

Scripts have access to `TxInfo.fee` via ScriptContext. If a script's logic depends on the fee value:

```
fee changes → different script execution path → different ExUnits → different fee
```

This creates a circular dependency that could theoretically not converge.

**Example of problematic script:**

```haskell
validator redeemer ctx =
    let fee = txInfoFee (scriptContextTxInfo ctx)
    in fee > 1_000_000  -- behavior depends on fee!
```

**Conclusion**: The current implementation that **re-evaluates scripts on each iteration is correct**. While most real-world scripts don't use `TxInfo.fee`, we cannot safely assume this. Caching would break scripts that do depend on fee.

**Alternative Consideration**: Could offer opt-in caching with explicit user acknowledgment that their scripts don't use `TxInfo.fee`, but this adds complexity for marginal benefit

### 5.3 Low Priority: Simplify completeLoop

**Goal**: Consolidate iteration logic

**Approach:**

- Move coin selection into finalize
- Single unified balancing loop

## 6. Code Review: Bugs and Improvements

### 6.1 Bugs Found

1. **TxBuilderCompleteTest line 793-824**: The test correctly identifies a fee calculation bug where `complete` doesn't add dummy signatures during fee calculation

2. **TxBuilderCompleteTest line 831-884**: Delayed redeemer bug - complete adds inputs dynamically but doesn't recompute delayed redeemers

3. **TxBuilder line 1019**: In `sign()` method, `tx` is assigned but not used:

   ```scala
   val tx = context.transaction  // unused
   val signedTx = signer.sign(transaction)
   ```

### 6.2 Code Improvements

1. **TransactionBuilder.scala line 670**: Magic number `20` should be a constant:

   ```scala
   val MaxBalancingIterations = 20
   if iteration > MaxBalancingIterations then ...
   ```

2. **TxBuilder.scala line 1134**: Magic number `10` should be a constant:

   ```scala
   val MaxCompleteIterations = 10
   ```

3. **Change.scala line 111-112**: Improve change output finding - consider edge cases with multiple outputs to same address

4. **UtxoPool.scala**: Consider implementing CIP-2 Random-Improve algorithm for better UTXO selection

### 6.3 Documentation Improvements

1. Add Scaladoc explaining the balancing algorithm
2. Document expected iteration counts and performance characteristics
3. Add examples for custom DiffHandlers

## 7. Test Improvements

### 7.1 Current Coverage

The existing tests cover:

- Basic ADA transfers
- Multi-asset transactions
- Script spending
- Collateral handling
- Error cases

### 7.2 Suggested Additional Tests

1. **Balancing convergence test**: Verify iteration count
2. **Large transaction test**: Many inputs/outputs
3. **Edge cases**: Exact amount transfers, minimum change
4. **Performance regression test**: Track timing

## 8. Implementation Plan

### Phase 1: Quick Wins

1. Fix the unused `tx` variable bug
2. Add constants for magic numbers
3. Add balancing iteration logging

### Phase 2: Optimization

1. Implement fee pre-estimation
2. Add script evaluation caching
3. Reduce to 2-iteration balancing

### Phase 3: Refactoring

1. Consolidate iteration loops
2. Implement CIP-2 coin selection option
3. Add comprehensive benchmarks

## 9. Conclusion

The current Scalus TxBuilder implementation is functional and performs well (1-4ms per transaction). The main areas for improvement are:

1. **Reduce iterations** from 3 to 2 through better initial fee estimation
2. **Fix minor bugs** in sign() method and fee calculation
3. **Add constants** for magic numbers
4. **Improve documentation** of the balancing algorithm

The recommended approach is to adopt the two-pass strategy from Lucid Evolution while maintaining Scalus's clean Scala 3 implementation.

## References

- [CIP-2: Coin Selection Algorithms for Cardano](https://cips.cardano.org/cip/CIP-2)
- [Cooked-Validators Balancing](https://github.com/tweag/cooked-validators/blob/main/doc/BALANCING.md)
- [cardano-coin-selection](https://github.com/IntersectMBO/cardano-coin-selection)
- [Lucid Evolution](https://github.com/Anastasia-Labs/lucid-evolution)
- [MeshJS](https://github.com/MeshJS/mesh)
- [Elm Cardano](https://github.com/elm-cardano/elm-cardano)
