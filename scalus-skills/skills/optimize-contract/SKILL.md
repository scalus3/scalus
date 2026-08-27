---
name: optimize-contract
description: Optimize Scalus/Cardano smart contracts for execution budget (CPU steps and memory). Analyzes @Compile annotated validators for performance issues — expensive patterns, unnecessary allocations, redundant traversals, missed short-circuits. Provides concrete Scalus rewrites with budget impact estimates. Use when reviewing on-chain code performance or when /optimize-contract is invoked. Requires explicit path argument.
---

# Smart Contract Optimization Review

Analyze Scalus/Cardano smart contracts for execution budget optimization opportunities.

## Prerequisites — Before You Optimize

1. **Establish a baseline.** Run the validator's tests with budget assertions (`assertBudgetEquals` or `assertBudgetWithin`) on representative inputs — simple case, worst case, and typical case.
2. **Identify actual hot paths.** Don't guess — measure. Use `EvalTestDsl` budget assertions to find which code paths dominate the budget.
3. **Optimize surgically.** Change one thing at a time and re-measure. Small, targeted changes are safer than sweeping rewrites.
4. **Re-benchmark after every significant change.** Budget is the only ground truth.

## Target Code Identification

Find on-chain code by searching for:
1. Objects/classes with `@Compile` annotation
2. Objects extending `Validator`, `DataParameterizedValidator`, or `ParameterizedValidator`
3. Objects compiled with `PlutusV3.compile()`, `PlutusV2.compile()`, or `PlutusV1.compile()`

Search patterns:
```
grep -rn "@Compile" --include="*.scala" <path>
grep -rn "extends Validator" --include="*.scala" <path>
grep -rn "extends DataParameterizedValidator" --include="*.scala" <path>
```

## Workflow

1. **Discovery**: Find all `@Compile` annotated code in specified path
2. **Profile**: Identify existing budget tests; note current memory/steps
3. **Analysis**: Check each validator against the optimization checklist below
4. **Prioritize**: Rank findings by estimated budget impact (high/medium/low)
5. **Rewrite**: Propose concrete code changes with before/after
6. **Verify**: Run `sbtn quick` or specific test to confirm budget improvement
7. **Report**: Generate structured report with budget deltas

## Optimization Checklist

For detailed patterns with Scalus code examples, see `references/patterns.md`.

### High Impact — Data Structures & Traversals

| ID | Pattern | Problem | Fix |
|----|---------|---------|-----|
| O001 | Multiple list traversals | Separate `filter` then `map` then `length` | Combine into single `foldLeft` |
| O002 | `foldRight` on lists | Not tail-recursive, builds thunks | Use `foldLeft` with `reverse` if order matters |
| O003 | `list.flatten` | O(n*m) via nested `foldRight` + `++` | Accumulate with `foldLeft` and prepend |
| O004 | `list.distinct` | O(n^2) — `foldLeft` with `exists` | Use `SortedMap` or deduplicate at source |
| O005 | `list :+ elem` (append) | O(n) per append | Use `elem +: list` (prepend) and reverse once |
| O006 | Reconstructing `Value` | Full `Value` maintains invariants expensively | Use `SortedMap` or `PairList` directly when possible |
| O007 | `AssocMap` for large maps | O(n) lookup without early termination | Use `SortedMap` (early termination via `Ord`) |
| O008 | `list.map(f).filter(p)` | Two traversals, intermediate list | Single `foldLeft` combining map + filter |
| O009 | `list.length` to check emptiness | O(n) traversal | Use `list.isEmpty` — O(1) |
| O010 | `AssocMap.fromList` on large input | O(n^2) dedup | Pre-sort and use `SortedMap.fromStrictlyAscendingList` |

### High Impact — Short-Circuiting & Ordering

| ID | Pattern | Problem | Fix |
|----|---------|---------|-----|
| O011 | Expensive checks before cheap ones | Wasted budget on failing txs | Put cheapest/most-likely-to-fail checks first |
| O012 | Late `require` for invalid input | Work done before validation | Fail fast — validate inputs at the top |
| O013 | Linear condition chains | Average n/2 evaluations for n conditions | Structure as binary decision tree |
| O014 | No short-circuit in `&&`/`||` | Evaluating both sides always | Scalus `&&`/`||` DO short-circuit — ensure expensive side is second |
| O015 | `list.exists` after construction | Building list just to search it | Inline the search into the fold that builds the data |

### Medium Impact — Data Representation

| ID | Pattern | Problem | Fix |
|----|---------|---------|-----|
| O016 | Typed equality on complex structures | Field-by-field comparison | Use `equalsData` for whole-structure comparison when types match |
| O017 | Constructing tuples/records to return | Allocation + destructuring overhead | Use continuation-passing or accumulator parameters |
| O018 | `List[(A, B)]` map operations | ~12 builtins per element | Use `PairList` — ~4 builtins per element via `fstPair`/`sndPair` |
| O019 | Pattern matching for Data access | Constructs intermediate Scala objects | Use `Data` builtins directly when structure is known |
| O020 | `Value` equality via `===` | Recursive map-of-maps comparison | Compare as `Data` with `equalsData` when checking unchanged values |

### Medium Impact — Computation

| ID | Pattern | Problem | Fix |
|----|---------|---------|-----|
| O021 | `pow(2, n)` | Generic exponentiation loop | Use `exp2(n)` — single builtin via byte shift |
| O022 | Manual log2 via division loop | O(log n) divisions | Use `log2(n)` — single builtin via `integerToByteString` |
| O023 | Recomputing same expression | Duplicated subexpressions | Use `let` bindings; V3 optimizer has CSE but don't rely on it |
| O024 | `generateErrorTraces = true` in prod | Trace strings bloat script and budget | Set `generateErrorTraces = false` for production builds |
| O025 | Complex pure computations on-chain | Expensive on-chain work | Move computation off-chain, pass result as redeemer, verify on-chain |

### Low Impact — Micro-Optimizations

| ID | Pattern | Problem | Fix |
|----|---------|---------|-----|
| O026 | Small recursive helpers | Call overhead per recursion | Unroll first 1-2 iterations for common small cases |
| O027 | Non-tail-recursive numeric loops | Stack growth | Rewrite with accumulator parameter |
| O028 | Redundant `FromData`/`ToData` conversions | Serialization round-trips | Keep data in `Data` form between operations |
| O029 | `list.reverse.foldLeft` | Extra O(n) reverse pass | Use `foldRight` if list is small, or build in correct order |
| O030 | Building closures in inner loops | Allocation per iteration | Lift closure outside loop if captures don't change |

## Key Scalus Optimization Principles

### 1. Don't Compute, Verify
The most impactful optimization: move work off-chain.

Instead of computing a result on-chain, have the off-chain code compute it
and pass it as a redeemer field. The validator only checks correctness.

```scala
// Expensive: compute on-chain
val sqrtResult = radicand.sqRoot

// Cheap: verify pre-computed result
val sqrtResult = redeemer.sqrtValue
require(sqrtResult * sqrtResult <= radicand)
require((sqrtResult + 1) * (sqrtResult + 1) > radicand)
```

### 2. Fail Fast
Put cheapest and most-likely-to-fail validations first. Every `require` that fails
early saves the budget of all subsequent code.

```scala
// Good: cheap check first
require(isSignedBy(txInfo, admin), "not admin")
require(expensiveValueCheck(txInfo), "value mismatch")

// Bad: expensive check first
require(expensiveValueCheck(txInfo), "value mismatch")
require(isSignedBy(txInfo, admin), "not admin")
```

### 3. Traverse Once
Never traverse a list twice when once will do. Combine filter + map + count
into a single fold.

```scala
// Bad: three traversals
val filtered = items.filter(_.isValid)
val mapped = filtered.map(_.amount)
val total = mapped.foldLeft(BigInt(0))(_ + _)

// Good: single traversal
val total = items.foldLeft(BigInt(0)) { (acc, item) =>
    if item.isValid then acc + item.amount else acc
}
```

### 4. Use PairList for Map Operations
`PairList` uses raw UPLC pair builtins (~4 ops/element) vs `List[(A, B)]` (~12 ops/element).

```scala
// Expensive
map.toList.map { case (k, v) => (k, f(v)) }

// Cheap — 3x fewer builtins
map.toPairList.mapValues(f)
```

### 5. Prefer Data Equality for Whole-Structure Comparison
When checking that a structure hasn't changed, `equalsData` is a single builtin call
vs recursive field-by-field comparison.

```scala
// Expensive: typed comparison traverses all fields
require(outputDatum === inputDatum)

// Cheap: single builtin comparing serialized form
require(equalsData(toData(outputDatum), toData(inputDatum)))
```

### 6. Leverage Ledger Invariants
The ledger guarantees: inputs are sorted by `TxOutRef`, values are ordered by policy ID,
outputs never contain negative quantities, minted values exclude ADA.
Align your algorithms with these invariants instead of re-validating them.

### 7. Build Caches for Repeated Lookups
If you check membership in the same set multiple times, build a decision closure once.

```scala
// Bad: O(n) per check
require(signatories.exists(_ === admin1))
require(signatories.exists(_ === admin2))

// Better: single traversal, check both
val (hasAdmin1, hasAdmin2) = signatories.foldLeft((false, false)) { case ((a1, a2), sig) =>
    (a1 || sig === admin1, a2 || sig === admin2)
}
require(hasAdmin1 && hasAdmin2)
```

### 8. Use Cheap Builtins for Math
`log2` and `exp2` use `integerToByteString`/`shiftByteString` — much cheaper
than iterative computation.

```scala
// Cheap
val bits = x.log2
val powerOf2 = n.exp2

// Expensive
val bits = manualLog2Loop(x)
val powerOf2 = pow(BigInt(2), n)
```

## Compiler Options That Affect Budget

```scala
given Options = Options(
  // Use V3 lowering for better optimizations (CSE, CaseConstr)
  targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
  // Disable error traces for production — saves budget on every require
  generateErrorTraces = false,
  // Enable UPLC optimizer pipeline
  optimizeUplc = true
)
```

## Measuring Budget

Use `EvalTestDsl` for precise budget measurement:

```scala
import scalus.testing.dsl.EvalTestDsl.*

// Exact budget match — catches regressions AND improvements
eval(compiled)
  .onVM(PlutusV3.makePlutusV3VM())
  .expectSuccess()
  .assertBudgetEquals(memory = 129528, steps = 37_067868)

// Upper bound — for tests where budget fluctuates slightly between builds
eval(compiled)
  .onVM(PlutusV3.makePlutusV3VM())
  .expectSuccess()
  .assertBudgetWithin(memory = 140000, steps = 40_000000)
```

**Format convention:** Always use named parameters and `_` at million boundary:
`ExUnits(memory = 129528, steps = 37_067868)`

## Output Format

Use clickable `file_path:line_number` format for all code locations.

### Finding Format

```
### [IMPACT] ID: Optimization Name

**Location:** `full/path/to/File.scala:LINE`
**Estimated savings:** ~X% memory, ~Y% steps (or: high/medium/low)

**Current code** (`full/path/to/File.scala:LINE-LINE`):
```scala
// actual code from file
```

**Optimized code:**
```scala
// proposed optimization
```

**Rationale:** Why this is faster and what budget cost it avoids.

---
```

### Summary Table

```
## Summary

| ID | Impact | Location | Pattern | Est. Savings |
|----|--------|----------|---------|-------------|
| O-01 | High | `path/File.scala:123` | Multiple traversals → single fold | ~30% steps |
| O-02 | Medium | `path/File.scala:87` | equalsData vs typed === | ~10% steps |
| O-03 | Low | `path/File.scala:200` | Unroll small recursion | ~2% steps |

**Current budget:** ExUnits(memory = X, steps = Y)
**Estimated budget after optimization:** ExUnits(memory = X', steps = Y')
```

## Interactive Workflow

For each finding:
1. Display issue with location and proposed optimization
2. Prompt: "Apply optimization? [y/n/s/d]"
   - y: Apply change, re-run budget test
   - n: Skip, log as "declined"
   - s: Skip without logging
   - d: Show detailed budget breakdown
3. After all findings: run `sbtn quick` to verify correctness
4. Generate summary report with actual budget deltas (before/after)

## Reference

For detailed optimization patterns with Scalus code examples, see:
- `references/patterns.md` — Full pattern catalog with before/after code and budget estimates
