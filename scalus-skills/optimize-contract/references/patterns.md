# Scalus Smart Contract Optimization Patterns

Detailed catalog of optimization patterns with Scalus code examples, UPLC budget impact,
and step-by-step rewrite guidance. Organized by impact category.

---

## High Impact — Data Structures & Traversals

### O001: Multiple List Traversals → Single Fold

**Problem:** Chaining `filter`, `map`, `length`, `exists` on the same list creates
multiple intermediate lists and traverses n elements multiple times.

**Before (3 traversals, 2 intermediate lists):**
```scala
val validOutputs = outputs.filter(o => o.address === targetAddress)
val amounts = validOutputs.map(o => o.value.getLovelace)
val totalPaid = amounts.foldLeft(BigInt(0))(_ + _)
require(totalPaid >= requiredAmount)
```

**After (1 traversal, no intermediate lists):**
```scala
val totalPaid = outputs.foldLeft(BigInt(0)) { (acc, o) =>
    if o.address === targetAddress then acc + o.value.getLovelace
    else acc
}
require(totalPaid >= requiredAmount)
```

**Budget impact:** ~40-60% reduction in steps for the traversal portion. Each eliminated
traversal saves O(n) builtin calls.

---

### O002: foldRight → foldLeft

**Problem:** `foldRight` is not tail-recursive in Scalus. It builds a chain of deferred
applications on the stack. For lists of 50+ elements this wastes significant budget.

**Before:**
```scala
val result = items.foldRight(List.empty[BigInt]) { (item, acc) =>
    if item.isValid then item.value +: acc else acc
}
```

**After:**
```scala
val result = items.foldLeft(List.empty[BigInt]) { (acc, item) =>
    if item.isValid then item.value +: acc else acc
}.reverse  // Only add reverse if order matters
```

**Note:** `filter`, `map`, and `flatMap` on `List` all use `foldRight` internally.
If you need to process a large list with these, consider writing a manual `foldLeft`.

**Budget impact:** Eliminates thunk allocation overhead. ~20-30% reduction for large lists.

---

### O003: list.flatten → foldLeft Accumulation

**Problem:** `flatten` uses `foldRight` with `++`, creating O(n*m) concatenation work.

**Before:**
```scala
val allTokens = outputs.map(_.value.toSortedMap.toList).flatten
```

**After:**
```scala
val allTokens = outputs.foldLeft(List.empty[(PolicyId, SortedMap[TokenName, BigInt])]) { (acc, o) =>
    o.value.toSortedMap.toList ++ acc  // prepend is cheaper
}
```

**Budget impact:** High for nested structures. Can reduce from O(n*m) to O(n+m).

---

### O005: Append → Prepend + Reverse

**Problem:** `list :+ elem` (append) is O(n) per call — it must traverse to the end.
In a loop, this becomes O(n^2).

**Before (O(n^2)):**
```scala
var result = List.empty[BigInt]
items.foldLeft(result) { (acc, item) =>
    acc :+ item.value  // O(n) per append
}
```

**After (O(n)):**
```scala
val result = items.foldLeft(List.empty[BigInt]) { (acc, item) =>
    item.value +: acc  // O(1) prepend
}.reverse  // Single O(n) reverse at the end
```

**Budget impact:** O(n) vs O(n^2) — dramatic for lists > 10 elements.

---

### O006: Avoid Reconstructing Value

**Problem:** `Value` is `SortedMap[PolicyId, SortedMap[TokenName, BigInt]]`. Constructing
a `Value` enforces invariants (no zero quantities, no empty inner maps). If you just need
to check or accumulate amounts, work with `SortedMap` or raw `Data` directly.

**Before:**
```scala
val outputValue = Value.singleton(policyId, tokenName, BigInt(1))
require(txOut.value === inputValue + outputValue)
```

**After (when checking specific token):**
```scala
require(txOut.value.quantityOf(policyId, tokenName) >= BigInt(1))
```

**Budget impact:** Avoids full Value construction. ~15-25% reduction depending on value complexity.

---

### O007: AssocMap → SortedMap for Larger Maps

**Problem:** `AssocMap.get` always scans the entire list (no ordering to exploit).
`SortedMap.get` terminates early when it passes the target key.

**When to switch:** If the map has > ~5 entries and you do multiple lookups.

**Budget impact:** Average case ~50% fewer comparisons for lookups.

---

### O010: AssocMap.fromList → SortedMap.fromStrictlyAscendingList

**Problem:** `AssocMap.fromList` is O(n^2) due to dedup via `foldLeft` + `exists`.

**If your data is already sorted (e.g., from ledger):**
```scala
// O(1) — no validation, trust the source
SortedMap.unsafeFromList(pairs)

// O(n) — validates ascending order
SortedMap.fromStrictlyAscendingList(pairs)
```

**Budget impact:** O(n) or O(1) vs O(n^2).

---

## High Impact — Short-Circuiting & Fail-Fast

### O011: Cheap Checks First

**Problem:** Expensive operations (value calculations, list traversals) execute even on
invalid transactions that would fail a simple signature or datum check.

**Before:**
```scala
// Expensive value check first
val totalValue = computeExpensiveValueSum(txInfo.inputs)
require(totalValue >= threshold)
// Cheap check last
require(isSignedBy(txInfo, admin))
```

**After:**
```scala
// Cheap check first — fails fast on unauthorized txs
require(isSignedBy(txInfo, admin))
// Expensive check only runs if signature valid
val totalValue = computeExpensiveValueSum(txInfo.inputs)
require(totalValue >= threshold)
```

**Budget impact:** For failing transactions (which is the attack surface), this can save
the entire cost of the expensive computation. For valid transactions, order doesn't matter.

---

### O012: Fail Fast — Validate Inputs at the Top

**Problem:** Work done before validation is wasted when the transaction is invalid.

**Principle:** Order your validator logic as:
1. Decode/validate redeemer (cheapest — it's passed directly)
2. Check signatures and simple datum fields
3. Filter/find relevant inputs and outputs
4. Compute and validate amounts

---

### O013: Binary Decision Trees for Multiple Conditions

**Problem:** A linear chain of `if/else if/else if` evaluates conditions one by one.
With 8 redeemer actions, the last one requires 7 failed comparisons.

**Before (linear — average 4 comparisons for 8 actions):**
```scala
if action === Mint then handleMint()
else if action === Burn then handleBurn()
else if action === Transfer then handleTransfer()
else if action === Stake then handleStake()
else if action === Unstake then handleUnstake()
else if action === Claim then handleClaim()
else if action === Update then handleUpdate()
else handleAdmin()
```

**After (binary — average 3 comparisons):**
```scala
if action.tag < 4 then
    if action.tag < 2 then
        if action === Mint then handleMint() else handleBurn()
    else
        if action === Transfer then handleTransfer() else handleStake()
else
    if action.tag < 6 then
        if action === Unstake then handleUnstake() else handleClaim()
    else
        if action === Update then handleUpdate() else handleAdmin()
```

**Budget impact:** ~25% fewer comparisons on average. More predictable worst case.

---

## Medium Impact — Data Representation

### O016: equalsData for Whole-Structure Comparison

**Problem:** Typed `===` on complex structures (e.g., `TxOut`, `Datum` with many fields)
recursively compares each field using type-specific `Eq` instances.

**Before:**
```scala
// Compares address field-by-field, then value field-by-field, then datum...
require(outputTxOut === expectedTxOut)
```

**After:**
```scala
import scalus.uplc.builtin.Builtins.equalsData
import scalus.uplc.builtin.ToData

// Single builtin — compares serialized CBOR representations
require(equalsData(toData(outputTxOut), toData(expectedTxOut)))
```

**When to use:** When both sides are already available as the same type and you
just need equality. NOT when you need to compare individual fields.

**Budget impact:** ~10-40% reduction depending on structure complexity.

---

### O017: Continuation-Passing to Avoid Tuple Allocation

**Problem:** Returning multiple values via tuples or case classes allocates on-chain
and requires destructuring.

**Before:**
```scala
def findMinMax(list: List[BigInt]): (BigInt, BigInt) = {
    list.foldLeft((BigInt("999999999"), BigInt(0))) { case ((min, max), x) =>
        (if x < min then x else min, if x > max then x else max)
    }
}
val (min, max) = findMinMax(amounts)
require(max - min < threshold)
```

**After (continuation-passing — no tuple allocated):**
```scala
def withMinMax(list: List[BigInt])(cont: (BigInt, BigInt) => Unit): Unit = {
    val min = list.foldLeft(BigInt("999999999"))((m, x) => if x < m then x else m)
    val max = list.foldLeft(BigInt(0))((m, x) => if x > m then x else m)
    cont(min, max)
}
withMinMax(amounts) { (min, max) =>
    require(max - min < threshold)
}
```

**Note:** In practice, the two-fold version may be comparable or cheaper than a
single fold returning tuples, because tuple allocation + destructuring can be
more expensive than two simple traversals. Measure both.

**Budget impact:** Low-medium. Depends on how many intermediate structures are avoided.

---

### O018: PairList for Map Operations

**Problem:** `List[(A, B)]` operations require pattern matching each pair (~12 builtins
per element). `PairList` uses `fstPair`/`sndPair` builtins directly (~4 builtins per element).

**Before:**
```scala
val tokens: AssocMap[TokenName, BigInt] = ...
val doubled = tokens.toList.map { case (name, qty) => (name, qty * 2) }
```

**After:**
```scala
val tokens: AssocMap[TokenName, BigInt] = ...
val doubled = tokens.toPairList.mapValues(_ * 2)
```

**Budget impact:** ~3x fewer builtin operations per element for map-over-values operations.

---

### O020: Data Equality for Unchanged Value Checks

**Problem:** A common pattern is checking that a continuing output preserves the same
value as the input. Typed `Value` equality compares the nested map-of-maps structure.

**Before:**
```scala
require(continuingOutput.value === ownInput.value)
```

**After:**
```scala
// If you have the raw Data available:
require(equalsData(continuingOutput.value.toData, ownInput.value.toData))
```

**Budget impact:** Significant for `Value` which is `SortedMap[BS, SortedMap[BS, BigInt]]`.

---

## Medium Impact — Computation

### O021 & O022: Use Builtin-Based Math

**Problem:** Generic `pow(2, n)` loops through exponentiation by squaring.
Manual `log2` divides repeatedly. Both are much more expensive than the
builtin-based alternatives.

```scala
// Cheap builtins:
import scalus.cardano.onchain.plutus.prelude.{log2, exp2}

val bits = x.log2    // integerToByteString + lengthOfByteString
val pow2 = n.exp2    // byteStringToInteger + shiftByteString
```

---

### O023: Use Let Bindings to Avoid Recomputation

**Problem:** Same expression evaluated multiple times.

**Before:**
```scala
require(txInfo.inputs.filter(i => i.resolved.address === ownAddress).length >= 1)
val ownInputs = txInfo.inputs.filter(i => i.resolved.address === ownAddress)
val totalValue = computeValue(ownInputs)
```

**After:**
```scala
val ownInputs = txInfo.inputs.filter(i => i.resolved.address === ownAddress)
require(ownInputs.length >= 1)
val totalValue = computeValue(ownInputs)
```

**Note:** The V3 optimizer has CSE (Common Subexpression Elimination) that can catch
some duplicates, but don't rely on it. Explicit `val` bindings are clearer and guaranteed.

---

### O024: Disable Error Traces for Production

**Problem:** `generateErrorTraces = true` adds `Trace` calls for every `require` message.
Each trace string is encoded in the script and evaluated even when the transaction succeeds.

```scala
// Development: traces help debugging
given Options = Options(generateErrorTraces = true)

// Production: no traces, smaller script, less budget
given Options = Options(generateErrorTraces = false)
```

**Budget impact:** Saves ~100-500 steps per `require` statement. Adds up with many requires.
Also reduces script size (smaller transaction fees).

---

### O025: Don't Compute, Verify

**The single most impactful optimization pattern.** Any expensive computation can be
moved off-chain if correctness can be verified cheaply on-chain.

**Examples:**

**Square root:**
```scala
// Off-chain: compute sqrt(n) and pass as redeemer field
// On-chain: verify
val s = redeemer.sqrtValue
require(s * s <= n && (s + 1) * (s + 1) > n, "invalid sqrt")
```

**Sorting:**
```scala
// Off-chain: sort the list and pass indices as redeemer
// On-chain: verify the list is sorted (single O(n) pass)
require(isSorted(redeemer.sortedList), "not sorted")
```

**Set membership with Merkle proofs:**
```scala
// Off-chain: compute Merkle proof for element
// On-chain: verify proof against known root hash
require(verifyMerkleProof(element, redeemer.proof, knownRoot), "not in set")
```

**Budget impact:** Potentially orders of magnitude. Verification is almost always
cheaper than computation.

---

## Low Impact — Micro-Optimizations

### O026: Unroll Small Recursions

**Problem:** For helper functions called on small inputs (1-3 elements), the recursion
overhead (lambda application, pattern matching) dominates.

**Before:**
```scala
def sumList(xs: List[BigInt]): BigInt = xs match
    case Nil => BigInt(0)
    case head :: tail => head + sumList(tail)
```

**After:**
```scala
def sumList(xs: List[BigInt]): BigInt = xs match
    case Nil => BigInt(0)
    case x1 :: Nil => x1
    case x1 :: x2 :: Nil => x1 + x2
    case x1 :: x2 :: rest => x1 + x2 + sumList(rest)  // unrolled by 2
```

**Budget impact:** ~5-15% for small lists. Negligible for large lists.

---

### O027: Tail-Recursive Numeric Loops

**Problem:** Non-tail-recursive loops waste budget on stack frames.

**Before:**
```scala
def countBits(n: BigInt): BigInt =
    if n == BigInt(0) then BigInt(0)
    else BigInt(1) + countBits(n / BigInt(2))
```

**After:**
```scala
def countBits(n: BigInt): BigInt = {
    def go(n: BigInt, acc: BigInt): BigInt =
        if n == BigInt(0) then acc
        else go(n / BigInt(2), acc + BigInt(1))
    go(n, BigInt(0))
}
```

**Or just use the builtin:** `n.log2 + 1`

---

### O028: Avoid Redundant FromData/ToData Conversions

**Problem:** Converting between Scala types and `Data` is not free. If you decode
a datum, modify one field, and re-encode, consider whether you can work at the
`Data` level directly.

**When to consider:** If you're doing `fromData` → small change → `toData` and the
structure is large, the serialization round-trip may dominate.

---

## Red Flags — Functions to Watch For

These standard library functions are correct but expensive. When found in
performance-critical validators, consider replacing with manual folds:

| Function | Cost | Alternative |
|----------|------|-------------|
| `list.map` | O(n), uses foldRight, creates intermediate list | Manual foldLeft |
| `list.flatMap` | O(n*m), uses foldRight | Manual foldLeft with prepend |
| `list.filter` | O(n), uses foldRight, creates intermediate list | Combine with other operations |
| `list.flatten` | O(n*m), nested foldRight + ++ | foldLeft with prepend |
| `list.distinct` | O(n^2) | Pre-sort or deduplicate at source |
| `list.reverse` | O(n) | Build in correct order |
| `list.sort` | O(n^2) insertion sort | Sort off-chain, verify on-chain |
| `AssocMap.fromList` | O(n^2) | SortedMap with pre-sorted input |
| `Value` arithmetic | Multiple map merges | Work with raw SortedMap |

## Optimization Workflow Summary

1. Write the simplest obviously correct validator
2. Add budget tests with `assertBudgetEquals`
3. Identify hot paths by testing worst-case inputs
4. Apply high-impact patterns first (single traversals, fail fast, don't compute/verify)
5. Re-measure after each change
6. Apply medium-impact patterns if budget is still too high
7. Micro-optimize only if you're within ~10% of target

**Cardinal rule:** Never sacrifice correctness for performance. A validator that's
fast but wrong is worse than one that's slow but correct.
