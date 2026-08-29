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

### O007: AssocMap → SortedMap

**Problem:** `AssocMap.get` always scans to a hit or to the end (no ordering to exploit).
`SortedMap.get` terminates early when it passes the target key. `AssocMap.union` is O(n*m):
it calls `rhs.get(k)` per left key and `lhs.toList.exists` per right key; `SortedMap.union`
is one linear merge. `AssocMap` also has no `Eq` instance.

**When to switch:** Always. Default to `SortedMap`; reach for `AssocMap` only when the source
order must be preserved.

**Budget impact:** Lookups stop early on a miss; `union` drops from O(n*m) to O(n+m).

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

### O016: Keep Key Types Concrete at Comparison Sites

**Problem:** `===` on a `BigInt` or `ByteString` that sits behind a type variable lowers to
`equalsData`, not to `equalsInteger` / `equalsByteString`. A generic `K: Eq` lookup pays this
on every element. Measured: 1 761 779 cpu for the generic form against 832 313 cpu for the
concrete-typed clone, a 2.1x gap that no optimizer pass closes.

**Before (generic key, `equalsData` per element):**
```scala
def lookup[K: Eq](key: K, entries: List[(K, BigInt)]): Option[BigInt] =
    entries.find(_._1 === key).map(_._2)
```

**After (concrete key, `equalsInteger` per element):**
```scala
def lookup(key: BigInt, entries: List[(BigInt, BigInt)]): Option[BigInt] =
    entries.find(_._1 === key).map(_._2)
```

**When to use:** Any lookup, dedup or membership test keyed by `BigInt`, `ByteString`,
`PolicyId`, `TokenName` or `PubKeyHash` that is written as a generic helper. Specialise the
helper, or inline the comparison at the call site where the key type is known.

**Budget impact:** 2.1x on the comparison (measured).

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

### O020: Hand-Written equalsData Buys Nothing

**Problem:** Older advice said to replace typed `===` with `equalsData(a.toData, b.toData)` or
`a.toData == b.toData`. For every Data-backed type (case classes, enums, `Value`, `TxOut`,
`Address`) `===` with a derived `Eq` already lowers to that one `equalsData` builtin. The two
spellings produce identical UPLC; both pin to 901 mem / 1 653 665 cpu on a `Value` (measured).

**Before (redundant, harder to read):**
```scala
require(equalsData(continuingOutput.value.toData, ownInput.resolved.value.toData))
```

**After:**
```scala
require(continuingOutput.value === ownInput.resolved.value)
```

**Two things that do cost:**
- `equalsData` is whole-tree: budget about 1 034 543 cpu per compared list element. `Value`
  equality is 1.65 M cpu for lovelace-only and 47.3 M cpu for three policies (measured). Never
  compare `TxInfo`-scale structures whole; compare the field you mean.
- For the continuing output, `===` on the value is usually the wrong check anyway: it rejects a
  builder that must add lovelace for min-ADA. Use
  `continuingOutput.value.hasSameTokensAndAtLeastAda(ownInput.resolved.value)`.

**Budget impact:** None from the spelling. The saving comes from comparing less.

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

## Medium Impact – Stdlib Idioms

The fused primitive already exists in the prelude for each of these. No optimizer pass fuses
two traversals or removes an `Option` allocation, so the choice of method is the optimization.

### O031: hasInlineDatum for Datum Equality

**Problem:** Decoding the datum and comparing the typed value pays for the decode and then for
a field-wise compare. When the check is "the datum equals this value", wrap instead of decode.

**Before (461 lovelace, measured):**
```scala
require(out.datum.inlineOrFail[VestingDatum](NoDatum) === expected, DatumChanged)
```

**After (286 lovelace, measured):**
```scala
require(out.hasInlineDatum(expected), DatumChanged)
```

**When to use:** Equality only. Use `inlineOrFail[T](msg)` when the validator reads fields.

**Budget impact:** 286 vs 461 lovelace per comparison; 706 vs 1 136 with the reference-script fee.

---

### O032: contains over exists for Equality Tests

**Problem:** `xs.exists(p)` is `find(p).isDefined`: it allocates an `Option` to produce a
`Boolean`, and nothing folds that allocation away. On V3 the tax is a fixed per-call
326 483 cpu (miss) / 564 996 cpu (hit), measured; it is 32% of the call at length 1 and
2% at length 20.

**Before:**
```scala
require(tx.signatories.exists(_ === owner), NotSigned)
```

**After:**
```scala
require(tx.signatories.contains(owner), NotSigned)
```

`contains` is an intrinsic: a plain `equalsData` scan with no `Option` and no `Eq` closure.
For `PubKeyHash` specifically, `tx.isSignedBy(owner)` is the named form.

**When it does not apply:** a predicate that is not an equality has no `contains` form. Use
`forall` with the negated predicate, or a hand fold; do not keep `exists` on a hot path.

**Budget impact:** 326 483 / 564 996 cpu per call (measured).

---

### O033: count over filter().length

**Problem:** `xs.filter(p).length` walks all n elements, allocates k `mkCons` cells for the
survivors, then walks the k survivors again. `filter` is a non-tail `foldRight`. No pass fuses
the two.

**Before:**
```scala
require(tx.inputs.filter(_.resolved.address.credential === own).length === BigInt(1), Many)
```

**After:**
```scala
require(tx.inputs.count(_.resolved.address.credential === own) === BigInt(1), Many)
```

`count` is one tail-recursive `foldLeft` with no allocation. When the element is needed too,
go one step further: O034.

**Budget impact:** one traversal instead of two, zero allocation (structural; no pin).

---

### O034: findUniqueOrFail over a Count Guard plus a Lookup

**Problem:** "Exactly one input/output matches, and I need it" is usually written as a count
guard and a separate `find`/`.head`, or as `filter(p)` and `.head`. That is two passes, and
the `.head` form fails with the wrong message (or silently takes the first of several).

**Before:**
```scala
require(tx.inputs.count(_.resolved.address.credential === own) === BigInt(1), Many)
val ownInput = tx.inputs.find(_.resolved.address.credential === own).getOrFail(NoInput)
```

**After:**
```scala
val ownInput = tx.inputs.findUniqueOrFail(_.resolved.address.credential === own, NotSingle)
```

One pass, returns the element, fails on zero or on two-plus matches. The same shape gives
`tx.findContinuingOutputOrFail(ownInput, msg)` (whole-address match) and, for a size-one
list with no predicate, `list.singleOrFail(msg)`.

**Budget impact:** measured against `count(p) === BigInt(1)` as the single-own-input guard:
fee 3 175 vs 3 307 lovelace on 3 inputs, 6 289 vs 6 804 on 10 inputs.

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
