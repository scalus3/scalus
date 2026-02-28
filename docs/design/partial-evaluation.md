# Partial Evaluation for UPLC Optimizer

## Overview

Add a partial evaluation pass to the Scalus UPLC optimizer that evaluates **closed subexpressions**
at compile time using the CEK machine, replacing them with their result.

Examples:

- `addInteger 2 3` → `(con integer 5)`
- `(λx. addInteger x 1) 2` → `(con integer 3)`
- `addInteger (addInteger 1 2) (multiplyInteger 3 4)` → `(con integer 15)`
- `case (constr 0 [1, 2]) of [λa.λb. addInteger a b]` → `(con integer 3)`

## Background

### What is Partial Evaluation?

Partial evaluation specializes a program with respect to known (static) inputs. In the context of
UPLC, when a subexpression is **closed** (no free variables) and consists entirely of evaluable
constructs, we can run it through the CEK machine at compile time and replace it with the result.

This is a well-studied technique in lambda calculus optimization
([A partial evaluator for the untyped lambda-calculus](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/partial-evaluator-for-the-untyped-lambdacalculus/EE324F936F0A009B6766B13FF6755DFC),
[Self-applicable partial evaluator for lambda calculus](https://dl.acm.org/doi/10.1145/128861.128864)).

### Why Use the CEK Machine Instead of Direct Builtin Evaluation?

An alternative approach would be to only fold saturated builtins with constant arguments (e.g.,
detect `addInteger 2 3` and call `BuiltinRuntime.apply()` directly). However, using the full CEK
machine is strictly more powerful:

| Approach            | `addInteger 2 3` | `(λx. addInteger x 1) 2` | `case (constr 0 [1,2]) [λa.λb. addInteger a b]` |
|---------------------|:----------------:|:------------------------:|:-----------------------------------------------:|
| Direct builtin eval |       Yes        |            No            |                       No                        |
| CEK machine         |       Yes        |           Yes            |                       Yes                       |

The CEK machine handles all closed computations uniformly:

- Lambda applications with constant arguments
- Nested builtin chains through lambdas
- Case/Constr elimination on known constructors
- Force/Delay reduction
- Any composition of the above

The overhead is modest: DeBruijn conversion + CEK startup per candidate. We mitigate this with a
budget cap to bail out of expensive computations.

### Why It Matters for Scalus

UPLC scripts run on-chain with strict execution budget limits. Every eliminated computation saves
both CPU and memory budget. The Scalus compiler generates many closed subexpressions that could be
evaluated at compile time — arithmetic on known values, constructor dispatch on known tags,
comparisons, etc.

Combined with the existing Inliner (which performs beta-reduction and substitutes constants),
partial evaluation creates a powerful cascade: after inlining constants, previously-open expressions
become closed and evaluable, and their results may enable further inlining.

### Current Optimizer Pipeline

```
term |> etaReduce |> inliner |> etaReduce |> inliner |> etaReduce |> inliner
     |> strictIf |> builtinsExtractor |> caseConstr
```

The pipeline already runs 3 inliner passes. Adding partial evaluation to the Inliner means it
cascades naturally.

## Design

### What Qualifies for Partial Evaluation

A subexpression is a candidate for partial evaluation when:

1. **It is closed** — has no free variables. Computed via `freeVars(term).isEmpty`.
2. **It is not already a value** — no point evaluating `Const(42)`, `LamAbs(...)`, `Delay(...)`,
   or `Builtin(...)` since they're already in normal form.
3. **It contains at least one reducible operation** — an `Apply`, `Force`, or `Case` node,
   indicating actual computation to perform.

This covers:

- **Saturated builtins:** `addInteger 2 3`, `equalsInteger 5 5`, `lengthOfByteString #deadbeef`
- **Lambda applications:** `(λx. addInteger x 1) 2`, `(λx.λy. x) 42 100`
- **Nested computations:** `addInteger (addInteger 1 2) (multiplyInteger 3 4)`
- **Case/Constr:** `case (constr 0 [1,2]) of [λa.λb. addInteger a b]`
- **Force/Delay:** `force (delay 42)` (already handled by Inliner, but unified here)
- **Any closed composition** of the above

### Approach: CEK Machine with Budget Cap

We run the CEK machine on closed candidate subexpressions. To handle potentially expensive
computations (e.g., large exponentiations), we use a `RestrictingBudgetSpender` with a reasonable
budget cap.

#### Algorithm

```
After the Inliner has processed all subterms (inlined, beta-reduced, etc.):

1. Check if the resulting term is a candidate for partial evaluation:
   a. freeVars(term) is empty (closed)
   b. Term is not already a value (Const, LamAbs, Delay, Builtin)
   c. Term contains a reducible operation (Apply, Force, Case)

2. DeBruijn-convert the term: DeBruijn.deBruijnTerm(term)

3. Evaluate with budget-capped CEK machine:
   - Use RestrictingBudgetSpender with a configurable max budget
   - Use NoLogger (no trace output needed)

4. On success:
   - Convert result back: DeBruijn.fromDeBruijnTerm(result)
   - If result is simpler than original (e.g., a Const), use it
   - Log the optimization

5. On failure (budget exceeded, runtime error, etc.):
   - Keep the original term unchanged
   - This preserves runtime semantics (errors still occur at runtime)
```

#### What "Simpler" Means

After CEK evaluation, the result could be:

- `Const(...)` — always simpler, replace
- `LamAbs(...)` — a closure; may or may not be simpler than the original. Replace if the result
  term is smaller (fewer nodes) than the input.
- `Delay(...)` — similar reasoning
- Other — keep original to be safe

For Phase 1, we replace only when the result is a `Const`. This is the common case and the safest.
We can extend to lambda/delay results later.

### Integration Point: Inside the Inliner

The partial evaluator integrates into the existing `Inliner.inlinePass` method. After all subterms
are processed, we check if the resulting term can be partially evaluated.

This is the ideal integration point because:

- The Inliner already runs 3 times in the pipeline, enabling cascading evaluation
- After beta-reduction and constant substitution, previously-open expressions become closed
- It's a natural extension of the Inliner's "simplify what you can" philosophy

#### Where in the code

In `Inliner.inlinePass`, the `go` method returns a processed term. We wrap the result:

```scala
// After processing any Apply/Force/Case node, try partial evaluation
val result = <existing processing>
  tryPartialEval(result).getOrElse(result)
```

Specifically, the check applies after:

- The `Apply` case (both the beta-reduction branch and the fallthrough branch)
- The `Force` case (after Force/Delay elimination)
- The `Case` case

### Closedness Check

We need a `freeVars` function. The Inliner already has one inside `substitute`, and `EtaReduce`
has `freeNames`. We should promote this to `TermAnalysis` as a shared utility.

```scala
// In TermAnalysis
def freeVars(term: Term): Set[String] = term match
  case Var(NamedDeBruijn(n, _), _) => Set(n)
  case LamAbs(n, body, _) => freeVars(body) - n
  case Apply(f, a, _) => freeVars(f) ++ freeVars(a)
  case Force(t, _) => freeVars(t)
  case Delay(t, _) => freeVars(t)
  case Constr(_, args, _) => args.flatMap(freeVars).toSet
  case Case(scrut, cases, _) => freeVars(scrut) ++ cases.flatMap(freeVars)
  case _: Const | _: Builtin | _: Error => Set.empty
```

### Purity Considerations

`TermAnalysis.isPure` classifies saturated builtin applications as **impure** because they *can*
fail (e.g., division by zero). This is correct for dead code elimination.

For partial evaluation, purity is **not** the gate. Instead:

- We attempt evaluation and only replace on success
- If evaluation throws, we keep the original (preserving runtime error semantics)
- The gate is **closedness** (no free variables), not purity

### Safety Considerations

**Semantic preservation:** We only replace when evaluation succeeds. Failed evaluations (division by
zero, out-of-range indexing, budget exceeded, etc.) leave the term unchanged.

**Determinism:** All UPLC operations are deterministic. Same inputs → same outputs.

**Non-termination:** UPLC has no native recursion. The Y-combinator could theoretically cause
non-termination, but:

1. It won't appear in closed constant subexpressions in compiler output
2. The budget cap guarantees termination of the CEK machine regardless

**Budget cap:** We use `RestrictingBudgetSpender` with a generous but finite budget (e.g., 1M CPU
steps). This prevents the optimizer from spending excessive time on complex computations that
wouldn't meaningfully reduce script size anyway.

**Platform specificity:** `Meaning.allBuiltins` uses JVM platform implementations. These are
consensus-critical and produce identical results across all platforms.

**Result size:** A folded constant could be larger than the expression that produced it (e.g.,
`replicateByteString 10000 0x00`). For Phase 1 we only fold to `Const`, and large constants are
unusual in practice. We can add a size check later if needed.

## Implementation Plan (TDD)

### Step 1: Add `freeVars` to TermAnalysis

Promote the free-variable computation to a shared utility in `TermAnalysis`, so both the Inliner
and the partial evaluator can use it.

**File:** `scalus-core/shared/src/main/scala/scalus/uplc/transform/TermAnalysis.scala`

Add as an extension method on `Term`:

```scala
extension (term: Term)
  def freeVars: Set[String] =...
```

**Tests:** Add tests in `TermAnalysisTest.scala` for `freeVars`:

- Variable: `freeVars(x) == Set("x")`
- Const: `freeVars(42) == Set.empty`
- Lambda binding: `freeVars(λx. x) == Set.empty`
- Lambda with free var: `freeVars(λx. y) == Set("y")`
- Apply: `freeVars(f x) == Set("f", "x")`
- Shadowing: `freeVars(λx. λx. x) == Set.empty`
- Closed expression: `freeVars(addInteger 1 2) == Set.empty`

### Step 2: Create `PartialEvaluator` object

**File:** `scalus-core/shared/src/main/scala/scalus/uplc/transform/PartialEvaluator.scala`

```scala
package scalus.uplc.transform

import scalus.uplc.*
import scalus.uplc.Term.*
import scalus.uplc.eval.*
import scalus.uplc.transform.TermAnalysis.freeVars
import scalus.cardano.ledger.ExUnits

object PartialEvaluator {

  /** Default budget cap for partial evaluation attempts.
   * Generous enough for typical constant folding, but prevents runaway evaluation.
   */
  val DefaultMaxBudget: ExUnits = ExUnits(memory = 1_000_000, steps = 1_000_000_000)

  /** Lazily initialized VM for partial evaluation. */
  private lazy val vm: PlutusVM = PlutusVM.makePlutusV3VM()

  /** Try to partially evaluate a closed term.
   *
   * Returns Some(result) if:
   * - The term is closed (no free variables)
   * - The term is reducible (contains Apply, Force, or Case)
   * - CEK evaluation succeeds within budget
   * - The result is a Const (Phase 1)
   *
   * Returns None otherwise, meaning the original term should be kept.
   */
  def tryEval(term: Term, maxBudget: ExUnits = DefaultMaxBudget): Option[Term] = {
    // Skip values already in normal form
    if isValue(term) then return None

    // Must be closed
    if term.freeVars.nonEmpty then return None

    // Must contain reducible structure
    if !hasReducibleOp(term) then return None

    try
      val debruijned = DeBruijn.deBruijnTerm(term)
      val budgetSpender = new RestrictingBudgetSpender(maxBudget)
      val result = vm.evaluateDeBruijnedTerm(debruijned, budgetSpender, NoLogger)
      result match
        case Const(_, _) => Some(result)
        case _ => None // Phase 1: only fold to constants
    catch
      case _: Exception => None // Budget exceeded, runtime error, etc.
  }

  /** A term is already a value (in normal form) if it's a constant, lambda, delay, or builtin. */
  private def isValue(term: Term): Boolean = term match
    case _: Const | _: LamAbs | _: Delay | _: Builtin => true
    case _ => false

  /** Check if a term contains at least one reducible operation. */
  private def hasReducibleOp(term: Term): Boolean = term match
    case Apply(_, _, _) | Force(_, _) | Case(_, _, _) => true
    case Constr(_, args, _) => args.exists(hasReducibleOp)
    case _ => false
}
```

### Step 3: Integrate into Inliner

Modify `Inliner.inlinePass` to attempt partial evaluation after processing subterms.

**Changes to `Inliner.scala`:**

Add a helper method:

```scala
private def tryPartialEval(term: Term): Term =
  PartialEvaluator.tryEval(term) match
    case Some(result) =>
      logger.log(s"Partial evaluation: ${term.showShort} => ${result.showShort}")
      result
    case None => term
```

Then call it in the `go` method:

1. In the `Apply` case, after the `case _ =>` fallthrough:

```scala
case _ =>
  tryPartialEval(Apply(inlinedF, inlinedArg, ann))
```

2. In the `Force` case (the non-Delay branch):

```scala
case Force(t, ann)
=> tryPartialEval(Force(go(t, env), ann))
```

3. In the `Case` case:

```scala
case Case(scrutinee, cases, ann)
=>
val result = Case(go(scrutinee, env), cases.map(c => go(c, env)), ann)
tryPartialEval(result)
```

### Step 4: Update Optimizer Pipelines (if needed)

The existing pipeline already runs 3 Inliner passes. Since partial evaluation is integrated into
the Inliner, no pipeline changes are needed. Verify that 3 passes suffice for cascading.

## Test Plan

### Test File

`scalus-core/shared/src/test/scala/scalus/uplc/transform/PartialEvaluatorTest.scala`

### Unit Tests for `PartialEvaluator.tryEval`

#### Basic Arithmetic Folding (Saturated Builtins)

```
1.  addInteger 2 3                    → Const(Integer(5))
2.  subtractInteger 10 3              → Const(Integer(7))
3.  multiplyInteger 4 5               → Const(Integer(20))
4.  divideInteger 10 3                → Const(Integer(3))
5.  quotientInteger 10 3              → Const(Integer(3))
6.  remainderInteger 10 3             → Const(Integer(1))
7.  modInteger 10 3                   → Const(Integer(1))
```

#### Comparison Folding

```
8.  equalsInteger 5 5                 → Const(Bool(true))
9.  equalsInteger 5 6                 → Const(Bool(false))
10. lessThanInteger 1 2               → Const(Bool(true))
11. lessThanInteger 2 1               → Const(Bool(false))
12. lessThanEqualsInteger 2 2         → Const(Bool(true))
```

#### ByteString Operations

```
13. appendByteString #dead #beef      → Const(ByteString(#deadbeef))
14. lengthOfByteString #deadbeef      → Const(Integer(4))  [note: #deadbeef = 4 bytes]
15. equalsByteString #dead #dead      → Const(Bool(true))
16. indexByteString #deadbeef 0       → Const(Integer(0xde))
```

#### String Operations

```
17. appendString "hello" " world"     → Const(String("hello world"))
18. equalsString "abc" "abc"          → Const(Bool(true))
19. encodeUtf8 "hello"               → Const(ByteString(...))
```

#### Lambda Applications (CEK machine advantage over direct builtin eval)

```
20. (λx. addInteger x 1) 2           → Const(Integer(3))
21. (λx.λy. addInteger x y) 2 3      → Const(Integer(5))
22. (λx. multiplyInteger x x) 5      → Const(Integer(25))
23. (λf.λx. f x) (λy. addInteger y 1) 2
                                      → Const(Integer(3))  [higher-order]
```

#### Nested Computations

```
24. addInteger (addInteger 1 2) (multiplyInteger 3 4)
                                      → Const(Integer(15))
25. multiplyInteger (subtractInteger 10 3) (addInteger 1 1)
                                      → Const(Integer(14))
```

#### Case/Constr Evaluation

```
26. case (constr 0 [1, 2]) of [λa.λb. addInteger a b, λc. c]
                                      → Const(Integer(3))
27. case (constr 1 [42]) of [λa.λb. a, λc. c]
                                      → Const(Integer(42))
```

#### Force/Delay

```
28. force (delay 42)                  → Const(Integer(42))
    (Already handled by Inliner, but verify unified behavior)
29. force (delay (addInteger 1 2))    → Const(Integer(3))
    (Inliner removes force/delay, then partial eval folds the addition)
```

#### Error Cases (Should Return None / Keep Original)

```
30. divideInteger 1 0                 → None (division by zero)
31. indexByteString #dead 99          → None (index out of bounds)
32. addInteger 2 x                    → None (x is free variable — not closed)
33. addInteger 2                      → None (partial application — already a value-like form)
34. Force(Builtin(HeadList))          → None (not saturated — partial application)
35. Error                             → None (already a value/not reducible)
36. λx. addInteger x 1               → None (has free var x under lambda; lambda is a value)
37. Const(Integer(42))               → None (already a value)
```

#### Edge Cases

```
38. addInteger 0 0                    → Const(Integer(0))
39. equalsInteger (-1) (-1)           → Const(Bool(true))
40. multiplyInteger large_a large_b   → Const(Integer(large_result))
    (Verify no overflow; BigInt handles arbitrary precision)
41. Very expensive computation with small budget cap → None (budget exceeded, keep original)
```

### Integration Tests with Inliner

```
42. Inliner(addInteger 2 3)          → Const(Integer(5))
43. Inliner(λ("x")(addInteger vr"x" 3) $ 2)
    → β-reduce to addInteger 2 3, then fold to Const(5)
44. Inliner(λ("x", "y")(addInteger vr"x" vr"y") $ 2 $ 3)
    → β-reduce then fold to Const(5)
45. Inliner(addInteger (addInteger 1 2) (multiplyInteger 3 4))
    → Fold all at once (closed expression): Const(15)
46. Inliner(λ("x")(42) $ (addInteger 2 3))
    → Fold addInteger 2 3 to Const(5), then DCE (pure arg, unused): Const(42)
47. Inliner(λ("x")(vr"x") $ (addInteger 2 3))
    → Fold then identity elimination: Const(5)
48. Inliner(equalsInteger 1 1)        → Const(Bool(true))
```

### Non-Folding Cases (Preserved by Inliner)

```
49. Inliner(addInteger vr"x" 3)      → addInteger vr"x" 3 (free variable)
50. Inliner(addInteger vr"x" vr"y")  → unchanged (both free)
51. Inliner(divideInteger 1 0)        → divideInteger 1 0 (preserves runtime error)
52. Inliner(Force(Builtin(HeadList))) → unchanged (partial application, already a value)
```

### Cascading Tests (Inliner + Partial Eval across passes)

```
53. Term: λ("a")(λ("b")(addInteger vr"a" vr"b") $ 3) $ 2
    Pass 1: β-reduce inner → λ("a")(addInteger vr"a" 3) $ 2
            β-reduce outer → addInteger 2 3 → fold → Const(5)
    (May complete in 1 pass since β-reduction and fold happen in same traversal)

54. Term: λ("x")(multiplyInteger vr"x" vr"x") $ (addInteger 1 2)
    Fold addInteger 1 2 → 3, then β-reduce → multiplyInteger 3 3 → fold → 9

55. Term: Complex nested let-bindings
    λ("a")(λ("b")(λ("c")(addInteger (multiplyInteger vr"a" vr"b") vr"c") $ 5) $ 3) $ 2
    → Should eventually fold to Const(Integer(11))  [2*3 + 5 = 11]
```

### Budget Cap Tests

```
56. Term with computation exceeding budget cap → None (kept unchanged)
    Construct a term that would require excessive steps to evaluate.
57. Normal-sized computation well within budget → successfully folded
```

### Interaction with Other Optimizers

```
58. After StrictIf: verify partial eval doesn't break strict if optimization
59. After EtaReduce: verify compositions work correctly
60. Full pipeline (V3Optimizer): verify end-to-end optimization including partial eval
```

### freeVars Tests (in TermAnalysisTest.scala)

```
61. Var("x").freeVars == Set("x")
62. Const(42).freeVars == Set.empty
63. Builtin(AddInteger).freeVars == Set.empty
64. LamAbs("x", Var("x")).freeVars == Set.empty
65. LamAbs("x", Var("y")).freeVars == Set("y")
66. Apply(Var("f"), Var("x")).freeVars == Set("f", "x")
67. Apply(Builtin(AddInteger), Const(1)).freeVars == Set.empty  [closed builtin app]
68. (addInteger 2 3).freeVars == Set.empty
69. LamAbs("x", LamAbs("x", Var("x"))).freeVars == Set.empty  [shadowing]
70. Force(Delay(Var("x"))).freeVars == Set("x")
71. Constr(0, [Const(1), Var("x")]).freeVars == Set("x")
72. Case(Var("s"), [Const(1), Var("x")]).freeVars == Set("s", "x")
73. Error.freeVars == Set.empty
```

## File Changes Summary

| File                                                   | Change                                          |
|--------------------------------------------------------|-------------------------------------------------|
| `scalus-core/.../transform/TermAnalysis.scala`         | Add `freeVars` extension method                 |
| `scalus-core/.../transform/PartialEvaluator.scala`     | **New** — `tryEval` using CEK machine           |
| `scalus-core/.../transform/Inliner.scala`              | Call `tryPartialEval` after processing subterms |
| `scalus-core/.../transform/PartialEvaluatorTest.scala` | **New** — comprehensive test suite              |
| `scalus-core/.../transform/TermAnalysisTest.scala`     | Add `freeVars` tests                            |

No changes to `OptimizerPipelines.scala` — the existing 3-pass structure handles cascading.

## Open Questions

1. **Budget cap value:** 1M memory / 1B steps is a starting point. We should tune based on
   real-world scripts. Too low = missed optimizations. Too high = slow compilation.

2. **Folding to non-Const results:** Phase 1 only folds when the result is `Const`. Should we also
   fold when the result is a smaller `LamAbs` or `Delay`? This could optimize more cases but adds
   complexity (need to compare term sizes). **Recommendation:** Phase 1 = Const only. Phase 2 =
   extend to smaller results.

3. **VM instance sharing:** Should the `PlutusVM` instance be created once (lazy val) or per
   optimization run? Lazy val is simpler and sufficient since the VM is stateless.
   **Recommendation:** Lazy val in `PartialEvaluator` object.

4. **Which Plutus version for the VM?** Using V3 covers the widest set of builtins. Scripts
   targeting V1/V2 use a subset that's compatible.
   **Recommendation:** V3 VM, which is a superset.

## Risks

- **Evaluation at compile-time with wrong semantics:** Mitigated by using the same CEK machine and
  builtin implementations used at runtime.
- **Performance regression in compiler:** `freeVars` computation on every Apply/Force/Case node.
  Mitigated by: (a) the check is O(n) in term size and only happens after inlining, (b) actual CEK
  evaluation only triggers for closed terms, which are a small subset.
- **Budget cap too conservative:** Could miss valid optimizations. Easily tunable.
- **Budget cap too generous:** Could slow compilation. The cap provides an upper bound; most folds
  will be cheap arithmetic that completes in microseconds.
- **Cross-platform consistency:** CEK machine produces identical results on all platforms for
  consensus-critical builtins. No risk.
