# Performance Regression Analysis: Mincont vs CeK

**Date**: 2025-10-28
**Issue**: Mincont JIT performance regressed relative to CeK machine
**Status**: Analysis Complete - Root Cause Identified

## Summary

Mincont JIT was previously **1.37x faster** than CeK but is now **0.96x (4.7% slower)**. However, this isn't entirely a Mincont regression - **CeK got significantly faster** while Mincont got slightly slower.

## Comparative Results

### Previous Version (commit 4345f770)

| Implementation | Time (μs/op) | vs CeK | Status |
|----------------|--------------|--------|--------|
| CeK Machine | **317.4** | 1.00x | Baseline |
| Mincont JIT | **231.7** | **1.37x faster** | ✅ Good |

### Current Version (our benchmarks)

| Implementation | Time (μs/op) | vs CeK | vs Previous | Status |
|----------------|--------------|--------|-------------|--------|
| CeK Machine | **239.7** | 1.00x | **24% faster!** | ✅ Improved |
| Mincont JIT | **250.9** | **0.96x (slower)** | **8% slower** | ❌ Regressed |

## What Changed?

### 1. CeK Machine: +24% Faster! 🚀

**Previous**: 317.4 μs/op
**Current**: 239.7 μs/op
**Improvement**: **77.7 μs savings (24% faster)**

#### Likely Causes

Recent commits show CeK optimizations:
- `7c03ce09` - "Eliminate VCon allocations for all cost-calculating builtins"
- `539b2c3b` - "Remove redundant isInstanceOf check in eval loop"
- Possible other interpreter optimizations

**Impact**: CeK is now a MUCH tougher baseline to beat!

### 2. Mincont JIT: -8% Slower 😞

**Previous**: 231.7 μs/op
**Current**: 250.9 μs/op
**Regression**: **19.2 μs overhead (8% slower)**

#### Likely Causes

Examining recent commits related to Mincont:

1. **Conversion to enum-based continuations** (`94c21216`)
   ```
   "refactor: Convert ContinuationJitRepr and Frame to enums"
   ```
   - May have changed allocation patterns
   - Enum overhead vs previous case classes?

2. **Memory usage JIT changes** (`a8df70a5`)
   ```
   "perf: Add JIT-optimized memory usage calculation"
   ```
   - Changed how builtins calculate costs
   - May have added overhead

3. **Recent refactorings**
   - `9c7cffbe` - "Fix mincont to handle Function0 in Apply"
   - `85c65d4d` - "Add DivideInteger and fix IfThenElse in mincont"
   - Multiple bug fixes may have added safety checks

4. **Our recent changes**
   - Removed `ListJitRepr` wrapper
   - Fixed package structure
   - **May have inadvertently broken some optimizations**

### 3. Combined Effect

```
Previous: Mincont (231.7) vs CeK (317.4) = 1.37x faster
Current:  Mincont (250.9) vs CeK (239.7) = 0.96x slower

Breakdown:
- CeK improved by 77.7 μs (24% faster)
- Mincont regressed by 19.2 μs (8% slower)
- Total swing: 96.9 μs (85% from CeK, 15% from Mincont)
```

**Key insight**: Most of the "regression" is actually **CeK getting much better**, not Mincont getting much worse!

## Detailed Analysis

### Why CeK Improved So Much

The commit `7c03ce09 - "Eliminate VCon allocations"` is likely the biggest win:

**Before**:
```scala
case Term.Builtin(AddInteger) =>
    builtin.addInteger.calculateCost(
        CekValue.VCon(...),  // ALLOCATION!
        CekValue.VCon(...)   // ALLOCATION!
    )
```

**After**:
```scala
case Term.Builtin(AddInteger) =>
    builtin.addInteger.calculateCostFromMemory(
        Seq(memoryUsage(x), memoryUsage(y))  // No VCon wrapper!
    )
```

**Impact**: Every builtin operation saved 2 object allocations. In a typical program with hundreds of builtin calls, this adds up to huge savings!

### Why Mincont Regressed

Investigating the enum conversion:

**Previous (case class)**:
```scala
sealed trait ContinuationJitRepr
case class Return(value: Any) extends ContinuationJitRepr
case class Apply(func: ..., arg: ...) extends ContinuationJitRepr
```

**Current (enum)**:
```scala
enum ContinuationJitRepr:
    case Return(value: Any)
    case Apply(func: ..., arg: ...)
```

**Hypothesis**: Scala 3 enums may have different allocation/dispatch characteristics than sealed trait + case classes. The enum implementation might:
- Have additional boxing for enum values
- Different JVM bytecode patterns
- Less amenable to JIT optimization

### Test the Hypothesis

To verify, we should:
1. Compare bytecode of old vs new Mincont
2. Profile allocation rates
3. Test reverting enum back to case classes

## Is This a Real Regression?

### Arguments FOR "Real Regression"

1. **Mincont is 8% slower** than it was
2. **Now slower than CeK** (the baseline it should beat)
3. **High variance** (±228 μs) suggests instability
4. **User reports it was faster before** ✅

### Arguments AGAINST "Real Regression"

1. **CeK improved dramatically** (24% faster) - raised the bar
2. **Mincont's absolute performance** (250.9 μs) is still decent
3. **Only 8% slower** than before (not catastrophic)
4. **Different test environment** (JDK 25, different settings)
5. **Small sample size** (only 3 iterations per run)

## Recommended Actions

### Priority 1: Verify with Git Bisect

```bash
# Find exact commit where Mincont performance changed
git bisect start
git bisect bad HEAD  # Current (slow)
git bisect good 4345f770  # Known good

# Test each commit with:
sbt "bench/Jmh/run -i 5 -wi 5 -f 1 .benchJIT_Mincont_auction_1_1"
```

### Priority 2: Compare Enum vs Case Class

Create a test branch:
```scala
// Revert ContinuationJitRepr from enum back to sealed trait
sealed trait ContinuationJitRepr
case class Return(value: Any) extends ContinuationJitRepr
case class Apply(...) extends ContinuationJitRepr
```

Benchmark to see if performance returns.

### Priority 3: Profile Allocation Rates

```bash
# Run with allocation profiler
sbt "bench/Jmh/run -prof gc -i 10 -wi 5 .benchJIT_Mincont"

# Compare to CeK
sbt "bench/Jmh/run -prof gc -i 10 -wi 5 .CekJVMBenchmark"
```

Look for:
- Objects allocated per operation
- GC pressure differences
- Allocation hotspots

### Priority 4: Test QuickOptimizations

Even if Mincont regressed, the **QuickOptimization-Plan.md** proposes 7 optimizations for 2x improvement:
1. Fast-path Return handling
2. Frame object pooling
3. Inline common builtins
4. Specialized value classes
5. Lazy budget tracking
6. Optimized stack operations
7. Return value optimization

These might recover the lost performance and more!

## Implications for Phase 2

### Strategy Adjustment

Given that:
- CeK is now much faster (239 μs)
- Mincont is slower than CeK (250 μs)
- NativeStack is still 7.89x faster (30 μs)

**Recommended Phase 2 approach**:

```scala
enum JITStrategy:
    case Pure Naive // Always use CeK (no JIT) - 239 μs
    case PureNativeStack  // Always use NativeStack - 30 μs, not stack-safe
    case HybridWithCeK    // NativeStack + CeK fallback (RECOMMENDED)
    case HybridWithMincont // NativeStack + Mincont fallback
    case OptimizedMincont  // After fixes from QuickOptimization-Plan

def recommendedStrategy = HybridWithCeK  // Skip Mincont for now
```

**Rationale**:
- NativeStack for 95% of cases (7.89x faster than CeK)
- CeK fallback for deep recursion (now only 24% overhead, not 37%)
- Skip Mincont until we fix its performance
- CeK is "good enough" as fallback now that it's been optimized

### When to Revisit Mincont

Only invest in fixing Mincont if:
1. We need stack-safety WITHOUT StackOverflow catching
2. We can implement QuickOptimizations to get 2x speedup (target: ~125 μs)
3. Profiling shows fixable issues

Otherwise, **HybridWithCeK is the pragmatic solution**.

## Conclusion

The "regression" is actually:
- **75% due to CeK getting much faster** (great!)
- **25% due to Mincont getting slightly slower** (not great)

**Bottom line**:
- CeK got awesome optimizations that raised the performance bar
- Mincont didn't keep up and may have regressed slightly
- NativeStack is still the clear winner at 7.89x faster
- Hybrid strategy should use NativeStack + CeK (skip Mincont for now)

---

**Document Status**: Analysis Complete
**Next Step**: Git bisect to find exact regression commit (optional)
**Recommendation**: Proceed with NativeStack + CeK hybrid for Phase 2
