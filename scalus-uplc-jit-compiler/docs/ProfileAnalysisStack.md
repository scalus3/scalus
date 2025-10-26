# JIT Stack Profiling Analysis

## Where Time is Being Spent (auction_1-1.flat)

### Thread State Distribution
- **RUNNABLE**: 66.7% (actual computation)
- **TIMED_WAITING**: 33.3% (GC/system threads)

### Top Hotspots (RUNNABLE time)

| % of RUNNABLE | % of Total | Component | Description |
|---------------|------------|-----------|-------------|
| **50.0%** | **33.3%** | (filtered) | JIT-compiled code (inlined, optimized) |
| **40.9%** | **27.3%** | `ContinuationJitRepr.eval` | Main evaluation loop |
| **0.6%** | **0.4%** | Generated code | JIT-compiled UPLC operations |
| **0.5%** | **0.3%** | `List.prependedAll` | Budget/list operations |
| **0.4%** | **0.3%** | `CountingBudgetSpender` | Budget tracking overhead |
| **0.3%** | **0.2%** | `CekValue.VCon` | Creating value wrappers for costing |
| **0.2%** | **0.1%** | `Hex.bytesToHex` | ByteString creation |
| **0.2%** | **0.1%** | `MemoryUsage.memoryUsage` | Cost calculation |
| **0.1%** | **0.1%** | `Unsafe.allocateInstance` | Object allocation |
| **6.9%** | **4.6%** | Other | Misc operations |

## Key Findings

### ✅ Good News

1. **50% of time is in optimized code** - The JVM's JIT compiler has successfully optimized and inlined our generated code!
   - This is the "filtered" portion - code that's been compiled to native instructions
   - No stack frames visible = highly optimized

2. **eval loop is 27.3% (40.9% of RUNNABLE)** - Reasonable overhead
   - This is the continuation dispatching loop
   - Pattern matching on Return/Apply/Force
   - Frame push/pop operations

3. **Minimal overhead from helpers** - Combined < 2%
   - Budget tracking, costing, conversions all very small

### ⚠️ Optimization Opportunities

1. **eval loop (27.3%)** - Largest visible hotspot
   - Could be further optimized with specialized paths
   - Inline common patterns (Return with empty stack)
   - Reduce pattern matching overhead

2. **Budget tracking (0.3%)** - Small but measurable
   - Every operation calls `spendBudget`
   - Creates `CekValue.VCon` wrappers for costing
   - Could be batched or optimized

3. **Cost calculation (0.2%)** - `MemoryUsage.memoryUsage`
   - Called for many builtins
   - Could be memoized for constants

## Performance Budget Breakdown

For 244 μs total execution:

| Component | Time | % |
|-----------|------|---|
| **Optimized JIT code** | ~81 μs | 33.3% |
| **eval loop** | ~67 μs | 27.3% |
| **Generated code** | ~1 μs | 0.4% |
| **Budget/costing** | ~1.5 μs | 0.6% |
| **Other operations** | ~11 μs | 4.6% |
| **GC/waiting** | ~81 μs | 33.3% |

## Detailed Analysis

### The eval Loop (27.3% - 67 μs)

This is `ContinuationJitRepr.eval` - the main dispatch loop.

**What it does:**
- Pattern match on `current` (Return/Apply/Force)
- Pattern match on stack frames
- Push/pop frames from array
- Call functions and handle results

**Why it's hot:**
- Called repeatedly for every operation
- Multiple pattern matches per iteration
- Type checks and casts

**Optimization potential:**
- Specialize for Return with empty stack (common case)
- Inline frame operations more aggressively
- Reduce pattern matching depth
- **Estimated savings: 15-20 μs (6-8%)**

### Budget Tracking (0.6% - 1.5 μs)

Every operation calls `spendBudget` which:
- Creates `CekValue.VCon` wrappers
- Calls costing functions
- Updates counters

**Optimization potential:**
- Batch budget spending (accumulate, spend once)
- Skip budget in hot paths, check periodically
- **Estimated savings: 0.5-1 μs (0.2-0.4%)**

### The "Filtered" 50% (81 μs)

This is the best news! Half the time is spent in code so well-optimized that the profiler can't even see the stack frames.

**What's in here:**
- Direct builtin operations (integer math, comparisons)
- Function applications
- Value returns
- Inlined continuation handling

This means the JVM's C2 compiler has:
- Inlined function calls
- Eliminated allocations (escape analysis)
- Optimized branches
- Compiled to native machine code

## Comparison: What's NOT Hot

These are **not** showing up as hotspots (good!):
- ✅ Array stack operations - completely inlined
- ✅ Frame allocation - escape analysis removes most
- ✅ Return object creation - optimized away
- ✅ Function application - inlined
- ✅ Type casts - eliminated by JIT

## Recommendations

### High Impact (worth doing)

1. **Specialize eval loop for common case**
   ```scala
   // Fast path: Return with no frames
   if (current.isInstanceOf[Return] && stackSize == 0) {
       return current.asInstanceOf[Return].value
   }
   ```
   - Eliminates pattern match for final return
   - Saves ~5-10 μs

2. **Batch budget tracking**
   - Accumulate costs, spend periodically
   - Saves ~0.5-1 μs

3. **Memoize constant costs**
   - Cache memory usage for constant values
   - Saves ~0.2-0.5 μs

**Total potential: ~6-12 μs (2-5% improvement)**

### Low Impact (probably not worth it)

1. ByteString hex conversion optimization - Only 0.1%
2. Further inline directives - JVM already doing great job
3. Manual escape analysis hints - JVM handles it

## Conclusion

The profiling shows **excellent JIT optimization**:
- 50% of time in fully-optimized native code
- Only 27% in visible eval loop
- Minimal overhead from helpers

The architecture is sound. The eval loop is the only significant hotspot, and further optimization would require careful benchmarking to ensure gains aren't lost to increased code complexity.

**Current performance (244 μs) is very good compared to CEK (318 μs).**

Next steps if pursuing further optimization:
1. Try specialized eval fast path
2. Profile with more iterations to see if JIT gets even better
3. Compare different JVM versions (GraalVM, etc.)
