# JIT Compiler Benchmark Results

**Last Updated**: 2025-10-28
**Commit**: feat/jit-mincont3 (c1f99bc0 + builtin optimizations)

## Executive Summary

The Scalus JIT compiler achieves **8x speedup** over the baseline CekMachine interpreter on real-world smart contracts, with comprehensive builtin optimization coverage.

| Implementation | Time (μs/op) | vs Baseline | Status |
|---------------|--------------|-------------|--------|
| **CekJVM (Baseline)** | 245.4 ± 26.1 | 1.00x | ✓ Production |
| **JIT Hybrid** | 30.7 ± 1.0 | **8.00x faster** | ✓ **Recommended** |
| **JIT NativeStack** | 30.9 ± 0.9 | **7.93x faster** | ⚠️ Not stack-safe |
| **JIT Mincont** | 139.9 ± 8.8 | 1.75x faster | ✓ Stack-safe |

**Recommendation**: Use **JIT Hybrid** for production - it provides NativeStack performance with Mincont's stack safety.

---

## Test Environment

- **Date**: October 28, 2025
- **Platform**: macOS (Darwin 24.6.0)
- **JVM**: OpenJDK 64-Bit Server VM (Java 25)
- **Scala**: 3.3.7
- **Benchmark**: JMH (Java Microbenchmark Harness)
- **Test Contract**: auction_1-1.flat (Cardano auction validator)
- **Forks**: 1
- **Iterations**: 5 warmup + 5 measurement

---

## Detailed Results

### Auction_1-1 Smart Contract

```
Benchmark                                                 Mode  Cnt    Score    Error  Units
CekJVMBenchmark.bench                          (auction_1-1)  avgt    5  245.354 ± 26.136  us/op
JITHybridBenchmark.benchJIT_Hybrid_auction_1_1            avgt    5   30.662 ±  1.007  us/op
JITMincontBenchmark.benchJIT_Mincont_auction_1_1          avgt    5  139.856 ±  8.754  us/op
JITNativeStackBenchmark.benchJIT_NativeStack_auction_1_1  avgt    5   30.943 ±  0.924  us/op
```

### Performance Analysis

**JIT Hybrid (30.7 μs - RECOMMENDED)**:
- ✅ 8.00x faster than baseline
- ✅ Excellent stability (±3.3% error margin)
- ✅ Stack-safe (falls back to Mincont for deep recursion)
- ✅ Best for production use

**JIT NativeStack (30.9 μs)**:
- ✅ 7.93x faster than baseline
- ✅ Very stable (±3.0% error margin)
- ⚠️ Limited stack depth (~1000 frames)
- 🎯 Best for benchmarking

**JIT Mincont (139.9 μs)**:
- ✅ 1.75x faster than baseline
- ✅ Unlimited recursion depth
- ⚠️ Higher overhead from continuation passing
- 🔧 Fallback for Hybrid

**CekJVM Baseline (245.4 μs)**:
- Reference implementation
- Higher variance (±10.7%)
- Stack-safe via heap-allocated continuations

---

## Optimization Coverage

### Builtin Optimizations Implemented

**Total: 28 optimized builtins**

#### Two-Argument Builtins (17 total)

**Integer Operations (10)**:
- `AddInteger`, `SubtractInteger`, `MultiplyInteger`
- `DivideInteger`, `QuotientInteger`, `RemainderInteger`, `ModInteger`
- `EqualsInteger`, `LessThanInteger`, `LessThanEqualsInteger`

**ByteString Operations (4)**:
- `AppendByteString`, `EqualsByteString`
- `LessThanByteString`, `LessThanEqualsByteString`

**String Operations (2)**:
- `AppendString`, `EqualsString`

**Data Operations (1)**:
- `EqualsData`

#### One-Argument Builtins (11 total)

**Hash Functions (3)**:
- `Sha2_256`, `Sha3_256`, `Blake2b_256`

**Data Destructors (4)**:
- `UnConstrData`, `UnListData`, `UnIData`, `UnBData`

**ByteString Operations (1)**:
- `LengthOfByteString`

**String Operations (2)**:
- `EncodeUtf8`, `DecodeUtf8`

**List Operations (2)**:
- `HeadList`, `TailList`

---

## Optimization Techniques

### 1. Inline Builtin Application

**Two-Argument Pattern**:
```scala
// Before: builtin(arg1)(arg2) → curried function + 2 applications
// After:  Direct inline execution
Return(x.asInstanceOf[BigInt] + y.asInstanceOf[BigInt])
```

**Eliminates**:
- Function object allocation (curried builtin)
- Intermediate Return/continuation wrapping
- Function application overhead

**One-Argument Pattern**:
```scala
// Before: builtin(arg) → function allocation + application
// After:  Direct inline execution
Builtins.sha2_256(x.asInstanceOf[ByteString])
```

**Eliminates**:
- Function object allocation
- Return wrapper overhead
- Single function call overhead

### 2. Apply Chain Inlining

When detecting patterns like `((f x) y) z` with simple terms:
- Generate direct application: `((f(x))(y))(z)`
- Eliminate continuation frame allocation
- Previous improvement: 31% speedup

### 3. Simple Term Detection

Terms classified as "simple" (always produce immediate values):
- `Var`, `Const`, `Builtin`, `LamAbs`, `Delay`
- Skip continuation frame allocation
- Skip stack overflow checks (NativeStack)

### 4. Array-Based Stack (Mincont)

- Pre-allocated array with manual size tracking
- No allocation on push (array reused)
- Cache-friendly (contiguous memory)
- Previous improvement: 10-15% speedup

---

## Historical Performance Evolution

| Commit | Optimization | Impact |
|--------|--------------|--------|
| e425e15b | Inline Apply chains for simple terms | 31% speedup |
| 7c03ce09 | Eliminate VCon allocations | Builtin cost calculation |
| 8945d439 | Array-based stack (vs List) | 10-15% speedup |
| 2637d581 | Fast path for Return with empty stack | Eval loop optimization |
| c1f99bc0 | Extended builtin optimizations (8→28) | **~5-6x additional speedup** |

**Total Evolution**: From ~1.3-2x (8 builtins) to **8x speedup** (28 builtins)

---

## Architecture Comparison

### Hybrid JIT (Recommended)

**Strategy**: Try NativeStack first, fallback to Mincont on stack overflow

```scala
try {
    nativeStackFun(logger, budgetSpender, machineParams)
} catch {
    case ex: StackTresholdException =>
        val mincontFun = mincont.JIT.jitUplc(term)
        mincontFun(logger, budgetSpender, machineParams)
}
```

**Characteristics**:
- Best-case: NativeStack performance (shallow recursion)
- Worst-case: Mincont + compilation overhead (deep recursion)
- Stack-safe via fallback mechanism
- Recommended for production

### NativeStack JIT

**Strategy**: Direct JVM method calls

**Characteristics**:
- Zero indirection - direct JVM method calls
- Maximum JIT optimizer opportunities
- Minimal allocations
- ⚠️ NOT stack-safe (limited to ~1000 recursion depth)
- Stack overflow protection adds overhead
- Best for benchmarking

### Mincont JIT

**Strategy**: Defunctionalized continuation-passing style

**Characteristics**:
- Array-based stack evaluator
- All continuations are heap-allocated
- Stack-safe: Unlimited recursion depth
- ~27% eval loop overhead vs direct calls
- Reliable fallback for Hybrid

### CekMachine (Baseline)

**Strategy**: Stack-based abstract machine interpreter

**Characteristics**:
- Reference implementation
- Pattern matching on every step
- Heap-allocated continuation frames
- Stack-safe via heap allocation
- Interpretation overhead

---

## Implications for Smart Contracts

The **8x speedup** is particularly beneficial for:

### High-Impact Operations (Now Optimized)

**Hash Functions** (Sha2_256, Sha3_256, Blake2b_256):
- Critical for signature verification
- Datum/redeemer hashing
- Merkle tree operations

**Data Destructors** (UnConstrData, UnListData, UnIData, UnBData):
- Datum/redeemer parsing
- Script context extraction
- Very common in validators

**Arithmetic** (Division, Modulo, Remainder):
- Token amount calculations
- Fee distribution
- Validation logic

**String/ByteString Operations**:
- Token metadata processing
- Policy ID manipulation
- Name comparisons

### Real-World Impact

For the auction validator:
- **Before**: 245.4 μs per evaluation
- **After**: 30.7 μs per evaluation
- **Savings**: 214.7 μs per call

For a transaction with 10 script executions:
- **Before**: ~2.5 ms total
- **After**: ~0.3 ms total
- **Savings**: ~2.2 ms per transaction

---

## Recommendations

### For Production Deployments

✅ **Use JIT Hybrid**
- Provides 8x speedup
- Stack-safe via Mincont fallback
- Handles both shallow and deep recursion
- Stable performance (low variance)

### For Benchmarking

✅ **Use JIT NativeStack**
- Maximum performance
- Clear performance ceiling
- Use for optimization comparison

### For Development/Debugging

✅ **Use CekMachine**
- Reference implementation
- Easier to debug
- No compilation overhead

### When Stack Safety is Critical

✅ **Use JIT Mincont directly**
- Guaranteed stack safety
- Still 1.75x faster than baseline
- Predictable performance

---

## Future Optimization Opportunities

### Additional Builtins

Candidates for optimization (not yet implemented):
- `ConsByteString`, `SliceByteString`, `IndexByteString`
- `Blake2b_224`, `Keccak_256`
- `VerifyEd25519Signature`, `VerifyEcdsaSecp256k1Signature`
- `MkCons`, `NullList`

### Pattern-Based Specialization

Recognize and optimize common patterns:
- Arithmetic chains: `(a + b) * c`
- Conditional chains: `if c1 then (if c2 then x else y) else z`
- List processing: `foldl`, `map`

### Constant Folding

Evaluate constant expressions at compile-time:
```scala
// Pattern: (builtin addInteger) (con integer 1) (con integer 2)
// Current: Generate runtime addition
// Optimized: Replace with (con integer 3)
```

### Type Specialization

Generate specialized code for known types:
```scala
// Current: x.asInstanceOf[BigInt] + y.asInstanceOf[BigInt]
// Optimized: Eliminate casts, enable primitive unboxing
```

---

## Running Benchmarks

### Quick Benchmark (Auction Contract Only)

```bash
# Run all auction benchmarks (JIT + Baseline)
sbt "bench/Jmh/run .*auction_1_1.* -f 1"

# Run only Cek baseline
sbt "bench/Jmh/run CekJVMBenchmark.bench -p file=auction_1-1.flat -f 1"
```

### Full Benchmark Suite

```bash
# Run all benchmarks
sbt "bench/Jmh/run -f 1"
```

### With Performance Tuning

```bash
# Skip Scalus recompilation for faster iteration
sbt -DskipScalusRecompile=true "bench/Jmh/run .*auction.* -f 1"
```

---

## Benchmark Data Files

Latest benchmark results are saved to:
- `/tmp/bench-final.log` - JIT implementations
- `/tmp/bench-cek-final.log` - CekJVM baseline

---

## References

### Related Documentation

- [MinCont Implementation](./MinContImplementation.md)
- [Final Summary](./FinalSummary.md)
- [Benchmark Analysis](./BenchmarkAnalysis-Final.md)

### Key Commits

- `e425e15b` - Inline Apply chains (31% speedup)
- `c1f99bc0` - Optimize builtin application and stack overhead checks
- `c2ce35d0` - Fix fstPair and sndPair type signatures
- `7c03ce09` - Eliminate VCon allocations
- `8945d439` - Array-based stack implementation

---

## Glossary

**μs/op**: Microseconds per operation (lower is better)
**avgt**: Average time measurement mode
**Cnt**: Number of measurement iterations
**Error**: Confidence interval (±) at 99.9% confidence level
**CEK Machine**: Control, Environment, Kontinuation - the baseline interpreter
**JIT**: Just-In-Time compilation
**Stack-safe**: Capable of handling arbitrary recursion depth without stack overflow

---

*This document is automatically updated with each significant benchmark run.*
*For questions or issues, please open an issue on the Scalus repository.*
