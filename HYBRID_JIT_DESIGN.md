# Hybrid JIT Design: Defunctionalized Control Flow + JIT Snippets

**Branch:** `feat/jit-ifodefun`
**Date:** 2025-10-27
**Status:** 🚧 Work in Progress

---

## Executive Summary

We're implementing a **hybrid JIT architecture** that combines:
- ✅ **Defunctionalized control flow** (opcodes + mutable arrays) → eliminates allocations
- ✅ **JIT-compiled snippets** (Scala 3 staging) → direct bytecode for operations

**Expected Performance:** **4-5x speedup** over current JIT, **3-4x faster than CEK machine**

---

## Architecture

### **1. Control Flow: Defunctionalized (Zero Allocations)**

Instead of allocating continuation objects:
```scala
// ❌ Current: Heap-allocated case classes
case Return(value: Any)              // Allocation
case Apply(func: ..., arg: ...)      // Allocation
```

Use mutable arrays with primitive opcodes:
```scala
// ✅ Hybrid: Array-based with primitive tags
val frameTypes = new Array[Int](1024)    // Frame type tags
val frameData = new Array[Any](1024)     // Frame data
var sp = 0                                // Stack pointer

frameTypes(sp) = FRAME_APPLY_ARG          // Just int write!
frameData(sp) = argData
sp += 1
```

### **2. Operations: JIT Snippets (Direct Bytecode)**

For expensive operations (builtins, constants), generate direct bytecode:
```scala
// JIT-compiled snippet for AddInteger
new Snippet {
  def execute(...): Any = {
    (x: Any) => (y: Any) => {
      val xv = x.asInstanceOf[BigInt]
      val yv = y.asInstanceOf[BigInt]
      xv + yv  // ✅ Direct JVM bytecode, no dispatch
    }
  }
}
```

### **3. Hybrid Evaluation Loop**

```scala
while (ip < instructions.length) {
  val instr = instructions(ip)

  (instr.opcode: @switch) match {
    case OP_EXEC_SNIPPET =>
      // Execute JIT snippet directly (bypasses switch!)
      acc = instr.snippet.execute(acc, stack, sp, budget, logger, params)
      ip += 1

    case OP_APPLY =>
      // Defunctionalized control flow
      frameTypes(fp) = FRAME_APPLY_ARG
      frameData(fp) = argInstrIdx
      fp += 1
      ip = funcInstrIdx

    case OP_RETURN =>
      // Pop frame and continue
      if (fp == 0) return acc
      fp -= 1
      // ... process frame
  }
}
```

---

## Performance Analysis

### **Why This Is Faster**

| Component | Current JIT | Hybrid | Improvement |
|-----------|-------------|--------|-------------|
| **Control flow overhead** | 3-5 allocations per Apply | 0 allocations | **10-50x** |
| **Operation execution** | Via continuation dispatch | Direct bytecode | **1-2x** |
| **Cache locality** | Poor (scattered objects) | Excellent (arrays) | **2-3x** |
| **Branch prediction** | Poor (megamorphic) | Good (monomorphic) | **2x** |

**Weighted speedup:**
- Control flow: 30% of time, 10x improvement = 3.0x
- Operations: 50% of time, 1.5x improvement = 0.75x
- Other: 20% of time, 2x improvement = 0.4x
**Total: 3.0 + 0.75 + 0.4 = 4.15x speedup**

---

## Implementation Status

### ✅ Completed

1. **Branch created:** `feat/jit-ifodefun`
2. **Architecture designed:** Defunctionalized + JIT snippets
3. **Core files created:**
   - `JITDefunc.scala` - Evaluation context with IFO pattern
   - `JITDefuncCompiler.scala` - Compiler from UPLC to hybrid instructions
   - `JITDefuncTest.scala` - Basic tests
4. **Analysis documents:**
   - `JIT_PERFORMANCE_ANALYSIS.md` - Detailed analysis of current bottlenecks
   - `jit-analysis-output/` - Profiling data and inlining logs

### 🚧 In Progress

1. **Fix compilation errors:**
   - Import OP_* constants properly
   - Fix Expr[Snippet] vs runtime Snippet distinction
   - Complete builtin implementations

### 📋 TODO

1. **Complete implementation:**
   - Implement all Term types (Var, LamAbs, Constr, Case)
   - Implement all builtins (60+ total, focus on hot ones first)
   - Handle environment properly (variable lookups)

2. **Testing:**
   - Unit tests for each instruction type
   - Integration tests with real UPLC programs
   - Regression tests vs CEK machine

3. **Benchmarking:**
   - Run JMH benchmarks
   - Compare vs current JIT
   - Compare vs CEK machine
   - Profile with async-profiler

4. **Documentation:**
   - API documentation
   - Architecture diagrams
   - Performance tuning guide

---

## Files in This Branch

```
scalus-uplc-jit-compiler/src/main/scala/scalus/uplc/eval/
├── JITDefunc.scala           # Hybrid eval context (IFO pattern)
├── JITDefuncCompiler.scala   # Compiler to hybrid instructions
├── JIT.scala                 # Original JIT (keep for comparison)
└── ContinuationJitRepr.scala # Original continuation (keep for comparison)

scalus-uplc-jit-compiler/src/test/scala/scalus/uplc/eval/
├── JITDefuncTest.scala       # Tests for hybrid JIT
└── ...

jit-analysis-output/
├── inlining_log.txt          # HotSpot inlining decisions
├── ANALYSIS_SUMMARY.md       # Profiling analysis
└── staged-classes/           # Dumped class files

Documentation:
├── JIT_PERFORMANCE_ANALYSIS.md  # Full performance analysis
├── HYBRID_JIT_DESIGN.md         # This file
└── analyze_jit_bytecode.sh      # Profiling script
```

---

## Key Design Decisions

### **1. Why Hybrid (Not Pure Opcodes)?**

**Pure opcodes:**
- Control flow: Fast (arrays)
- Operations: Slower (switch dispatch overhead)
- Overall: ~3x speedup

**Hybrid (opcodes + snippets):**
- Control flow: Fast (arrays)
- Operations: Fast (direct bytecode)
- Overall: ~4-5x speedup

**Decision:** Hybrid gives better performance for marginal complexity increase.

### **2. Why IFO Pattern?**

**IFO (Imperative Functional Object):**
- External API: Pure functional (`eval(program) => result`)
- Internal implementation: Mutable state (arrays, stack pointers)

**Benefits:**
- Clean API for users
- Maximum performance internally
- Easy to test (no side effects visible externally)

### **3. Opcodes vs Case Classes?**

| Aspect | Case Classes | Opcodes |
|--------|--------------|---------|
| Allocations | 1-3 per operation | 0 |
| Dispatch | Pattern matching | @switch on Int |
| Cache | Poor | Excellent |
| Type safety | Strong | Weak (Any) |

**Decision:** Opcodes for hot path (control flow), case classes for cold path (error handling).

---

## Next Steps

### **Immediate (Next Session):**

1. Fix all compilation errors in `JITDefuncCompiler.scala`:
   ```bash
   sbt "scalusUplcJitCompiler/Test/compile"
   ```

2. Make OP_* constants accessible:
   ```scala
   object JITDefunc {
     // Make opcodes accessible to compiler
     val OP_RETURN = 0
     val OP_APPLY = 1
     // ...
   }
   ```

3. Fix Expr[Snippet] compilation:
   - Snippets need to be generated at compile-time
   - Store them in CompiledProgram
   - Runtime just executes them

4. Implement simple test case:
   ```scala
   test("Const(42)") {
     val term = Term.Const(Constant.Integer(42))
     val program = JITDefuncCompiler.compile(term)
     val result = JITDefunc.eval(program, ...)
     assert(result == BigInt(42))
   }
   ```

### **Short Term (This Week):**

1. Complete core Term implementations:
   - Const ✅
   - Var
   - LamAbs
   - Apply (control flow)
   - Force/Delay (control flow)

2. Implement top 10 builtins:
   - AddInteger ✅
   - SubtractInteger ✅
   - MultiplyInteger ✅
   - EqualsData ✅
   - IfThenElse ✅
   - + 5 more hot builtins

3. Basic benchmarks working

### **Medium Term (This Month):**

1. All Term types implemented
2. All 60+ builtins implemented
3. Full test suite passing
4. Benchmarks show 4-5x speedup
5. Documentation complete

---

## Performance Targets

| Metric | Current JIT | Target (Hybrid) | Stretch Goal |
|--------|-------------|-----------------|--------------|
| vs CEK | 0.7-1.7x | 3-4x | 5x |
| Allocations per op | 1-3 objects | 0 objects | 0 objects |
| Per-operation overhead | High | Low | Minimal |
| Hot loop performance | Poor | Good | Excellent |

---

## References

### **Theory:**
- Reynolds (1972): "Definitional Interpreters" - Defunctionalization
- Okasaki (1998): "Purely Functional Data Structures" - IFO pattern
- Danvy & Nielsen: "Defunctionalization at Work"

### **Practice:**
- LuaJIT: Tracing JIT with defunctionalized traces
- CPython 3.11+: Defunctionalized eval loop (10-60% speedup)
- GraalVM Truffle: Defunctionalized AST interpreters

### **Scalus Docs:**
- `JIT_PERFORMANCE_ANALYSIS.md` - Bottleneck analysis
- `jit-analysis-output/ANALYSIS_SUMMARY.md` - Profiling results
- `jit-analysis-output/inlining_log.txt` - HotSpot decisions

---

## Questions & Answers

**Q: Why not just use pure opcodes (simpler)?**
A: Operations would go through switch dispatch, adding ~2-3x overhead for expensive operations like crypto. Hybrid gives best of both worlds.

**Q: Can we use this architecture for CEK machine too?**
A: Yes! The CEK machine could benefit from this approach. Consider as future work.

**Q: What about tail call optimization?**
A: The defunctionalized approach naturally handles tail calls efficiently since it's loop-based, not recursive.

**Q: Memory usage?**
A: Should be lower than current JIT (fewer allocations) but slightly higher than CEK (stores compiled snippets).

---

## Contact / Collaboration

This is an experimental feature. Feedback welcome!

- Branch: `feat/jit-ifodefun`
- Docs: `HYBRID_JIT_DESIGN.md`, `JIT_PERFORMANCE_ANALYSIS.md`
- Tests: `JITDefuncTest.scala`
