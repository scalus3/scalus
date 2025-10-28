# JIT Compiler Re-engineering Summary

## Problem

The current JIT interpreter in `scalus-uplc-jit-compiler` has **the same performance as CekMachine**, delivering only **1.51x speedup** (317μs → 210μs) despite the complexity of JIT compilation. This is insufficient to justify the maintenance burden and complexity.

## Analysis Findings

### Performance Profile (auction_1-1.flat benchmark)

| Metric | CEK | Current JIT | Ratio |
|--------|-----|-------------|-------|
| **Time/op** | 317.4 μs | 210.5 μs | 1.51x faster |
| **Allocation/op** | 1.34 MB | 547 KB | 2.45x less |
| **GC cycles** | 197 | 351 | 1.78x more |

### Root Causes

1. **Continuation Overhead**: Every operation creates continuation objects (Apply/Force/Return)
2. **Pattern Matching Tax**: Eval loop does full pattern match on every iteration
3. **Excessive Indirection**: Multiple layers between UPLC term and actual execution
4. **No Specialization**: All values are `Any`, requiring casts and boxing
5. **Generic Builtins**: No optimization for common operations like integer arithmetic

### Current Architecture

```
UPLC Term
  ↓ [Scala 3 Staging]
  ↓
Generated Scala Code → ContinuationJitRepr
  ↓
Eval Loop (Pattern Match)
  ↓
Frame Stack (Heap Allocated)
  ↓
Result
```

**Problem:** Too many abstraction layers, no direct code generation.

## Proposed Solutions

### Three-Phase Approach

#### **Phase 1: Quick Wins** (1-2 weeks) → 2x improvement
Target: 210μs → 105μs

**Optimizations:**
- Fast-path for Return with empty stack
- Frame object pooling
- Inline top 5 builtins (AddInteger, etc.)
- Specialized value handling
- Optimized stack operations
- Lazy budget tracking

**Deliverable:** `QuickOptimization-Plan.md` with detailed implementation steps

---

#### **Phase 2: Bytecode Generation** (3-4 weeks) → 5x improvement total
Target: 210μs → 40μs

**Approach:**
- Replace Scala 3 staging with ASM-based direct bytecode generation
- Generate specialized classes for each UPLC term
- Inline common operations directly in bytecode
- Type inference for specialization
- Constant folding and dead code elimination

**Architecture:**
```
UPLC Term
  ↓ [ASM Bytecode Generator]
  ↓
JVM Class (specialized)
  ↓ [ClassLoader]
  ↓
Direct Execution (no eval loop!)
  ↓
Result
```

**Deliverable:** `BytecodeGeneration-Design.md` with complete technical specification

---

#### **Phase 3: Advanced Optimizations** (2-3 weeks) → 8-10x improvement
Target: 210μs → 20-30μs

**Techniques:**
- Tail call optimization
- Escape analysis
- Method inlining
- Profile-guided optimization
- Specialized method variants

---

### Alternative Approaches Considered

1. **Threaded Interpreter** - Compile to internal bytecode, stack-based VM
   - Simpler than direct bytecode generation
   - Could achieve 2-4x improvement
   - Good fallback if bytecode generation proves too complex

2. **LLVM Backend** - Ultimate performance but high complexity
   - Potential 10x+ improvement
   - Requires native integration
   - Long-term research project

3. **Optimize CekMachine Instead** - Simplest approach
   - Make interpreter faster rather than JIT
   - Lower risk, lower reward
   - Consider if JIT re-engineering fails

## Deliverables

Created three comprehensive documents in `scalus-uplc-jit-compiler/docs/`:

### 1. **JIT-Reengineering-Proposal.md**
- Problem analysis
- Root cause identification  
- Four re-engineering strategies with tradeoffs
- Recommended phased approach
- Implementation timeline
- Success metrics and risk mitigation

### 2. **BytecodeGeneration-Design.md**
- Complete technical design for ASM-based bytecode generation
- Module structure and architecture
- Type system for specialization
- Detailed bytecode generation examples
- Optimization techniques (constant folding, inlining, etc.)
- Testing strategy
- 10-week implementation plan

### 3. **QuickOptimization-Plan.md**
- 7 specific optimizations for immediate 2x improvement
- Implementation details for each optimization
- 2-week implementation schedule
- Benchmarking and validation strategy
- Code locations and changes required

## Recommendations

### Immediate Action (Next 2 Weeks)
✅ **Implement Phase 1 (Quick Wins)**
- Low risk, high reward
- Validates optimization approach
- Provides immediate value
- If successful, proceed to Phase 2

### Short Term (Next 2-3 Months)
✅ **Implement Phase 2 (Bytecode Generation)**
- Major architectural improvement
- Expected 5x total speedup
- Positions Scalus as performance leader
- Well-documented design ready for implementation

### Long Term (6+ Months)
⚠️ **Evaluate Advanced Optimizations**
- After Phase 2, assess if more optimization needed
- Consider threaded interpreter as alternative
- Keep LLVM backend as research direction

## Success Criteria

### Phase 1 Target
- ✅ 2x faster than current JIT (210μs → 105μs)
- ✅ 100% test compatibility
- ✅ Implemented in 2 weeks

### Phase 2 Target
- ✅ 5x faster than current JIT (210μs → 40μs)
- ✅ 10x faster than CekMachine (317μs → 40μs)
- ✅ 3x less memory allocation
- ✅ All UPLC features supported
- ✅ Implemented in 8-10 weeks

### Overall Goal
**Make Scalus UPLC evaluation the fastest in the ecosystem**
- Faster than Plutus Haskell implementation
- Competitive with native implementations
- Enable new use cases requiring high performance

## Next Steps

1. **Review** these documents with team
2. **Approve** Phase 1 implementation
3. **Create** development branch: `feat/jit-optimization-phase1`
4. **Implement** quick optimizations from QuickOptimization-Plan.md
5. **Benchmark** and validate improvements
6. **Decide** on Phase 2 based on Phase 1 results

## Files Created

```
scalus-uplc-jit-compiler/docs/
├── JIT-Reengineering-Proposal.md      (11.4 KB)
├── BytecodeGeneration-Design.md       (14.7 KB)
├── QuickOptimization-Plan.md          (10.1 KB)
└── README-ReengineeringDocs.md        (this file)
```

---

**Date:** 2025-10-28  
**Status:** Proposal Phase - Awaiting Review and Approval
