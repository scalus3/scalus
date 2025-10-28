# JIT Compiler Documentation Index

## Overview

The JIT compiler for UPLC (Untyped Plutus Core) is currently underperforming, delivering only 1.51x speedup compared to the interpreted CekMachine. This directory contains comprehensive analysis and re-engineering proposals to achieve 5-10x performance improvement.

## Documents

### 📊 Performance Analysis

1. **[ProfileAnalysisGC.md](ProfileAnalysisGC.md)** - GC and memory profiling
   - Current performance metrics
   - Comparison with CekMachine
   - Memory allocation patterns
   - GC behavior analysis

2. **[ProfileAnalysisStack.md](ProfileAnalysisStack.md)** - Stack trace analysis
   - Hot path identification
   - Call graph analysis

### 📝 Implementation History

3. **[MinContImplementation.md](MinContImplementation.md)** - Current implementation
   - Minimal continuation approach
   - Architecture overview
   - What was changed from CallCCTrampoline

4. **[FinalSummary.md](FinalSummary.md)** - Previous iteration summary

### 🚀 Re-engineering Proposals

5. **[README-ReengineeringDocs.md](README-ReengineeringDocs.md)** - **START HERE**
   - Executive summary
   - Problem statement
   - Proposed solutions overview
   - Recommendations

6. **[JIT-Reengineering-Proposal.md](JIT-Reengineering-Proposal.md)** - Strategic proposal
   - Root cause analysis
   - Four re-engineering strategies
   - Phased implementation approach
   - Success metrics and risk mitigation

7. **[QuickOptimization-Plan.md](QuickOptimization-Plan.md)** - Phase 1: Quick wins
   - 7 immediate optimizations for 2x improvement
   - 2-week implementation plan
   - Detailed code changes
   - **Ready to implement**

8. **[BytecodeGeneration-Design.md](BytecodeGeneration-Design.md)** - Phase 2: Major rewrite
   - Complete technical design for ASM-based bytecode generation
   - 10-week implementation roadmap
   - Expected 5x improvement
   - **Detailed implementation guide**

## Quick Reference

### Current Performance

| Benchmark | CEK | Current JIT | Improvement |
|-----------|-----|-------------|-------------|
| auction_1-1 | 317.4 μs | 210.5 μs | **1.51x** ❌ Not enough |

### Performance Targets

| Phase | Timeline | Target | Improvement vs Current |
|-------|----------|--------|----------------------|
| **Phase 1: Quick Wins** | 2 weeks | 105 μs | 2x |
| **Phase 2: Bytecode Gen** | 10 weeks | 40 μs | 5x |
| **Phase 3: Advanced Opt** | Future | 20-30 μs | 8-10x |

## Getting Started

### For Reviewers
1. Read [README-ReengineeringDocs.md](README-ReengineeringDocs.md) for overview
2. Review [JIT-Reengineering-Proposal.md](JIT-Reengineering-Proposal.md) for strategy
3. Evaluate risk/reward tradeoffs

### For Implementers
1. Start with [QuickOptimization-Plan.md](QuickOptimization-Plan.md)
2. Implement 7 quick optimizations
3. Benchmark and validate
4. If successful, proceed to [BytecodeGeneration-Design.md](BytecodeGeneration-Design.md)

### For Architects
1. Review all performance analysis documents
2. Evaluate alternative approaches in [JIT-Reengineering-Proposal.md](JIT-Reengineering-Proposal.md)
3. Provide feedback on [BytecodeGeneration-Design.md](BytecodeGeneration-Design.md)

## Key Findings

### Root Causes of Poor Performance

1. **Continuation Overhead** - Every operation creates wrapper objects
2. **Pattern Matching Tax** - Full match on every eval loop iteration
3. **Excessive Indirection** - Multiple abstraction layers
4. **No Specialization** - Everything typed as `Any`
5. **Staging Limitations** - Scala 3 staging doesn't optimize well

### Recommended Solution

**Three-phase approach:**
- ✅ Phase 1: Quick optimizations (2 weeks, 2x gain)
- ✅ Phase 2: Direct bytecode generation (10 weeks, 5x gain)
- ⚠️ Phase 3: Advanced optimizations (future, 8-10x gain)

## Status

- **Current:** Proposal phase
- **Next:** Awaiting approval for Phase 1 implementation
- **Branch:** Will create `feat/jit-optimization-phase1` when approved

## Contact

For questions or feedback on these proposals, please contact the Scalus development team.

---

**Last Updated:** 2025-10-28
