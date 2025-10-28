# Quick Optimization Plan for JIT Compiler

## Goal
Achieve 2x performance improvement (210μs → 105μs) with minimal code changes in 1-2 weeks.

## Current Performance Bottlenecks

Based on analysis of `ContinuationJitRepr.eval` and generated code:

1. **Pattern Matching Overhead** (~25% of time)
   - Every iteration does pattern match on `ContinuationJitRepr`
   - Type checks and casts throughout

2. **Frame Allocation** (~15% of time)
   - `ApplyFuncFrame`, `ApplyArgFrame`, `ForceFrame` allocated for every operation
   - Array resizing when stack grows

3. **Continuation Wrapping** (~20% of time)
   - Every value wrapped in `Return(value)`
   - Unpacking and repacking in loop

4. **Generic Operations** (~20% of time)
   - All values typed as `Any`
   - `asInstanceOf` casts everywhere
   - No specialization for primitives

5. **Budget Tracking** (~20% of time)
   - Called on every operation
   - HashMap lookups in TallyingBudgetSpender
   - Tuple allocations for tracking

## Optimization 1: Fast-Path Return Handling

### Problem
Most common case is `Return` with empty stack, but still goes through full pattern match.

### Solution
Add fast-path check before pattern matching:

```scala
def eval(cont: ContinuationJitRepr): Any = {
  var current = cont
  var stack = new Array[Frame](32)
  var stackSize = 0
  
  while true do {
    // FAST PATH: Return with empty stack - most common case
    if stackSize == 0 then {
      current match {
        case Return(value) => return value
        case _ => // continue to normal handling
      }
    }
    
    // Normal handling
    current match {
      // ... existing code ...
    }
  }
}
```

**Expected Improvement:** 10-15% faster (saves pattern match in common case)

---

## Optimization 2: Pre-allocated Frame Pool

### Problem
Frames are allocated on every Apply/Force operation.

### Solution
Object pooling for frame reuse:

```scala
object FramePool {
  private val applyFuncPool = new ArrayBlockingQueue[ApplyFuncFrame](256)
  private val applyArgPool = new ArrayBlockingQueue[ApplyArgFrame](256)
  private val forcePool = new ArrayBlockingQueue[ForceFrame](1)
  
  @inline
  def borrowApplyFunc(arg: ContinuationJitRepr): ApplyFuncFrame = {
    val frame = applyFuncPool.poll()
    if frame != null then {
      frame.arg = arg
      frame
    } else {
      ApplyFuncFrame(arg)
    }
  }
  
  @inline
  def returnFrame(frame: Frame): Unit = frame match {
    case f: ApplyFuncFrame => applyFuncPool.offer(f)
    case f: ApplyArgFrame => applyArgPool.offer(f)
    case f: ForceFrame => forcePool.offer(f)
  }
}

// In eval loop:
pushFrame(FramePool.borrowApplyFunc(arg))
// After use:
FramePool.returnFrame(popFrame())
```

**Expected Improvement:** 10-15% faster (less GC pressure)

---

## Optimization 3: Inline Common Builtins

### Problem
Common operations like AddInteger go through multiple function applications:
```scala
Return((x: Any) => Return((y: Any) => x.asInstanceOf[BigInt] + y.asInstanceOf[BigInt]))
```

### Solution
Generate specialized inline code for hot builtins:

```scala
// In JIT.scala genCode:
case Term.Builtin(DefaultFun.AddInteger) =>
  // Generate specialized flat code instead of nested lambdas
  '{
    Return(new InlinedAddInteger($budget, $params))
  }

// Specialized class (avoids closure allocations)
final class InlinedAddInteger(
  budget: BudgetSpender,
  params: MachineParams
) extends (Any => Any) {
  private var x: BigInt = null
  
  def apply(arg: Any): Any = {
    if x == null then {
      x = arg.asInstanceOf[BigInt]
      this  // Return self for second application
    } else {
      val y = arg.asInstanceOf[BigInt]
      budget.spendBudget(
        Step(StepKind.Builtin),
        params.builtinCostModel.addInteger.calculateCostFromMemory(
          Seq(MemoryUsageJit.memoryUsage(x), MemoryUsageJit.memoryUsage(y))
        ),
        Nil
      )
      x + y  // Direct return, no Return wrapper needed
    }
  }
}
```

**Target Builtins:**
- AddInteger, SubtractInteger, MultiplyInteger
- LessThanInteger, LessThanEqualsInteger, EqualsInteger
- EqualsData
- IfThenElse

**Expected Improvement:** 15-20% faster (eliminates closures and Return wrappers)

---

## Optimization 4: Specialized Value Classes

### Problem
All values are `Any`, requiring boxing and casts.

### Solution
Use specialized methods for common types:

```scala
// Add specialized eval methods
def evalInt(cont: ContinuationJitRepr): BigInt = {
  eval(cont).asInstanceOf[BigInt]
}

def evalBool(cont: ContinuationJitRepr): Boolean = {
  eval(cont).asInstanceOf[Boolean]
}

// Generate specialized code when types are known
case Term.Builtin(DefaultFun.AddInteger) =>
  '{
    Return((x: Any) => Return((y: Any) => {
      val xv = x.asInstanceOf[BigInt]
      val yv = y.asInstanceOf[BigInt]
      // ... budget ...
      xv + yv  // Returns BigInt directly, not wrapped
    }))
  }
```

**Expected Improvement:** 5-10% faster (less boxing/unboxing)

---

## Optimization 5: Lazy Budget Tracking

### Problem
Budget tracking called on every operation, even when not needed.

### Solution
Add fast-path for NoBudgetSpender:

```scala
// In generated code:
trait BudgetSpender {
  @inline final def isEnabled: Boolean = this != NoBudgetSpender
  
  def spendBudget(cat: ExBudgetCategory, budget: ExBudget, env: CekValEnv): Unit
}

// Generated code:
if ($budget.isEnabled) {
  $budget.spendBudget(Step(StepKind.Apply), $params.machineCosts.applyCost, Nil)
}
```

**Expected Improvement:** 10-15% when budget tracking disabled

---

## Optimization 6: Optimized Stack Operations

### Problem
Current array-based stack with dynamic growth.

### Solution
Pre-allocate larger stack, use simpler push/pop:

```scala
def eval(cont: ContinuationJitRepr): Any = {
  var current = cont
  var stack = new Array[Frame](256)  // Larger initial size
  var stackSize = 0
  
  @inline def push(frame: Frame): Unit = {
    stack(stackSize) = frame
    stackSize += 1
  }
  
  @inline def pop(): Frame = {
    stackSize -= 1
    val frame = stack(stackSize)
    stack(stackSize) = null  // Help GC
    frame
  }
  
  // Remove growth logic - just fail if stack too deep
  // (256 should be enough for any real UPLC program)
}
```

**Expected Improvement:** 5% faster (simpler operations)

---

## Optimization 7: Return Value Optimization

### Problem
Every return wraps value in `Return(value)`, then unwraps in loop.

### Solution
Direct return when possible:

```scala
// In eval loop, optimize the common pattern:
current = result match {
  case cont: ContinuationJitRepr => cont
  case v => Return(v)
}

// Can be optimized to:
if result.isInstanceOf[ContinuationJitRepr] then
  current = result.asInstanceOf[ContinuationJitRepr]
else
  current = Return.wrap(result)  // Use cached Return objects

// In Return companion:
object Return {
  private val cache = new Array[Return](8)
  private var cacheSize = 0
  
  @inline def wrap(v: Any): Return = {
    if cacheSize > 0 then {
      cacheSize -= 1
      val r = cache(cacheSize)
      r.value = v
      r
    } else {
      Return(v)
    }
  }
  
  @inline def recycle(r: Return): Unit = {
    if cacheSize < cache.length then {
      cache(cacheSize) = r
      cacheSize += 1
    }
  }
}
```

**Expected Improvement:** 5-10% faster

---

## Implementation Plan

### Week 1

**Day 1-2: Infrastructure**
- Add frame pooling infrastructure
- Add specialized builtin classes
- Set up benchmarking harness

**Day 3-4: Core Optimizations**
- Implement Optimization 1 (fast-path)
- Implement Optimization 2 (frame pool)
- Implement Optimization 6 (stack optimization)

**Day 5: Testing**
- Run full test suite
- Benchmark improvements
- Fix any issues

**Expected: 1.5x improvement**

### Week 2

**Day 1-2: Builtin Specialization**
- Implement Optimization 3 for top 5 builtins
- Add specialized value classes (Optimization 4)

**Day 3-4: Final Optimizations**
- Implement Optimization 5 (lazy budget)
- Implement Optimization 7 (return optimization)
- Fine-tuning

**Day 5: Validation**
- Comprehensive testing
- Performance benchmarking
- Documentation

**Expected: 2x total improvement**

---

## Measurement and Validation

### Benchmarks to Run

```scala
// Before each optimization:
@Benchmark
def baseline(): Any = {
  jitted(NoLogger, NoBudgetSpender, params)
}

@Benchmark  
def withBudgetTracking(): Any = {
  val spender = CountingBudgetSpender()
  jitted(NoLogger, spender, params)
}

@Benchmark
def withLogging(): Any = {
  val logger = Log()
  jitted(logger, NoBudgetSpender, params)
}
```

### Success Metrics

| Metric | Current | Target | Stretch |
|--------|---------|--------|---------|
| Time/op (no budget) | 210 μs | 105 μs | 85 μs |
| Time/op (with budget) | 250 μs | 125 μs | 100 μs |
| Alloc/op | 547 KB | 300 KB | 200 KB |
| GC count | 351 | 200 | 150 |

---

## Risk Mitigation

### Risk 1: Correctness
**Mitigation:** 
- Run full test suite after each optimization
- Compare with CekMachine on all test cases
- Add property-based tests

### Risk 2: Complexity
**Mitigation:**
- Implement one optimization at a time
- Keep old code for comparison
- Document each change thoroughly

### Risk 3: Marginal Gains
**Mitigation:**
- Benchmark each optimization separately
- Only keep changes that show >5% improvement
- Be ready to roll back

---

## Code Locations

Files to modify:
1. `scalus-uplc-jit-compiler/src/main/scala/scalus/uplc/eval/ContinuationJitRepr.scala`
   - Add fast-path in eval loop
   - Optimize stack operations

2. `scalus-uplc-jit-compiler/src/main/scala/scalus/uplc/eval/JIT.scala`
   - Generate specialized builtin code
   - Add inline optimizations

3. New file: `scalus-uplc-jit-compiler/src/main/scala/scalus/uplc/eval/FramePool.scala`
   - Frame pooling implementation

4. New file: `scalus-uplc-jit-compiler/src/main/scala/scalus/uplc/eval/SpecializedBuiltins.scala`
   - Specialized builtin implementations

---

## Summary

These optimizations target the hottest paths in the JIT compiler with minimal code changes. Each optimization is independent and can be implemented incrementally. The combination should achieve 2x performance improvement while maintaining correctness and code quality.

**Next Steps:**
1. Review and approve this plan
2. Set up development branch
3. Implement optimizations in order
4. Measure and validate each change
5. Proceed to bytecode generation phase if successful
