# JIT Inlining Analysis Summary

**Log Location:** `/Users/rssh/packages/nau/scalus/jit-analysis-output/inlining_log.txt`
**Log Size:** 6.9 MB (59,024 lines)
**Generated:** 2025-10-27

---

## Key Findings from Inlining Log

### 1. **ContinuationJitRepr::eval** - The Main Eval Loop

**Compilation:**
```
Task ID: 10612, Tier: 3 (C1 compiled)
Method: scalus.uplc.eval.ContinuationJitRepr$::eval (445 bytes)
```

**Inlining Decisions:**

✅ **Successfully Inlined:**
- `Return::unapply` (2 bytes)
- `Return::_1` (5 bytes)
- `Return::value` (5 bytes)
- `popFrame$1` (23 bytes)
- `Frame$ApplyFuncFrame::unapply` (2 bytes)
- `Frame$ApplyFuncFrame::_1` (5 bytes)
- `Frame$ApplyFuncFrame::arg` (5 bytes)
- `Frame$ApplyArgFrame::apply` (9 bytes)
- `Frame$ApplyArgFrame::<init>` (10 bytes)
- `Apply::unapply` (2 bytes)
- `Apply::_1`, `Apply::_2` (5 bytes each)

❌ **Failed to Inline (CRITICAL BOTTLENECK):**
```
@ 128   ContinuationJitRepr$::pushFrame$1 (85 bytes)
        ❌ failed to inline: callee is too large

@ 374   ContinuationJitRepr$::pushFrame$1 (85 bytes)
        ❌ failed to inline: callee is too large

@ 422   ContinuationJitRepr$::pushFrame$1 (85 bytes)
        ❌ failed to inline: callee is too large
```

**Impact:** `pushFrame$1` is called in the hot path but cannot be inlined. This adds function call overhead on every frame push operation.

---

### 2. **CekMachine::computeCek** - CEK Evaluation

**Compilation:**
```
Task ID: 10825, Tier: 4 (C2 optimized - highest tier!)
Method: scalus.uplc.eval.CekMachine::computeCek (999 bytes)
```

**Key Difference:** Despite being **2.2x larger** than ContinuationJitRepr::eval, the CEK machine's `computeCek` gets compiled with the **C2 compiler** (highest optimization tier).

**Why CEK is Faster:**
1. **C2 compilation** - More aggressive optimizations
2. **Direct field access** - `this.ctx`, `this.env`, `this.value` inline trivially
3. **Primitive-based dispatch** - `@switch` on Int states
4. **No intermediate allocations** - Mutable state updates

---

### 3. **CekMachine::returnCek**

**Compilation:**
```
Task ID: 10824, Tier: 4 (C2 optimized)
Method: scalus.uplc.eval.CekMachine::returnCek (747 bytes)
```

Again, **C2 compiled** despite large size.

---

## Comparison Table

| Metric | JIT (ContinuationJitRepr) | CEK Machine |
|--------|---------------------------|-------------|
| **Eval method size** | 445 bytes | 999 bytes |
| **Compilation tier** | C1 (level 3) | C2 (level 4) ⭐ |
| **Critical inlining failures** | `pushFrame$1` (85 bytes) × 3 | None |
| **Allocations per operation** | 1-3 objects | 0 objects |
| **Dispatch mechanism** | Pattern matching on objects | @switch on primitives |
| **Type safety** | Runtime casts (`asInstanceOf`) | Static types |

---

## Performance Impact Analysis

### **Why pushFrame$1 Matters**

From ContinuationJitRepr.scala:61-70:
```scala
@inline def pushFrame(frame: Frame): Unit = {
  if stackSize >= stack.length then {
    // Grow array if needed (rare)
    val newStack = new Array[Frame](stack.length * 2)
    System.arraycopy(stack, 0, newStack, 0, stack.length)
    stack = newStack
  }
  stack(stackSize) = frame
  stackSize += 1
}
```

**Why it's 85 bytes:**
- Array bounds check
- Conditional array growth logic
- System.arraycopy call
- Frame allocation
- Stack pointer update

**Why it can't inline:**
- JVM's default MaxInlineSize is typically 35 bytes
- Can be increased with `-XX:MaxInlineSize=85` but not recommended
- Better solution: simplify the logic

---

## Useful grep Commands for the Log

### Find all inlining failures:
```bash
grep "failed to inline" inlining_log.txt | sort | uniq -c | sort -rn
```

### Find JIT-specific methods:
```bash
grep "scalus.uplc.eval.ContinuationJitRepr" inlining_log.txt
```

### Find CEK-specific methods:
```bash
grep "scalus.uplc.eval.CekMachine" inlining_log.txt
```

### Find methods compiled to C2 (highest tier):
```bash
grep "4       scalus" inlining_log.txt | grep -v "made not entrant"
```

### Find allocation hotspots:
```bash
grep "::new\|::<init>" inlining_log.txt | grep inline | head -50
```

---

## Top Inlining Failure Categories

From the log analysis:

1. **"callee is too large"** - 307 occurrences
   - Most common failure mode
   - Includes `pushFrame$1` in critical path

2. **"no static binding"** - ~150 occurrences
   - Megamorphic call sites
   - Function interface calls (`Function1::apply`, `Function2::apply`)

3. **"recursive inlining too deep"** - 31 occurrences
   - Prevents excessive code bloat

4. **"callee uses too much stack"** - 23 occurrences
   - Stack depth limits

---

## Recommended JVM Flags for Future Analysis

### See assembly (requires hsdis):
```bash
-XX:+UnlockDiagnosticVMOptions
-XX:+PrintAssembly
-XX:CompileCommand=print,scalus/uplc/eval/ContinuationJitRepr.eval
-XX:CompileCommand=print,scalus/uplc/eval/CekMachine.computeCek
```

### Force C2 compilation earlier:
```bash
-XX:CompileThreshold=1000     # Compile after 1000 invocations (default: 10000)
-XX:Tier3InvocationThreshold=100
```

### Increase inline size (experimental):
```bash
-XX:MaxInlineSize=85          # Allow larger methods to inline
-XX:FreqInlineSize=500        # For hot methods
```

### Disable tiered compilation (C2 only):
```bash
-XX:-TieredCompilation
```

---

## Next Steps

1. ✅ **Completed:** Identified that `pushFrame$1` cannot inline
2. 📋 **Recommended:** Implement defunctionalization to eliminate `pushFrame` entirely
3. 📋 **Alternative:** Split `pushFrame` into specialized versions:
   - `pushFrameFast` - no growth check (used when capacity known)
   - `pushFrameSlow` - with growth (rarely used)

4. 📊 **Monitor:** Re-run analysis after optimizations:
   ```bash
   sbt "set scalusUplcJitCompiler/Test/javaOptions ++= Seq(\"-XX:+PrintInlining\"); \
        scalusUplcJitCompiler/Test/runMain scalus.uplc.eval.AnalyzeJITBytecode" \
        2>&1 | tee jit-analysis-output/inlining_log_after_opt.txt
   ```

---

## Files in This Directory

```
jit-analysis-output/
├── ANALYSIS_SUMMARY.md          # This file
├── inlining_log.txt             # Full inlining log (6.9MB)
└── staged-classes/              # Dumped class files (if -Dscala.quoted.dumpClassesTo used)
```

---

## Key Insight

**The JIT compiler's continuation-based design creates a fundamental impedance mismatch with HotSpot's optimization heuristics.**

- Small methods (5-23 bytes) inline easily
- Medium methods (85 bytes) fail to inline in critical paths
- Large methods (445 bytes) get C1 compilation only
- CEK's very large methods (999 bytes) get C2 compilation with full optimization

**Lesson:** Sometimes "bigger is better" for JVM optimization - monolithic methods with direct field access can be more optimizable than many small methods with allocation overhead.
