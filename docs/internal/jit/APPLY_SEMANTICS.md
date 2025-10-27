# Apply Operation Semantics - Hybrid JIT

## Overview

The Apply operation `Term.Apply(fun, arg)` implements function application in UPLC. In the hybrid JIT, it uses **defunctionalized control flow** with a continuation-passing style implemented via frame stack.

---

## Compilation Rules

### Input
```scala
Term.Apply(fun: Term, arg: Term)
```

### Compilation Steps

1. **Compile subterms**:
   ```scala
   val funIdx = compileTerm(fun, env)  // Instruction index for function
   val argIdx = compileTerm(arg, env)  // Instruction index for argument
   ```

2. **Emit OP_APPLY instruction**:
   ```scala
   Instruction(
     opcode = OP_APPLY,
     data = (funIdx, argIdx)  // Tuple of instruction indices
   )
   ```

### Example

**Source:**
```scala
Apply(AddInteger, Const(5))
```

**Compiled Instructions:**
```
0: EXEC_SNIPPET(AddIntegerSnippet)    // funIdx = 0
1: EXEC_SNIPPET(Const(5))             // argIdx = 1
2: OP_APPLY(funcInstrIdx=0, argInstrIdx=1)
3: OP_RETURN
```

---

## Execution Semantics

The execution follows a **3-phase protocol** using frame-based continuations:

### Phase 1: Initial Apply (OP_APPLY)

**When:** `ip` points to an `OP_APPLY` instruction

**Action:**
1. Extract instruction indices:
   ```scala
   val funcInstrIdx = instr.data._1
   val argInstrIdx = instr.data._2
   ```

2. Push continuation frame (FRAME_APPLY_ARG):
   ```scala
   frameTypes(fp) = FRAME_APPLY_ARG
   frameData(fp) = argInstrIdx        // Save arg index for later
   fp += 1
   ```

3. Jump to evaluate function:
   ```scala
   ip = funcInstrIdx
   ```

**State Change:**
```
Before: acc = <previous value>
        fp = n

After:  acc = <previous value>
        fp = n+1
        frameTypes(n) = FRAME_APPLY_ARG
        frameData(n) = argInstrIdx
        ip = funcInstrIdx
```

**Invariant:** We will evaluate `fun` next, and when it returns, we'll handle FRAME_APPLY_ARG.

---

### Phase 2: Function Evaluated (FRAME_APPLY_ARG)

**When:** Function evaluation completes (OP_RETURN with FRAME_APPLY_ARG on top)

**Precondition:**
- `acc` contains the evaluated function value
- `frameData(fp-1)` contains `argInstrIdx`

**Action:**
1. Pop and inspect frame:
   ```scala
   fp -= 1
   val frameType = frameTypes(fp)  // = FRAME_APPLY_ARG
   ```

2. Save function value and prepare for argument evaluation:
   ```scala
   val funcValue = acc                          // Save function
   val argInstrIdx = frameData(fp).asInstanceOf[Int]
   ```

3. Push new frame (FRAME_APPLY_EXEC):
   ```scala
   frameTypes(fp) = FRAME_APPLY_EXEC
   frameData(fp) = funcValue                    // Save function for Phase 3
   fp += 1
   ```

4. Jump to evaluate argument:
   ```scala
   ip = argInstrIdx
   ```

**State Change:**
```
Before: acc = <function value>
        fp = n+1
        frameTypes(n) = FRAME_APPLY_ARG
        frameData(n) = argInstrIdx

After:  acc = <function value>  (unchanged)
        fp = n+1
        frameTypes(n) = FRAME_APPLY_EXEC
        frameData(n) = <function value>
        ip = argInstrIdx
```

**Invariant:** We will evaluate `arg` next, and when it returns, we'll handle FRAME_APPLY_EXEC.

---

### Phase 3: Both Evaluated (FRAME_APPLY_EXEC)

**When:** Argument evaluation completes (OP_RETURN with FRAME_APPLY_EXEC on top)

**Precondition:**
- `acc` contains the evaluated argument value
- `frameData(fp-1)` contains the evaluated function value

**Action:**
1. Pop frame:
   ```scala
   fp -= 1
   val frameType = frameTypes(fp)  // = FRAME_APPLY_EXEC
   ```

2. Extract function and argument:
   ```scala
   val funcValue = frameData(fp)
   val argValue = acc
   ```

3. Execute application (type-based dispatch):
   ```scala
   funcValue match {
     case f: Function1[?, ?] =>
       // Scala function - direct call
       acc = f.asInstanceOf[Any => Any](argValue)
       ip += 1

     case snippet: Snippet =>
       // JIT snippet - execute with full context
       acc = snippet.execute(argValue, stack, sp, budget, logger, params)
       ip += 1

     case _ =>
       throw new IllegalStateException(
         s"Cannot apply non-function: ${funcValue.getClass}"
       )
   }
   ```

**State Change:**
```
Before: acc = <argument value>
        fp = n+1
        frameTypes(n) = FRAME_APPLY_EXEC
        frameData(n) = <function value>

After:  acc = <result of application>
        fp = n
        ip += 1
```

**Invariant:** Application complete, result in `acc`, ready to continue.

---

## Complete Example Trace

**Source Program:**
```scala
Apply(Apply(AddInteger, Const(5)), Const(10))
```

**Compiled Instructions:**
```
0: EXEC_SNIPPET(AddIntegerSnippet)  // Returns curried (x: Any) => (y: Any) => x + y
1: EXEC_SNIPPET(Const(5))           // Returns BigInt(5)
2: OP_APPLY(funcIdx=0, argIdx=1)    // (AddInteger 5)
3: EXEC_SNIPPET(Const(10))          // Returns BigInt(10)
4: OP_APPLY(funcIdx=2, argIdx=3)    // ((AddInteger 5) 10)
5: OP_RETURN
```

### Execution Trace

| Step | IP | Opcode | Action | acc | fp | Frame Stack |
|------|----|----|--------|-----|----|----|
| 1 | 0 | EXEC_SNIPPET | Execute AddIntegerSnippet | `<func1>` | 0 | [] |
| 2 | 1 | EXEC_SNIPPET | Execute Const(5) | `BigInt(5)` | 0 | [] |
| 3 | 2 | OP_APPLY | Push FRAME_APPLY_ARG(argIdx=1) | `BigInt(5)` | 1 | [APPLY_ARG(1)] |
| 4 | 0 | EXEC_SNIPPET | Jump to funcIdx=0, execute AddIntegerSnippet | `<func1>` | 1 | [APPLY_ARG(1)] |
| 5 | 1 | OP_RETURN | Hit return, pop frame, see FRAME_APPLY_ARG | `<func1>` | 1 | [APPLY_ARG(1)] |
| 6 | - | (APPLY_ARG) | Transform to FRAME_APPLY_EXEC(`<func1>`) | `<func1>` | 1 | [APPLY_EXEC(`<func1>`)] |
| 7 | 1 | EXEC_SNIPPET | Jump to argIdx=1, execute Const(5) | `BigInt(5)` | 1 | [APPLY_EXEC(`<func1>`)] |
| 8 | 2 | OP_RETURN | Hit return, pop frame, see FRAME_APPLY_EXEC | `BigInt(5)` | 1 | [APPLY_EXEC(`<func1>`)] |
| 9 | - | (APPLY_EXEC) | Apply `<func1>(BigInt(5))` | `<func2>` | 0 | [] |
| 10 | 3 | (continue) | Continue to next instruction | `<func2>` | 0 | [] |
| 11 | 3 | EXEC_SNIPPET | Execute Const(10) | `BigInt(10)` | 0 | [] |
| 12 | 4 | OP_APPLY | Push FRAME_APPLY_ARG(argIdx=3) | `BigInt(10)` | 1 | [APPLY_ARG(3)] |
| 13 | 2 | OP_APPLY | Jump to funcIdx=2 (which is another OP_APPLY) | `BigInt(10)` | 1 | [APPLY_ARG(3)] |
| ... | ... | ... | (Inner apply repeats Phase 1-3) | ... | ... | ... |
| N | 5 | OP_RETURN | Final return | `BigInt(15)` | 0 | [] |

**Note:** Step 13 shows that jumping to OP_APPLY creates nested apply handling. This is correct for `Apply(Apply(...), arg)`.

---

## Key Properties

### 1. Zero Allocations
- Frame data stored in pre-allocated arrays (`frameTypes`, `frameData`)
- No heap objects created for continuations
- Only the final result values are allocated

### 2. Tail Call Optimization
- When the last operation is Apply, no additional frame is needed
- Direct jumps via `ip` assignment
- Stack depth bounded by expression nesting, not call depth

### 3. Type Safety
- Runtime type checks via pattern matching on `funcValue`
- Clear error messages for type violations
- Supports both Scala functions and JIT snippets

### 4. Budget Handling
- Budget spending happens in snippet execution
- Apply itself is a control flow operation (no budget cost)
- Budget tracking remains functional

---

## Implementation Notes

### Current Status (JITDefunc.scala)
✅ **Implemented:**
- 3-phase protocol with frame stack
- Function1 application support
- Snippet application support
- Error handling

### Potential Issues

1. **Re-evaluation Problem:**
   - Current implementation jumps to instruction indices and re-executes
   - If instructions have side effects, they'll happen twice
   - **Solution:** Instructions should be pure or handle re-entry

2. **Instruction Index Validity:**
   - No bounds checking on `funcInstrIdx` and `argInstrIdx`
   - **Solution:** Add assertions or compile-time validation

3. **Stack Overflow:**
   - Fixed-size frame arrays (1024 frames)
   - Deeply nested expressions could overflow
   - **Solution:** Add overflow checks or dynamic resizing

---

## Comparison with CEK Machine

| Aspect | CEK Machine | Hybrid JIT |
|--------|-------------|------------|
| Continuation | Heap-allocated case classes | Array-based frames |
| Apply protocol | 2-phase (eval fun, eval arg) | 3-phase (push, eval fun, eval arg, apply) |
| Memory | One Context object per apply | Two array entries per apply |
| Type dispatch | Pattern matching on CekValue | Pattern matching on Any |
| Performance | Good (mutable) | Better (arrays + no allocations) |

---

## Future Optimizations

1. **Inline Apply for Simple Cases:**
   - When `fun` and `arg` are both constants or variables
   - Skip frame push and directly apply

2. **Static Call Resolution:**
   - When function is statically known (e.g., builtin)
   - Generate direct snippet call instead of OP_APPLY

3. **Currying Optimization:**
   - Detect chains like `Apply(Apply(AddInteger, x), y)`
   - Generate single multi-arg instruction

4. **Escape Analysis:**
   - Track which values escape to heap
   - Keep others on value stack for better cache locality

---

## References

- **Reynolds (1972):** "Definitional Interpreters for Higher-Order Programming Languages"
- **Danvy & Nielsen (2001):** "Defunctionalization at Work"
- **CPython 3.11+:** Similar defunctionalized eval loop for bytecode
- **JITDefunc.scala:** Lines 199-226 (OP_APPLY, FRAME_APPLY_ARG, FRAME_APPLY_EXEC)
- **JITDefuncCompiler.scala:** Lines 127-137 (Apply compilation)
