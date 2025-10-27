# Lambda Compilation Scheme for Defunctionalized JIT

## Overview

The defunctionalized JIT compiler uses a hybrid approach combining:
- **Control flow opcodes** (OP_APPLY, OP_LAMBDA, OP_RETURN) for managing evaluation
- **JIT snippets** for constants, builtins, and operations
- **De Bruijn indices** for variable lookup from the data stack

## Core Data Structures

### Closure
```scala
case class Closure(
    bodyInstrIdx: Int,  // Where the lambda body starts
    env: Array[Any]     // Currently unused - env is implicit in stack
)
```

Key insight: **No environment copying needed!** De Bruijn indices already encode variable positions relative to the stack.

### Frame Types
- `FRAME_APPLY_ARG` - Waiting to evaluate argument after function
- `FRAME_APPLY_EXEC` - Have both function and argument, ready to apply
- `FRAME_FORCE` - Force a delayed computation
- `FRAME_RESTORE_ENV` - Clean up stack after closure evaluation

## Compilation Rules

### Constants
```
Term.Const(value)
→
[i]   EXEC_SNIPPET(constant_value)
[i+1] RETURN
```

### Variables
```
Term.Var(NamedDeBruijn(name, index))
→
[i]   EXEC_SNIPPET(dataStack.peek(index))
[i+1] RETURN
```

### Builtins
```
Term.Builtin(AddInteger)
→
[i]   EXEC_SNIPPET(curried_function)
[i+1] RETURN
```

### Lambda Abstraction
```
Term.LamAbs(name, body)
→
[i]   OP_LAMBDA(bodyIdx = j)  // Creates Closure(j, [])
[i+1] RETURN
...   (lambda won't fall through to body)
[j]   <compiled body>
[k]   RETURN                  // Body's RETURN
```

**Key points:**
- RETURN after OP_LAMBDA prevents fall-through to body
- Body is compiled separately with extended environment
- Body index is back-patched after body compilation

### Application
```
Term.Apply(fun, arg)
→
[i]   <compiled fun>
[i+x] RETURN
[j]   <compiled arg>
[j+y] RETURN
[k]   OP_APPLY(funIdx=i, argIdx=j)
[k+1] RETURN
```

**Problem:** Each sub-term emits its own RETURN, causing issues with nested Applies!

## Execution Model

### Simple Lambda Application: `(\x -> x) 42`

**Compilation:**
```
[0] Const(42) snippet
[1] RETURN
[2] OP_LAMBDA(bodyIdx=5)
[3] RETURN
[4] OP_APPLY(funIdx=2, argIdx=0)
[5] RETURN                    ← Entry point
[6] Var(x, index=0) snippet   ← Lambda body starts here
[7] RETURN
```

**Execution Trace:**
```
1. ip=5, Execute OP_APPLY(2, 0)
   - Push FRAME_APPLY_ARG(argIdx=0, returnAddr=6)
   - Jump to funIdx=2

2. ip=2, Execute OP_LAMBDA(bodyIdx=6)
   - Create Closure(bodyIdx=6, env=[])
   - acc = Closure
   - ip=3

3. ip=3, Execute RETURN
   - Pop FRAME_APPLY_ARG
   - funcValue = Closure (from acc)
   - Push FRAME_APPLY_EXEC(funcValue, returnAddr=6)
   - Jump to argIdx=0

4. ip=0, Execute Const(42) snippet
   - acc = 42
   - ip=1

5. ip=1, Execute RETURN
   - Pop FRAME_APPLY_EXEC
   - funcValue = Closure, argValue = 42
   - Apply Closure to 42:
     * Push argValue (42) onto dataStack
     * Push FRAME_RESTORE_ENV(valuesToPop=1, returnAddr=6)
     * Jump to bodyIdx=6

6. ip=6, Execute Var(x, index=0) snippet
   - acc = dataStack.peek(0) = 42
   - ip=7

7. ip=7, Execute RETURN
   - Pop FRAME_RESTORE_ENV
   - Pop 1 value from dataStack
   - ip=6 (returnAddr)

8. ip=6, Execute RETURN (the one after OP_APPLY)
   - frameStack is empty
   - Return acc = 42 ✓
```

### Nested Application: `(\x -> x + 1) 5` ❌ BROKEN

**Compilation:**
```
[0]  Const(5) snippet
[1]  RETURN
[2]  OP_LAMBDA(bodyIdx=5)
[3]  RETURN
[4]  OP_APPLY(funIdx=2, argIdx=0)
[5]  RETURN                       ← Entry point
[6]  AddInteger snippet           ← Lambda body starts here
[7]  RETURN
[8]  Var(x, index=0) snippet
[9]  RETURN
[10] OP_APPLY(funIdx=6, argIdx=8) ← Inner Apply
[11] RETURN
[12] Const(1) snippet
[13] RETURN
[14] OP_APPLY(funIdx=10, argIdx=12) ← Outer Apply (body entry)
[15] RETURN
```

**Execution Trace (BUGGY):**
```
1-5. Same as identity function, but jump to bodyIdx=14

6. ip=14, Execute OP_APPLY(funIdx=10, argIdx=12)
   Frame stack: [FRAME_RESTORE_ENV]
   - Push FRAME_APPLY_ARG(argIdx=12, returnAddr=15)
   - Jump to funIdx=10
   Frame stack: [FRAME_RESTORE_ENV, FRAME_APPLY_ARG]

7. ip=10, Execute OP_APPLY(funIdx=6, argIdx=8)
   - Push FRAME_APPLY_ARG(argIdx=8, returnAddr=11)
   - Jump to funIdx=6
   Frame stack: [FRAME_RESTORE_ENV, FRAME_APPLY_ARG(12,15), FRAME_APPLY_ARG(8,11)]

8. ip=6, Execute AddInteger snippet → curried function
   ip=7

9. ip=7, Execute RETURN
   - Pop FRAME_APPLY_ARG(8,11)
   - Push FRAME_APPLY_EXEC(curried_func, returnAddr=11)
   - Jump to argIdx=8
   Frame stack: [FRAME_RESTORE_ENV, FRAME_APPLY_ARG(12,15), FRAME_APPLY_EXEC]

10. ip=8, Execute Var(x) → 5
    ip=9

11. ip=9, Execute RETURN
    - Pop FRAME_APPLY_EXEC
    - Apply curried_func(5) → partial_func
    - acc = partial_func
    - ip=11 (returnAddr)
    Frame stack: [FRAME_RESTORE_ENV, FRAME_APPLY_ARG(12,15)]

12. ip=11, Execute RETURN  ← BUG HAPPENS HERE!
    - Pop FRAME_APPLY_ARG(12,15)  ← This is the OUTER Apply's frame!
    - funcValue = partial_func (in acc)
    - Push FRAME_APPLY_EXEC(partial_func, returnAddr=15)
    - Jump to argIdx=12
    Frame stack: [FRAME_RESTORE_ENV, FRAME_APPLY_EXEC]

13. ip=12, Execute Const(1) → 1
    ip=13

14. ip=13, Execute RETURN
    - Pop FRAME_APPLY_EXEC
    - Apply partial_func(1) → 6
    - acc = 6
    - ip=15 (returnAddr)
    Frame stack: [FRAME_RESTORE_ENV]

15. ip=15, Execute RETURN
    - Pop FRAME_RESTORE_ENV
    - Pop 1 value from dataStack
    - ip = original returnAddr

16. Result = 6 ✓
```

**Wait!** According to this trace, it should work and return 6!

## The Mystery

The manual trace shows the execution SHOULD return 6, but the actual test returns a Function object. This suggests:

1. **Either the trace is wrong** - Missing something about how the frames work
2. **Or the implementation differs** - Bug in the actual code vs. the model
3. **Or the instruction indices are wrong** - `compileTerm` returns wrong indices

## Root Cause Analysis

The issue is at step 12 above. The RETURN at [11] is supposed to "return from the inner Apply". But in a defunctionalized system, RETURN always pops a frame and continues. The RETURN at [11] pops the outer Apply's FRAME_APPLY_ARG, which happens to be correct for continuing the evaluation!

**Hypothesis:** Maybe the actual bug is that the inner OP_APPLY at [10] has the wrong returnAddr? Or perhaps `compileTerm` for the inner Apply is returning the wrong index?

## Alternative Compilation Strategies

### Strategy 1: Inline Sub-expressions
Don't use OP_APPLY for sub-expressions; compile them inline with direct evaluation.

**Problem:** Loses the defunctionalization benefit; would need a different execution model.

### Strategy 2: Conditional RETURN
Add a flag to control whether to emit RETURN after each term.

**Problem:** Already tried this; breaks the OP_APPLY control flow which relies on RETURNs.

### Strategy 3: Different Opcode for Nested Applies
Use OP_APPLY for top-level applications, OP_APPLY_INLINE for nested ones.

**Problem:** Increases complexity; unclear if it solves the fundamental issue.

### Strategy 4: CPS-style Compilation
Compile everything in continuation-passing style explicitly.

**Problem:** Major refactoring; might defeat the purpose of defunctionalization.

## Current Status

- **9/10 tests passing**
- Identity function works
- Simple arithmetic works
- Nested arithmetic (no lambdas) works
- Lambda with nested builtin applications fails

## Debug Strategy

1. Add comprehensive logging to trace actual frame operations
2. Print instruction sequences and verify indices
3. Compare with mincont JIT execution
4. Add assertion checks for frame stack consistency
5. Test with simpler nested cases to isolate the issue

## References

- `JITCompiler.scala` - Compilation logic
- `EvalContext.scala` - Execution logic with frame management
- `Program.scala` - Data structures (Closure, Instruction, Snippet)
- `JIT.scala` - Opcode definitions
