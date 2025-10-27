# Compilation Issue Analysis

## Problem

Tests are hanging or failing with `ArrayIndexOutOfBoundsException`. The issue is with how we're compiling terms and managing control flow.

## Current Compilation Strategy (BROKEN)

```scala
Apply(AddInteger, Const(5))
```

Compiles to:
```
0: EXEC_SNIPPET(AddInteger)
1: OP_RETURN                 // ❌ This breaks the flow
2: EXEC_SNIPPET(Const(5))
3: OP_RETURN                 // ❌ This breaks the flow
4: OP_APPLY(funcIdx=0, argIdx=2)
5: OP_RETURN
```

When OP_APPLY executes:
1. Pushes FRAME_APPLY_ARG with argIdx=2
2. Jumps to funcIdx=0
3. Executes AddInteger snippet → returns function
4. Increments ip to 1
5. **Hits OP_RETURN at 1** → Pops frame, processes FRAME_APPLY_ARG
6. Now needs to evaluate arg, jumps to argIdx=2
7. Executes Const(5) snippet → returns BigInt(5)
8. Increments ip to 3
9. **Hits OP_RETURN at 3** → But frame stack is now wrong!

## Root Cause

The problem is that **snippets execute and return directly**, they don't need separate OP_RETURN instructions. The OP_RETURN should only appear:
1. At the END of the entire program
2. Never after individual snippets

## Correct Compilation Strategy

```scala
Apply(AddInteger, Const(5))
```

Should compile to:
```
0: EXEC_SNIPPET(AddInteger)
1: EXEC_SNIPPET(Const(5))
2: OP_APPLY(funcIdx=0, argIdx=1)
3: OP_RETURN  // Only at the end
```

But wait - this still has the same problem! After executing snippet at 0, we increment ip to 1, and execute snippet at 1, then increment to 2...

## The Real Issue

The issue is that **we're mixing two evaluation models**:

### Model 1: Direct Execution (what snippets do)
- Execute snippet
- Get result in `acc`
- Continue to next instruction

### Model 2: Jump-Based Execution (what OP_APPLY does)
- Jump to an instruction index
- Execute until OP_RETURN
- Return control

These two models are incompatible!

## Solution: Unified Jump-Based Model

**Every term should end with OP_RETURN**, and control flow should ALWAYS use jumps:

```scala
// Compilation:
Apply(AddInteger, Const(5))

// Emits:
0: EXEC_SNIPPET(AddInteger)
1: OP_RETURN
2: EXEC_SNIPPET(Const(5))
3: OP_RETURN
4: OP_APPLY(funcIdx=0, argIdx=2)
5: OP_RETURN
```

**Execution flow:**
```
Start at ip=4 (entry point is the Apply)
  OP_APPLY(funcIdx=0, argIdx=2):
    Push FRAME_APPLY_ARG(argIdx=2)
    Jump to ip=0

  EXEC_SNIPPET(AddInteger):
    acc = <function>
    ip = 1

  OP_RETURN:
    fp > 0, pop frame FRAME_APPLY_ARG
    Switch FRAME_APPLY_ARG to FRAME_APPLY_EXEC(<function>)
    Jump to argIdx=2

  EXEC_SNIPPET(Const(5)):
    acc = BigInt(5)
    ip = 3

  OP_RETURN:
    fp > 0, pop frame FRAME_APPLY_EXEC
    Apply <function>(BigInt(5))
    acc = <result>
    ip = 5 (continue after Apply)

  OP_RETURN:
    fp == 0, done!
    return acc
```

## Key Insight

The OP_RETURN instruction should **NOT increment ip**. Instead, it should:
1. If fp == 0: return result
2. If fp > 0: pop frame, process frame, and **set ip explicitly**

Currently, after executing a snippet, we increment ip. But with OP_RETURN following every snippet, the OP_RETURN handler should determine where to go next based on the frame type.

## Fix Required

1. **Keep OP_RETURN after every snippet** (already done)
2. **Fix OP_RETURN handler** to not rely on ip increment
3. **Fix OP_APPLY handler** to properly continue after frame processing

The key is that after processing FRAME_APPLY_EXEC, we should **continue to the instruction AFTER the OP_APPLY**, not just increment ip.

## Implementation Note

Currently in JITDefunc.scala:206, we have:
```scala
frameTypes(fp) = FRAME_APPLY_ARG
```

But we never store WHERE TO RETURN TO after the apply completes. We need to track the return address!

## Proposed Fix

Add a return address to each frame:

```scala
// In EvalContext:
private val frameTypes = new Array[Int](1024)
private val frameData = new Array[Any](1024)
private val frameReturnAddrs = new Array[Int](1024)  // NEW!
```

When pushing a frame, store where to return to:
```scala
case OP_APPLY =>
  val funcInstrIdx = instr.data._1
  val argInstrIdx = instr.data._2

  frameTypes(fp) = FRAME_APPLY_ARG
  frameData(fp) = argInstrIdx
  frameReturnAddrs(fp) = ip + 1  // Return to instruction AFTER this Apply
  fp += 1

  ip = funcInstrIdx  // Jump to function
```

When popping a frame after completion:
```scala
case OP_RETURN =>
  if (fp == 0) return acc

  fp -= 1
  val frameType = frameTypes(fp)
  val returnAddr = frameReturnAddrs(fp)

  (frameType: @switch) match {
    case FRAME_APPLY_ARG => ...
    case FRAME_APPLY_EXEC =>
      // Apply function to arg
      val funcValue = frameData(fp)
      val argValue = acc
      acc = funcValue.asInstanceOf[Function1[Any, Any]](argValue)
      ip = returnAddr  // Return to caller!
```

This way, after an Apply completes, we return to the instruction AFTER the Apply, not just blindly incrementing ip.
