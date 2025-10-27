package scalus.uplc.eval.defunc

import scala.annotation.switch
import scalus.uplc.eval.{BudgetSpender, Logger, MachineParams}
import scalus.uplc.eval.defunc.{Closure, CompiledProgram, Snippet}
import scalus.uplc.eval.defunc.JIT.*

/** A single frame on the continuation stack.
  *
  * Mutable fields to avoid allocation overhead during push operations. Frames are preallocated and
  * reused.
  */
private[eval] class Frame {
    var frameType: Int = 0
    var data: Any = null
    var returnAddr: Int = 0
}

/** Manages the value stack for intermediate computation results.
  *
  * Simple array-based stack with a pointer for push/pop operations.
  */
private[eval] class DataStack(capacity: Int = 4096) {
    private val data = new Array[Any](capacity)
    private var sp = 0 // Stack pointer

    /** Push a value onto the stack. */
    def push(value: Any): Unit = {
        data(sp) = value
        sp += 1
    }

    /** Pop a value from the stack. */
    def pop(): Any = {
        sp -= 1
        data(sp)
    }

    /** Peek at the top value without removing it. */
    def peek(): Any = data(sp - 1)

    /** Get value at specific offset from top (0 = top, 1 = second from top, etc.). */
    def peek(offset: Int): Any = data(sp - 1 - offset)

    /** Check if the stack is empty. */
    def isEmpty: Boolean = sp == 0

    /** Get the current stack depth. */
    def size: Int = sp

    /** Get the underlying array (for snippet access). */
    def array: Array[Any] = data

    /** Get the current stack pointer (for snippet access). */
    def pointer: Int = sp
}

/** Manages the continuation stack using a single array of Frame objects.
  *
  * This replaces the previous approach of 3 parallel arrays (frameTypes, frameData,
  * frameReturnAddrs) with a single array for better cache locality and cleaner code.
  *
  * Frames are allocated lazily on demand in blocks - only when needed. Once allocated, frames are
  * reused by mutating their fields (zero allocation in steady state).
  */
private[eval] class FrameStack(capacity: Int = 1024, blockSize: Int = 64) {
    private val frames = new Array[Frame](capacity)
    private var fp = 0 // Frame pointer (current stack depth)
    private var allocatedCount = 0 // Number of frames already allocated

    /** Push a new frame onto the stack.
      *
      * Lazily allocates frames in blocks on demand, then reuses by mutating fields - zero
      * allocation in steady state.
      */
    def push(frameType: Int, data: Any, returnAddr: Int): Unit = {
        // Allocate frames in blocks if we need more
        if fp >= allocatedCount then {
            // Allocate a block of frames
            val blockEnd = math.min(allocatedCount + blockSize, capacity)
            var i = allocatedCount
            while i < blockEnd do {
                frames(i) = new Frame()
                i += 1
            }
            allocatedCount = blockEnd
        }

        // Reuse frame by mutating fields
        val frame = frames(fp)
        frame.frameType = frameType
        frame.data = data
        frame.returnAddr = returnAddr
        fp += 1
    }

    /** Pop a frame from the stack and return it. */
    def pop(): Frame = {
        fp -= 1
        frames(fp)
    }

    /** Update the top frame on the stack. */
    def updateTop(frameType: Int, data: Any, returnAddr: Int): Unit = {
        val frame = frames(fp - 1)
        frame.frameType = frameType
        frame.data = data
        frame.returnAddr = returnAddr
    }

    /** Check if the stack is empty. */
    def isEmpty: Boolean = fp == 0

    /** Get the current stack depth. */
    def size: Int = fp
}

/** Mutable evaluation context.
  *
  * Uses IFO pattern:
  *   - External: Pure functional API (eval method in JITDefunc)
  *   - Internal: Mutable state for zero-allocation execution
  */
private[eval] class EvalContext(
    program: CompiledProgram,
    budget: BudgetSpender,
    logger: Logger,
    params: MachineParams
) {

    // Mutable state - hidden from external API
    private val dataStack = new DataStack(4096) // Value stack for intermediate results
    private val frameStack = new FrameStack(1024) // Continuation stack
    private var ip = program.entryPoint // Instruction pointer
    private var acc: Any = null // Accumulator

    /** Execute the program (imperative loop with mutable state). */
    def run(): Any = {
        // Main evaluation loop
        while ip >= 0 && ip < program.instructions.length do {
            val instr = program.instructions(ip)

            // Switch on opcode (control flow)
            (instr.opcode: @switch) match {

                case OP_LAMBDA =>
                    // Create a closure - no need to capture environment
                    // De Bruijn indices already encode variable positions
                    val bodyIdx = instr.data.asInstanceOf[Int]
                    
                    // Just store the body index - environment is implicit in the stack
                    acc = Closure(bodyIdx, Array.empty)  // Empty env - we'll use current stack
                    ip += 1

                case OP_EXEC_SNIPPET =>
                    // Execute JIT snippet directly (bypasses switch!)
                    if instr.snippet != null then {
                        acc = instr.snippet.execute(acc, dataStack, budget, logger, params)
                        ip += 1
                    } else {
                        throw new IllegalStateException(
                          s"OP_EXEC_SNIPPET with null snippet at ip=$ip"
                        )
                    }

                case OP_RETURN =>
                    // Return from current context
                    if frameStack.isEmpty then {
                        // Top level - done!
                        return acc
                    }

                    // Pop frame and continue
                    val frame = frameStack.pop()

                    (frame.frameType: @switch) match {
                        case FRAME_APPLY_ARG =>
                            // We have function in acc, now evaluate argument
                            // Push new frame to apply after arg evaluation
                            val funcValue = acc
                            val argInstrIdx = frame.data.asInstanceOf[Int]
                            frameStack.push(FRAME_APPLY_EXEC, funcValue, frame.returnAddr)
                            ip = argInstrIdx

                        case FRAME_APPLY_EXEC =>
                            // We have both function and argument, execute application
                            val funcValue = frame.data
                            val argValue = acc

                            // Execute function application
                            funcValue match {
                                case closure: Closure =>
                                    // Apply closure by pushing argument and jumping to body
                                    // The De Bruijn indices in the body will reference the stack correctly
                                    dataStack.push(argValue)
                                    
                                    // Push frame to pop the argument after body evaluation
                                    frameStack.push(FRAME_RESTORE_ENV, 1, frame.returnAddr)  // Pop 1 value
                                    ip = closure.bodyInstrIdx

                                case f: Function1[?, ?] =>
                                    acc = f.asInstanceOf[Any => Any](argValue)
                                    ip = frame.returnAddr // Return to caller

                                case snippet: Snippet =>
                                    // Function is a snippet - execute it
                                    acc = snippet.execute(
                                      argValue,
                                      dataStack,
                                      budget,
                                      logger,
                                      params
                                    )
                                    ip = frame.returnAddr // Return to caller

                                case _ =>
                                    throw new IllegalStateException(
                                      s"Cannot apply non-function: ${funcValue.getClass}"
                                    )
                            }

                        case FRAME_FORCE =>
                            // Force a delayed computation
                            acc match {
                                case thunk: Function0[?] =>
                                    acc = thunk()
                                    ip = frame.returnAddr // Return to caller

                                case snippet: Snippet =>
                                    acc = snippet.execute(
                                      null,
                                      dataStack,
                                      budget,
                                      logger,
                                      params
                                    )
                                    ip = frame.returnAddr // Return to caller

                                case _ =>
                                    throw new IllegalStateException(
                                      s"Cannot force non-delayed value: ${acc.getClass}"
                                    )
                            }

                        case FRAME_RESTORE_ENV =>
                            // Restore environment after closure body evaluation
                            val valuesToPop = frame.data.asInstanceOf[Int]
                            // Pop values pushed by the closure body (keep result in acc)
                            var i = 0
                            while i < valuesToPop do {
                                dataStack.pop()
                                i += 1
                            }
                            // Continue to actual return address
                            ip = frame.returnAddr

                        case _ =>
                            throw new IllegalStateException(
                              s"Unknown frame type: ${frame.frameType}"
                            )
                    }

                case OP_APPLY =>
                    // Apply: evaluate function, then argument
                    // Push frame to evaluate argument after function
                    val funcInstrIdx = instr.data.asInstanceOf[(Int, Int)]._1
                    val argInstrIdx = instr.data.asInstanceOf[(Int, Int)]._2

                    frameStack.push(FRAME_APPLY_ARG, argInstrIdx, ip + 1)

                    // Start evaluating function
                    ip = funcInstrIdx

                case OP_FORCE =>
                    // Force: evaluate delayed computation
                    val delayedInstrIdx = instr.data.asInstanceOf[Int]

                    frameStack.push(FRAME_FORCE, null, ip + 1)

                    // Evaluate the delayed computation
                    ip = delayedInstrIdx

                case _ =>
                    throw new IllegalStateException(s"Unknown opcode: ${instr.opcode}")
            }
        }

        // Shouldn't reach here
        throw new IllegalStateException("Program terminated without returning")
    }
}
