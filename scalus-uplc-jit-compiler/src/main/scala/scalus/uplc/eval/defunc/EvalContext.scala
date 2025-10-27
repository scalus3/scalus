package scalus.uplc.eval.defunc

import scala.annotation.switch
import scala.collection.mutable
import scalus.uplc.eval.{BudgetSpender, ExBudgetCategory, Logger, MachineParams, StepKind}
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

/** State for case application - tracking remaining args to apply.
  *
  * When applying constructor arguments to a case function, we may need
  * to pause for VM evaluation (e.g., closure application), then resume.
  */
private[eval] class CaseAppState(
    val remainingArgs: List[Any],
    val returnAddr: Int
)

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
    private val caseStack = mutable.ArrayBuffer[CaseAppState]() // Case application state stack
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
                    
                    logger.log(s"OP_RETURN: frameStack.isEmpty=${frameStack.isEmpty}, caseStack.size=${caseStack.size}, acc=${if acc == null then "null" else acc.getClass.getSimpleName}")
                    
                    // First check if we're in the middle of case argument application
                    if caseStack.nonEmpty && frameStack.isEmpty then {
                        // Case function finished - start applying args
                        val caseState = caseStack.last
                        val remainingArgs = caseState.remainingArgs
                        val finalReturnAddr = caseState.returnAddr
                        
                        logger.log(s"OP_RETURN: Case active, remainingArgs.size=${remainingArgs.size}")
                        
                        if remainingArgs.isEmpty then {
                            // No args to apply - just return
                            caseStack.remove(caseStack.length - 1)
                            logger.log(s"OP_RETURN: No args, returning acc=$acc")
                            return acc
                        }
                        
                        // Apply args
                        var funcValue = acc
                        var currentArgs = remainingArgs
                        var hitClosure = false
                        
                        while currentArgs.nonEmpty && !hitClosure do {
                            val arg :: rest = currentArgs: @unchecked
                            funcValue match {
                                case closure: Closure =>
                                    // Need VM evaluation
                                    caseStack(caseStack.length - 1) = CaseAppState(rest, finalReturnAddr)
                                    dataStack.push(arg)
                                    frameStack.push(FRAME_RESTORE_ENV, 1, -1)
                                    ip = closure.bodyInstrIdx
                                    hitClosure = true // Exit while loop, let VM continue
                                    
                                case f: Function1[?, ?] =>
                                    funcValue = f.asInstanceOf[Any => Any](arg)
                                    currentArgs = rest
                                    
                                case snippet: Snippet =>
                                    funcValue = snippet.execute(arg, dataStack, budget, logger, params)
                                    currentArgs = rest
                                    
                                case _ =>
                                    throw new IllegalStateException(
                                      s"Cannot apply non-function in case: ${funcValue.getClass}"
                                    )
                            }
                        }
                        
                        // If all args applied without hitting closure
                        if !hitClosure then {
                            caseStack.remove(caseStack.length - 1)
                            acc = funcValue
                            return acc
                        }
                        // Otherwise fall through to continue VM loop
                    } else if frameStack.isEmpty then {
                        // Top level - done!
                        return acc
                    } else {
                        // Pop frame and continue
                        val frame = frameStack.pop()
                        
                        // Debug logging
                        logger.log(s"RETURN: popped frame type=${frame.frameType}, acc=${acc.getClass.getSimpleName}")

                        (frame.frameType: @switch) match {
                        case FRAME_APPLY_ARG =>
                            // We have function in acc, now evaluate argument
                            // Push new frame to apply after arg evaluation
                            val funcValue = acc
                            val argInstrIdx = frame.data.asInstanceOf[Int]
                            
                            logger.log(s"FRAME_APPLY_ARG: funcValue=${funcValue.getClass.getSimpleName}, argIdx=$argInstrIdx, returnAddr=${frame.returnAddr}")
                            
                            frameStack.push(FRAME_APPLY_EXEC, funcValue, frame.returnAddr)
                            ip = argInstrIdx

                        case FRAME_APPLY_EXEC =>
                            // We have both function and argument, execute application
                            val funcValue = frame.data
                            val argValue = acc
                            
                            logger.log(s"FRAME_APPLY_EXEC: funcValue=${funcValue.getClass.getSimpleName}, argValue=$argValue, returnAddr=${frame.returnAddr}")

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
                                    logger.log(s"Applied Function1 to $argValue, result=${acc.getClass.getSimpleName}")
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

                                case closure: Closure =>
                                    // Delay creates a Closure with no arguments
                                    // Jump to body without pushing arguments
                                    frameStack.push(FRAME_RESTORE_ENV, 0, frame.returnAddr)
                                    ip = closure.bodyInstrIdx

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
                            
                            logger.log(s"FRAME_RESTORE_ENV: Popped, valuesToPop=$valuesToPop, returnAddr=${frame.returnAddr}, caseStack.size=${caseStack.size}")
                            
                            // Pop values pushed by the closure body (keep result in acc)
                            var i = 0
                            while i < valuesToPop do {
                                dataStack.pop()
                                i += 1
                            }
                            
                            // Check if we're in the middle of case argument application
                            if caseStack.nonEmpty then {
                                // Continue applying remaining args
                                val caseState = caseStack.last
                                val remainingArgs = caseState.remainingArgs
                                val finalReturnAddr = caseState.returnAddr
                                
                                logger.log(s"FRAME_RESTORE_ENV: caseStack active, remainingArgs.size=${remainingArgs.size}, acc=${acc}, accType=${acc.getClass.getSimpleName}")
                                
                                // Apply next arg
                                if remainingArgs.nonEmpty then {
                                    val nextArg :: restArgs = remainingArgs: @unchecked
                                    var funcValue = acc
                                    
                                    logger.log(s"FRAME_RESTORE_ENV: Starting to apply ${remainingArgs.size} args")
                                    
                                    // Try to apply as many non-closure args as possible
                                    var currentArgs = remainingArgs
                                    var allApplied = false
                                    while currentArgs.nonEmpty && !allApplied do {
                                        val arg :: rest = currentArgs: @unchecked
                                        logger.log(s"FRAME_RESTORE_ENV: Applying arg=$arg to funcValue=${funcValue.getClass.getSimpleName}")
                                        funcValue match {
                                            case closure: Closure =>
                                                // Need VM evaluation - update state and break
                                                logger.log(s"FRAME_RESTORE_ENV: Hit closure, updating caseStack with ${rest.size} remaining args")
                                                caseStack(caseStack.length - 1) = CaseAppState(rest, finalReturnAddr)
                                                dataStack.push(arg)
                                                frameStack.push(FRAME_RESTORE_ENV, 1, -1) // Will re-enter here
                                                ip = closure.bodyInstrIdx
                                                allApplied = true // Exit loop, let VM continue
                                                return
                                                
                                            case f: Function1[?, ?] =>
                                                funcValue = f.asInstanceOf[Any => Any](arg)
                                                currentArgs = rest
                                                logger.log(s"FRAME_RESTORE_ENV: Applied Function1, result=${funcValue.getClass.getSimpleName}")
                                                
                                            case snippet: Snippet =>
                                                funcValue = snippet.execute(arg, dataStack, budget, logger, params)
                                                currentArgs = rest
                                                logger.log(s"FRAME_RESTORE_ENV: Applied Snippet, result=${funcValue.getClass.getSimpleName}")
                                                
                                            case _ =>
                                                throw new IllegalStateException(
                                                  s"Cannot apply non-function in case: ${funcValue.getClass}"
                                                )
                                        }
                                    }
                                    
                                    // All args applied (or hit closure)
                                    if currentArgs.isEmpty then {
                                        // Done with this case application
                                        logger.log(s"FRAME_RESTORE_ENV: All args applied, final result=${funcValue.getClass.getSimpleName}")
                                        caseStack.remove(caseStack.length - 1)
                                        acc = funcValue
                                        ip = finalReturnAddr
                                    } else {
                                        acc = funcValue
                                        // Closure case handled above with return
                                    }
                                } else {
                                    // No more args - case application finished
                                    logger.log(s"FRAME_RESTORE_ENV: No more args, removing caseStack, acc=${acc.getClass.getSimpleName}, finalReturnAddr=$finalReturnAddr")
                                    caseStack.remove(caseStack.length - 1)
                                    ip = finalReturnAddr
                                }
                            } else {
                                // Normal case - just continue to return address
                                ip = frame.returnAddr
                            }

                        case FRAME_CONSTR_ARG =>
                            // Finished evaluating one constructor argument
                            // data = (tag, argIndices, evaluatedArgs, currentIndex)
                            val (tag, argIndices, evaluatedArgs, currentIdx) = 
                                frame.data.asInstanceOf[(Long, Array[Int], List[Any], Int)]
                            
                            // Add evaluated arg to list
                            val newEvaluatedArgs = acc :: evaluatedArgs
                            val nextIdx = currentIdx + 1
                            
                            if nextIdx >= argIndices.length then {
                                // All arguments evaluated - build tuple
                                acc = (tag, newEvaluatedArgs.reverse)
                                ip = frame.returnAddr
                            } else {
                                // More arguments to evaluate
                                frameStack.push(
                                  FRAME_CONSTR_ARG,
                                  (tag, argIndices, newEvaluatedArgs, nextIdx),
                                  frame.returnAddr
                                )
                                ip = argIndices(nextIdx)
                            }

                        case FRAME_CASE_APPLY =>
                            // Scrutinee evaluated, now select and apply case
                            val caseIndices = frame.data.asInstanceOf[Array[Int]]
                            
                            // acc should be a tuple (tag, args)
                            val (tag, args) = acc.asInstanceOf[(Long, List[Any])]
                            
                            // Select the appropriate case function
                            val tagInt = tag.toInt
                            if tagInt < 0 || tagInt >= caseIndices.length then {
                                throw new IllegalStateException(
                                  s"Case tag $tagInt out of bounds [0, ${caseIndices.length})"
                                )
                            }
                            
                            val caseFuncIdx = caseIndices(tagInt)
                            
                            // Push case state onto case stack
                            caseStack += CaseAppState(args, frame.returnAddr)
                            
                            // Evaluate the case function
                            ip = caseFuncIdx
                            
                        case _ =>
                            throw new IllegalStateException(
                              s"Unknown frame type: ${frame.frameType}"
                            )
                    }
                }

                case OP_APPLY =>
                    // Apply: evaluate function, then argument
                    // Push frame to evaluate argument after function
                    val funcInstrIdx = instr.data.asInstanceOf[(Int, Int)]._1
                    val argInstrIdx = instr.data.asInstanceOf[(Int, Int)]._2
                    
                    logger.log(s"OP_APPLY: ip=$ip, funIdx=$funcInstrIdx, argIdx=$argInstrIdx, frameStack.size=${frameStack.size}")

                    frameStack.push(FRAME_APPLY_ARG, argInstrIdx, ip + 1)

                    // Start evaluating function
                    ip = funcInstrIdx

                case OP_FORCE =>
                    // Force: evaluate delayed computation
                    val delayedInstrIdx = instr.data.asInstanceOf[Int]

                    frameStack.push(FRAME_FORCE, null, ip + 1)

                    // Evaluate the delayed computation
                    ip = delayedInstrIdx

                case OP_CONSTR =>
                    // Build constructor tuple (tag, args)
                    // data = (tag: Long, argInstrIndices: Array[Int])
                    val (tag, argIndices) = instr.data.asInstanceOf[(Long, Array[Int])]
                    
                    budget.spendBudget(
                      ExBudgetCategory.Step(StepKind.Constr),
                      params.machineCosts.constrCost,
                      Nil
                    )
                    
                    if argIndices.isEmpty then {
                        // No arguments - create tuple immediately
                        acc = (tag, Nil)
                        ip += 1
                    } else {
                        // Evaluate arguments left-to-right
                        // Push frame with: (tag, remaining args, evaluated args so far)
                        frameStack.push(
                          FRAME_CONSTR_ARG, 
                          (tag, argIndices, List.empty[Any], 0), 
                          ip + 1
                        )
                        // Start evaluating first argument
                        ip = argIndices(0)
                    }

                case OP_CASE =>
                    // Pattern match on constructor
                    // data = (scrutInstrIdx: Int, caseInstrIndices: Array[Int])
                    val (scrutIdx, caseIndices) = instr.data.asInstanceOf[(Int, Array[Int])]
                    
                    budget.spendBudget(
                      ExBudgetCategory.Step(StepKind.Case),
                      params.machineCosts.caseCost,
                      Nil
                    )
                    
                    // Evaluate scrutinee first, then select case
                    frameStack.push(FRAME_CASE_APPLY, caseIndices, ip + 1)
                    ip = scrutIdx

                case _ =>
                    throw new IllegalStateException(s"Unknown opcode: ${instr.opcode}")
            }
        }

        // Shouldn't reach here
        // If we exit the loop without returning, something went wrong
        logger.log(s"EXITED LOOP: ip=$ip, frameStack.isEmpty=${frameStack.isEmpty}, caseStack.size=${caseStack.size}, acc=${if acc == null then "null" else acc.getClass.getSimpleName}")
        throw new IllegalStateException("Program terminated without returning")
    }
}
