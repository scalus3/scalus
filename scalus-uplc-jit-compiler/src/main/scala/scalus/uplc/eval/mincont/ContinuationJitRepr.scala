package scalus.uplc.eval.mincont

/** Minimal continuation representation for JIT-compiled code.
  *
  * All continuations are processed in a top-level loop, avoiding stack overflow. No recursive calls -
  * everything is flattened to heap-allocated continuation nodes.
  */
enum ContinuationJitRepr {

    /** A computed value - terminal continuation */
    case Return(value: Any)

    /** Apply function to argument - both must be evaluated first */
    case Apply(func: ContinuationJitRepr, arg: ContinuationJitRepr)

    /** Force a delayed computation */
    case Force(delayed: ContinuationJitRepr)
}

object ContinuationJitRepr {

    object Return {

        /** Helper to create a delayed Return without triggering compiler slowdown. Accepts the
          * computation by-name to avoid pattern matching issues.
          */
        def delayed(body: => ContinuationJitRepr): Return = Return(() => body)
    }



    /** Stack frames for the evaluator */
    enum Frame {

        /** After evaluating function in Apply, evaluate argument */
        case ApplyFuncFrame(arg: ContinuationJitRepr)

        /** After evaluating argument in Apply, apply function to it */
        case ApplyArgFrame(func: Any)

        /** After evaluating delayed computation, force it */
        case ForceFrame
    }

    /** Top-level evaluator - processes continuations iteratively without recursion.
      *
      * This is the core of the minimal continuation approach:
      *   - No call stack recursion - everything goes through this loop
      *   - Stack frames are on the heap, not the JVM call stack
      *   - Array-based stack for efficient push/pop operations
      *   - Fast path for common case (Return with empty stack)
      *   - Each iteration is simple and JIT-friendly
      */
    def eval(cont: ContinuationJitRepr): Any = {
        import Frame.*
        import ContinuationJitRepr.*

        var current = cont
        // Array-based stack for better performance than List
        var stack = new Array[Frame](32) // Initial capacity
        var stackSize = 0

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

        @inline def popFrame(): Frame = {
            stackSize -= 1
            stack(stackSize)
        }

        while true do {
            current match {
                case Return(value) =>
                    // Check if we're done (no more frames)
                    if stackSize == 0 then {
                        return value
                    }

                    // We have a value and frames to process
                    val frame = popFrame()
                    frame match {
                        case ApplyFuncFrame(arg) =>
                            // We evaluated the function, now evaluate the argument
                            pushFrame(ApplyArgFrame(value))
                            current = arg

                        case ApplyArgFrame(func) =>
                            // We have both function and argument - apply!
                            // Special case: if func is Function0 and arg is Unit, just call the Function0
                            val result = func match {
                                case f0: Function0[?] => f0()
                                case f1               => f1.asInstanceOf[Any => Any](value)
                            }
                            // Result might be a continuation, continue evaluation
                            // Avoid creating new Return if result is already one
                            current = result match {
                                case cont: ContinuationJitRepr => cont
                                case v                         => Return(v)
                            }

                        case ForceFrame =>
                            // Force the delayed computation
                            val result = value.asInstanceOf[() => Any]()
                            // Result might be a continuation, continue evaluation
                            // Avoid creating new Return if result is already one
                            current = result match {
                                case cont: ContinuationJitRepr => cont
                                case v                         => Return(v)
                            }
                    }

                case Apply(func, arg) =>
                    // Push frame to evaluate arg after func, then evaluate func
                    pushFrame(ApplyFuncFrame(arg))
                    current = func

                case Force(delayed) =>
                    // Push frame to force after evaluation, then evaluate
                    pushFrame(ForceFrame)
                    current = delayed
            }
        }
    }

}
