package scalus.uplc.eval

/** Minimal continuation representation for JIT-compiled code.
  *
  * All continuations are processed in a top-level loop, avoiding stack overflow. No recursive calls -
  * everything is flattened to heap-allocated continuation nodes.
  */
sealed trait ContinuationJitRepr

object ContinuationJitRepr {

    /** A computed value - terminal continuation */
    case class Return(value: Any) extends ContinuationJitRepr

    object Return {

        /** Helper to create a delayed Return without triggering compiler slowdown. Accepts the
          * computation by-name to avoid pattern matching issues.
          */
        def delayed(body: => ContinuationJitRepr): Return = Return(() => body)
    }

    /** Apply function to argument - both must be evaluated first */
    case class Apply(func: ContinuationJitRepr, arg: ContinuationJitRepr)
        extends ContinuationJitRepr

    /** Force a delayed computation */
    case class Force(delayed: ContinuationJitRepr) extends ContinuationJitRepr

    /** Stack frames for the evaluator */
    sealed trait Frame

    /** After evaluating function in Apply, evaluate argument */
    case class ApplyFuncFrame(arg: ContinuationJitRepr) extends Frame

    /** After evaluating argument in Apply, apply function to it */
    case class ApplyArgFrame(func: Any) extends Frame

    /** After evaluating delayed computation, force it */
    case object ForceFrame extends Frame

    /** Top-level evaluator - processes continuations iteratively without recursion.
      *
      * This is the core of the minimal continuation approach:
      *   - No call stack recursion - everything goes through this loop
      *   - Stack frames are on the heap, not the JVM call stack
      *   - Each iteration is simple and JIT-friendly
      */
    def eval(cont: ContinuationJitRepr): Any = {
        var current = cont
        var stack: List[Frame] = Nil

        while true do {
            current match {
                case Return(value) =>
                    // We have a value - process next frame or return
                    stack match {
                        case Nil =>
                            // No more frames - we're done
                            return value

                        case (frame: ApplyFuncFrame) :: rest =>
                            // We evaluated the function, now evaluate the argument
                            stack = ApplyArgFrame(value) :: rest
                            current = frame.arg

                        case (frame: ApplyArgFrame) :: rest =>
                            // We have both function and argument - apply!
                            stack = rest
                            val result = frame.func.asInstanceOf[Any => Any](value)
                            // Result might be a continuation, continue evaluation
                            // Avoid creating new Return if result is already one
                            current = result match {
                                case cont: ContinuationJitRepr => cont
                                case v                         => Return(v)
                            }

                        case ForceFrame :: rest =>
                            // Force the delayed computation
                            stack = rest
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
                    stack = ApplyFuncFrame(arg) :: stack
                    current = func

                case Force(delayed) =>
                    // Push frame to force after evaluation, then evaluate
                    stack = ForceFrame :: stack
                    current = delayed
            }
        }
    }

}
