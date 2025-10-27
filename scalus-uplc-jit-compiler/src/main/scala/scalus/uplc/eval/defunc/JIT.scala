package scalus.uplc.eval.defunc

import scalus.uplc.eval.{BudgetSpender, Logger, MachineParams}
import scalus.uplc.eval.defunc.CompiledProgram

/** Hybrid JIT compiler using defunctionalized control flow + JIT snippets for operations.
  *
  * Architecture:
  *   - Control flow (Apply, Force, Return): Defunctionalized with opcodes + mutable arrays
  *   - Operations (builtins, constants): JIT-compiled snippets (direct bytecode execution)
  *
  * This gives us:
  *   - Zero allocation overhead from continuations/frames
  *   - Direct bytecode execution for expensive operations
  *   - Best of both worlds: ~4-5x speedup
  */
object JIT {

  // ============================================
  // Control Flow Opcodes (Defunctionalized)
  // ============================================
  // These are package-private so the compiler can access them

  private[eval] val OP_RETURN = 0        // Return with value in accumulator
  private[eval] val OP_APPLY = 1         // Apply function to argument
  private[eval] val OP_FORCE = 2         // Force a delayed computation
  private[eval] val OP_EXEC_SNIPPET = 3  // Execute a JIT-compiled snippet
  private[eval] val OP_LAMBDA = 4        // Create a closure (data = body instruction index)
  private[eval] val OP_CONSTR = 5        // Build constructor tuple (tag, args)
  private[eval] val OP_CASE = 6          // Pattern match on constructor

  // Frame types for continuation stack
  private[eval] val FRAME_DONE = 0           // Top level - evaluation complete
  private[eval] val FRAME_APPLY_ARG = 1      // Have function, need to evaluate argument
  private[eval] val FRAME_APPLY_EXEC = 2     // Have function and argument, execute application
  private[eval] val FRAME_FORCE = 3          // Force delayed computation
  private[eval] val FRAME_RESTORE_ENV = 4    // Restore environment after closure evaluation
  private[eval] val FRAME_CONSTR_ARG = 5     // Evaluating constructor arguments
  private[eval] val FRAME_CASE_APPLY = 6     // Applying case function to arguments
  private[eval] val FRAME_CASE_APPLY_REST = 7 // Apply remaining args after first application

  // ============================================
  // External API (Functional - IFO Pattern)
  // ============================================

  /** Evaluate a compiled program (pure functional interface).
    *
    * @param program The compiled program
    * @param budget Budget spender
    * @param logger Logger
    * @param params Machine parameters
    * @return Evaluation result
    */
  def eval(
      program: CompiledProgram,
      budget: BudgetSpender,
      logger: Logger,
      params: MachineParams
  ): Any = {
    val ctx = new EvalContext(program, budget, logger, params)
    ctx.run()
  }
}
