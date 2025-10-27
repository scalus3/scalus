package scalus.uplc.eval.defunc

import scalus.uplc.eval.{BudgetSpender, Logger, MachineParams}

/** A JIT-compiled snippet of code.
  *
  * Snippets are generated at compile-time using Scala 3 staging. They execute directly without
  * going through the opcode switch.
  */
trait Snippet {

    def execute(
        acc: Any,
        dataStack: DataStack,
        budget: BudgetSpender,
        logger: Logger,
        params: MachineParams
    ): Any
}

/** A closure captures the environment and a reference to the lambda body.
  *
  * When applied, it extends the environment with the argument and evaluates the body.
  * 
  * @param bodyInstrIdx The instruction index where the lambda body starts
  * @param env The captured environment (values on the data stack at closure creation time)
  */
case class Closure(
    bodyInstrIdx: Int,
    env: Array[Any]  // Snapshot of the environment
)

/** A single instruction in the compiled program.
  *
  * @param opcode
  *   Control flow operation (OP_RETURN, OP_APPLY, etc.)
  * @param snippet
  *   Optional JIT-compiled snippet for direct execution
  * @param data
  *   Optional data payload (for constants, etc.)
  * @param jumpTarget
  *   For control flow: where to jump next
  */
case class Instruction(
    opcode: Int,
    snippet: Snippet | Null = null,
    data: Any = null,
    jumpTarget: Int = -1
)

/** A compiled UPLC program ready for execution. */
case class CompiledProgram(
    instructions: Array[Instruction],
    entryPoint: Int = 0
)
