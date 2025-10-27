package scalus.uplc.eval

import scalus.uplc.Term
import scalus.uplc.eval.mincont.{JIT as MincontJIT}
import scalus.uplc.eval.defunc.{JIT as DefuncJIT, JITCompiler as DefuncCompiler}

/** Common interface for JIT compiler implementations to enable testing both versions. */
trait JITImplementation {
  def name: String
  def eval(term: Term, logger: Logger, budget: BudgetSpender, params: MachineParams): Any
  def isImplemented: Boolean = true
}

object JITImplementation {

  /** Minimal continuation-based JIT (original implementation). */
  object Mincont extends JITImplementation {
    def name: String = "mincont"
    def eval(term: Term, logger: Logger, budget: BudgetSpender, params: MachineParams): Any = {
      val compiled = MincontJIT.jitUplc(term)
      compiled(logger, budget, params)
    }
  }

  /** Defunctionalized JIT (new zero-allocation implementation). */
  object Defunc extends JITImplementation {
    def name: String = "defunc"
    def eval(term: Term, logger: Logger, budget: BudgetSpender, params: MachineParams): Any = {
      val program = DefuncCompiler.compile(term)
      DefuncJIT.eval(program, budget, logger, params)
    }

    // Override for features not yet implemented
    override def isImplemented: Boolean = true
  }

  /** All JIT implementations to test. */
  val all: List[JITImplementation] = List(Mincont, Defunc)

  /** Only fully implemented JIT compilers. */
  val implemented: List[JITImplementation] = all.filter(_.isImplemented)
}
