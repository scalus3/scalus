package scalus.uplc.jit

import scalus.uplc.eval.MachineError

class JitEvaluationFailure(msg: String, cause: Throwable = null) extends MachineError(msg) {
    if cause != null then initCause(cause)
}
