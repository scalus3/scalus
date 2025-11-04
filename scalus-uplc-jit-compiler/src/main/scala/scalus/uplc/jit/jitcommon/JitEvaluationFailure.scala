package scalus.uplc.jit.jitcommon

import scalus.uplc.eval.MachineError

class JitEvaluationFailure(msg: String) extends MachineError(msg)
