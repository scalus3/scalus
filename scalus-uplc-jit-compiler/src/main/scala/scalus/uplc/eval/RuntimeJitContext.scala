package scalus.uplc.eval

class RuntimeJitContext {
    var stackDepth: Int = 0

    final def withCheckOverflow(vDelayed: => Any): Any = {
        stackDepth += 1
        if stackDepth >= RuntimeJitContext.MAX_STACK_DEPTH then
            throw CallCCTrampolineException("lambda", () => vDelayed)
        else {
            val r = vDelayed
            stackDepth -= 1
            r
        }
    }

    final def trampolinedApply(fDelayed: => Any => Any, argDelayed: => Any): Any = {
        stackDepth += 1
        val f =
            try fDelayed
            catch
                case ecc1: CallCCTrampolineException =>
                    throw ecc1.map { fResult =>
                        val a =
                            try {
                                argDelayed
                            } catch {
                                case ecc2: CallCCTrampolineException =>
                                    throw ecc2.map(argResult =>
                                        fResult.asInstanceOf[Any => Any](argResult)
                                    )
                            }
                        fResult.asInstanceOf[Any => Any](a)
                    }
        val a =
            try argDelayed
            catch
                case ecc: CallCCTrampolineException =>
                    throw ecc.map(argResult => f(argResult))
        val r = f.asInstanceOf[Any => Any](a)
        stackDepth -= 1
        r
    }

}

object RuntimeJitContext {
    final val MAX_STACK_DEPTH: Int = 500

}
