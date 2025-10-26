package scalus.uplc.eval

class RuntimeJitContext {
    var stackDepth: Int = 0

    final def withCheckOverflow(vDelayed: => Any): Any = {
        stackDepth += 1
        if stackDepth >= RuntimeJitContext.MAX_STACK_DEPTH then CallCCTrampoline(() => vDelayed)
        else {
            val r = vDelayed
            stackDepth -= 1
            r
        }
    }

    final def trampolinedApply(fDelayed: => Any, argDelayed: => Any): Any = {
        stackDepth += 1
        if stackDepth >= RuntimeJitContext.MAX_STACK_DEPTH then
            CallCCTrampoline(() => trampolinedApply(fDelayed, argDelayed))
        else
            val f = fDelayed
            val a = argDelayed
            (f, a) match
                case (cc: CallCCTrampoline, _) =>
                    stackDepth -= 1
                    cc.map { fv =>
                        fv match
                            case fvcc: CallCCTrampoline =>
                                fvcc.map(fvFinal => fvFinal.asInstanceOf[Any => Any](a))
                            case _ =>
                                fv.asInstanceOf[Any => Any](a)
                    }
                case (_, acc: CallCCTrampoline) =>
                    stackDepth -= 1
                    acc.map { av =>
                        av match
                            case avcc: CallCCTrampoline =>
                                avcc.map(avFinal => f.asInstanceOf[Any => Any](avFinal))
                            case _ =>
                                f.asInstanceOf[Any => Any](av)
                    }
                case _ =>
                    stackDepth -= 1
                    f.asInstanceOf[Any => Any](a)
    }

}

object RuntimeJitContext {
    final val MAX_STACK_DEPTH: Int = 1000

}
