package scalus.uplc.eval

final class CallCCTrampoline(val cont: () => Any) {

    def map(f: (Any) => Any): CallCCTrampoline = {
        new CallCCTrampoline(() =>
            cont() match
                case cc: CallCCTrampoline =>
                    cc.map(f)
                case c =>
                    f(c) match
                        case fcc: CallCCTrampoline => fcc
                        case v                     => v
        )
    }

}

object CallCCTrampoline {

    final def eval(e: () => Any, ctx: RuntimeJitContext): Any = {
        var done = false
        var c = e
        var r: Any = null
        var trampolineCount = 0
        while !done do {
            r = c.apply()
            trampolineCount += 1
            r match
                case cc: CallCCTrampoline =>
                    c = cc.cont
                    ctx.stackDepth = 0
                case _ =>
                    done = true
        }
        r
    }

    final def evalOrMap(e: => Any, f: Any => Any): Any = {
        e match
            case cc: CallCCTrampoline =>
                cc.map(f)
            case _ => f(e)
    }

}
