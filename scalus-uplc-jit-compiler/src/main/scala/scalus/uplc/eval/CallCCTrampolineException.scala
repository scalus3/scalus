package scalus.uplc.eval

final class CallCCTrampolineException(val m: String, val cont: () => Any)
    extends RuntimeException(m, null, false, false) {

    def map(f: (Any) => Any): CallCCTrampolineException = {
        new CallCCTrampolineException(
          this.getMessage,
          () =>
              val c =
                  try {
                      cont()
                  } catch {
                      case ex: CallCCTrampolineException =>
                          throw ex.map(f)
                  }
              f(c)
        )
    }

}

object CallCCTrampolineException {

    def eval(e: () => Any, ctx: RuntimeJitContext): Any = {
        var done = false
        var c = e
        var r: Any = null
        var trampolineCount = 0
        while !done do {
            try {
                r = c.apply()
                done = true
            } catch {
                case ex: CallCCTrampolineException =>
                    trampolineCount += 1
                    c = ex.cont
                    ctx.stackDepth = 0
            }
        }
        r
    }

}
