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

    final def eval(e: () => Any, ctx: RuntimeJitContext): Any = {
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

    final def runApply(fun: () => Any => Any, arg: () => Any): Any = {
        val f =
            try {
                val result = fun.apply()
                result
            } catch
                case ecc: CallCCTrampolineException =>
                    throw ecc.map { funResult =>
                        val argValue =
                            try {
                                val argRes = arg.apply()
                                argRes
                            } catch {
                                case ecc1: CallCCTrampolineException =>
                                    // We need to throw an exception that will:
                                    // 1. Evaluate arg's continuation to get argResult
                                    // 2. Apply funResult to argResult
                                    // We do this by mapping arg's exception with the application
                                    throw ecc1.map(argResult => {
                                        val applied = funResult.asInstanceOf[Any => Any](argResult)
                                        applied
                                    })
                            }
                        val applied = funResult.asInstanceOf[Any => Any](argValue)
                        applied
                    }
        val a =
            try {
                val result = arg.apply()
                result
            } catch
                case ecc: CallCCTrampolineException =>
                    throw ecc.map(argResult => {
                        val applied = f(argResult)
                        applied
                    })
        val result = f(a)
        result
    }

}
