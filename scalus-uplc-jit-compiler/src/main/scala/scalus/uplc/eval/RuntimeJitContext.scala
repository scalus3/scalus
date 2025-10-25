package scalus.uplc.eval

class RuntimeJitContext {
    var stackDepth: Int = 0
}

object RuntimeJitContext {
    final val MAX_STACK_DEPTH: Int = 500
}
