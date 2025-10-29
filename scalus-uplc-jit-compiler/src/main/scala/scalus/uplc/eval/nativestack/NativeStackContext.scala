package scalus.uplc.eval.nativestack

class StackTresholdException extends RuntimeException("max-stack-depth", null, false, false) {}

class NativeStackContext {

    var stackDepth: Long = 0
    var throwOnOverlow = true

    def incr(): Unit = {
        stackDepth = stackDepth + 1
        if throwOnOverlow && stackDepth >= NativeStackContext.MAX_STACK_DEPTH then
            throw StackTresholdException()
    }

    def decr(): Unit = {
        stackDepth = stackDepth - 1
    }

}

object NativeStackContext {

    val MAX_STACK_DEPTH = 1000

}
