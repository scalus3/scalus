package scalus.uplc.jit.nativestack

/** Thrown by JIT-compiled code when its call depth reaches its limit (`maxDepth`).
  * `Compiled.withFallback` then evaluates the term with the CEK machine. No stack trace: it is
  * control flow, and a deep stack is expensive to fill in.
  */
class StackThresholdException extends RuntimeException("max-stack-depth", null, false, false)

/** The call-depth counter of one evaluation of JIT-compiled code.
  *
  * Generated code counts the UPLC applications it is inside of (`incr` and `decr` around each call,
  * see `nativestack.JIT.genCode`) and throws [[StackThresholdException]] at the limit, while the
  * JVM stack still has room. It never catches `StackOverflowError`: one thrown during class
  * initialization marks the class erroneous, and one thrown while an `invokedynamic` call site
  * links breaks that site for good. The limit assumes a thread stack of 64 MB (`-Xss64m`), as the
  * tests and the bench use; one counted level takes a few JVM frames.
  */
final class NativeStackContext(maxDepth: Int) {

    private var stackDepth: Int = 0

    def incr(): Unit = {
        stackDepth += 1
        if stackDepth >= maxDepth then throw StackThresholdException()
    }

    def decr(): Unit = stackDepth -= 1
}

object NativeStackContext {

    /** A call-depth limit that a 64 MB thread stack (`-Xss64m`) holds for the corpus scripts, as
      * the tests and the bench use.
      */
    final val DefaultMaxDepth = 25000
}
