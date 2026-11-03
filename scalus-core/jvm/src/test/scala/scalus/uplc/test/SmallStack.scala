package scalus.uplc.test

import org.scalatest.Assertions.fail

import java.util.concurrent.atomic.AtomicReference

/** Runs code on a thread whose stack is far too small for one stack frame per nesting level of
  * deeply nested data, to show that the code does not recurse over the nesting.
  */
object SmallStack {

    /** Runs `body` on a daemon thread with a 256 KiB stack and returns its result.
      *
      * Fails the test when `body` throws, including a `StackOverflowError`, which is fatal to
      * ScalaTest, and when it does not finish within 60 s: a regression of a loop is more likely to
      * loop forever than to fail, and a thread stuck in a loop must not keep the test JVM alive.
      */
    def run[A](body: => A): A = onStack(256L * 1024)(body)

    /** Runs `body` as [[run]] does, on a daemon thread with a stack of `stackSize` bytes, or of the
      * JVM's default size where `stackSize` is 0, and returns its result.
      */
    def onStack[A](stackSize: Long)(body: => A): A = {
        val stack =
            if stackSize == 0 then "the default stack" else s"a ${stackSize / 1024} KiB stack"
        val result = new AtomicReference[Either[Throwable, A]]()
        val thread = new Thread(
          null,
          () =>
              result.set(
                try Right(body)
                catch { case e: Throwable => Left(e) }
              ),
          "small-stack",
          stackSize
        )
        thread.setDaemon(true)
        thread.start()
        thread.join(60000)
        if thread.isAlive then fail(s"did not finish within 60 s on $stack")
        result.get().fold(e => fail(s"failed on $stack: $e"), identity)
    }
}
