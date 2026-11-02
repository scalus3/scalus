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
    def run[A](body: => A): A = {
        val result = new AtomicReference[Either[Throwable, A]]()
        val thread = new Thread(
          null,
          () =>
              result.set(
                try Right(body)
                catch { case e: Throwable => Left(e) }
              ),
          "small-stack",
          256L * 1024
        )
        thread.setDaemon(true)
        thread.start()
        thread.join(60000)
        if thread.isAlive then fail("did not finish within 60 s on a 256 KiB stack")
        result.get().fold(e => fail(s"failed on a 256 KiB stack: $e"), identity)
    }
}
