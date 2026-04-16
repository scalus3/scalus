package scalus.cardano.n2n.jvm

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.n2n.CancelSource

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.duration.DurationInt

/** Real-wall-clock smoke test for [[JvmTimer]]. Delays are tiny to keep test runtime small;
  * deterministic state-machine tests use `FakeTimer` instead.
  */
class JvmTimerSuite extends AnyFunSuite {

    test("scheduled action fires after delay") {
        val timer = new JvmTimer()
        try {
            val latch = new CountDownLatch(1)
            val _ = timer.schedule(20.millis)(latch.countDown())
            assert(latch.await(2, TimeUnit.SECONDS), "action did not fire within 2s")
        } finally timer.shutdown()
    }

    test("cancel before fire prevents action") {
        val timer = new JvmTimer()
        try {
            val fired = new AtomicInteger(0)
            val handle = timer.schedule(200.millis)(fired.incrementAndGet())
            handle.cancel()
            Thread.sleep(300) // ensure deadline has passed
            assert(fired.get == 0)
        } finally timer.shutdown()
    }

    test("integrates with CancelSource — schedule fires cancel()") {
        val timer = new JvmTimer()
        try {
            val src = CancelSource()
            val _ = timer.schedule(20.millis)(src.cancel())
            val latch = new CountDownLatch(1)
            src.token.onCancel(() => latch.countDown())
            assert(latch.await(2, TimeUnit.SECONDS), "cancel did not fire within 2s")
            assert(src.token.isCancelled)
        } finally timer.shutdown()
    }

    test("exception in action does not break subsequent scheduling") {
        val timer = new JvmTimer()
        try {
            val _ = timer.schedule(10.millis) { throw new RuntimeException("boom") }
            Thread.sleep(30)
            val latch = new CountDownLatch(1)
            val _ = timer.schedule(10.millis)(latch.countDown())
            assert(latch.await(2, TimeUnit.SECONDS), "second action did not fire")
        } finally timer.shutdown()
    }
}
