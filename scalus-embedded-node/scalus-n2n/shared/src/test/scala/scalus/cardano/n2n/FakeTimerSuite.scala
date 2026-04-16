package scalus.cardano.n2n

import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.DurationInt

class FakeTimerSuite extends AnyFunSuite {

    test("scheduled action does not fire before advance") {
        val timer = new FakeTimer
        val fired = new AtomicInteger(0)
        timer.schedule(100.millis)(fired.incrementAndGet())
        assert(fired.get == 0)
    }

    test("advance past deadline fires action") {
        val timer = new FakeTimer
        val fired = new AtomicInteger(0)
        timer.schedule(100.millis)(fired.incrementAndGet())
        timer.advance(99.millis)
        assert(fired.get == 0)
        timer.advance(1.milli)
        assert(fired.get == 1)
    }

    test("cancelled action does not fire even after advance past deadline") {
        val timer = new FakeTimer
        val fired = new AtomicInteger(0)
        val handle = timer.schedule(50.millis)(fired.incrementAndGet())
        handle.cancel()
        timer.advance(1.second)
        assert(fired.get == 0)
    }

    test("cancel after fire is a no-op") {
        val timer = new FakeTimer
        val fired = new AtomicInteger(0)
        val handle = timer.schedule(50.millis)(fired.incrementAndGet())
        timer.advance(60.millis)
        assert(fired.get == 1)
        handle.cancel() // should not blow up
        assert(fired.get == 1)
    }

    test("multiple schedules fire in deadline order on a single advance") {
        val timer = new FakeTimer
        val order = new java.util.concurrent.ConcurrentLinkedQueue[Int]
        timer.schedule(30.millis)(order.add(3))
        timer.schedule(10.millis)(order.add(1))
        timer.schedule(20.millis)(order.add(2))
        timer.advance(100.millis)
        assert(
          List(order.poll(), order.poll(), order.poll()) == List(1, 2, 3)
        )
    }

    test("fires CancelSource.cancel on schedule — integration smoke") {
        val timer = new FakeTimer
        val src = CancelSource()
        val _ = timer.schedule(50.millis)(src.cancel())
        timer.advance(49.millis)
        assert(!src.token.isCancelled)
        timer.advance(1.milli)
        assert(src.token.isCancelled)
    }

    test("pendingCount reflects scheduled and cancelled state") {
        val timer = new FakeTimer
        assert(timer.pendingCount == 0)
        val h1 = timer.schedule(10.millis)(())
        val _ = timer.schedule(20.millis)(())
        assert(timer.pendingCount == 2)
        h1.cancel()
        assert(timer.pendingCount == 1)
        timer.advance(100.millis)
        assert(timer.pendingCount == 0)
    }

    test("nested schedule from inside firing action works") {
        val timer = new FakeTimer
        val fired = new AtomicInteger(0)
        timer.schedule(10.millis) {
            fired.incrementAndGet()
            val _ = timer.schedule(10.millis)(fired.incrementAndGet())
        }
        timer.advance(10.millis)
        assert(fired.get == 1)
        assert(timer.pendingCount == 1) // nested one pending
        timer.advance(10.millis)
        assert(fired.get == 2)
    }
}
