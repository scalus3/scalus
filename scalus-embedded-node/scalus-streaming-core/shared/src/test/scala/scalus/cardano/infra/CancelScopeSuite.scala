package scalus.cardano.infra

import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.DurationInt

/** Covers [[CancelScope]] — the ergonomic bundle of [[CancelSource]] + [[Timer]] used by
  * mini-protocol drivers. Tests drive the timer through [[FakeTimer]] for determinism.
  */
class CancelScopeSuite extends AnyFunSuite {

    test("linked() creates a child source linked to the scope's token") {
        val parent = CancelSource()
        val cs = new CancelScope(parent, new FakeTimer())
        val child = cs.linked()
        assert(!child.token.isCancelled)
        parent.cancel(new RuntimeException("parent gone"))
        assert(child.token.isCancelled)
        assert(child.token.cause.exists(_.getMessage == "parent gone"))
    }

    test("linked() child cancel does not propagate up to the enclosing scope") {
        val parent = CancelSource()
        val cs = new CancelScope(parent, new FakeTimer())
        cs.linked().cancel(new RuntimeException("per-call"))
        assert(!parent.token.isCancelled)
    }

    test("schedule fires target.cancel with the provided cause on deadline") {
        val parent = CancelSource()
        val timer = new FakeTimer()
        val cs = new CancelScope(parent, timer)
        val target = cs.linked()
        val cause = new RuntimeException("deadline")
        val _ = cs.schedule(500.millis, target, cause)
        timer.advance(499.millis)
        assert(!target.token.isCancelled)
        timer.advance(1.milli)
        assert(target.token.isCancelled)
        assert(target.token.cause.contains(cause))
    }

    test("schedule handle.cancel() before deadline prevents firing") {
        val parent = CancelSource()
        val timer = new FakeTimer()
        val cs = new CancelScope(parent, timer)
        val target = cs.linked()
        val handle = cs.schedule(500.millis, target, new RuntimeException("deadline"))
        handle.cancel()
        timer.advance(1.second)
        assert(!target.token.isCancelled)
    }

    test("linkedTo factory constructs a scope linked to the supplied parent") {
        val root = CancelSource()
        val timer = new FakeTimer()
        val cs = CancelScope.linkedTo(root.token, timer)
        val fired = new AtomicInteger(0)
        cs.token.onCancel(() => { val _ = fired.incrementAndGet() })
        root.cancel()
        assert(fired.get == 1)
    }
}
