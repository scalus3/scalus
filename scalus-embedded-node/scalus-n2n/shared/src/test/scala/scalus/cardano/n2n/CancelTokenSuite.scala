package scalus.cardano.n2n

import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import scala.jdk.CollectionConverters.*

class CancelTokenSuite extends AnyFunSuite {

    test("fresh source is not cancelled and has no cause") {
        val src = CancelSource()
        assert(!src.token.isCancelled)
        assert(src.token.cause.isEmpty)
    }

    test("cancel sets flag and stores cause") {
        val src = CancelSource()
        val cause = new RuntimeException("boom")
        src.cancel(cause)
        assert(src.token.isCancelled)
        assert(src.token.cause.contains(cause))
    }

    test("onCancel listener fires on cancel, in registration order") {
        val src = CancelSource()
        val order = new java.util.concurrent.ConcurrentLinkedQueue[Int]
        src.token.onCancel(() => order.add(1))
        src.token.onCancel(() => order.add(2))
        src.token.onCancel(() => order.add(3))
        src.cancel()
        assert(order.asScala.toList == List(1, 2, 3))
    }

    test("onCancel after cancel fires synchronously") {
        val src = CancelSource()
        src.cancel()
        val fired = new AtomicInteger(0)
        src.token.onCancel(() => fired.incrementAndGet())
        assert(fired.get == 1) // already fired by return of onCancel
    }

    test("double cancel is idempotent — listeners fire exactly once") {
        val src = CancelSource()
        val count = new AtomicInteger(0)
        src.token.onCancel(() => count.incrementAndGet())
        src.cancel()
        src.cancel(new RuntimeException("second"))
        assert(count.get == 1)
    }

    test("listener exception does not prevent other listeners from firing") {
        val src = CancelSource()
        val order = new java.util.concurrent.ConcurrentLinkedQueue[Int]
        src.token.onCancel(() => order.add(1))
        src.token.onCancel(() => throw new RuntimeException("nope"))
        src.token.onCancel(() => order.add(3))
        src.cancel()
        assert(order.asScala.toList == List(1, 3))
    }

    test("linkedTo: parent.cancel triggers child.cancel with same cause") {
        val parent = CancelSource()
        val child = CancelSource.linkedTo(parent.token)
        val cause = new RuntimeException("parent failure")
        parent.cancel(cause)
        assert(child.token.isCancelled)
        assert(child.token.cause.contains(cause))
    }

    test("linkedTo: child.cancel does NOT cancel parent") {
        val parent = CancelSource()
        val child = CancelSource.linkedTo(parent.token)
        child.cancel()
        assert(child.token.isCancelled)
        assert(!parent.token.isCancelled)
    }

    test("linkedTo on already-cancelled parent fires child immediately") {
        val parent = CancelSource()
        parent.cancel(new RuntimeException("already"))
        val child = CancelSource.linkedTo(parent.token)
        assert(child.token.isCancelled)
        assert(child.token.cause.exists(_.getMessage == "already"))
    }

    test("onCancel returns a handle that deregisters the listener") {
        val src = CancelSource()
        val fired = new AtomicInteger(0)
        val handle = src.token.onCancel(() => fired.incrementAndGet())
        handle.cancel() // deregister before source cancels
        src.cancel()
        assert(fired.get == 0)
    }

    test("linkedTo: child.cancel deregisters from parent's listener list — no leak") {
        val parent = CancelSource()
        // Sentinel listener — proves cancel propagation still works after the cleanup path.
        val sentinelFired = new AtomicInteger(0)
        val _ = parent.token.onCancel(() => sentinelFired.incrementAndGet())

        // Create and cancel many linked children. Each child's own-cancel should
        // remove its entry from the parent's listener list.
        val impl = parent.asInstanceOf[CancelSourceImpl]
        val initial = impl.listenerCount // sentinel + any linkedTo bookkeeping
        val n = 100
        (1 to n).foreach { _ => CancelSource.linkedTo(parent.token).cancel() }

        // After n children come-and-go, parent's listener count must not have grown by n.
        assert(
          impl.listenerCount == initial,
          s"listenerCount=${impl.listenerCount} (expected $initial); indicates leaked linkedTo closures"
        )

        parent.cancel()
        assert(sentinelFired.get == 1)
    }

    test("CancelToken.never is never cancelled and onCancel does nothing") {
        val t = CancelToken.never
        val fired = new AtomicInteger(0)
        t.onCancel(() => fired.incrementAndGet())
        assert(!t.isCancelled)
        assert(t.cause.isEmpty)
        assert(fired.get == 0)
    }

    test("threadsafety: concurrent onCancel registrations race cancel — every listener fires exactly once") {
        val src = CancelSource()
        val count = new AtomicInteger(0)
        val latch = new CountDownLatch(1)
        val exec = Executors.newFixedThreadPool(8)
        val n = 500
        try {
            // Register listeners on 8 threads while a 9th thread fires cancel mid-flight.
            (1 to n).foreach { _ =>
                exec.submit(new Runnable {
                    def run(): Unit = {
                        latch.await()
                        src.token.onCancel(() => count.incrementAndGet())
                    }
                })
            }
            exec.submit(new Runnable {
                def run(): Unit = {
                    latch.await()
                    // Give registrations a moment to start, then cancel mid-race.
                    Thread.sleep(1)
                    src.cancel()
                }
            })
            latch.countDown()
            exec.shutdown()
            assert(exec.awaitTermination(10, TimeUnit.SECONDS))
            // All n listeners fire exactly once — the ones registered before cancel via the
            // slow path, the ones after via the fast path.
            assert(count.get == n)
        } finally {
            exec.shutdownNow()
        }
    }

    test("deep linkedTo chain: root.cancel fans out to every descendant") {
        val root = CancelSource()
        val levels = scala.collection.mutable.ListBuffer.empty[CancelSource]
        var parent: CancelToken = root.token
        var i = 0
        while i < 10 do {
            val child = CancelSource.linkedTo(parent)
            levels += child
            parent = child.token
            i += 1
        }
        root.cancel(new RuntimeException("root fires"))
        levels.foreach(c => assert(c.token.isCancelled))
        assert(levels.forall(_.token.cause.exists(_.getMessage == "root fires")))
    }

    test("captures cause passed to the no-arg cancel via CancelledException default") {
        val src = CancelSource()
        src.cancel()
        assert(src.token.isCancelled)
        assert(src.token.cause.exists {
            case e: CancelledException => e.getMessage == CancelSource.DefaultReason
            case _                     => false
        })
    }
}
