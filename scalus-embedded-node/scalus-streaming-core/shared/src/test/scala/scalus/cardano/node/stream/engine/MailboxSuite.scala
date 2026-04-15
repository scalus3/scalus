package scalus.cardano.node.stream.engine

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.infra.ScalusBufferOverflowException

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*

class MailboxSuite extends AnyFunSuite {

    private val timeout: FiniteDuration = 1.second

    test("DeltaMailbox: buffered events are pulled in FIFO order") {
        val mb = Mailbox.delta[Int](maxSize = 10)
        mb.offer(1)
        mb.offer(2)
        mb.offer(3)
        assert(Await.result(mb.pull(), timeout).contains(1))
        assert(Await.result(mb.pull(), timeout).contains(2))
        assert(Await.result(mb.pull(), timeout).contains(3))
    }

    test("DeltaMailbox: pull parks on empty mailbox and wakes on the next offer") {
        val mb = Mailbox.delta[Int]()
        val parked: Future[Option[Int]] = mb.pull()
        assert(!parked.isCompleted)
        mb.offer(42)
        assert(Await.result(parked, timeout).contains(42))
    }

    test("DeltaMailbox: close delivers None on pending pull") {
        val mb = Mailbox.delta[Int]()
        val parked = mb.pull()
        mb.close()
        assert(Await.result(parked, timeout).isEmpty)
    }

    test("DeltaMailbox: close after buffering drains the buffer first then emits None") {
        val mb = Mailbox.delta[Int]()
        mb.offer(1)
        mb.offer(2)
        mb.close()
        assert(Await.result(mb.pull(), timeout).contains(1))
        assert(Await.result(mb.pull(), timeout).contains(2))
        assert(Await.result(mb.pull(), timeout).isEmpty)
    }

    test("DeltaMailbox: fail delivers a failed Future") {
        val mb = Mailbox.delta[Int]()
        val parked = mb.pull()
        val boom = new RuntimeException("boom")
        mb.fail(boom)
        val res = Await.ready(parked, timeout)
        assert(res.value.get.failed.get eq boom)
    }

    test(
      "DeltaMailbox: bounded overflow terminates with ScalusBufferOverflowException after draining"
    ) {
        val mb = Mailbox.delta[Int](maxSize = 2)
        mb.offer(1)
        mb.offer(2)
        mb.offer(3) // overflow → terminates
        assert(Await.result(mb.pull(), timeout).contains(1))
        assert(Await.result(mb.pull(), timeout).contains(2))
        val failed = Await.ready(mb.pull(), timeout)
        assert(failed.value.get.failed.get.isInstanceOf[ScalusBufferOverflowException])
    }

    test("DeltaMailbox: cancel is idempotent and fires onCancel once") {
        var count = 0
        val mb = Mailbox.delta[Int](onCancel = () => count += 1)
        mb.cancel()
        mb.cancel()
        assert(count == 1)
    }

    test("LatestValueMailbox: newer offer overwrites unread value") {
        val mb = Mailbox.latestValue[Int]()
        mb.offer(1)
        mb.offer(2)
        mb.offer(3)
        assert(Await.result(mb.pull(), timeout).contains(3))
        // Nothing buffered after the pull — next pull parks.
        val parked = mb.pull()
        assert(!parked.isCompleted)
    }

    test("LatestValueMailbox: parked pull wakes on next offer") {
        val mb = Mailbox.latestValue[Int]()
        val parked = mb.pull()
        mb.offer(10)
        assert(Await.result(parked, timeout).contains(10))
    }

    test("LatestValueMailbox: close after buffered value drains then emits None") {
        val mb = Mailbox.latestValue[Int]()
        mb.offer(5)
        mb.close()
        assert(Await.result(mb.pull(), timeout).contains(5))
        assert(Await.result(mb.pull(), timeout).isEmpty)
    }

    test("LatestValueMailbox: fail delivers failed Future") {
        val mb = Mailbox.latestValue[Int]()
        val parked = mb.pull()
        val boom = new RuntimeException("boom")
        mb.fail(boom)
        val res = Await.ready(parked, timeout)
        assert(res.value.get.failed.get eq boom)
    }
}
