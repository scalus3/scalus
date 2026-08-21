package scalus.cardano.node.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.infra.ScalusBufferOverflowException
import scalus.cardano.node.stream.internal.Mailbox

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/** The buffering semantics adapters rely on and therefore do not reimplement. */
class MailboxTest extends AnyFunSuite {

    private given ExecutionContext = ExecutionContext.parasitic

    private def completed[A](m: Mailbox[A]): Option[Option[A]] = m.pull().value.map {
        case Success(v) => v
        case Failure(e) => throw e
    }

    test("delta mailbox preserves order and delivers buffered values") {
        val m = Mailbox.delta[Int]()
        m.offer(1)
        m.offer(2)
        assert(completed(m).contains(Some(1)))
        assert(completed(m).contains(Some(2)))
        assert(completed(m).isEmpty, "no value buffered, so the pull stays outstanding")
    }

    test("a value offered while a pull is outstanding completes that pull") {
        val m = Mailbox.delta[Int]()
        val pending = m.pull()
        assert(pending.value.isEmpty)
        m.offer(7)
        assert(pending.value.map(_.get).contains(Some(7)))
    }

    test("pulling twice while outstanding yields the same future, not a lost promise") {
        val m = Mailbox.delta[Int]()
        val first = m.pull()
        val second = m.pull()
        m.offer(3)
        assert(first.value.map(_.get).contains(Some(3)))
        assert(second.value.map(_.get).contains(Some(3)))
    }

    test("a bounded delta mailbox fails rather than dropping events") {
        val m = Mailbox.delta[Int](maxSize = 2)
        m.offer(1)
        m.offer(2)
        m.offer(3)
        val outcome = m.pull().value
        assert(outcome.exists(_.isFailure), "overflow must terminate the subscription")
        outcome.foreach {
            case Failure(e) => assert(e.isInstanceOf[ScalusBufferOverflowException])
            case Success(v) => fail(s"expected failure, got $v")
        }
    }

    test("a latest-value mailbox keeps only the newest value") {
        val m = Mailbox.latestValue[String]()
        m.offer("stale")
        m.offer("staler")
        m.offer("current")
        assert(completed(m).contains(Some("current")))
        assert(completed(m).isEmpty, "coalesced values must not reappear")
    }

    test("close ends the stream and completes an outstanding pull") {
        val m = Mailbox.delta[Int]()
        val pending = m.pull()
        m.close()
        assert(pending.value.map(_.get).contains(None))
        assert(completed(m).contains(None), "a closed mailbox keeps reporting end of stream")
    }

    test("close after buffering still delivers what was buffered") {
        val m = Mailbox.delta[Int]()
        m.offer(1)
        m.close()
        assert(completed(m).contains(Some(1)))
        assert(completed(m).contains(None))
    }

    test("close unregisters the subscription") {
        var cancels = 0
        val m = Mailbox.delta[Int](onCancel = () => cancels += 1)
        m.close()
        assert(
          cancels == 1,
          "a subscription that ends cleanly must still stop costing the provider"
        )
    }

    test("a producer failure unregisters the subscription") {
        var cancels = 0
        val m = Mailbox.delta[Int](onCancel = () => cancels += 1)
        m.fail(new RuntimeException("boom"))
        assert(cancels == 1)
    }

    test("a buffer overflow unregisters the subscription") {
        var cancels = 0
        val m = Mailbox.delta[Int](maxSize = 1, onCancel = () => cancels += 1)
        m.offer(1)
        m.offer(2)
        assert(
          cancels == 1,
          "an overflowed subscription is dead; leaving it registered means matching every " +
              "subsequent block against a consumer that can never be told the result"
        )
    }

    test("offerBuffered never completes an outstanding waiter; flush does") {
        val m = Mailbox.delta[Int]()
        val pending = m.pull()
        m.offerBuffered(1)
        // This is the property the hub depends on: a producer can enqueue while holding its own
        // lock without the consumer's continuation running inside that lock.
        assert(pending.value.isEmpty, "offerBuffered must not complete a waiting pull")
        m.flush()
        assert(pending.value.map(_.get).contains(Some(1)))
    }

    test("cancel fires its hook once, drops buffered events and ends the stream") {
        var cancels = 0
        val m = Mailbox.delta[Int](onCancel = () => cancels += 1)
        m.offer(1)
        m.cancel()
        m.cancel()
        assert(cancels == 1, "cancellation must be idempotent")
        assert(completed(m).contains(None))
        m.offer(2)
        assert(completed(m).contains(None), "a cancelled mailbox accepts nothing further")
    }
}
