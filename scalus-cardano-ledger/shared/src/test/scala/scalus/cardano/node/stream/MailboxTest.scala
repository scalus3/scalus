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
