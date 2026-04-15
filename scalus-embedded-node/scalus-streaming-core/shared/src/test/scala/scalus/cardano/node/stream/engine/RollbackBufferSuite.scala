package scalus.cardano.node.stream.engine

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.stream.ChainPoint

import EngineTestFixtures.*

class RollbackBufferSuite extends AnyFunSuite {

    test("applyForward tracks the tip and volatile tail until eviction") {
        val buffer = new RollbackBuffer(securityParam = 3)
        val b1 = block(1)
        val b2 = block(2)
        val b3 = block(3)

        assert(buffer.applyForward(b1).isEmpty)
        assert(buffer.applyForward(b2).isEmpty)
        assert(buffer.applyForward(b3).isEmpty)

        assert(buffer.tip.contains(tip(3, 3)))
        assert(buffer.size == 3)
        assert(buffer.volatileTail.map(_.point) == Seq(point(1), point(2), point(3)))
    }

    test("applyForward evicts the oldest block once securityParam is exceeded") {
        val buffer = new RollbackBuffer(securityParam = 2)
        buffer.applyForward(block(1))
        buffer.applyForward(block(2))
        val evicted = buffer.applyForward(block(3))

        assert(evicted.map(_.point) == Seq(point(1)))
        assert(buffer.volatileTail.map(_.point) == Seq(point(2), point(3)))
    }

    test("rollbackTo reverts down to the named point and returns the reverted blocks") {
        val buffer = new RollbackBuffer(securityParam = 5)
        buffer.applyForward(block(1))
        buffer.applyForward(block(2))
        buffer.applyForward(block(3))
        buffer.applyForward(block(4))

        buffer.rollbackTo(point(2)) match {
            case RollbackBuffer.RollbackOutcome.Reverted(reverted) =>
                assert(reverted.map(_.point) == Seq(point(4), point(3)))
            case other => fail(s"expected Reverted, got $other")
        }
        assert(buffer.tip.contains(tip(2, 2)))
        assert(buffer.volatileTail.map(_.point) == Seq(point(1), point(2)))
    }

    test("rollbackTo past the immutable tip yields PastHorizon and empties the buffer") {
        val buffer = new RollbackBuffer(securityParam = 5)
        buffer.applyForward(block(5))
        buffer.applyForward(block(6))

        buffer.rollbackTo(point(1)) match {
            case RollbackBuffer.RollbackOutcome.PastHorizon(drained) =>
                assert(drained.size == 2)
            case other => fail(s"expected PastHorizon, got $other")
        }
        assert(buffer.isEmpty)
        assert(buffer.tip.isEmpty)
    }

    test("rollbackTo to origin drains the full buffer cleanly") {
        val buffer = new RollbackBuffer(securityParam = 5)
        buffer.applyForward(block(1))
        buffer.applyForward(block(2))

        buffer.rollbackTo(ChainPoint.origin) match {
            case RollbackBuffer.RollbackOutcome.Reverted(reverted) =>
                assert(reverted.size == 2)
            case other => fail(s"expected Reverted(origin), got $other")
        }
    }
}
