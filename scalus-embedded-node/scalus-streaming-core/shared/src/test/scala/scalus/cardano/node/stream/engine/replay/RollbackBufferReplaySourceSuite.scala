package scalus.cardano.node.stream.engine.replay

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.stream.ChainPoint
import scalus.cardano.node.stream.engine.AppliedBlock
import scalus.cardano.node.stream.engine.EngineTestFixtures.*

class RollbackBufferReplaySourceSuite extends AnyFunSuite {

    private val snapshot: Seq[AppliedBlock] = (1L to 5L).map(block(_)).toSeq

    test("iterate(from, to) emits blocks strictly after `from` up to and including `to`") {
        val src = new RollbackBufferReplaySource(snapshot)
        val pts = src.iterate(point(2), point(4)).map(_.map(_.point).toList)
        assert(pts == Right(List(point(3), point(4))))
    }

    test("iterate with from == to produces an empty iterator") {
        val src = new RollbackBufferReplaySource(snapshot)
        assert(src.iterate(point(3), point(3)).exists(_.isEmpty))
    }

    test("iterate with from == origin emits all blocks up to `to`") {
        val src = new RollbackBufferReplaySource(snapshot)
        val pts = src.iterate(ChainPoint.origin, point(3)).map(_.map(_.point).toList)
        assert(pts == Right(List(point(1), point(2), point(3))))
    }

    test("iterate with from not in snapshot returns Left(ReplaySourceExhausted)") {
        val src = new RollbackBufferReplaySource(snapshot)
        val res = src.iterate(point(999), point(5))
        assert(res.left.exists(_.point == point(999)))
    }

    test("iterate across the whole snapshot") {
        val src = new RollbackBufferReplaySource(snapshot)
        val pts = src.iterate(point(1), point(5)).map(_.map(_.point).toList)
        assert(pts == Right(List(point(2), point(3), point(4), point(5))))
    }
}
