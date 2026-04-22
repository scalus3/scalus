package scalus.cardano.node.stream.engine

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.stream.ChainPoint
import scalus.cardano.node.stream.engine.kvstore.InMemoryKvStore
import scalus.cardano.node.stream.engine.replay.ReplayError

import EngineTestFixtures.*

class KvChainStoreSuite extends AnyFunSuite {

    private def newStore(): KvChainStore = new KvChainStore(InMemoryKvStore())

    test("empty store has no tip and blocksBetween(origin, origin) is empty") {
        val store = newStore()
        assert(store.tip.isEmpty)
        assert(store.blocksBetween(ChainPoint.origin, ChainPoint.origin).toOption.get.isEmpty)
    }

    test("append + blocksBetween round-trips a single block") {
        val store = newStore()
        val blk = block(1, tx(100, producing = IndexedSeq(output(addressA, 10L))))
        store.appendBlock(blk)

        assert(store.tip.contains(blk.tip))
        val out = store.blocksBetween(ChainPoint.origin, blk.point).toOption.get.toList
        assert(out.map(_.point) == Seq(blk.point))
        assert(out.head.transactions == blk.transactions)
    }

    test("blocksBetween honours exclusive-lower, inclusive-upper") {
        val store = newStore()
        val b1 = block(1, tx(100))
        val b2 = block(2, tx(200))
        val b3 = block(3, tx(300))
        Seq(b1, b2, b3).foreach(store.appendBlock)

        val out = store.blocksBetween(b1.point, b3.point).toOption.get.toList.map(_.point)
        assert(out == Seq(b2.point, b3.point))
    }

    test("blocksBetween from origin returns everything up to `to`") {
        val store = newStore()
        val blocks = (1L to 3L).map(n => block(n, tx(100 + n)))
        blocks.foreach(store.appendBlock)

        val out = store.blocksBetween(ChainPoint.origin, blocks.last.point).toOption.get.toList
        assert(out.map(_.point) == blocks.map(_.point))
    }

    test("blocksBetween against uncovered checkpoint returns ReplaySourceExhausted") {
        val store = newStore()
        store.appendBlock(block(5, tx(500)))
        store.appendBlock(block(6, tx(600)))
        val err = store.blocksBetween(point(2), point(6))
        assert(err == Left(ReplayError.ReplaySourceExhausted(point(2))))
    }

    test("rollbackTo trims blocks past `to` and rewrites the tip") {
        val store = newStore()
        (1L to 5L).foreach(n => store.appendBlock(block(n, tx(100 + n))))
        store.rollbackTo(point(3))

        assert(store.tip.map(_.point).contains(point(3)))
        val out = store.blocksBetween(ChainPoint.origin, point(5)).toOption.get.toList
        assert(out.map(_.point) == Seq(point(1), point(2), point(3)))
    }

    test("rollbackTo(origin) empties the store") {
        val store = newStore()
        (1L to 3L).foreach(n => store.appendBlock(block(n, tx(100 + n))))
        store.rollbackTo(ChainPoint.origin)

        assert(store.tip.isEmpty)
        assert(store.blocksBetween(ChainPoint.origin, point(5)).toOption.get.isEmpty)
    }

    test("append is idempotent for same (slot, hash)") {
        val store = newStore()
        val b = block(1, tx(100, producing = IndexedSeq(output(addressA, 10L))))
        store.appendBlock(b)
        store.appendBlock(b)
        val out = store.blocksBetween(ChainPoint.origin, b.point).toOption.get.toList
        assert(out.size == 1)
    }
}
