package scalus.cardano.node.stream.engine

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.stream.engine.kvstore.InMemoryKvStore
import scalus.cardano.node.{UtxoQuery, UtxoSource}

import EngineTestFixtures.*

class KvChainStoreUtxoSuite extends AnyFunSuite {

    private def newStore(): KvChainStore = new KvChainStore(InMemoryKvStore())

    test("appendBlock adds created outputs to the UTxO set") {
        val store = newStore()
        val blk = block(1, tx(100, producing = IndexedSeq(output(addressA, 10L))))
        store.appendBlock(blk)

        val q = UtxoQuery(UtxoSource.FromAddress(addressA))
        val utxos = store.findUtxosFromStore(q).getOrElse(fail("expected Some"))
        assert(utxos.size == 1)
        val (in, out) = utxos.head
        assert(in == input(100, 0))
        assert(out.address == addressA)
    }

    test("appendBlock removes spent inputs from the UTxO set") {
        val store = newStore()
        val b1 = block(1, tx(100, producing = IndexedSeq(output(addressA, 10L))))
        val b2 = block(2, tx(200, spending = Set(input(100, 0))))
        store.appendBlock(b1)
        store.appendBlock(b2)

        val q = UtxoQuery(UtxoSource.FromAddress(addressA))
        val utxos = store.findUtxosFromStore(q).getOrElse(fail("expected Some"))
        assert(utxos.isEmpty)
    }

    test("rollbackTo inverts per-block UTxO deltas (restores spent outputs)") {
        val store = newStore()
        val b1 = block(1, tx(100, producing = IndexedSeq(output(addressA, 10L))))
        val b2 = block(2, tx(200, spending = Set(input(100, 0))))
        store.appendBlock(b1)
        store.appendBlock(b2)

        store.rollbackTo(point(1))
        val q = UtxoQuery(UtxoSource.FromAddress(addressA))
        val utxos = store.findUtxosFromStore(q).getOrElse(fail("expected Some"))
        assert(utxos.size == 1, s"expected utxo to be restored after rollback, got $utxos")
        assert(utxos.head._1 == input(100, 0))
    }

    test("rollbackTo past a creating block removes the UTxO entirely") {
        val store = newStore()
        val b1 = block(1, tx(100, producing = IndexedSeq(output(addressA, 10L))))
        store.appendBlock(b1)
        store.rollbackTo(scalus.cardano.node.stream.ChainPoint.origin)
        val utxos =
            store.findUtxosFromStore(UtxoQuery(UtxoSource.FromAddress(addressA))).getOrElse(fail())
        assert(utxos.isEmpty)
    }

    test("restoreUtxoSet bulk-replaces the UTxO set") {
        val store = newStore()
        val tipT = tip(100L, blockNo = 100L)
        val entries = Iterator(
          input(1000L, 0) -> output(addressA, 10L),
          input(1000L, 1) -> output(addressB, 20L),
          input(2000L, 0) -> output(addressA, 30L)
        )
        store.restoreUtxoSet(tipT, entries)

        assert(store.tip.contains(tipT))
        val byA = store
            .findUtxosFromStore(UtxoQuery(UtxoSource.FromAddress(addressA)))
            .getOrElse(fail())
        assert(byA.size == 2)
        val byB = store
            .findUtxosFromStore(UtxoQuery(UtxoSource.FromAddress(addressB)))
            .getOrElse(fail())
        assert(byB.size == 1)
    }

    test("findUtxosFromStore honours UtxoSource.FromTxHash (TxOuts key)") {
        val store = newStore()
        val blk = block(
          1,
          tx(
            idN = 777,
            producing = IndexedSeq(output(addressA, 5L), output(addressB, 7L))
          )
        )
        store.appendBlock(blk)

        val q = UtxoQuery(UtxoSource.FromTransaction(txHash(777L)))
        val utxos = store.findUtxosFromStore(q).getOrElse(fail())
        assert(utxos.size == 2)
    }

    test("findUtxosFromStore honours UtxoSource.FromInputs (primary-keyspace lookup)") {
        val store = newStore()
        val blk = block(1, tx(100, producing = IndexedSeq(output(addressA, 10L))))
        store.appendBlock(blk)

        val q = UtxoQuery(UtxoSource.FromInputs(Set(input(100, 0))))
        val utxos = store.findUtxosFromStore(q).getOrElse(fail())
        assert(utxos.size == 1)

        val empty = store.findUtxosFromStore(
          UtxoQuery(UtxoSource.FromInputs(Set(input(999, 0))))
        )
        assert(empty.getOrElse(fail()).isEmpty)
    }

    test("multiple appendBlock + rollback cycles are consistent") {
        val store = newStore()
        (1L to 5L).foreach { n =>
            store.appendBlock(
              block(n, tx(idN = 100 + n, producing = IndexedSeq(output(addressA, n))))
            )
        }
        // Spend one from block 2 in block 6, then roll back past block 6.
        store.appendBlock(block(6, tx(600, spending = Set(input(102, 0)))))
        val beforeRollback =
            store.findUtxosFromStore(UtxoQuery(UtxoSource.FromAddress(addressA))).getOrElse(fail())
        assert(beforeRollback.size == 4)

        store.rollbackTo(point(5))
        val afterRollback =
            store.findUtxosFromStore(UtxoQuery(UtxoSource.FromAddress(addressA))).getOrElse(fail())
        assert(afterRollback.size == 5)
    }
}
