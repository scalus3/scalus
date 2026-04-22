package scalus.cardano.node.stream.engine.kvstore.rocksdb

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.stream.ChainPoint
import scalus.cardano.node.stream.engine.{EngineTestFixtures, KvChainStore}

import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

import EngineTestFixtures.*

/** End-to-end: open a real RocksDB, populate a [[KvChainStore]] across an open → close → reopen
  * cycle, assert tip and range scans are correct on the reopened store.
  */
class RocksDbChainStoreSuite extends AnyFunSuite {

    private def withTempDir[A](body: Path => A): A = {
        val dir = Files.createTempDirectory("scalus-rocksdb-chainstore-")
        try body(dir)
        finally deleteRecursive(dir.toFile)
    }

    private def deleteRecursive(f: java.io.File): Unit = {
        if f.isDirectory then {
            val children = f.listFiles()
            if children != null then children.foreach(deleteRecursive)
        }
        val _ = f.delete()
    }

    test("blocks + tip survive close and reopen") {
        withTempDir { dir =>
            val kv1 = RocksDbKvStore.open(dir)
            val store1 = new KvChainStore(kv1)
            val blocks = (1L to 5L).map(n => block(n, tx(100 + n)))
            blocks.foreach(store1.appendBlock)
            val tipBeforeClose = store1.tip
            store1.close()

            val kv2 = RocksDbKvStore.open(dir)
            val store2 = new KvChainStore(kv2)
            try {
                assert(store2.tip == tipBeforeClose)
                val loaded = store2
                    .blocksBetween(ChainPoint.origin, blocks.last.point)
                    .toOption
                    .get
                    .toList
                assert(loaded.map(_.point) == blocks.map(_.point))
            } finally store2.close()
        }
    }

    test("rollbackTo survives reopen") {
        withTempDir { dir =>
            val kv1 = RocksDbKvStore.open(dir)
            val store1 = new KvChainStore(kv1)
            (1L to 5L).foreach(n => store1.appendBlock(block(n, tx(100 + n))))
            store1.rollbackTo(point(3))
            store1.close()

            val kv2 = RocksDbKvStore.open(dir)
            val store2 = new KvChainStore(kv2)
            try {
                assert(store2.tip.map(_.point).contains(point(3)))
                val out = store2.blocksBetween(ChainPoint.origin, point(5)).toOption.get.toList
                assert(out.map(_.point) == Seq(point(1), point(2), point(3)))
            } finally store2.close()
        }
    }

    test("close is idempotent") {
        withTempDir { dir =>
            val kv = RocksDbKvStore.open(dir)
            val store = new KvChainStore(kv)
            store.close()
            try store.close()
            catch { case NonFatal(_) => fail("second close must not throw") }
        }
    }

    test("second open of the same path in the same JVM fails cleanly") {
        withTempDir { dir =>
            val kv1 = RocksDbKvStore.open(dir)
            try {
                val thrown = intercept[Throwable](RocksDbKvStore.open(dir))
                val _ = thrown // just assert that it threw — exact type is RocksDB-specific
            } finally kv1.close()
        }
    }
}
