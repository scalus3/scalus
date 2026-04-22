package scalus.cardano.node.stream.engine.kvstore.rocksdb

import scalus.cardano.node.stream.engine.kvstore.{KvStore, KvStoreContract}

import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

class RocksDbKvStoreSuite extends KvStoreContract {

    protected def withFreshStore[A](body: KvStore => A): A = {
        val dir: Path = Files.createTempDirectory("scalus-rocksdb-test-")
        val store = RocksDbKvStore.open(dir)
        try body(store)
        finally {
            try store.close()
            catch { case NonFatal(_) => () }
            // Best-effort recursive delete; the dir contains RocksDB sstables and a LOCK file.
            deleteRecursive(dir.toFile)
        }
    }

    private def deleteRecursive(f: java.io.File): Unit = {
        if f.isDirectory then {
            val children = f.listFiles()
            if children != null then children.foreach(deleteRecursive)
        }
        val _ = f.delete()
    }
}
