package scalus.cardano.node.stream.engine.kvstore.rocksdb

import org.rocksdb.{Options, ReadOptions, RocksDB, RocksIterator, WriteBatch, WriteOptions}
import scalus.cardano.node.stream.engine.kvstore.KvStore
import scalus.uplc.builtin.ByteString

import java.nio.file.{Files, Path}
import scala.collection.mutable.ArrayBuffer

/** RocksDB-backed [[KvStore]]. Runs on JVM only because rocksdbjni is a native library.
  *
  * One RocksDB instance per directory. The concrete `KvChainStore` built on top of this store
  * separates its own keyspaces via key-prefix bytes, so a single default column family is enough
  * and we don't pay the column-family coordination cost for the narrow benefit we'd get.
  *
  * Not thread-safe — inherits the [[KvStore]] single-writer contract. The engine worker is the
  * only caller.
  */
final class RocksDbKvStore private (
    private val db: RocksDB,
    private val options: Options,
    private val writeOptions: WriteOptions
) extends KvStore {

    @volatile private var closed: Boolean = false

    def get(key: ByteString): Option[ByteString] = {
        val raw = db.get(key.bytes)
        Option(raw).map(ByteString.unsafeFromArray)
    }

    def put(key: ByteString, value: ByteString): Unit =
        db.put(writeOptions, key.bytes, value.bytes)

    def delete(key: ByteString): Unit =
        db.delete(writeOptions, key.bytes)

    def batch(writes: Seq[KvStore.Write]): Unit = {
        if writes.isEmpty then return
        val wb = new WriteBatch()
        try {
            writes.foreach {
                case KvStore.Put(k, v) => wb.put(k.bytes, v.bytes)
                case KvStore.Delete(k) => wb.delete(k.bytes)
            }
            db.write(writeOptions, wb)
        } finally wb.close()
    }

    def rangeScan(
        from: ByteString,
        until: ByteString
    ): Iterator[(ByteString, ByteString)] = {
        // Materialise to a list so the caller can intersperse writes without the cursor being
        // invalidated by RocksDB's WriteBatch pending-memtable rewrite — see KvStore.rangeScan
        // docstring on the snapshot contract.
        val readOpts = new ReadOptions()
        val it: RocksIterator = db.newIterator(readOpts)
        val buf = ArrayBuffer.empty[(ByteString, ByteString)]
        try {
            it.seek(from.bytes)
            while it.isValid do {
                val key = ByteString.unsafeFromArray(it.key())
                if byteStringOrdering.compare(key, until) >= 0 then {
                    return buf.iterator
                }
                buf += (key -> ByteString.unsafeFromArray(it.value()))
                it.next()
            }
        } finally {
            it.close()
            readOpts.close()
        }
        buf.iterator
    }

    def close(): Unit = {
        if closed then return
        closed = true
        try db.close()
        finally try writeOptions.close()
            finally options.close()
    }

    private val byteStringOrdering: Ordering[ByteString] = summon[Ordering[ByteString]]
}

object RocksDbKvStore {

    RocksDB.loadLibrary()

    /** Open (or create) a RocksDB database at `path`. The directory is created if it doesn't
      * exist. RocksDB's own `LOCK` file enforces single-process access — a second open against
      * the same path from the same process (or another) fails with a typed RocksDBException.
      */
    def open(path: Path): RocksDbKvStore = {
        val _ = Files.createDirectories(path)
        val options = new Options().setCreateIfMissing(true)
        val writeOptions = new WriteOptions()
        try {
            val db = RocksDB.open(options, path.toAbsolutePath.toString)
            new RocksDbKvStore(db, options, writeOptions)
        } catch {
            case t: Throwable =>
                // Close what we already allocated before rethrowing so a failed open doesn't leak
                // native resources.
                try writeOptions.close()
                finally options.close()
                throw t
        }
    }
}
