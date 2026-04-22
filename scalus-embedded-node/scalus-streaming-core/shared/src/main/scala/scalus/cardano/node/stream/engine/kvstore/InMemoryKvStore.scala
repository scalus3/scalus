package scalus.cardano.node.stream.engine.kvstore

import scalus.uplc.builtin.ByteString

import scala.collection.mutable

/** In-memory [[KvStore]] backed by a [[scala.collection.mutable.TreeMap]] keyed on [[ByteString]]
  * (which carries the repo-wide unsigned-lex ordering). Used for JS (no native database available)
  * and as the fast-path backend in tests.
  *
  * Not durable — `close()` is a no-op. For a persistent store, use a JVM backend (e.g.
  * `RocksDbKvStore` in `scalus-chain-store-rocksdb`).
  */
final class InMemoryKvStore private () extends KvStore {

    private val store = mutable.TreeMap.empty[ByteString, ByteString]

    def get(key: ByteString): Option[ByteString] = store.get(key)

    def put(key: ByteString, value: ByteString): Unit = store.update(key, value)

    def delete(key: ByteString): Unit = {
        val _ = store.remove(key)
    }

    def batch(writes: Seq[KvStore.Write]): Unit =
        // Single-threaded access contract, so sequential apply is already atomic w.r.t. readers.
        writes.foreach {
            case KvStore.Put(k, v) => put(k, v)
            case KvStore.Delete(k) => delete(k)
        }

    def rangeScan(
        from: ByteString,
        until: ByteString
    ): Iterator[(ByteString, ByteString)] =
        // Materialise to a List — see KvStore.rangeScan docstring: implementations return a
        // snapshot, so the iterator is safe to hold across subsequent writes.
        store.range(from, until).iterator.toList.iterator

    def close(): Unit = ()
}

object InMemoryKvStore {
    def apply(): InMemoryKvStore = new InMemoryKvStore()
}
