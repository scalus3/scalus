package scalus.cardano.node.stream.engine.kvstore

import scalus.uplc.builtin.ByteString

/** Minimal byte-level key/value store that [[scalus.cardano.node.stream.engine.ChainStore]] builds
  * on. Six methods; every concrete backend implements exactly this surface and is free to choose
  * its own on-disk / in-memory representation.
  *
  * Implementations are not required to be thread-safe — the engine calls every method from its
  * worker thread, so [[KvStore]] has no concurrency contract beyond "happens-before on the calling
  * thread".
  *
  * Key ordering is lexicographic on the raw byte arrays ([[ByteString.bytes]] interpreted
  * unsigned). This matches RocksDB / LMDB / `TreeMap[Array[Byte]]` default ordering and is what
  * [[scalus.cardano.node.stream.engine.ChainStore]] relies on for chain-ordered range scans.
  */
trait KvStore {

    /** Return the value for `key`, or `None` if absent. */
    def get(key: ByteString): Option[ByteString]

    /** Insert or overwrite `value` at `key`. */
    def put(key: ByteString, value: ByteString): Unit

    /** Remove `key`. No-op if absent. */
    def delete(key: ByteString): Unit

    /** Apply `writes` atomically — either all land or none do. `KvStore.Write` is a single
      * put/delete tagged op; the store is free to optimise the batch (RocksDB → WriteBatch, LMDB →
      * single transaction, in-memory → sequential apply-under-a-lock).
      */
    def batch(writes: Seq[KvStore.Write]): Unit

    /** Iterator over `(key, value)` pairs whose key is in `[from, until)` in ascending unsigned
      * lexicographic order (the [[ByteString]] ordering in scalus-core).
      *
      * Implementations return a fully materialised snapshot of the range. Callers may safely
      * intersperse writes with iteration — the iterator observes the state at the moment of the
      * `rangeScan` call, not live updates.
      *
      * An inclusive upper bound is not exposed; callers build `until` by bumping the last byte of
      * their inclusive target or using a prefix beyond the target.
      */
    def rangeScan(from: ByteString, until: ByteString): Iterator[(ByteString, ByteString)]

    /** Release any resources (file handles, native allocations). Idempotent. After `close()`, any
      * further call on this instance has undefined behaviour.
      */
    def close(): Unit
}

object KvStore {

    /** Tagged write op for [[KvStore.batch]]. `Put(k, v)` inserts/overwrites; `Delete(k)` removes
      * `k` (no-op if absent).
      */
    sealed trait Write
    final case class Put(key: ByteString, value: ByteString) extends Write
    final case class Delete(key: ByteString) extends Write
}
