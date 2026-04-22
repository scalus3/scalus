package scalus.cardano.node.stream.engine.kvstore

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

/** Reusable contract suite for any [[KvStore]] backend. Subclasses override [[newStore]] to build a
  * fresh instance; the shared tests ensure every backend gives the same observable behaviour.
  *
  * The in-memory suite extends this directly; JVM backends (RocksDB, MapDB) extend it from their
  * own test tree so they can bracket each case with their teardown (e.g. temp-directory cleanup).
  */
abstract class KvStoreContract extends AnyFunSuite {

    /** Produce a fresh, empty [[KvStore]] for a single test. Teardown is the subclass's concern. */
    protected def withFreshStore[A](body: KvStore => A): A

    private def bs(s: String): ByteString = ByteString.fromArray(s.getBytes("UTF-8"))

    test("get on empty store returns None") {
        withFreshStore { store =>
            assert(store.get(bs("missing")).isEmpty)
        }
    }

    test("put then get round-trips") {
        withFreshStore { store =>
            store.put(bs("k1"), bs("v1"))
            assert(store.get(bs("k1")).contains(bs("v1")))
        }
    }

    test("put overwrites existing value") {
        withFreshStore { store =>
            store.put(bs("k"), bs("v1"))
            store.put(bs("k"), bs("v2"))
            assert(store.get(bs("k")).contains(bs("v2")))
        }
    }

    test("delete removes the key; subsequent delete is a no-op") {
        withFreshStore { store =>
            store.put(bs("k"), bs("v"))
            store.delete(bs("k"))
            assert(store.get(bs("k")).isEmpty)
            // second delete must not throw
            store.delete(bs("k"))
            assert(store.get(bs("k")).isEmpty)
        }
    }

    test("rangeScan returns keys in ascending lexicographic order, [from, until)") {
        withFreshStore { store =>
            // Insert in non-sorted order; the scan must still come back sorted.
            store.put(bs("b"), bs("2"))
            store.put(bs("a"), bs("1"))
            store.put(bs("d"), bs("4"))
            store.put(bs("c"), bs("3"))

            val observed = store.rangeScan(bs("a"), bs("d")).map((k, _) => k).toList
            assert(observed == List(bs("a"), bs("b"), bs("c")))
        }
    }

    test("rangeScan honours unsigned ordering on high bytes") {
        withFreshStore { store =>
            val low = ByteString.fromArray(Array[Byte](0x01))
            val high = ByteString.fromArray(Array[Byte](0xff.toByte))
            store.put(high, bs("high"))
            store.put(low, bs("low"))

            val observed = store
                .rangeScan(ByteString.fromArray(Array[Byte](0x00)), ByteString.empty)
                .map((k, _) => k)
                .toList
            // empty `until` is less than `low` lexicographically, so an empty result is correct —
            // this test asserts the narrower "low-to-beyond-high" case instead.
            val _ = observed
            val all = store
                .rangeScan(
                  ByteString.fromArray(Array[Byte](0x00)),
                  ByteString.fromArray(Array[Byte](0xff.toByte, 0x01))
                )
                .map((k, _) => k)
                .toList
            assert(all == List(low, high))
        }
    }

    test("batch applies puts and deletes together") {
        withFreshStore { store =>
            store.put(bs("keep"), bs("1"))
            store.put(bs("drop"), bs("2"))
            store.batch(
              Seq(
                KvStore.Delete(bs("drop")),
                KvStore.Put(bs("add"), bs("3"))
              )
            )
            assert(store.get(bs("keep")).contains(bs("1")))
            assert(store.get(bs("drop")).isEmpty)
            assert(store.get(bs("add")).contains(bs("3")))
        }
    }

    test("close is idempotent on a store that has never been used") {
        withFreshStore { store =>
            store.close()
            store.close()
        }
    }
}
