package scalus.cardano.node.stream.engine.kvstore

class InMemoryKvStoreSuite extends KvStoreContract {
    protected def withFreshStore[A](body: KvStore => A): A = body(InMemoryKvStore())
}
