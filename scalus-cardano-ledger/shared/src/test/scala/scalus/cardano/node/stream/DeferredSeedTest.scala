package scalus.cardano.node.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.stream.internal.*
import scalus.cardano.node.{UtxoQuery, UtxoSource}
import scalus.testing.kit.Party
import scalus.uplc.builtin.ByteString

/** Registering a subscription whose snapshot has not arrived yet.
  *
  * A provider that reads its snapshot over the network cannot hand one to `registerUtxo`: it must
  * register eagerly — that is what makes `subscribe(q); submit(tx)` race-free — while the read is
  * still in flight. The subscription is therefore held back until the seed lands, and the whole
  * question is whether the blocks that arrive in the gap are still delivered afterwards, exactly
  * once each.
  */
class DeferredSeedTest extends AnyFunSuite {

    private given CardanoInfo = CardanoInfo.mainnet

    private val alice = Party.Alice.address
    private val bob = Party.Bob.address

    private val caps = StreamCapabilities(
      kinds = SubscriptionKind.all,
      pushdown = Set(PushdownKind.Address),
      scanning = ScanSupport.Metered,
      replay = ReplaySupport.NoReplay,
      rollbackHorizon = None,
      maxConfirmations = None,
      idleSignals = true
    )

    private def hub() = new SubscriptionHub(CardanoInfo.mainnet, caps)

    private def point(n: Long): ChainPoint = {
        val bytes = new Array[Byte](32)
        bytes(31) = n.toByte
        ChainPoint(n, BlockHash.fromByteString(ByteString.fromArray(bytes)))
    }

    private def txHash(byte: String): TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex(byte * 32))

    private def utxo(
        source: String,
        index: Int,
        owner: Address
    ): (TransactionInput, TransactionOutput) =
        TransactionInput(txHash(source), index) -> TransactionOutput(owner, Value.ada(10))

    /** A block reporting what `probed` did at height `n`. */
    private def block(
        n: Long,
        probed: Set[Address],
        created: Utxos = Map.empty,
        spent: Utxos = Map.empty
    ): AppliedBlock = AppliedBlock(
      point(n),
      n,
      if created.isEmpty && spent.isEmpty then Seq.empty
      else Seq(AppliedTransaction(Transaction.empty, created, spent)),
      None,
      BlockCoverage.Sources(probed.map(UtxoSource.FromAddress(_)))
    )

    private def query(owner: Address) =
        UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(owner)), UtxoEventType.all)

    private val opts = SubscriptionOptions(includeExistingUtxos = true)

    private def drain[A](m: Mailbox[A]): List[A] = {
        val buf = List.newBuilder[A]
        var more = true
        while more do
            m.pull().value.flatMap(_.toOption).flatten match
                case Some(a) => buf += a
                case None    => more = false
        buf.result()
    }

    private def created(events: List[UtxoEvent]): List[TransactionInput] =
        events.collect { case UtxoEvent.Created(u, _, _) => u.input }

    test("a subscription awaiting its seed is delivered nothing, not even an Idle") {
        val h = hub()
        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxoDeferred(
          h.nextSubscriptionId(),
          query(alice),
          opts.copy(idleSignals = true),
          mailbox
        )
        h.applyBlock(block(1, Set(alice), created = Map(utxo("01", 0, alice))))
        assert(
          drain(mailbox).isEmpty,
          "a live event ahead of the seed it belongs after would leave a subscriber folding " +
              "events into a set it can never reconcile"
        )
    }

    test("blocks that arrived while the seed was in flight are delivered behind it") {
        val h = hub()
        val id = h.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        val existing = utxo("00", 0, alice)
        h.registerUtxoDeferred(id, query(alice), opts, mailbox)

        // Two covered blocks land before the snapshot read returns.
        val fromBlock1 = utxo("01", 0, alice)
        val fromBlock2 = utxo("02", 0, alice)
        h.applyBlock(block(1, Set(alice), created = Map(fromBlock1)))
        h.applyBlock(block(2, Set(alice), created = Map(fromBlock2)))

        // The snapshot is read after them, so it contains all three.
        h.seedUtxo(id, Map(existing, fromBlock1, fromBlock2))

        val events = drain(mailbox)
        assert(
          created(events) == List(existing._1, fromBlock1._1, fromBlock2._1),
          "the seed comes first and each block follows it once, in order; anything else means a " +
              s"block was dropped, duplicated or reordered — got ${created(events)}"
        )
    }

    test("a snapshot is not wound back over blocks this subscription will never be given") {
        val h = hub()
        val id = h.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxoDeferred(id, query(alice), opts, mailbox)

        // Bob's watcher found a transaction that happens to pay Alice. The block covers Bob, not
        // Alice, so it is never delivered to this subscription — but the snapshot taken afterwards
        // does contain what it created.
        val paidToAlice = utxo("01", 0, alice)
        h.applyBlock(block(1, Set(bob), created = Map(paidToAlice)))
        h.seedUtxo(id, Map(paidToAlice))

        assert(
          created(drain(mailbox)) == List(paidToAlice._1),
          "winding the seed back over a block that will not be replayed removes a UTxO nothing " +
              "puts back: the subscriber never hears about it, with no event, no Idle and no error"
        )
    }

    test("a snapshot is wound back over blocks that will be replayed, so nothing arrives twice") {
        val h = hub()
        val id = h.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxoDeferred(id, query(alice), opts, mailbox)

        val fresh = utxo("01", 0, alice)
        h.applyBlock(block(1, Set(alice), created = Map(fresh)))
        h.seedUtxo(id, Map(fresh))

        assert(
          created(drain(mailbox)) == List(fresh._1),
          "the block is covered and will be delivered, so seeding it as well reports one UTxO as " +
              "created twice"
        )
    }

    test("a UTxO spent while the seed was in flight is restored into the seed, then retracted") {
        val h = hub()
        val id = h.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxoDeferred(id, query(alice), opts, mailbox)

        // Existed before the subscription, consumed in a covered block, and so absent from the
        // snapshot taken afterwards.
        val consumed = utxo("00", 0, alice)
        h.applyBlock(block(1, Set(alice), spent = Map(consumed)))
        h.seedUtxo(id, Map.empty)

        val events = drain(mailbox)
        assert(
          created(events) == List(consumed._1),
          s"without restoring it the subscriber is handed a Spent for a UTxO it never saw created" +
              s" — got $events"
        )
        assert(events.collect { case s: UtxoEvent.Spent => s.utxo.input } == List(consumed._1))
    }

    test("the seed does not spend the live buffer's allowance") {
        val h = hub()
        val id = h.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent](2)
        h.registerUtxoDeferred(id, query(alice), opts, mailbox)
        val snapshot = (0 until 8).map(i => utxo("0a", i, alice)).toMap
        h.seedUtxo(id, snapshot)
        assert(
          created(drain(mailbox)).size == 8,
          "the seed is the subscription's initial state, not evidence of a consumer falling " +
              "behind; a wallet with more UTxOs than the bound must still be able to subscribe"
        )
    }

    test("a snapshot that cannot be read fails the subscription rather than starting it empty") {
        val h = hub()
        val id = h.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxoDeferred(id, query(alice), opts, mailbox)
        h.failUtxo(id, new RuntimeException("no route to host"))
        val pulled = mailbox.pull().value
        assert(
          pulled.exists(_.isFailure),
          "a subscriber that asked to be seeded and silently got a live-only stream would believe " +
              s"its UTxO set was complete when it was empty — got $pulled"
        )
    }

    test("seeding a subscription that is already gone is a no-op") {
        val h = hub()
        val id = h.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxoDeferred(id, query(alice), opts, mailbox)
        // Cancelled while the read was in flight, which is why the provider may fire the
        // continuation without checking.
        h.unregisterUtxo(id)
        h.seedUtxo(id, Map(utxo("01", 0, alice)))
        assert(drain(mailbox).isEmpty)
    }

    test("seeding twice adds nothing the second time") {
        val h = hub()
        val id = h.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxoDeferred(id, query(alice), opts, mailbox)
        val existing = utxo("00", 0, alice)
        h.seedUtxo(id, Map(existing))
        h.seedUtxo(id, Map(existing))
        assert(
          created(drain(mailbox)) == List(existing._1),
          "the handshake completes once; a repeat would duplicate the whole snapshot"
        )
    }

    test("a deferred subscription resumes normal delivery once seeded") {
        val h = hub()
        val id = h.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxoDeferred(id, query(alice), opts, mailbox)
        h.seedUtxo(id, Map.empty)
        val later = utxo("03", 0, alice)
        h.applyBlock(block(1, Set(alice), created = Map(later)))
        assert(created(drain(mailbox)) == List(later._1))
    }

    test("history a seed-pending subscription still needs is held rather than pruned") {
        // At the default depth the retention window is one block, so without this the blocks that
        // arrive while a snapshot is being read are gone before it lands.
        val h = hub()
        val id = h.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxoDeferred(id, query(alice), opts, mailbox)
        val first = utxo("01", 0, alice)
        h.applyBlock(block(1, Set(alice), created = Map(first)))
        for n <- 2L to 20L do h.applyBlock(block(n, Set(alice)))
        h.seedUtxo(id, Map(first))
        assert(
          created(drain(mailbox)) == List(first._1),
          "block 1 was still due when the seed landed, so it must have been kept and replayed"
        )
    }

    test("a seed that arrives after history was lost fails, rather than skipping the gap") {
        // Past the bound the hub cannot hold any more, and the snapshot's own height is not
        // knowable — so there is a range of blocks neither the seed nor the replay can be shown to
        // account for. Saying so beats a subscriber silently missing them.
        val h = hub()
        val id = h.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxoDeferred(id, query(alice), opts, mailbox)
        for n <- 1L to 200L do h.applyBlock(block(n, Set(alice)))
        h.seedUtxo(id, Map(utxo("01", 0, alice)))
        val pulled = mailbox.pull().value
        assert(
          pulled.exists(_.isFailure),
          s"expected a resync failure rather than a quietly incomplete stream — got $pulled"
        )
    }

    test("a rollback does not retract events a seed-pending subscription never received") {
        // `RolledBack` as a subscription's very first event retracts what it never saw. Its
        // watermark still has to come down, because that is the delivery cursor too.
        val rollbackCaps = caps.copy(rollbackHorizon = Some(5))
        val h = new SubscriptionHub(CardanoInfo.mainnet, rollbackCaps)
        for n <- 1L to 5L do h.applyBlock(block(n, Set(alice)))
        val id = h.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxoDeferred(id, query(alice), opts, mailbox)
        h.rollbackTo(ChainTip(point(3), 3))
        h.seedUtxo(id, Map.empty)
        val after = utxo("09", 0, alice)
        h.applyBlock(block(4, Set(alice), created = Map(after)))
        val events = drain(mailbox)
        assert(
          !events.exists(_.isInstanceOf[UtxoEvent.RolledBack]),
          s"nothing had been delivered, so there was nothing to retract — got $events"
        )
        assert(
          created(events) == List(after._1),
          s"the watermark had to come down with the rollback for this to be due — got $events"
        )
    }
}
