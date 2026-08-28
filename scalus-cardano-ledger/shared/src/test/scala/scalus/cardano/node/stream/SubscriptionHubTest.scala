package scalus.cardano.node.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.node.stream.internal.{AppliedBlock, AppliedTransaction, Mailbox, SubscriptionHub}
import scalus.cardano.node.{UtxoQuery, UtxoSource}
import scalus.testing.kit.Party
import scalus.uplc.builtin.ByteString

/** Hub behaviour that the provider-level suites cannot reach: seeding, watermarks, rollback
  * preconditions, and the capability gates.
  */
class SubscriptionHubTest extends AnyFunSuite {

    private given CardanoInfo = CardanoInfo.mainnet

    private val alice = Party.Alice.address

    private def caps(
        rollbackHorizon: Option[Int] = None,
        idleSignals: Boolean = true
    ): StreamCapabilities = StreamCapabilities(
      kinds = SubscriptionKind.all,
      pushdown = PushdownKind.all,
      scanning = ScanSupport.Free,
      replay = ReplaySupport.NoReplay,
      rollbackHorizon = rollbackHorizon,
      maxConfirmations = None,
      idleSignals = idleSignals
    )

    private def hub(c: StreamCapabilities = caps()) = new SubscriptionHub(CardanoInfo.mainnet, c)

    private def point(n: Long): ChainPoint = {
        val bytes = new Array[Byte](32)
        bytes(31) = n.toByte
        ChainPoint(n, BlockHash.fromByteString(ByteString.fromArray(bytes)))
    }

    private def block(n: Long): AppliedBlock = AppliedBlock(point(n), n, Seq.empty)

    private def seedUtxos: Utxos = Map(
      TransactionInput(
        TransactionHash.fromByteString(ByteString.fromArray(new Array[Byte](32))),
        0
      ) -> TransactionOutput(alice, Value.ada(10))
    )

    private def addressQuery(types: Set[UtxoEventType]) =
        UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(alice)), types)

    private def drain[A](m: Mailbox[A]): List[A] = {
        val buf = List.newBuilder[A]
        var more = true
        while more do
            m.pull().value.flatMap(_.toOption).flatten match
                case Some(a) => buf += a
                case None    => more = false
        buf.result()
    }

    test("a Spent-only subscription is not seeded with Created events") {
        val h = hub()
        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxo(
          h.nextSubscriptionId(),
          addressQuery(Set(UtxoEventType.Spent)),
          SubscriptionOptions(includeExistingUtxos = true),
          mailbox,
          seedUtxos
        )
        assert(
          drain(mailbox).isEmpty,
          "the seed must honour the subscription's event types, as the live path does"
        )
    }

    test("seed events are stamped origin, not the current tip") {
        val h = hub()
        h.applyBlock(block(7))
        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxo(
          h.nextSubscriptionId(),
          addressQuery(UtxoEventType.all),
          SubscriptionOptions(includeExistingUtxos = true),
          mailbox,
          seedUtxos
        )
        val seeded = drain(mailbox)
        assert(seeded.size == 1)
        assert(
          seeded.head.asInstanceOf[UtxoEvent.Created].at == ChainPoint.origin,
          "a snapshot UTxO was not produced by the block at the tip, and saying it was would make " +
              "a later rollback retract state that is still on chain"
        )
    }

    test("a noRollback subscription is never told to roll back") {
        val h = hub(caps(rollbackHorizon = Some(5)))
        (1L to 10L).foreach(n => h.applyBlock(block(n)))

        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxo(
          h.nextSubscriptionId(),
          addressQuery(UtxoEventType.all),
          SubscriptionOptions(includeExistingUtxos = false, noRollback = true),
          mailbox,
          Map.empty
        )
        // Within the horizon, and above where a depth-5 subscription has actually been delivered.
        h.rollbackTo(ChainTip(point(7), 7L))

        assert(
          !drain(mailbox).exists(_.isInstanceOf[UtxoEvent.RolledBack]),
          "noRollback promises the subscriber never sees RolledBack, so it has no branch for one"
        )
    }

    test("a provider that declares no rollback horizon may not roll back") {
        val h = hub(caps(rollbackHorizon = None))
        h.applyBlock(block(1))
        assertThrows[IllegalStateException](h.rollbackTo(ChainTip(point(0), 0L)))
    }

    test("a rollback past the horizon demands a resync instead of lying") {
        val h = hub(caps(rollbackHorizon = Some(3)))
        (1L to 20L).foreach(n => h.applyBlock(block(n)))
        assertThrows[scalus.cardano.infra.ResyncRequiredException](
          h.rollbackTo(ChainTip(point(2), 2L))
        )
    }

    test("idle signals need the provider's agreement, not just the subscriber's") {
        val h = hub(caps(idleSignals = false))
        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxo(
          h.nextSubscriptionId(),
          addressQuery(UtxoEventType.all),
          SubscriptionOptions(includeExistingUtxos = false, idleSignals = true),
          mailbox,
          Map.empty
        )
        h.applyBlock(block(1))
        assert(
          drain(mailbox).isEmpty,
          "a provider declaring idleSignals = false must not have the hub emit them on its behalf"
        )
    }

    test("a closed hub refuses new subscriptions") {
        val h = hub()
        h.closeAll()
        assertThrows[IllegalStateException](
          h.require(
            SubscriptionRequest.Utxo(addressQuery(UtxoEventType.all), SubscriptionOptions())
          )
        )
    }

    test("a replay request is refused rather than silently downgraded to live-only") {
        val h = hub()
        val request = SubscriptionRequest.Utxo(
          addressQuery(UtxoEventType.all),
          SubscriptionOptions(startFrom = StartFrom.At(point(3)))
        )
        assertThrows[scalus.cardano.infra.UnsupportedSubscriptionException](h.require(request))
    }
    private def input(txByte: Byte, index: Int): TransactionInput = {
        val bytes = new Array[Byte](32)
        bytes(31) = txByte
        TransactionInput(TransactionHash.fromByteString(ByteString.fromArray(bytes)), index)
    }

    private def output(): TransactionOutput = TransactionOutput(alice, Value.ada(3))

    /** A block whose single transaction creates and spends the given UTxOs. */
    private def blockWith(n: Long, created: Utxos, spent: Utxos): AppliedBlock =
        AppliedBlock(
          point(n),
          n,
          Seq(AppliedTransaction(Transaction.empty, created, spent))
        )

    test("the seed is wound back over blocks that have not been delivered yet") {
        val h = hub(caps(rollbackHorizon = Some(3)))
        val fresh = input(1, 0)
        (1L to 4L).foreach(n => h.applyBlock(block(n)))
        // Block 5 creates `fresh`, and a depth-2 subscription has not been delivered it yet.
        h.applyBlock(blockWith(5L, created = Map(fresh -> output()), spent = Map.empty))

        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxo(
          h.nextSubscriptionId(),
          addressQuery(UtxoEventType.all),
          SubscriptionOptions(includeExistingUtxos = true, confirmations = 2),
          mailbox,
          // The snapshot describes the chain now, so it contains `fresh`.
          Map(fresh -> output())
        )

        assert(
          drain(mailbox).isEmpty,
          "block 5 will be delivered later and will report this UTxO's creation itself; seeding " +
              "it as well would report the same UTxO twice"
        )
    }

    test("a UTxO spent in an undelivered block is restored to the seed") {
        val h = hub(caps(rollbackHorizon = Some(3)))
        val old = input(2, 0)
        (1L to 4L).foreach(n => h.applyBlock(block(n)))
        // Block 5 spends a UTxO that existed before the subscription's watermark.
        h.applyBlock(blockWith(5L, created = Map.empty, spent = Map(old -> output())))

        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxo(
          h.nextSubscriptionId(),
          addressQuery(UtxoEventType.all),
          SubscriptionOptions(includeExistingUtxos = true, confirmations = 2),
          mailbox,
          // Already spent, so the current snapshot does not contain it.
          Map.empty
        )

        assert(
          drain(mailbox).collect { case UtxoEvent.Created(u, _, _) => u.input } == List(old),
          "block 5's Spent is still to be delivered, and a Spent for a UTxO the subscriber was " +
              "never told about is not something it can act on"
        )
    }

    // Specification rather than regression: with the rewind removed the snapshot is already
    // empty here, so this one pins the intended rule without being able to catch its loss.
    test("a UTxO created and spent within the undelivered window is in neither") {
        val h = hub(caps(rollbackHorizon = Some(3)))
        val ephemeral = input(3, 0)
        (1L to 4L).foreach(n => h.applyBlock(block(n)))
        h.applyBlock(blockWith(5L, created = Map(ephemeral -> output()), spent = Map.empty))
        h.applyBlock(blockWith(6L, created = Map.empty, spent = Map(ephemeral -> output())))

        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxo(
          h.nextSubscriptionId(),
          addressQuery(UtxoEventType.all),
          SubscriptionOptions(includeExistingUtxos = true, confirmations = 2),
          mailbox,
          Map.empty
        )

        assert(
          drain(mailbox).isEmpty,
          "it did not exist at the watermark, and both its creation and its spend are still to " +
              "be delivered"
        )
    }

    // Likewise a guard, not a regression test: at depth 0 there is nothing pending, so the two
    // implementations agree by construction. It pins that the rewind stays a no-op there.
    test("with no confirmation gate the seed is the snapshot unchanged") {
        val h = hub()
        val u = input(4, 0)
        h.applyBlock(blockWith(1L, created = Map(u -> output()), spent = Map.empty))

        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxo(
          h.nextSubscriptionId(),
          addressQuery(UtxoEventType.all),
          SubscriptionOptions(includeExistingUtxos = true),
          mailbox,
          Map(u -> output())
        )

        assert(
          drain(mailbox).collect { case UtxoEvent.Created(x, _, _) => x.input } == List(u),
          "nothing is pending at depth 0, so the rewind must be a no-op"
        )
    }

}
