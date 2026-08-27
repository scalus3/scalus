package scalus.cardano.node.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.node.stream.internal.{AppliedBlock, Mailbox, SubscriptionHub}
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
      scanning = ScanCost.Free,
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
}
