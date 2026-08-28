package scalus.cardano.node.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.stream.internal.{AppliedBlock, BlockCoverage, Mailbox, SubscriptionHub}
import scalus.cardano.node.{UtxoQuery, UtxoSource}
import scalus.testing.kit.Party
import scalus.uplc.builtin.ByteString

/** Delivery under partial coverage.
  *
  * A metered provider cannot afford to fetch whole blocks, so what it learns about a height is only
  * ever "what these sources did in it". The hub delivers a block and advances the receiving
  * subscription's watermark in one step, so a block that did not cover a subscription must not
  * reach it — otherwise it is told "nothing here for you" about a height nobody looked at on its
  * behalf, and the real events for that height become undeliverable.
  */
class BlockCoverageTest extends AnyFunSuite {

    private given CardanoInfo = CardanoInfo.mainnet

    private val alice = Party.Alice.address
    private val bob = Party.Bob.address

    private val caps = StreamCapabilities(
      kinds = SubscriptionKind.all,
      pushdown = PushdownKind.all,
      // Metered is the whole reason partial coverage exists.
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

    private def probing(n: Long, addresses: Address*): AppliedBlock = AppliedBlock(
      point(n),
      n,
      Seq.empty,
      None,
      BlockCoverage.Sources(addresses.map(UtxoSource.FromAddress(_)).toSet)
    )

    private def complete(n: Long): AppliedBlock = AppliedBlock(point(n), n, Seq.empty)

    private def watch(h: SubscriptionHub, address: Address): Mailbox[UtxoEvent] = {
        val mailbox = Mailbox.delta[UtxoEvent]()
        h.registerUtxo(
          h.nextSubscriptionId(),
          UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(address)), UtxoEventType.all),
          // idleSignals is off by default; these tests are about which blocks produce a progress
          // signal, so without it every assertion below would hold vacuously.
          SubscriptionOptions(
            includeExistingUtxos = false,
            idleSignals = true,
            allowUnindexedScan = true
          ),
          mailbox,
          Map.empty
        )
        mailbox
    }

    private def drain[A](m: Mailbox[A]): List[A] = {
        val buf = List.newBuilder[A]
        var more = true
        while more do
            m.pull().value.flatMap(_.toOption).flatten match
                case Some(a) => buf += a
                case None    => more = false
        buf.result()
    }

    private def idlePoints(m: Mailbox[UtxoEvent]): List[ChainPoint] =
        drain(m).collect { case UtxoEvent.Idle(at) => at }

    test("a block that probed another source does not idle an uncovered subscription") {
        val h = hub()
        val watchingBob = watch(h, bob)

        h.applyBlock(probing(1L, alice))

        assert(
          idlePoints(watchingBob).isEmpty,
          "Idle claims the provider looked and found nothing; it looked for Alice, not for Bob, " +
              "and saying otherwise reports progress that was never made"
        )
    }

    test("a covered subscription still gets its idle signal") {
        val h = hub()
        val watchingAlice = watch(h, alice)

        h.applyBlock(probing(1L, alice))

        assert(
          idlePoints(watchingAlice) == List(point(1L)),
          "coverage gates delivery; it must not suppress it for the source actually probed"
        )
    }

    test("a subscription is not advanced over heights examined only for others") {
        val h = hub()
        val watchingBob = watch(h, bob)

        // A provider on a request budget probes Alice at height 1 and gets to both at height 2.
        h.applyBlock(probing(1L, alice))
        h.applyBlock(probing(2L, alice, bob))

        assert(
          idlePoints(watchingBob) == List(point(2L)),
          "height 1 was never examined for Bob, so it is neither an event nor progress for him"
        )
    }

    test("complete coverage reaches every subscription, as it always did") {
        val h = hub()
        val watchingAlice = watch(h, alice)
        val watchingBob = watch(h, bob)

        h.applyBlock(complete(1L))

        assert(idlePoints(watchingAlice) == List(point(1L)))
        assert(idlePoints(watchingBob) == List(point(1L)))
    }

    test("a union is only as covered as its worst arm") {
        val probed = BlockCoverage.Sources(
          Set(UtxoSource.FromAddress(alice))
        )
        val union = UtxoSource.Or(UtxoSource.FromAddress(alice), UtxoSource.FromAddress(bob))
        assert(
          !BlockCoverage.covers(probed, union),
          "the events we would miss are exactly the ones the unprobed arm would have found"
        )
        assert(BlockCoverage.covers(probed, UtxoSource.FromAddress(alice)))
    }

    test("an intersection needs only one covered arm, since the rest post-filters") {
        val probed = BlockCoverage.Sources(Set(UtxoSource.FromAddress(alice)))
        val both =
            UtxoSource.And(UtxoSource.FromAddress(alice), UtxoSource.FromAddress(bob))
        assert(
          BlockCoverage.covers(probed, both),
          "probing Alice yields every candidate; the other condition filters data already in hand"
        )
    }
}
