package scalus.cardano.node.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.node.stream.internal.*
import scalus.cardano.node.{UtxoQuery, UtxoSource}
import scalus.testing.kit.Party
import scalus.uplc.builtin.ByteString

import scala.concurrent.ExecutionContext

/** The component that joins a follower to the hub, and therefore owns the lifecycle: what a clean
  * end of stream means, what a failure means, and what closing means. All three are observable only
  * as "what happens to a subscriber who is waiting", which is what these assert.
  */
class HubDriverTest extends AnyFunSuite {

    private given CardanoInfo = CardanoInfo.mainnet
    private given ExecutionContext = ExecutionContext.parasitic

    private val alice = Party.Alice.address

    private val caps = StreamCapabilities(
      kinds = SubscriptionKind.all,
      pushdown = PushdownKind.all,
      scanning = ScanSupport.Free,
      replay = ReplaySupport.NoReplay,
      rollbackHorizon = None,
      maxConfirmations = None,
      idleSignals = true
    )

    private def point(n: Long): ChainPoint = {
        val bytes = new Array[Byte](32)
        bytes(31) = n.toByte
        ChainPoint(n, BlockHash.fromByteString(ByteString.fromArray(bytes)))
    }

    /** A follower whose events are pushed by the test rather than polled from anywhere. */
    private class ManualFollower extends ChainFollower {
        val mailbox: Mailbox[ChainEvent] = Mailbox.delta[ChainEvent]()
        var closed = false
        var started = false

        override def start(): Unit = started = true

        override def events: ScalusAsyncSource[ChainEvent] = mailbox
        // Complete coverage: everything after the origin covers any subscription.
        override def watch(sources: Set[UtxoSource]): ChainPoint = ChainPoint.origin
        override def stopWatching(sources: Set[UtxoSource]): Unit = ()
        override def close(): Unit = { closed = true; mailbox.close() }
    }

    private def subscribe(hub: SubscriptionHub): Mailbox[UtxoEvent] = {
        val mailbox = Mailbox.delta[UtxoEvent]()
        hub.registerUtxo(
          hub.nextSubscriptionId(),
          UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(alice)), UtxoEventType.all),
          SubscriptionOptions(includeExistingUtxos = false),
          mailbox,
          Map.empty
        )
        mailbox
    }

    private def fixture(): (SubscriptionHub, ManualFollower, HubDriver) = {
        val hub = new SubscriptionHub(CardanoInfo.mainnet, caps)
        val follower = new ManualFollower
        val driver = new HubDriver(hub, follower)
        (hub, follower, driver)
    }

    test("events reach the hub, and the tip advances") {
        val (hub, follower, driver) = fixture()
        driver.start()
        follower.mailbox.offer(ChainEvent.RollForward(AppliedBlock(point(1L), 1L, Seq.empty)))
        assert(hub.currentTip.blockNo == 1L)
    }

    test("a clean end of stream completes subscribers rather than parking them") {
        val (hub, follower, driver) = fixture()
        val sub = subscribe(hub)
        driver.start()
        follower.mailbox.close()

        assert(
          sub.pull().value.contains(scala.util.Success(None)),
          "the chain feed ended, so subscriptions are finished — not broken, and not left waiting"
        )
    }

    test("a failed feed fails subscribers, so nobody trusts a stale view") {
        val (hub, follower, driver) = fixture()
        val sub = subscribe(hub)
        driver.start()
        follower.mailbox.fail(new RuntimeException("backend fell over"))

        val settled = sub.pull().value.get
        assert(settled.isFailure, "a subscriber must learn its view stopped tracking the chain")
    }

    test("close() ends subscriptions instead of leaving them waiting forever") {
        val (hub, follower, driver) = fixture()
        val sub = subscribe(hub)
        driver.start()
        driver.close()

        assert(follower.closed, "closing the driver closes its follower")
        assert(
          sub.pull().value.contains(scala.util.Success(None)),
          "the driver is the only thing observing the follower's end, so if it stops pumping " +
              "before that arrives it has to end the subscriptions itself"
        )
    }

    test("a second start() is harmless") {
        val (hub, follower, driver) = fixture()
        driver.start()
        driver.start()

        follower.mailbox.offer(ChainEvent.RollForward(AppliedBlock(point(1L), 1L, Seq.empty)))
        follower.mailbox.offer(ChainEvent.RollForward(AppliedBlock(point(2L), 2L, Seq.empty)))
        assert(hub.currentTip.blockNo == 2L)
    }
    // Honest scope: this does not *detect* a missing `started` guard, and pretending otherwise
    // would be worse than not having it. Without the guard both pumps are handed the same pull
    // promise and apply the same event twice — but the second application is almost invisible from
    // outside, because `releaseLocked` has already advanced the watermark past that height, so no
    // duplicate events are emitted. What it actually corrupts is `recent`, which gains a second
    // entry at one height and so breaks the one-block-per-ascending-height invariant that
    // `AppliedBlock` documents and that pruning relies on. The guard is defensive; this test only
    // pins that a double start does not break ordinary progress.

    test("a large backlog drains completely") {
        val (hub, follower, driver) = fixture()
        // Buffered before the driver starts, so every pull completes synchronously — the shape
        // that makes a recursive pump nest one frame per event.
        (1L to 20000L).foreach(n =>
            follower.mailbox.offer(ChainEvent.RollForward(AppliedBlock(point(n), n, Seq.empty)))
        )
        driver.start()

        assert(hub.currentTip.blockNo == 20000L, "every buffered event must reach the hub")
    }
    // Also honest scope: 20,000 nested frames did *not* overflow when tried against the recursive
    // version, so this is a drain-completeness test, not a stack-safety one. Stack safety is
    // argued in `pump`'s comment and enforced by re-dispatching rather than recursing; a test that
    // reliably demonstrated the overflow would have to be tuned to a particular stack size and
    // would be flaky across JVMs and Scala.js.

    test("starting the driver starts the follower") {
        // The two halves of the lifecycle, in the one order that is safe: a follower that produced
        // before the pump existed would be filling a source nobody was reading, and a metered one
        // would be spending quota for it.
        val follower = new ManualFollower
        val h = new SubscriptionHub(CardanoInfo.mainnet, caps)
        assert(!follower.started)
        new HubDriver(h, follower).start()
        assert(
          follower.started,
          "a driver that never starts its follower pulls forever from a source that produces nothing"
        )
    }
}
