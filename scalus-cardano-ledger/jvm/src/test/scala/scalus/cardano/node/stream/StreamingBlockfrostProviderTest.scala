package scalus.cardano.node.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, ByronAddress, ShelleyAddress}
import scalus.cardano.infra.UnsupportedSubscriptionException
import scalus.cardano.ledger.*
import scalus.cardano.node.stream.internal.*
import scalus.cardano.node.{BlockfrostProvider, UtxoQuery, UtxoSource}
import scalus.testing.kit.Party
import scalus.uplc.builtin.ByteString
import sttp.client4.Backend
import sttp.client4.testing.BackendStub

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, Promise}

/** The provider's own job, which is everything the follower and the hub do not do: refusing what it
  * cannot cover, telling the follower what to watch before registering anything, and letting go of
  * an address when the last subscription on it ends.
  *
  * The follower is a fake, so no network and no clock are involved; the Blockfrost client behind it
  * is real, over a stubbed HTTP backend, because the snapshot read on the subscribe path is a real
  * request and its ordering relative to `watch` is the point of several of these.
  */
class StreamingBlockfrostProviderTest extends AnyFunSuite {

    private given ExecutionContext = ExecutionContext.global
    private given CardanoInfo = CardanoInfo.mainnet

    private val alice = Party.Alice.address
    private val bob = Party.Bob.address
    private val byron: Address =
        ByronAddress.fromBase58("Ae2tdPwUPEZDoUnyXuAgqzhkjNXNJeiZ5nqwprg9sArZmRNjySfJ5uz4FjB").get

    private def bech32(a: Address): String = a.asInstanceOf[ShelleyAddress].toBech32.get

    private def txHash(byte: String): TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex(byte * 32))

    private def point(n: Long): ChainPoint = {
        val bytes = new Array[Byte](32)
        bytes(31) = n.toByte
        ChainPoint(n, BlockHash.fromByteString(ByteString.fromArray(bytes)))
    }

    /** Records what it was told to watch, and when — the ordering half of these tests. */
    private class FakeFollower extends ChainFollower {
        val mailbox: Mailbox[ChainEvent] = Mailbox.delta[ChainEvent]()
        @volatile var started = false
        @volatile var closed = false
        private var calls: Vector[Set[UtxoSource]] = Vector.empty

        def start(): Unit = started = true
        def events: ScalusAsyncSource[ChainEvent] = mailbox
        def watch(sources: Set[UtxoSource]): ChainPoint = synchronized {
            if closed then throw new IllegalStateException("watch on a closed follower")
            calls = calls :+ sources
            ChainPoint.origin
        }
        def stopWatching(sources: Set[UtxoSource]): Unit = synchronized {
            if !closed then calls = calls :+ sources
        }
        def close(): Unit = { closed = true; mailbox.close() }

        @volatile var observing: Option[Boolean] = None
        override def setObserving(active: Boolean): Unit = observing = Some(active)

        def watchCalls: Vector[Set[UtxoSource]] = synchronized(calls)
        def watching: Set[Address] = watchCalls.lastOption.getOrElse(Set.empty).collect {
            case UtxoSource.FromAddress(a) => a
        }
    }

    /** A Blockfrost client whose only live endpoint is the address-UTxO read the seed uses.
      *
      * `onSnapshotRequest` runs when that request is served, which is how the tests below observe
      * what had already happened by the time the snapshot was read.
      */
    private def stubbedClient(onSnapshotRequest: () => Unit = () => ()): BlockfrostProvider = {
        // Recorded in the matcher, which the stub evaluates per request; the response body is a
        // value it evaluates once, so a side effect there would fire before any subscription.
        given Backend[Future] = BackendStub.asynchronousFuture
            .whenRequestMatches { _ =>
                onSnapshotRequest(); true
            }
            .thenRespondAdjust("[]")
        new BlockfrostProvider("stub-key", "http://stub.invalid", 5, CardanoInfo.mainnet)
    }

    /** A `delay` the test steps by hand, so the parameter loop runs exactly as many times as the
      * test wants it to and no faster than the test can observe.
      */
    private class SteppableDelay extends (FiniteDuration => Future[Unit]) {
        private var waiting: List[Promise[Unit]] = Nil
        def apply(d: FiniteDuration): Future[Unit] = synchronized {
            val p = Promise[Unit]()
            waiting = p :: waiting
            p.future
        }
        def pending: Int = synchronized(waiting.size)
        def step(): Unit = {
            val next = synchronized {
                val head = waiting.lastOption
                waiting = waiting.dropRight(1)
                head
            }
            next.foreach(_.success(()))
        }
    }

    /** A client whose every request fails, so the parameter refresh cannot succeed. */
    private def failingParamsClient(): BlockfrostProvider = {
        given Backend[Future] =
            BackendStub.asynchronousFuture.whenAnyRequest.thenRespondServerError()
        new BlockfrostProvider("stub-key", "http://stub.invalid", 5, CardanoInfo.mainnet)
    }

    private def providerWith(
        follower: FakeFollower,
        client: BlockfrostProvider = stubbedClient()
    ): StreamingBlockfrostProvider = {
        val p = new StreamingBlockfrostProvider(client, follower, 1.hour, _ => Future.never)
        p.ensureStarted()
        p
    }

    private def utxoQuery(owner: Address) =
        UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(owner)), UtxoEventType.all)

    private def liveOnly = SubscriptionOptions(includeExistingUtxos = false)

    // ── what it declares, and therefore refuses ─────────────────────────────

    test("it declares no Block kind, because it never holds a block") {
        val caps = StreamingBlockfrostProvider.capabilities
        assert(!caps.kinds.contains(SubscriptionKind.Block))
        assert(!caps.kinds.contains(SubscriptionKind.TransactionStatus))
        assert(caps.scanning == ScanSupport.Unsupported)
        assert(caps.pushdown == Set(PushdownKind.Address))
        assert(
          caps.rollbackHorizon.isEmpty,
          "it detects reorgs and fails; it never emits RolledBack, and says so"
        )
    }

    test("a block subscription is refused rather than accepted and never served") {
        val p = providerWith(new FakeFollower)
        assertThrows[UnsupportedSubscriptionException](
          p.subscribeBlockQuery(BlockQuery.All, liveOnly)
        )
    }

    test("a transaction-status subscription is refused rather than left Pending forever") {
        val p = providerWith(new FakeFollower)
        assertThrows[UnsupportedSubscriptionException](
          p.subscribeTransactionStatus(txHash("ab"))
        )
    }

    test("an unindexable query is refused even with allowUnindexedScan") {
        // The flag consents to an expense. Here the scan is not expensive but impossible, and
        // consenting to something that cannot happen is worse than being refused.
        val p = providerWith(new FakeFollower)
        val consenting = SubscriptionOptions(
          includeExistingUtxos = false,
          allowUnindexedScan = true
        )
        assertThrows[UnsupportedSubscriptionException](
          p.subscribeTransactionQuery(TransactionQuery.All, consenting)
        )
        val byAsset = UtxoEventQuery(
          UtxoQuery(UtxoSource.FromAsset(ScriptHash.fromHex("00" * 28), AssetName.fromHex("cafe"))),
          UtxoEventType.all
        )
        assertThrows[UnsupportedSubscriptionException](
          p.subscribeUtxoQuery(byAsset, consenting)
        )
    }

    test("an address Blockfrost cannot be asked about is refused at subscribe") {
        // Not in the poll loop: the follower feeds every subscriber, so one unwatchable address
        // discovered there would fail all of them for a mistake only one of them made.
        val follower = new FakeFollower
        val p = providerWith(follower)
        assertThrows[UnsupportedSubscriptionException](
          p.subscribeUtxoQuery(utxoQuery(byron), liveOnly)
        )
        assert(
          follower.watchCalls.isEmpty,
          "a refused subscription must not have left an address being polled for"
        )
    }

    // ── watching ────────────────────────────────────────────────────────────

    test("the follower is told to watch before the snapshot is read") {
        // The ordering the whole seed argument rests on: a snapshot taken after the watch already
        // contains the effects of any block the watch did not reach in time.
        @volatile var watchedWhenRead: Option[Int] = None
        val follower = new FakeFollower
        val client = stubbedClient(onSnapshotRequest =
            () => watchedWhenRead = Some(follower.watchCalls.size)
        )
        val p = new StreamingBlockfrostProvider(client, follower, 1.hour, _ => Future.never)
        p.ensureStarted()
        p.subscribeUtxoQuery(utxoQuery(alice), SubscriptionOptions())
        eventually(
          watchedWhenRead.exists(_ > 0),
          s"the snapshot was read before any watch landed: $watchedWhenRead"
        )
    }

    test("the follower is always given the union, never one subscription's own sources") {
        val follower = new FakeFollower
        val p = providerWith(follower)
        p.subscribeUtxoQuery(utxoQuery(alice), liveOnly)
        p.subscribeUtxoQuery(utxoQuery(bob), liveOnly)
        assert(
          follower.watching == Set(alice, bob),
          s"watch replaces rather than adds, so a caller passing only its own sources would " +
              s"silently unwatch the other subscription — got ${follower.watching}"
        )
    }

    test("cancelling the last subscription on an address stops watching it") {
        // Shrinking matters on a metered backend: a cancelled subscription that kept its address
        // in the watched set would cost one request per block for the life of the provider.
        val follower = new FakeFollower
        val p = providerWith(follower)
        val aliceSub = p.subscribeUtxoQuery(utxoQuery(alice), liveOnly)
        p.subscribeUtxoQuery(utxoQuery(bob), liveOnly)
        aliceSub.cancel()
        assert(follower.watching == Set(bob), s"got ${follower.watching}")
    }

    test("a transaction subscription watches the addresses its query names") {
        val follower = new FakeFollower
        val p = providerWith(follower)
        p.subscribeTransactionQuery(
          TransactionQuery.InvolvesAddress(alice),
          liveOnly
        )
        assert(follower.watching == Set(alice))
    }

    // ── delivery ────────────────────────────────────────────────────────────

    test("a covered block reaches a live subscription") {
        val follower = new FakeFollower
        val p = providerWith(follower)
        val src: ScalusAsyncSource[UtxoEvent] =
            p.subscribeUtxoQuery(utxoQuery(alice), liveOnly)
        // Pulled before the block is offered, and held: a fresh `pull` each time round the spin
        // would start a new wait once the first one completed, and never look at what it delivered.
        val pulled = src.pull()
        val created: Utxos =
            Map(TransactionInput(txHash("01"), 0) -> TransactionOutput(alice, Value.ada(10)))
        follower.mailbox.offer(
          ChainEvent.RollForward(
            AppliedBlock(
              point(1),
              1,
              Seq(AppliedTransaction(Transaction.empty, created, Map.empty)),
              None,
              BlockCoverage.Sources(Set(UtxoSource.FromAddress(alice)))
            )
          )
        )
        eventually(
          pulled.value.isDefined,
          "the driver pumps the follower's events into the hub; nothing arrived"
        )
        assert(
          pulled.value.flatMap(_.toOption).flatten.exists {
              case UtxoEvent.Created(u, _, _) => u.input == TransactionInput(txHash("01"), 0)
              case _                          => false
          },
          s"expected the block's Created event, got ${pulled.value}"
        )
    }

    test("close stops the follower and ends every subscription") {
        val follower = new FakeFollower
        val p = providerWith(follower)
        val src: ScalusAsyncSource[UtxoEvent] =
            p.subscribeUtxoQuery(utxoQuery(alice), liveOnly)
        p.close()
        assert(follower.closed, "a metered follower left running keeps spending quota")
        assert(
          src.pull().value.exists(_.toOption.exists(_.isEmpty)),
          "a close that left subscribers parked on promises would leak what it was called to free"
        )
    }

    test("cancelling after close does not try to re-watch a stopped follower") {
        val follower = new FakeFollower
        val p = providerWith(follower)
        val src: ScalusAsyncSource[UtxoEvent] =
            p.subscribeUtxoQuery(utxoQuery(alice), liveOnly)
        p.close()
        src.cancel()
        assert(follower.closed)
    }

    // ── the extraction agrees with the classifier ───────────────────────────

    test("every query this provider accepts is covered by the sources it hands the follower") {
        // The extraction and the classifier have to agree about what "indexed by address" means,
        // and the hub's coverage predicate has to agree with both. Asserting the set is non-empty
        // would not catch the interesting drift; asserting that a block probing exactly that set
        // actually reaches the subscription does. `Idle` is the signal, because it fires precisely
        // when a block covers a subscription and matches nothing in it.
        val policy = ScriptHash.fromHex("00" * 28)
        val name = AssetName.fromHex("cafe")
        val caps = StreamingBlockfrostProvider.capabilities
        val opts = SubscriptionOptions(includeExistingUtxos = false, idleSignals = true)

        val utxoQueries = Seq(
          UtxoQuery(UtxoSource.FromAddress(alice)),
          UtxoQuery(UtxoSource.FromAddress(alice) || UtxoSource.FromAddress(bob)),
          UtxoQuery(UtxoSource.FromAddress(alice) && UtxoSource.FromAsset(policy, name)),
          // The arm order that a left-biased extraction would get wrong: the asset is not
          // pushdownable here, so the address arm is the one that must be observed.
          UtxoQuery(UtxoSource.FromAsset(policy, name) && UtxoSource.FromAddress(bob)),
          UtxoQuery(UtxoSource.FromAddress(alice)) || UtxoQuery(UtxoSource.FromAddress(bob))
        )
        for q <- utxoQueries do
            val request = SubscriptionRequest.Utxo(UtxoEventQuery(q), opts)
            if SubscriptionSupport.of(request, caps) == SubscriptionSupport.Indexed then
                val follower = new FakeFollower
                val p = providerWith(follower)
                val src: ScalusAsyncSource[UtxoEvent] =
                    p.subscribeUtxoQuery(UtxoEventQuery(q), opts)
                val pulled = src.pull()
                follower.mailbox.offer(
                  ChainEvent.RollForward(
                    AppliedBlock(
                      point(1),
                      1,
                      Seq.empty,
                      None,
                      BlockCoverage.Sources(follower.watching.map(UtxoSource.FromAddress(_)))
                    )
                  )
                )
                eventually(pulled.value.isDefined, s"accepted but never covered: $q")
                assert(
                  pulled.value.flatMap(_.toOption).flatten.exists(_.isInstanceOf[UtxoEvent.Idle]),
                  s"expected an Idle for $q, got ${pulled.value}"
                )
                p.close()

        val txQueries = Seq(
          TransactionQuery.InvolvesAddress(alice),
          TransactionQuery.InvolvesAddress(alice) && TransactionQuery.MintsPolicy(policy),
          TransactionQuery.InvolvesAddress(alice) || TransactionQuery.InvolvesAddress(bob)
        )
        for q <- txQueries do
            val request = SubscriptionRequest.Transaction(q, opts)
            if SubscriptionSupport.of(request, caps) == SubscriptionSupport.Indexed then
                assert(
                  SubscriptionSupport.sourcesFor(request, caps).nonEmpty,
                  s"accepted but nothing to watch: $q"
                )
    }

    test("an intersection is watched through one arm, not both") {
        // Both arms would double this subscription's per-block request cost for the life of the
        // provider, on a backend the whole design exists to economise on. One covered arm answers
        // an intersection; the other post-filters data already in hand.
        val follower = new FakeFollower
        val p = providerWith(follower)
        val query = UtxoEventQuery(
          UtxoQuery(UtxoSource.FromAddress(alice) && UtxoSource.FromAddress(bob))
        )
        p.subscribeUtxoQuery(query, liveOnly)
        assert(
          follower.watching.size == 1,
          s"one address is enough to cover an intersection — got ${follower.watching}"
        )
    }

    /** Spin briefly: the driver's pump and the seed read complete on the execution context, not on
      * this thread, so a bare assertion would race them.
      *
      * The last evaluation is what is asserted on, rather than a fresh one. Some of these
      * conditions consume what they observe — a `pull` takes the event — and re-evaluating after
      * the loop would ask an emptied mailbox and fail on every condition that had just held.
      */
    private def eventually(condition: => Boolean, clue: String): Unit = {
        var remaining = 500
        var held = condition
        while remaining > 0 && !held do
            remaining -= 1
            Thread.sleep(10)
            held = condition
        assert(held, clue)
    }

    test("a failing feed fails every subscription, not just the first") {
        // The release hook of a terminating subscription shrinks the watched set, and a throw from
        // there aborts `failAll`'s fan-out partway — leaving every subscription after the first
        // parked on a promise with the feed already dead.
        val follower = new FakeFollower
        val p = providerWith(follower)
        val sources = Seq(alice, bob, alice).map(a => p.subscribeUtxoQuery(utxoQuery(a), liveOnly))
        val pulls = sources.map(_.pull())
        follower.mailbox.fail(
          new RuntimeException("the chain forked below the last reported block")
        )
        eventually(
          pulls.forall(_.value.exists(_.isFailure)),
          s"only some subscriptions were failed: ${pulls.map(_.value)}"
        )
    }

    test("starting is idempotent, so a second subscription does not run a second parameter loop") {
        val follower = new FakeFollower
        val delay = new SteppableDelay
        val p = new StreamingBlockfrostProvider(stubbedClient(), follower, 1.hour, delay)
        p.ensureStarted()
        p.ensureStarted()
        assert(
          delay.pending == 1,
          s"two parameter loops would double the refresh rate against a metered quota, and race " +
              s"each other's updates — got ${delay.pending}"
        )
    }

    test("one failed parameter refresh is retried rather than ending the feed") {
        // Parameters change at epoch boundaries five days apart, so a single failed hourly poll
        // cannot have made the held value wrong. Failing subscribers over it would be noise.
        val follower = new FakeFollower
        val delay = new SteppableDelay
        val client = failingParamsClient()
        val p = new StreamingBlockfrostProvider(client, follower, 1.hour, delay)
        p.ensureStarted()
        val params: ScalusAsyncSource[ProtocolParams] =
            p.subscribeProtocolParams()
        assert(params.pull().value.isDefined, "the current value is delivered on subscribe")
        delay.step()
        eventually(delay.pending == 1, "the loop should have scheduled its next attempt")
        assert(
          params.pull().value.isEmpty,
          "one failure must not fail the subscriber; the value it holds cannot yet be stale"
        )
    }

    test("a run of failed refreshes fails the subscribers and stops offering the feed") {
        val follower = new FakeFollower
        val delay = new SteppableDelay
        val p = new StreamingBlockfrostProvider(failingParamsClient(), follower, 1.hour, delay)
        p.ensureStarted()
        val params: ScalusAsyncSource[ProtocolParams] =
            p.subscribeProtocolParams()
        params.pull()
        for _ <- 1 to StreamingBlockfrostProvider.maxParamFailures do
            eventually(delay.pending == 1, "the loop should still be scheduling attempts")
            delay.step()
        eventually(
          params.pull().value.exists(_.isFailure),
          "by now the held value may genuinely be stale, and the subscriber must be told"
        )
        assertThrows[IllegalStateException](p.subscribeProtocolParams())
    }

    test("a closed provider serves no new tip or parameter subscriptions") {
        // Every other subscribe method refuses through `hub.require`; these two are not routed
        // through it, and would hand back a stream holding one stale value and then park forever.
        val p = providerWith(new FakeFollower)
        p.close()
        assertThrows[IllegalStateException](p.subscribeTip())
        assertThrows[IllegalStateException](p.subscribeProtocolParams())
    }

    // ── the feed follows demand ─────────────────────────────────────────────

    test("registering a subscription does not start the feed; consuming it does") {
        val follower = new FakeFollower
        val p =
            new StreamingBlockfrostProvider(stubbedClient(), follower, 1.hour, _ => Future.never)

        val events = p.subscribeUtxoQuery(utxoQuery(alice), liveOnly)
        assert(
          !follower.started,
          "a registered subscription nobody reads must cost nothing: on a metered backend the " +
              "poll is a request per interval whether or not the chain moved"
        )

        events.pull()
        assert(follower.started, "the first pull is what starts the feed")
    }

    test("a set of subscriptions registered before the first pull shares one starting position") {
        val follower = new FakeFollower
        val p =
            new StreamingBlockfrostProvider(stubbedClient(), follower, 1.hour, _ => Future.never)

        val a = p.subscribeUtxoQuery(utxoQuery(alice), liveOnly)
        val b = p.subscribeUtxoQuery(utxoQuery(bob), liveOnly)
        assert(!follower.started, "registration alone starts nothing")

        // Whichever is read first starts the feed, and by then both are registered — which is the
        // whole reason the trigger is the pull and not the subscribe.
        a.pull()
        assert(follower.started)
        assert(
          follower.watching == Set(alice, bob),
          s"both were watched before anything was observed; got ${follower.watching}"
        )
        b.pull()
    }

    test("the last subscription going away idles the feed, and a later read resumes it") {
        val follower = new FakeFollower
        val p =
            new StreamingBlockfrostProvider(stubbedClient(), follower, 1.hour, _ => Future.never)

        val events = p.subscribeUtxoQuery(utxoQuery(alice), liveOnly)
        events.pull()
        assert(follower.observing.contains(true))

        events.cancel()
        assert(
          follower.observing.contains(false),
          "a cached view whose readers have all gone must stop spending the quota"
        )

        val again = p.subscribeUtxoQuery(utxoQuery(alice), liveOnly)
        again.pull()
        assert(
          follower.observing.contains(true),
          "idling is not closing: the view is cached on the provider and must serve a later " +
              "subscription, which a closed follower could never do"
        )
    }

    test("cancelling twice does not idle a feed another subscription is still reading") {
        val follower = new FakeFollower
        val p =
            new StreamingBlockfrostProvider(stubbedClient(), follower, 1.hour, _ => Future.never)

        val first = p.subscribeUtxoQuery(utxoQuery(alice), liveOnly)
        val second = p.subscribeUtxoQuery(utxoQuery(bob), liveOnly)
        first.pull()

        first.cancel()
        first.cancel() // idempotent by ScalusAsyncSource's contract
        assert(
          follower.observing.contains(true),
          "a double cancel must not drive the live count negative and idle a feed `second` is " +
              "still reading"
        )

        second.cancel()
        assert(follower.observing.contains(false))
    }
}
