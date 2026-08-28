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

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

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
            calls = calls :+ sources
            ChainPoint.origin
        }
        def close(): Unit = { closed = true; mailbox.close() }

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
    private def stubbedClient(
        utxosFor: Address => String = _ => "[]",
        onSnapshotRequest: () => Unit = () => ()
    ): BlockfrostProvider = {
        given Backend[Future] = BackendStub.asynchronousFuture.whenAnyRequest
            .thenRespondAdjust {
                onSnapshotRequest()
                "[]"
            }
        new BlockfrostProvider("stub-key", "http://stub.invalid", 5, CardanoInfo.mainnet)
    }

    private def providerWith(
        follower: FakeFollower,
        client: BlockfrostProvider = stubbedClient()
    ): StreamingBlockfrostProvider = {
        val p = new StreamingBlockfrostProvider(client, follower, 1.hour, _ => Future.never)
        p.start()
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
          p.subscribeBlockQuery[ScalusAsyncSource](BlockQuery.All, liveOnly)
        )
    }

    test("a transaction-status subscription is refused rather than left Pending forever") {
        val p = providerWith(new FakeFollower)
        assertThrows[UnsupportedSubscriptionException](
          p.subscribeTransactionStatus[ScalusAsyncSource](txHash("ab"))
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
          p.subscribeTransactionQuery[ScalusAsyncSource](TransactionQuery.All, consenting)
        )
        val byAsset = UtxoEventQuery(
          UtxoQuery(UtxoSource.FromAsset(PolicyId.fromHex("00" * 28), AssetName.fromHex("cafe"))),
          UtxoEventType.all
        )
        assertThrows[UnsupportedSubscriptionException](
          p.subscribeUtxoQuery[ScalusAsyncSource](byAsset, consenting)
        )
    }

    test("an address Blockfrost cannot be asked about is refused at subscribe") {
        // Not in the poll loop: the follower feeds every subscriber, so one unwatchable address
        // discovered there would fail all of them for a mistake only one of them made.
        val follower = new FakeFollower
        val p = providerWith(follower)
        assertThrows[UnsupportedSubscriptionException](
          p.subscribeUtxoQuery[ScalusAsyncSource](utxoQuery(byron), liveOnly)
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
        p.start()
        p.subscribeUtxoQuery[ScalusAsyncSource](utxoQuery(alice), SubscriptionOptions())
        eventually(
          watchedWhenRead.exists(_ > 0),
          s"the snapshot was read before any watch landed: $watchedWhenRead"
        )
    }

    test("the follower is always given the union, never one subscription's own sources") {
        val follower = new FakeFollower
        val p = providerWith(follower)
        p.subscribeUtxoQuery[ScalusAsyncSource](utxoQuery(alice), liveOnly)
        p.subscribeUtxoQuery[ScalusAsyncSource](utxoQuery(bob), liveOnly)
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
        val aliceSub = p.subscribeUtxoQuery[ScalusAsyncSource](utxoQuery(alice), liveOnly)
        p.subscribeUtxoQuery[ScalusAsyncSource](utxoQuery(bob), liveOnly)
        aliceSub.cancel()
        assert(follower.watching == Set(bob), s"got ${follower.watching}")
    }

    test("a transaction subscription watches the addresses its query names") {
        val follower = new FakeFollower
        val p = providerWith(follower)
        p.subscribeTransactionQuery[ScalusAsyncSource](
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
            p.subscribeUtxoQuery[ScalusAsyncSource](utxoQuery(alice), liveOnly)
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
          src.pull().value.flatMap(_.toOption).flatten.exists {
              case UtxoEvent.Created(u, _, _) => u.input == TransactionInput(txHash("01"), 0)
              case _                          => false
          },
          "the driver pumps the follower's events into the hub; nothing arrived"
        )
    }

    test("close stops the follower and ends every subscription") {
        val follower = new FakeFollower
        val p = providerWith(follower)
        val src: ScalusAsyncSource[UtxoEvent] =
            p.subscribeUtxoQuery[ScalusAsyncSource](utxoQuery(alice), liveOnly)
        p.close()
        assert(follower.closed, "a metered follower left running keeps spending quota")
        assert(
          src.pull().value.exists(_.contains(None)),
          "a close that left subscribers parked on promises would leak what it was called to free"
        )
    }

    test("cancelling after close does not try to re-watch a stopped follower") {
        val follower = new FakeFollower
        val p = providerWith(follower)
        val src: ScalusAsyncSource[UtxoEvent] =
            p.subscribeUtxoQuery[ScalusAsyncSource](utxoQuery(alice), liveOnly)
        p.close()
        src.cancel()
        assert(follower.closed)
    }

    // ── the extraction agrees with the classifier ───────────────────────────

    test("every query this provider accepts names at least one address to watch") {
        // The two have to agree about what "indexed by address" means. If they drift, a
        // subscription is accepted that no block can ever cover: no events, no Idle, no error.
        val policy = PolicyId.fromHex("00" * 28)
        val name = AssetName.fromHex("cafe")
        val utxoQueries = Seq(
          UtxoQuery(UtxoSource.FromAddress(alice)),
          UtxoQuery(UtxoSource.FromAddress(alice) || UtxoSource.FromAddress(bob)),
          UtxoQuery(UtxoSource.FromAddress(alice) && UtxoSource.FromAsset(policy, name)),
          UtxoQuery(UtxoSource.FromAsset(policy, name) && UtxoSource.FromAddress(bob)),
          UtxoQuery(UtxoSource.FromAddress(alice)) || UtxoQuery(UtxoSource.FromAddress(bob))
        )
        for q <- utxoQueries do
            val request = SubscriptionRequest.Utxo(UtxoEventQuery(q), SubscriptionOptions())
            if SubscriptionSupport.of(request, StreamingBlockfrostProvider.capabilities) ==
                    SubscriptionSupport.Indexed
            then
                assert(
                  StreamingBlockfrostProvider.utxoQuerySources(q).nonEmpty,
                  s"accepted but nothing to watch: $q"
                )

        val txQueries = Seq(
          TransactionQuery.InvolvesAddress(alice),
          TransactionQuery.InvolvesAddress(alice) && TransactionQuery.MintsPolicy(policy),
          TransactionQuery.InvolvesAddress(alice) || TransactionQuery.InvolvesAddress(bob)
        )
        for q <- txQueries do
            val request = SubscriptionRequest.Transaction(q, SubscriptionOptions())
            if SubscriptionSupport.of(request, StreamingBlockfrostProvider.capabilities) ==
                    SubscriptionSupport.Indexed
            then
                assert(
                  StreamingBlockfrostProvider.transactionQuerySources(q).nonEmpty,
                  s"accepted but nothing to watch: $q"
                )
    }

    /** Spin briefly: the driver's pump and the seed read complete on the execution context, not on
      * this thread, so a bare assertion would race them.
      */
    private def eventually(condition: => Boolean, clue: String): Unit = {
        var remaining = 500
        while remaining > 0 && !condition do
            remaining -= 1
            Thread.sleep(10)
        assert(condition, clue)
    }
}
