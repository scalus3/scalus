package scalus.testing.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.infra.UnsupportedSubscriptionException
import scalus.cardano.ledger.{TransactionHash, Value}
import scalus.cardano.node.stream.*
import scalus.cardano.node.{BlockchainReader, UtxoQuery, UtxoSource}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

/** What a [[BlockchainStreaming]] must do, whatever it is backed by.
  *
  * The point of the facade is that application code moves between an emulator, a REST-backed
  * provider and a full indexer by changing construction and nothing else. That promise needs a
  * test, not a paragraph: every implementation runs this suite, including implementations living
  * outside this repository.
  *
  * The suite reads the provider's own [[StreamCapabilities]] and holds it to what it declared — so
  * it does not merely check that a provider works, it checks that its self-description is
  * **honest**. A provider claiming it never signals rollbacks must not emit one; a provider
  * claiming it cannot replay must refuse `StartFrom.At`; a provider claiming a query kind is
  * indexed must accept it without `allowUnindexedScan`. That is what makes capabilities safe to act
  * on.
  */
abstract class StreamProviderConformance extends AnyFunSuite {

    /** A fresh provider and a way to make the chain move. */
    protected def newFixture(): StreamConformanceFixture

    /** How long to wait for an event that should already be on its way. */
    protected def patience: FiniteDuration = 5.seconds

    /** A reader that never drops an event.
      *
      * `pull` is a commitment, not a poll: on an empty source it installs a waiter that the next
      * offered event completes. Calling `pull` and discarding the future would therefore route the
      * next event into a promise nobody holds — invisible against a provider that delivers
      * synchronously, and silent data loss against every remote one this suite exists to certify.
      * So the outstanding future is kept and re-inspected instead.
      */
    protected final class Reader[A](val source: ScalusAsyncSource[A]) {
        private var outstanding: Option[Future[Option[A]]] = None

        private def current(): Future[Option[A]] = {
            val f = outstanding.getOrElse(source.pull())
            outstanding = Some(f)
            f
        }

        /** Wait for the next event. */
        def next(): A = {
            val value = Await.result(current(), patience)
            outstanding = None
            value.getOrElse(fail("stream ended while awaiting an event"))
        }

        /** The next event if one has already arrived; does not wait for one that has not. */
        def peek(): Option[A] = current().value match
            case Some(Success(value)) =>
                outstanding = None
                value
            case Some(Failure(e)) => throw e
            case None             => None

        /** Everything already delivered. */
        def drain(): List[A] = {
            val buf = List.newBuilder[A]
            var more = true
            while more do
                peek() match
                    case Some(a) => buf += a
                    case None    => more = false
            buf.result()
        }

        def isEnded: Boolean = Await.result(current(), patience).isEmpty

        def cancel(): Unit = source.cancel()
    }

    private def withFixture(body: StreamConformanceFixture => Unit): Unit = {
        val fixture = newFixture()
        try body(fixture)
        finally fixture.close()
    }

    // ── the throw law ───────────────────────────────────────────────────────

    test("subscribe refuses exactly what SubscriptionSupport refuses") {
        withFixture { f =>
            val caps = f.provider.streamCapabilities
            val requests = candidateRequests(f)
            requests.foreach { request =>
                val verdict = SubscriptionSupport.of(request, caps)
                val refused = attemptSubscribe(f, request).isLeft
                val shouldRefuse = verdict match
                    case SubscriptionSupport.Unsupported(_) => true
                    case SubscriptionSupport.Unindexed      => !request.options.allowUnindexedScan
                    case SubscriptionSupport.Indexed        => false
                assert(
                  refused == shouldRefuse,
                  s"SubscriptionSupport.of said $verdict but subscribe " +
                      s"${if refused then "threw" else "succeeded"} for $request"
                )
            }
        }
    }

    test("a subscription kind that is not declared is refused") {
        withFixture { f =>
            val caps = f.provider.streamCapabilities
            SubscriptionKind.all.diff(caps.kinds).foreach { kind =>
                val request = requestOfKind(kind, f)
                assert(
                  attemptSubscribe(f, request).isLeft,
                  s"provider does not declare $kind but accepted the subscription"
                )
            }
        }
    }

    test("StartFrom.At is refused unless capabilities allow replay") {
        withFixture { f =>
            val caps = f.provider.streamCapabilities
            val opts = SubscriptionOptions(startFrom = StartFrom.At(ChainPoint.origin))
            val request = SubscriptionRequest.Utxo(addressQuery(f.freshAddress()), opts)
            val expectRefusal = SubscriptionSupport.of(request, caps) match
                case SubscriptionSupport.Unsupported(_) => true
                case _                                  => false
            assert(attemptSubscribe(f, request).isLeft == expectRefusal)
        }
    }

    // ── delivery ────────────────────────────────────────────────────────────

    test("subscribe then submit on the same thread is race-free") {
        withFixture { f =>
            val target = f.freshAddress()
            val events = subscribeUtxo(f, target, SubscriptionOptions(includeExistingUtxos = false))
            val hash = f.payTo(target, Value.ada(10))
            events.next() match
                case UtxoEvent.Created(utxo, producedBy, _) =>
                    assert(producedBy == hash)
                    assert(utxo.output.address == target)
                case other => fail(s"expected a Created event, got $other")
        }
    }

    test("an existing UTxO is seeded before live events") {
        withFixture { f =>
            val target = f.freshAddress()
            f.payTo(target, Value.ada(10))

            val events = subscribeUtxo(f, target, SubscriptionOptions(includeExistingUtxos = true))
            val seeded = events.next()
            assert(
              seeded.isInstanceOf[UtxoEvent.Created],
              s"seed should arrive as Created, got $seeded"
            )

            val second = f.payTo(target, Value.ada(5))
            val live = events.next()
            live match
                case UtxoEvent.Created(_, producedBy, _) => assert(producedBy == second)
                case other => fail(s"expected the live Created event, got $other")
        }
    }

    test("includeExistingUtxos = false yields a live-only stream") {
        withFixture { f =>
            val target = f.freshAddress()
            f.payTo(target, Value.ada(10))
            val events = subscribeUtxo(f, target, SubscriptionOptions(includeExistingUtxos = false))
            assert(events.peek().isEmpty, "a live-only subscription must not replay existing UTxOs")
        }
    }

    test("spending a watched UTxO produces Spent after its Created") {
        withFixture { f =>
            val target = f.freshAddress()
            val events = subscribeUtxo(f, target, SubscriptionOptions(includeExistingUtxos = false))
            f.payTo(target, Value.ada(20))
            val created = events.next() match
                case c: UtxoEvent.Created => c
                case other                => fail(s"expected Created, got $other")

            f.spendFrom(target, Value.ada(5))
            val spent = events.next() match
                case s: UtxoEvent.Spent => s
                case other              => fail(s"expected Spent, got $other")
            assert(spent.utxo.input == created.utxo.input, "Spent must retract the created UTxO")
        }
    }

    test("cancelling a subscription stops delivery and ends the stream") {
        withFixture { f =>
            val target = f.freshAddress()
            val events = subscribeUtxo(f, target, SubscriptionOptions(includeExistingUtxos = false))
            events.cancel()
            f.payTo(target, Value.ada(10))
            assert(
              events.isEnded,
              "a cancelled subscription must end rather than keep delivering"
            )
        }
    }

    // ── latest-value streams ────────────────────────────────────────────────

    test("tip is delivered on subscribe and advances with the chain") {
        withFixture { f =>
            val tips = Reader(f.provider.subscribeTip())
            val first = tips.next()
            f.payTo(f.freshAddress(), Value.ada(10))
            val second = tips.next()
            assert(
              second.blockNo > first.blockNo,
              s"tip should advance: $first then $second"
            )
        }
    }

    test("transaction status follows the transaction on chain") {
        withFixture { f =>
            assume(
              f.provider.streamCapabilities.kinds.contains(SubscriptionKind.TransactionStatus),
              "provider does not declare TransactionStatus"
            )
            val target = f.freshAddress()
            val hash = f.payTo(target, Value.ada(10))
            val statuses =
                Reader(f.provider.subscribeTransactionStatus(hash))
            assert(
              statuses.next() == scalus.cardano.node.TransactionStatus.Confirmed,
              "a transaction already in a block should read as Confirmed"
            )
        }
    }

    test("a transaction that was never submitted is not reported as confirmed") {
        withFixture { f =>
            val unknown = TransactionHash.fromByteString(
              scalus.uplc.builtin.ByteString.fromArray(Array.fill[Byte](32)(0x7f))
            )
            val status = Await.result(f.reader.checkTransaction(unknown), patience)
            assert(
              status != scalus.cardano.node.TransactionStatus.Confirmed,
              "reporting an unsubmitted transaction as Confirmed makes submitAndPoll report " +
                  s"success for something that never reached the chain, got $status"
            )
        }
    }

    /** The same claim, for a transaction that predates the subscription.
      *
      * `ignore`d because it fails: the hub seeds a new status subscription from its own table and
      * defaults to `NotFound`, and a transaction applied before the hub was following it is absent
      * from that table — permanently, since nothing later revises a block already in the past. The
      * subscriber cannot tell that from a transaction which genuinely never reached the chain.
      *
      * Kept rather than deleted because the test above cannot catch it: that one pays *after*
      * building the view, and the ordering is the whole bug. See scalus3/scalus#358; un-ignore with
      * the fix.
      */
    ignore("the one-shot status and its subscription agree for an earlier transaction") {
        withFixture { f =>
            assume(
              f.provider.streamCapabilities.kinds.contains(SubscriptionKind.TransactionStatus),
              "provider does not declare TransactionStatus"
            )
            // Applied before anything subscribes to it.
            val hash = f.payTo(f.freshAddress(), Value.ada(10))
            val oneShot = Await.result(f.reader.checkTransaction(hash), patience)
            val subscribed = Reader(f.provider.subscribeTransactionStatus(hash)).next()
            assert(
              oneShot == subscribed,
              s"a one-shot read is the head of its own subscription; got $oneShot vs $subscribed"
            )
        }
    }

    test("the one-shot status and its subscription agree") {
        withFixture { f =>
            assume(
              f.provider.streamCapabilities.kinds.contains(SubscriptionKind.TransactionStatus),
              "provider does not declare TransactionStatus"
            )
            val hash = f.payTo(f.freshAddress(), Value.ada(10))
            val subscribed =
                Reader(f.provider.subscribeTransactionStatus(hash)).next()
            val oneShot = Await.result(f.reader.checkTransaction(hash), patience)
            assert(
              oneShot == subscribed,
              s"a one-shot read is the head of its own subscription; got $oneShot vs $subscribed"
            )
        }
    }

    // ── honesty of the declaration ──────────────────────────────────────────

    test("a provider declaring no rollback horizon never emits RolledBack") {
        withFixture { f =>
            if f.provider.streamCapabilities.rollbackHorizon.isEmpty then
                val target = f.freshAddress()
                val events =
                    subscribeUtxo(f, target, SubscriptionOptions(includeExistingUtxos = false))
                (1 to 3).foreach(_ => f.payTo(target, Value.ada(2)))
                val observed = events.drain()
                assert(
                  !observed.exists(_.isInstanceOf[UtxoEvent.RolledBack]),
                  s"provider declared rollbackHorizon = None but emitted a rollback: $observed"
                )
        }
    }

    test("a provider that does not declare idle signals never emits one") {
        withFixture { f =>
            val watched = f.freshAddress()
            val unrelated = f.freshAddress()
            val opts = SubscriptionOptions(includeExistingUtxos = false, idleSignals = true)
            val events = subscribeUtxo(f, watched, opts)
            (1 to 3).foreach(_ => f.payTo(unrelated, Value.ada(3)))
            val observed = events.drain()
            // Only the negative direction is a contract. `UtxoEvent.Idle` is explicitly emitted at
            // provider discretion, so a provider that reports progress every tenth block, or on a
            // timer, is behaving correctly — demanding one for the very next non-matching block
            // would fail it for no reason.
            if !f.provider.streamCapabilities.idleSignals then
                assert(
                  !observed.exists(_.isInstanceOf[UtxoEvent.Idle]),
                  s"provider does not declare idle signals but emitted one: $observed"
                )
            assert(
              !observed.exists {
                  case _: UtxoEvent.Created => true
                  case _: UtxoEvent.Spent   => true
                  case _                    => false
              },
              s"a subscription must not receive another address's events: $observed"
            )
        }
    }

    test("an idle signal, when emitted, marks a real position on the chain") {
        withFixture { f =>
            if f.provider.streamCapabilities.idleSignals then
                val watched = f.freshAddress()
                val unrelated = f.freshAddress()
                val opts = SubscriptionOptions(includeExistingUtxos = false, idleSignals = true)
                val events = subscribeUtxo(f, watched, opts)
                (1 to 3).foreach(_ => f.payTo(unrelated, Value.ada(3)))
                val idles = events.drain().collect { case i: UtxoEvent.Idle => i.at }
                assert(
                  idles == idles.distinct,
                  s"an idle signal is a checkpointable position, so it must advance: $idles"
                )
        }
    }

    // ── helpers ─────────────────────────────────────────────────────────────

    private def addressQuery(address: Address): UtxoEventQuery =
        UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(address)))

    private def subscribeUtxo(
        f: StreamConformanceFixture,
        address: Address,
        opts: SubscriptionOptions
    ): Reader[UtxoEvent] =
        // The expected type is what picks `C`; the suite deliberately works at the
        // ScalusAsyncSource level so it needs no stream library to test one.
        Reader(f.provider.subscribeUtxoQuery(addressQuery(address), opts))

    private def requestOfKind(
        kind: SubscriptionKind,
        f: StreamConformanceFixture
    ): SubscriptionRequest = kind match
        case SubscriptionKind.Utxo =>
            SubscriptionRequest.Utxo(addressQuery(f.freshAddress()), SubscriptionOptions())
        case SubscriptionKind.Transaction =>
            SubscriptionRequest.Transaction(
              TransactionQuery.InvolvesAddress(f.freshAddress()),
              SubscriptionOptions()
            )
        case SubscriptionKind.Block =>
            SubscriptionRequest.Block(BlockQuery.All, SubscriptionOptions())
        case SubscriptionKind.TransactionStatus =>
            SubscriptionRequest.TransactionStatus(f.payTo(f.freshAddress(), Value.ada(1)))

    /** Every shape worth classifying: indexed, unindexed, and unindexed-but-accepted. */
    private def candidateRequests(f: StreamConformanceFixture): Seq[SubscriptionRequest] = {
        val address = f.freshAddress()
        Seq(
          SubscriptionRequest.Utxo(addressQuery(address), SubscriptionOptions()),
          SubscriptionRequest.Transaction(TransactionQuery.All, SubscriptionOptions()),
          SubscriptionRequest.Transaction(
            TransactionQuery.All,
            SubscriptionOptions(allowUnindexedScan = true)
          ),
          SubscriptionRequest.Transaction(
            TransactionQuery.InvolvesAddress(address),
            SubscriptionOptions()
          )
        )
    }

    private def attemptSubscribe(
        f: StreamConformanceFixture,
        request: SubscriptionRequest
    ): Either[UnsupportedSubscriptionException, Unit] = {
        val p = f.provider
        try
            request match
                case SubscriptionRequest.Utxo(q, o) => p.subscribeUtxoQuery(q, o).cancel()
                case SubscriptionRequest.Transaction(q, o) =>
                    p.subscribeTransactionQuery(q, o).cancel()
                case SubscriptionRequest.Block(q, o) => p.subscribeBlockQuery(q, o).cancel()
                case SubscriptionRequest.TransactionStatus(h) =>
                    p.subscribeTransactionStatus(h).cancel()
            Right(())
        catch case e: UnsupportedSubscriptionException => Left(e)
    }
}

/** The provider under test, plus the minimum needed to make its chain move.
  *
  * Kept deliberately small: anything richer would be expressible only by some backends, and a
  * conformance suite that only the emulator can run is not a conformance suite.
  */
trait StreamConformanceFixture {

    def provider: BlockchainStreaming

    /** The provider the streaming view came from.
      *
      * Streaming no longer serves one-shot reads — they stay on the provider a caller already holds
      * — so the two suites below that compare a read against its subscription need both halves.
      * Supplying it is a fixture's job, not a widening of the streaming API.
      */
    def reader: BlockchainReader

    /** An address the fixture can spend from. */
    def payer: Address

    /** An address nothing has paid yet, distinct on each call. */
    def freshAddress(): Address

    /** Apply one transaction paying `amount` to `address`, and return its hash. */
    def payTo(address: Address, amount: Value): TransactionHash

    /** Spend from `address`, sending `amount` onwards. Used to observe `Spent` events. */
    def spendFrom(address: Address, amount: Value): TransactionHash

    def close(): Unit
}
