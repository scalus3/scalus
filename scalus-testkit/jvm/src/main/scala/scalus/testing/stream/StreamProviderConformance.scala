package scalus.testing.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.infra.UnsupportedSubscriptionException
import scalus.cardano.ledger.{TransactionHash, Value}
import scalus.cardano.node.stream.*
import scalus.cardano.node.{UtxoQuery, UtxoSource}

import scala.concurrent.Await
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** What a [[BlockchainStreamProvider]] must do, whatever it is backed by.
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

    protected def next[A](src: ScalusAsyncSource[A]): A =
        Await.result(src.pull(), patience).getOrElse(fail("stream ended while awaiting an event"))

    /** The next event if one is already available, without waiting for one that is not. */
    protected def peek[A](src: ScalusAsyncSource[A]): Option[A] =
        src.pull().value.flatMap(_.toOption).flatten

    protected def drain[A](src: ScalusAsyncSource[A]): List[A] = {
        val buf = List.newBuilder[A]
        var more = true
        while more do
            peek(src) match
                case Some(a) => buf += a
                case None    => more = false
        buf.result()
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
            next(events) match
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
            val seeded = next(events)
            assert(
              seeded.isInstanceOf[UtxoEvent.Created],
              s"seed should arrive as Created, got $seeded"
            )

            val second = f.payTo(target, Value.ada(5))
            val live = next(events)
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
            assert(peek(events).isEmpty, "a live-only subscription must not replay existing UTxOs")
        }
    }

    test("spending a watched UTxO produces Spent after its Created") {
        withFixture { f =>
            val target = f.freshAddress()
            val events = subscribeUtxo(f, target, SubscriptionOptions(includeExistingUtxos = false))
            f.payTo(target, Value.ada(20))
            val created = next(events) match
                case c: UtxoEvent.Created => c
                case other                => fail(s"expected Created, got $other")

            f.spendFrom(target, Value.ada(5))
            val spent = next(events) match
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
              Await.result(events.pull(), patience).isEmpty,
              "a cancelled subscription must end rather than keep delivering"
            )
        }
    }

    // ── latest-value streams ────────────────────────────────────────────────

    test("tip is delivered on subscribe and advances with the chain") {
        withFixture { f =>
            val tips: ScalusAsyncSource[ChainTip] = f.provider.subscribeTip()
            val first = next(tips)
            f.payTo(f.freshAddress(), Value.ada(10))
            val second = next(tips)
            assert(
              second.blockNo > first.blockNo,
              s"tip should advance: $first then $second"
            )
        }
    }

    test("transaction status follows the transaction on chain") {
        withFixture { f =>
            val target = f.freshAddress()
            val hash = f.payTo(target, Value.ada(10))
            val statuses: ScalusAsyncSource[scalus.cardano.node.TransactionStatus] =
                f.provider.subscribeTransactionStatus(hash)
            assert(
              next(statuses) == scalus.cardano.node.TransactionStatus.Confirmed,
              "a transaction already in a block should read as Confirmed"
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
                val observed = drain(events)
                assert(
                  !observed.exists(_.isInstanceOf[UtxoEvent.RolledBack]),
                  s"provider declared rollbackHorizon = None but emitted a rollback: $observed"
                )
        }
    }

    test("idle signals appear only when both provider and subscription allow them") {
        withFixture { f =>
            val watched = f.freshAddress()
            val unrelated = f.freshAddress()
            val opts = SubscriptionOptions(includeExistingUtxos = false, idleSignals = true)
            val events = subscribeUtxo(f, watched, opts)
            f.payTo(unrelated, Value.ada(3))
            val observed = drain(events)
            if f.provider.streamCapabilities.idleSignals then
                assert(
                  observed.exists(_.isInstanceOf[UtxoEvent.Idle]),
                  "provider declares idle signals but produced none for a non-matching block"
                )
            else assert(observed.isEmpty, "provider does not declare idle signals but emitted some")
        }
    }

    // ── helpers ─────────────────────────────────────────────────────────────

    private def addressQuery(address: Address): UtxoEventQuery =
        UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(address)))

    private def subscribeUtxo(
        f: StreamConformanceFixture,
        address: Address,
        opts: SubscriptionOptions
    ): ScalusAsyncSource[UtxoEvent] =
        // The declared return type is what picks `C`; the suite deliberately works at the
        // ScalusAsyncSource level so it needs no stream library to test one.
        f.provider.subscribeUtxoQuery(addressQuery(address), opts)

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

    def provider: BlockchainStreamProvider

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
