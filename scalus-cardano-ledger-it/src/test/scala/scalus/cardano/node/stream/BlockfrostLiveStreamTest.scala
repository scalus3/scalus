package scalus.cardano.node.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.node.{UtxoQuery, UtxoSource}
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.integration.IntegrationTest
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

/** The streaming provider against a real chain, end to end.
  *
  * Everything else covering this provider is a fake follower or a stubbed HTTP backend, and
  * `BlockfrostStreamingEndpointsTest` only checks that the endpoints behave. This is the one test
  * that puts the whole pipeline together — REST client, poll loop, driver, hub, subscription — and
  * makes the chain move underneath it.
  *
  * It also exercises the API shape the redesign is for: the streaming view comes from the provider
  * the test already has, `ctx.provider.streaming()`, and submission stays on that provider.
  *
  * Costs one preprod transaction and a few minutes; needs `BLOCKFROST_API_KEY` and
  * `WALLET_MNEMONIC_PREPROD` with a funded first party.
  *
  * Run with: {{{
  *   SCALUS_TEST_ENV=preprod BLOCKFROST_API_KEY=... WALLET_MNEMONIC_PREPROD=... \
  *     sbt "scalusCardanoLedgerIt/testOnly *BlockfrostLiveStreamTest"
  * }}}
  */
class BlockfrostLiveStreamTest extends AnyFunSuite with IntegrationTest {

    private val patience = 6.minutes

    test("a payment to a watched address arrives on a live subscription") {
        assume(testEnvName == "preprod", "meaningful only against a real chain")
        val ctx = createTestContext()
        val target = ctx.bob.address

        // The streaming view is asked of the provider the test already holds — no second kind of
        // provider, and nothing is polled until the subscription below is read.
        val streaming = ctx.provider.streaming()

        val events = streaming.subscribeUtxoQuery(
          UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(target)), UtxoEventType.all),
          SubscriptionOptions(includeExistingUtxos = false)
        )

        // Start observing *before* paying, so the payment cannot land at or below the tip the
        // follower starts from. This first pull is what starts the feed.
        val first = events.pull()

        val tx = TxBuilder(ctx.cardanoInfo)
            .payTo(target, Value.ada(5))
            .complete(ctx.provider, ctx.alice.address)
            .await(60.seconds)
            .sign(ctx.alice.signer)
            .transaction
        val hash = ctx.provider.submit(tx).await(120.seconds) match
            case Right(h)    => h
            case Left(error) => fail(s"could not submit the fixture payment: $error")
        info(s"submitted ${hash.toHex}; waiting for it on the subscription")

        def awaitCreated(pending: scala.concurrent.Future[Option[UtxoEvent]], left: Int): UtxoEvent =
            if left == 0 then fail(s"no Created event for ${hash.toHex} within $patience")
            else
                scala.concurrent.Await.result(pending, patience) match
                    case Some(e @ UtxoEvent.Created(_, producedBy, _)) if producedBy == hash => e
                    case Some(other) =>
                        info(s"  (ignoring $other)")
                        awaitCreated(events.pull(), left - 1)
                    case None => fail("the subscription ended before the payment arrived")

        val created = awaitCreated(first, 50)
        created match
            case UtxoEvent.Created(utxo, producedBy, at) =>
                assert(producedBy == hash)
                assert(utxo.output.address == target, "the event must name the watched address")
                assert(at.slot > 0, "the event must be placed on the chain")
                info(s"observed at slot ${at.slot}, block ${at.blockHash.toHex.take(12)}")
            case other => fail(s"expected Created, got $other")

        events.cancel()
        streaming.close().await(30.seconds)
    }
}
