package scalus.cardano.node.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.blockfrost.TransactionInfo
import scalus.cardano.ledger.*
import scalus.cardano.node.stream.internal.*
import scalus.cardano.node.{BlockfrostProvider, BlockfrostProviderPlatform, UtxoSource}
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.integration.YaciTestContext
import scalus.testing.yaci.YaciDevKit
import scalus.uplc.builtin.ByteString
import scalus.utils.await

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}

/** Does Yaci Store actually serve what the streaming provider polls for?
  *
  * The provider's whole cost argument — one request per watched address per block, rather than a
  * block scan — rests on two endpoint behaviours that the existing `BlockfrostEndpointsTest` does
  * not cover: it runs against Preprod only, and it exercises `page`/`count` rather than the things
  * below. Both fail *silently* if the backend does not honour them, which is why they are checked
  * before anything is built on top:
  *
  *   - **`/addresses/{addr}/transactions` must honour `from`/`to`.** If the range is ignored, every
  *     poll returns the address's entire history and the follower reports long-settled
  *     transactions as new, for ever.
  *   - **`/blocks/{hash}/next` must 404 for a block it no longer has.** That 404 is the only reorg
  *     signal this provider has; served as an empty page instead, a follower sits at an orphaned
  *     position reporting nothing while the chain moves on.
  *
  * ## Three of these are `ignore`d, and must not be weakened into passing
  *
  * The store bundled in `yaci-cli:0.12.0-beta5` fails both checks above, so the two that assert
  * them — and the end-to-end follower test that depends on both — are `ignore`d rather than
  * deleted. They are the executable statement of what a backend has to do before this provider
  * can run against it, and the first one's *negative control* is the only thing that catches a
  * silently-ignored range filter: a test asserting only that the payment appears in a query for
  * its own block passes against a backend that ignores the range entirely.
  *
  * This is a bundling gap rather than a missing feature, which is why the tests are kept whole
  * and waiting. Upstream `yaci-store` implements the range end to end —
  * `BFAddressController.getAddressTransactions` declares `from`/`to`,
  * `BFAddressStorageReaderImpl.findAddressTransactions` applies it — in the `extensions/blockfrost`
  * module, which is absent at `v2.0.2` and present at `v3.0.0-beta3`. Spring drops undeclared
  * query parameters silently, which is exactly what the probe observed. Tracked as
  * [[https://github.com/bloxbean/yaci-devkit/issues/187 yaci-devkit#187]]; un-ignore them when the
  * devkit's bundled store moves to the v3 line.
  *
  * Two further gaps sit behind them, both upstream and both on `/txs/{hash}/cbor`, which the
  * follower needs for the transaction body: it requires *two* coupled flags rather than the one
  * its 404 names (`store.transaction.save-cbor` plus `store.cardano.return-tx-body-cbor` —
  * yaci-store#1086), and it served the transaction *body* rather than the full four-element
  * transaction, which `Transaction.fromCbor` cannot parse (yaci-store#1090, fixed upstream
  * 2026-08-25).
  *
  * Run with: {{{ sbt "scalusCardanoLedgerIt/testOnly *YaciStreamingEndpointsTest" }}}
  */
class YaciStreamingEndpointsTest extends AnyFunSuite with YaciDevKit {

    private given ExecutionContext = scala.concurrent.ExecutionContext.global

    private val timeout = 30.seconds

    private lazy val ctx: YaciTestContext = createYaciContext()

    /** A client of our own rather than `ctx.provider`, which is typed as the one-shot interface —
      * these tests are about the Blockfrost-specific endpoints underneath it.
      */
    private lazy val bf: BlockfrostProvider = BlockfrostProvider
        .localYaci(
          container.getYaciStoreApiUrl.stripSuffix("/"),
          container.getLocalClusterApiUrl.stripSuffix("/")
        )
        .await(60.seconds)

    /** An address with no history at all.
      *
      * Random rather than a test party's, because the negative control below asks "did this
      * transaction appear outside its own block" — and an address that already had transactions
      * would make that question unanswerable.
      */
    private def freshAddress(): ShelleyAddress = {
        val bytes = new Array[Byte](28)
        scala.util.Random.nextBytes(bytes)
        val keyHash: AddrKeyHash = Hash(ByteString.fromArray(bytes))
        ShelleyAddress(Network.Testnet, ShelleyPaymentPart.Key(keyHash), ShelleyDelegationPart.Null)
    }

    private def payTo(address: ShelleyAddress, amount: Value): TransactionHash = {
        val tx = TxBuilder(ctx.cardanoInfo)
            .payTo(address, amount)
            .complete(ctx.provider, ctx.alice.address)
            .await(timeout)
            .sign(ctx.alice.signer)
            .transaction
        ctx.submitTx(tx) match
            case Right(_)    => tx.id
            case Left(error) => fail(s"fixture payment failed to submit: $error")
    }

    /** Wait until Yaci Store has indexed the transaction, which lags block production. */
    private def awaitIndexed(hash: TransactionHash): TransactionInfo = {
        val deadline = System.currentTimeMillis() + 90_000
        def attempt(): Option[TransactionInfo] =
            Await.ready(bf.fetchTransactionInfo(hash.toHex), timeout).value.flatMap(_.toOption)
        var found = attempt()
        while found.isEmpty && System.currentTimeMillis() < deadline do
            Thread.sleep(1000)
            found = attempt()
        found.getOrElse(fail(s"transaction ${hash.toHex} was not indexed within 90s"))
    }

    // ── the two that decide feasibility ─────────────────────────────────────

    ignore("/addresses/{addr}/transactions honours the from/to block range") {
        val target = freshAddress()
        val bech32 = BlockfrostRestChainApi.bech32(target)
        val hash = payTo(target, Value.ada(10))
        val height = awaitIndexed(hash).blockHeight
        assume(height > 1, "need a chain deeper than one block to ask about a range below it")

        val inItsOwnBlock =
            bf.fetchAddressTransactionsInRange(bech32, height, height).await(timeout)
        assert(
          inItsOwnBlock.map(_.txHash).contains(hash.toHex),
          s"the payment is in block $height, but a range query for exactly that block did not " +
              s"return it: ${inItsOwnBlock.map(_.txHash)}"
        )

        // The negative control, and the whole point of this test. If the range is ignored the
        // query below returns the payment too, and every poll would re-report the address's whole
        // history as new — silently, and for ever.
        val everythingBefore =
            bf.fetchAddressTransactionsInRange(bech32, 1, height - 1).await(timeout)
        assert(
          !everythingBefore.map(_.txHash).contains(hash.toHex),
          s"the from/to range is being ignored: asking for blocks 1..${height - 1} returned a " +
              s"transaction from block $height"
        )
    }

    ignore("/blocks/{hash}/next tells a block it does not have apart from having no next block") {
        val latest = bf.fetchLatestBlock.await(timeout)
        assert(
          bf.fetchBlockNextOrGone(latest.hash).await(timeout).isDefined,
          "a block that is on chain must answer with a page, even an empty one"
        )
        val neverOnChain = "de" * 32
        assert(
          bf.fetchBlockNextOrGone(neverOnChain).await(timeout).isEmpty,
          "a hash the backend does not have must 404: it is the only reorg signal this provider " +
              "has, and read as an empty page it leaves a follower parked on an orphaned position"
        )
    }

    // ── the two that would degrade rather than break ────────────────────────

    test("/blocks/latest carries both a height and a slot") {
        val latest = bf.fetchLatestBlock.await(timeout)
        assert(latest.height.isDefined, "without a height a block cannot be ordered")
        assert(latest.slot.isDefined, "without a slot a ChainPoint cannot name it")
    }

    test("/txs/{hash}/utxos resolves what a transaction created and consumed") {
        val target = freshAddress()
        val hash = payTo(target, Value.ada(10))
        awaitIndexed(hash)
        val effects = bf.fetchTransactionEffects(hash).await(timeout)
        assert(
          effects.created.values.exists(_.address == target),
          s"the payment's output is missing from created: ${effects.created.values.map(_.address)}"
        )
        assert(
          effects.spent.nonEmpty,
          "a subscriber watching the payer needs to know which of its UTxOs disappeared, and an " +
              "input reference alone does not say"
        )
        assert(
          effects.spent.values.forall(_.address == ctx.alice.address),
          s"spent should resolve to the payer's own outputs: ${effects.spent.values.map(_.address)}"
        )
    }

    // ── and the whole thing, end to end ─────────────────────────────────────

    ignore("the follower reports a watched address's transaction once, in its own block") {
        val target = freshAddress()
        val follower = new BlockfrostChainFollower(
          new BlockfrostRestChainApi(bf),
          500.millis,
          d => BlockfrostProviderPlatform.delayFuture(d.toMillis)
        )
        follower.watch(Set(UtxoSource.FromAddress(target)))
        follower.start()

        // Wait until it is demonstrably polling before paying, so the payment cannot land at or
        // below the tip the follower started from.
        val warmup = collect(follower, 60.seconds)(_ => true)
        assert(warmup.nonEmpty, "the follower produced no blocks at all")

        val hash = payTo(target, Value.ada(7))
        val observed = collect(follower, 120.seconds)(_.txs.exists(_.txHash == hash))
        follower.close()

        val matching = observed.filter(_.txs.exists(_.txHash == hash))
        assert(
          matching.size == 1,
          s"the payment should be reported in exactly one block; got ${matching.map(_.blockNo)}"
        )
        assert(
          observed.forall(_.coverage == BlockCoverage.Sources(Set(UtxoSource.FromAddress(target)))),
          "every block must state the sources it was assembled from, matches or not, or the hub " +
              "cannot tell a height that was examined from one that was not"
        )
        assert(
          observed.map(_.blockNo) == observed.map(_.blockNo).sorted.distinct,
          s"heights must be reported once each and in order: ${observed.map(_.blockNo)}"
        )
    }

    /** Pull blocks until `stop` holds or the deadline passes. */
    private def collect(follower: ChainFollower, within: FiniteDuration)(
        stop: AppliedBlock => Boolean
    ): List[AppliedBlock] = {
        val deadline = System.currentTimeMillis() + within.toMillis
        val buf = List.newBuilder[AppliedBlock]
        var done = false
        while !done && System.currentTimeMillis() < deadline do
            Await.result(follower.events.pull(), within) match
                case Some(ChainEvent.RollForward(block)) =>
                    buf += block
                    if stop(block) then done = true
                case Some(ChainEvent.RollBackward(to)) => fail(s"unexpected rollback to $to")
                case None                              => done = true
        buf.result()
    }
}
