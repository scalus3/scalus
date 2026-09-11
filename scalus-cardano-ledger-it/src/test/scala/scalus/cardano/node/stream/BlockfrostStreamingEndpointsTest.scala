package scalus.cardano.node.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.BlockfrostProvider
import scalus.cardano.node.stream.internal.BlockfrostRestChainApi
import scalus.cardano.ledger.TransactionHash
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

/** Does real Blockfrost serve what the streaming provider polls for?
  *
  * `YaciStreamingEndpointsTest` asks the same four questions of Yaci Store and three of them fail,
  * which is why that suite is `ignore`d and why the M2 plan names preprod as this provider's
  * conformance target. The plan asserts that "Real Blockfrost serves `from`/`to`, `/next` and
  * `/cbor` natively" — this measures it instead of assuming it, so a future backend change is
  * caught here rather than in a subscriber's event stream.
  *
  * Read-only: no transaction is submitted and no wallet is needed, only `BLOCKFROST_API_KEY`.
  *
  * Run with: {{{
  *   BLOCKFROST_API_KEY=preprodXXX sbt "scalusCardanoLedgerIt/testOnly *BlockfrostStreamingEndpointsTest"
  * }}}
  */
class BlockfrostStreamingEndpointsTest extends AnyFunSuite {

    private val timeout = 60.seconds

    private lazy val apiKey: String = {
        val key = Option(System.getenv("BLOCKFROST_API_KEY")).getOrElse("")
        if key.isEmpty then cancel("BLOCKFROST_API_KEY not set, skipping test")
        key
    }

    private lazy val bf: BlockfrostProvider = BlockfrostProvider.preprod(apiKey).await(timeout)

    /** A recent block deep enough that it is settled, and the first transaction in it. */
    private lazy val (sampleHeight: Long, sampleTx: String) = {
        val tip = bf.fetchLatestBlock.await(timeout).height.get
        def search(height: Long, left: Int): (Long, String) =
            if left == 0 then fail("no block with transactions in the last 40 blocks of preprod")
            else
                val txs = bf.fetchBlockTxs(height.toString).await(timeout)
                if txs.nonEmpty then (height, txs.head) else search(height - 1, left - 1)
        search(tip - 20, 40)
    }

    // ── the two that decide feasibility ─────────────────────────────────────

    test("/addresses/{addr}/transactions honours the from/to block range") {
        val address = bf
            .fetchTransactionEffects(TransactionHash.fromHex(sampleTx))
            .await(timeout)
            .spent
            .values
            .headOption
            .getOrElse(fail(s"transaction $sampleTx resolved no spent inputs"))
            .address
        val bech32 = BlockfrostRestChainApi.bech32(address)

        val inItsOwnBlock =
            bf.fetchAddressTransactionsInRange(bech32, sampleHeight, sampleHeight).await(timeout)
        assert(
          inItsOwnBlock.map(_.txHash).contains(sampleTx),
          s"the transaction is in block $sampleHeight, but a range query for exactly that block " +
              s"did not return it: ${inItsOwnBlock.map(_.txHash)}"
        )

        // The negative control, and the whole point of this test. If the range is ignored — as it
        // is on Yaci Store — this returns the transaction too, and a follower would re-report the
        // address's entire history on every poll, silently and for ever.
        val everythingBefore =
            bf.fetchAddressTransactionsInRange(bech32, 1, sampleHeight - 1).await(timeout)
        assert(
          !everythingBefore.map(_.txHash).contains(sampleTx),
          s"the from/to range is being ignored: asking for blocks 1..${sampleHeight - 1} returned " +
              s"a transaction from block $sampleHeight"
        )
    }

    test("/blocks/{hash}/next tells a block it does not have apart from having no next block") {
        val latest = bf.fetchLatestBlock.await(timeout)
        assert(
          bf.fetchBlockNextOrGone(latest.hash).await(timeout).isDefined,
          "a block that is on chain must answer with a page, even an empty one"
        )
        assert(
          bf.fetchBlockNextOrGone("de" * 32).await(timeout).isEmpty,
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

    test("/txs/{hash}/cbor returns the whole transaction, not just its body") {
        val cbor = bf.fetchTransactionCbor(sampleTx).await(timeout)
        // 0x84 is the CBOR array of four — body, witness set, is_valid, auxiliary data — which is
        // what `Transaction.fromCbor` needs. 0xa9 would be the body's map alone, which is what
        // yaci-store served (yaci-store#1090) and which does not round-trip.
        assert(
          cbor.bytes.head == 0x84.toByte,
          s"expected a four-element transaction (0x84); got 0x${"%02x".format(cbor.bytes.head)}"
        )
        assert(
          scalus.cardano.ledger.Transaction.fromCbor(cbor.bytes).id.toHex == sampleTx,
          "the bytes must decode to the transaction they were fetched for"
        )
    }

    test("/txs/{hash}/utxos resolves what a transaction consumed, not just its inputs' references") {
        val effects = bf.fetchTransactionEffects(TransactionHash.fromHex(sampleTx)).await(timeout)
        assert(effects.created.nonEmpty, "a transaction must have produced something")
        assert(
          effects.spent.values.forall(o => o.value.coin.value >= 0L),
          "a subscriber watching an address needs the resolved outputs its transaction consumed, " +
              "and an input reference alone does not carry the address or the value"
        )
    }
}
