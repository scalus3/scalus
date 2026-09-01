package scalus.cardano.node.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.node.BlockfrostProvider
import scalus.cardano.node.stream.internal.BlockfrostRestChainApi
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.integration.YaciTestContext
import scalus.testing.yaci.{YaciConfig, YaciDevKit}
import scalus.uplc.builtin.ByteString
import scalus.utils.await

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import scala.concurrent.duration.*
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global

/** Diagnostic: exactly what Yaci Store serves, in full, for the endpoints the follower polls.
  *
  * Prints rather than asserts. The first pass of this ([[YaciStreamingEndpointsTest]]) asserted, and
  * an assertion tells you only that something did not hold — here the question is what the backend
  * actually returns, and three of the earlier answers turned on the *body* of a 404 rather than its
  * status. Bodies are printed untruncated for that reason.
  *
  * Run with: {{{ sbt "scalusCardanoLedgerIt/testOnly *YaciEndpointReportTest" }}}
  */
class YaciEndpointReportTest extends AnyFunSuite with YaciDevKit {

    /** `/txs/{hash}/cbor` needs *two* coupled flags, not the one its 404 body names.
      *
      * The 404 says only `store.transaction.save-cbor=true`, and setting that alone writes nothing
      * and reports nothing at default log level: `store.transaction.save-cbor` gates whether the
      * CBOR is collected, but the field it reads is populated only when
      * `store.cardano.return-tx-body-cbor` is also set. That is upstream yaci-store#1086, and it
      * is why the first pass of this probe concluded the variables "did not take" — the names were
      * right and the set was incomplete.
      *
      * Both are forward-only with no backfill, so they must be set before the initial sync, which
      * a fresh container gives us for free. Screaming-snake because Spring's relaxed binding takes
      * that form for environment variables.
      *
      * Even with both set, expect the payload to be the transaction *body* rather than the full
      * four-element transaction on this image — yaci-store#1090, fixed upstream on 2026-08-25 —
      * which is why the report prints the leading byte: `0x84` is the full transaction,
      * `0xa9` the body alone.
      */
    override protected def yaciConfig: YaciConfig = YaciConfig(extraEnv =
        Map(
          "STORE_TRANSACTION_SAVE_CBOR" -> "true",
          "STORE_CARDANO_RETURN_TX_BODY_CBOR" -> "true"
        )
    )

    private lazy val ctx: YaciTestContext = createYaciContext()
    private lazy val base: String = container.getYaciStoreApiUrl.stripSuffix("/")
    private lazy val bf: BlockfrostProvider = BlockfrostProvider
        .localYaci(base, container.getLocalClusterApiUrl.stripSuffix("/"))
        .await(60.seconds)
    private val http = HttpClient.newHttpClient()

    private def get(path: String): (Int, String) = {
        val response = http.send(
          HttpRequest.newBuilder(URI.create(s"$base$path")).GET().build(),
          HttpResponse.BodyHandlers.ofString()
        )
        (response.statusCode(), response.body())
    }

    private def full(label: String, path: String): String = {
        val (code, body) = get(path)
        println(s"\n--- [$code] $label   $path")
        println(body)
        body
    }

    test("report: full bodies for everything the follower touches") {
        println("\n########## BLOCKS ##########")
        val latestBody = full("blocks/latest", "/blocks/latest")
        val latest = ujson.read(latestBody)
        val tip = latest("height").num.toLong
        val tipHash = latest("hash").str

        println(s"\n>>> does the block JSON carry previous_block? ${latest.obj.contains("previous_block")}")
        println(s">>> keys: ${latest.obj.keys.toList.sorted.mkString(", ")}")

        full("blocks by height, tip - 2", s"/blocks/${tip - 2}")
        full("blocks by height, tip - 1", s"/blocks/${tip - 1}")

        println("\n########## A PAYMENT ##########")
        val bytes = new Array[Byte](28)
        scala.util.Random.nextBytes(bytes)
        val keyHash: AddrKeyHash = Hash(ByteString.fromArray(bytes))
        val target = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Key(keyHash),
          ShelleyDelegationPart.Null
        )
        val tx = TxBuilder(ctx.cardanoInfo)
            .payTo(target, Value.ada(10))
            .complete(ctx.provider, ctx.alice.address)
            .await(30.seconds)
            .sign(ctx.alice.signer)
            .transaction
        println(s"  submit: ${ctx.submitTx(tx)}")
        val hash = tx.id.toHex

        var waited = 0
        while waited < 60 && get(s"/txs/$hash")._1 != 200 do
            Thread.sleep(3000)
            waited += 3
        println(s"  indexed after ~${waited}s")

        println("\n########## TRANSACTIONS ##########")
        full("txs/{hash}", s"/txs/$hash")
        val cborBody = full("txs/{hash}/cbor  (save-cbor probe)", s"/txs/$hash/cbor")
        // The leading byte settles yaci-store#1090 without decoding anything: 0x84 is the CBOR
        // array of 4 the full transaction is, 0xa9 the 9-pair map that is the body alone. The
        // follower's `Transaction.fromCbor` needs the former.
        if get(s"/txs/$hash/cbor")._1 == 200 then
            val hex = ujson.read(cborBody).obj.get("cbor").map(_.str).getOrElse("")
            println(s">>> cbor leading byte: ${hex.take(2)} (84 = full transaction, a9 = body only)")
            println(s">>> cbor payload bytes: ${hex.length / 2}")
        full("txs/{hash}/utxos", s"/txs/$hash/utxos")

        println("\n>>> what does fetchTransactionInfo actually do with that body?")
        Await.ready(bf.fetchTransactionInfo(hash), 30.seconds).value match
            case Some(scala.util.Success(info)) => println(s"    parsed: $info")
            case Some(scala.util.Failure(t))    => println(s"    FAILED: ${t.getClass.getName}: ${t.getMessage}")
            case None                           => println("    still pending")

        println("\n>>> and fetchTransactionEffects, which the follower calls per matching tx?")
        Await.ready(bf.fetchTransactionEffects(tx.id), 30.seconds).value match
            case Some(scala.util.Success(e)) =>
                println(s"    created: ${e.created.map((i, o) => s"${i.index}->${o.value.coin}")}")
                println(s"    spent:   ${e.spent.map((i, o) => s"${i.transactionId.toHex.take(8)}#${i.index}")}")
            case Some(scala.util.Failure(t)) => println(s"    FAILED: ${t.getClass.getName}: ${t.getMessage}")
            case None                        => println("    still pending")

        println("\n########## ADDRESSES ##########")
        val targetBech32 = BlockfrostRestChainApi.bech32(target)
        val nowTip = ujson.read(get("/blocks/latest")._2)("height").num.toLong
        full("target transactions, unfiltered", s"/addresses/$targetBech32/transactions")
        full("target transactions, from=1&to=1", s"/addresses/$targetBech32/transactions?from=1&to=1")
        full(
          "payer transactions, from=tip&to=tip",
          s"/addresses/${BlockfrostRestChainApi.bech32(ctx.alice.address)}/transactions?from=$nowTip&to=$nowTip"
        )
        println()
    }
}
