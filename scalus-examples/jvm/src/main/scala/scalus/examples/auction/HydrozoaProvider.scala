package scalus.examples.auction

import com.bloxbean.cardano.client.cip.cip30.CIP30DataSigner
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.node.*
import scalus.cardano.wallet.{ExtendedKeyPair, KeyPair}
import scalus.uplc.builtin.Data

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

case class HeadInfo(headId: String, headAddress: String, submissionDurationSeconds: Long, currentTimePosixSeconds: Long)
object HeadInfo { given JsonValueCodec[HeadInfo] = JsonCodecMaker.make }

/** BlockchainProvider adapter that routes transactions to a Hydrozoa L2 instance via HTTP.
  *
  * This is the only class needed to switch from Cardano L1 to Hydrozoa L2 —
  * all validators, tx builders, and wallet code stay identical.
  */
class HydrozoaProvider(
    baseUrl: String,
    info: CardanoInfo,
    initialUtxos: Utxos,
    keyPair: KeyPair,
    signerAddress: ShelleyAddress,
    headId: String
)(using ec: ExecutionContext)
    extends BlockchainProvider {

    private val http = HttpClient.newHttpClient()
    private val utxos: mutable.Map[TransactionInput, TransactionOutput] = mutable.Map.from(initialUtxos)
    private var slot: SlotNo = 0

    override def executionContext: ExecutionContext = ec
    override def cardanoInfo: CardanoInfo = info
    override def fetchLatestParams: Future[ProtocolParams] = Future.successful(info.protocolParams)
    override def currentSlot: Future[SlotNo] = Future.successful(slot)
    override def getDatum(datumHash: DataHash): Future[Option[Data]] = Future.successful(None)
    def setSlot(s: SlotNo): Unit = slot = s

    override def findUtxos(query: UtxoQuery): Future[Either[UtxoQueryError, Utxos]] =
        Future.successful(Right(EmulatorBase.evalQuery(utxos.toMap, query)))

    override def submit(transaction: Transaction): Future[Either[SubmitError, TransactionHash]] =
        Future {
            val cbor = transaction.toCbor
            val bodyHash = blake2b256Hex(cbor)
            val now = java.time.Instant.now().getEpochSecond
            val header = s"""{"headId":"$headId","validityStart":$now,"validityEnd":${now + 600},"bodyHash":"$bodyHash"}"""
            val (coseKey, coseSig) = signCose(header)
            val body = s"""{"header":$header,"transaction":{"l2Payload":"${hex(cbor)}"},"coseKey":"$coseKey","coseSignature":"$coseSig"}"""

            val req = HttpRequest.newBuilder()
                .uri(URI.create(s"$baseUrl/api/l2/submit"))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(body))
                .build()
            val resp = http.send(req, HttpResponse.BodyHandlers.ofString())

            if resp.statusCode() == 200 then
                applyTx(transaction)
                Right(transaction.id)
            else Left(NetworkSubmitError.ConnectionError(
                s"Hydrozoa submit failed (${resp.statusCode()}): ${resp.body()}"
            ))
        }

    private def applyTx(tx: Transaction): Unit = {
        tx.body.value.inputs.toSeq.foreach(utxos.remove)
        tx.body.value.outputs.zipWithIndex.foreach { (out, i) =>
            utxos.put(TransactionInput(tx.id, i), out.value)
        }
    }

    private def hex(bytes: Array[Byte]): String = bytes.map(b => f"${b & 0xff}%02x").mkString

    private def blake2b256Hex(bytes: Array[Byte]): String = {
        val d = new org.bouncycastle.crypto.digests.Blake2bDigest(256)
        d.update(bytes, 0, bytes.length)
        val h = new Array[Byte](32)
        d.doFinal(h, 0)
        hex(h)
    }

    private def signCose(payload: String): (String, String) = {
        val pvtKey = keyPair match
            case ekp: ExtendedKeyPair => ekp.extendedSigningKey.bytes
            case _ => throw UnsupportedOperationException("COSE signing requires HD wallet keys")
        val ds = CIP30DataSigner.INSTANCE.signData(
            signerAddress.toBytes.bytes,
            payload.getBytes(StandardCharsets.UTF_8),
            pvtKey,
            keyPair.verificationKey.bytes
        )
        (ds.key(), ds.signature())
    }
}

object HydrozoaProvider {
    def fetchHeadInfo(baseUrl: String): HeadInfo = {
        val resp = HttpClient.newHttpClient().send(
          HttpRequest.newBuilder().uri(URI.create(s"$baseUrl/api/head-info")).GET().build(),
          HttpResponse.BodyHandlers.ofString()
        )
        if resp.statusCode() != 200 then
            throw RuntimeException(s"GET /api/head-info failed: ${resp.statusCode()} ${resp.body()}")
        readFromString[HeadInfo](resp.body())
    }
}
