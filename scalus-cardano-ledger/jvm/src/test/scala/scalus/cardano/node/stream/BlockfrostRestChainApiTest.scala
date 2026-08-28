package scalus.cardano.node.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, ByronAddress, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.node.BlockfrostProvider
import scalus.cardano.node.stream.internal.{BlockRef, BlockfrostRestChainApi}
import scalus.testing.kit.Party
import sttp.client4.Backend
import sttp.client4.testing.BackendStub

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

/** The translation layer between Blockfrost's REST vocabulary and the follower's.
  *
  * Thin, but not trivial: one of these mappings is how a reorg is detected at all, and getting it
  * wrong does not produce an error — it produces a follower waiting quietly at a position that is
  * no longer on the chain.
  */
class BlockfrostRestChainApiTest extends AnyFunSuite {

    private given ExecutionContext = ExecutionContext.global
    private given CardanoInfo = CardanoInfo.mainnet

    private val blockHash = "ab" * 32

    private def blockJson(
        hash: String = blockHash,
        height: String = "1234",
        slot: String = "9999"
    ): String =
        s"""{"time":1,"height":$height,"hash":"$hash","slot":$slot,"epoch":5,"epoch_slot":7,
           |"slot_leader":"pool1","size":100,"tx_count":2,"previous_block":null,
           |"next_block":null,"confirmations":3}""".stripMargin

    private def api(
        respond: PartialFunction[String, sttp.client4.Response[String]]
    ): BlockfrostRestChainApi = {
        given Backend[Future] = BackendStub.asynchronousFuture.whenRequestMatchesPartial {
            case req if respond.isDefinedAt(req.uri.toString) =>
                respond(req.uri.toString).copy(body = sttp.client4.testing.StubBody.Adjust(
                  respond(req.uri.toString).body
                ))
        }
        new BlockfrostRestChainApi(
          new BlockfrostProvider("k", "http://stub.invalid", 5, CardanoInfo.mainnet)
        )
    }

    private def await[A](f: Future[A]): A = Await.result(f, 5.seconds)

    test("a 404 on /next is reported as a reorg, not as an empty page") {
        // Blockfrost answers 404 when it no longer has the block on its chain, and 200 with an
        // empty array when the block is simply the tip. Conflating them leaves a follower waiting
        // quietly at an orphaned position while the chain moves on without it.
        given Backend[Future] = BackendStub.asynchronousFuture.whenAnyRequest.thenRespondNotFound()
        val client = new BlockfrostProvider("k", "http://stub.invalid", 5, CardanoInfo.mainnet)
        val ref = BlockRef(ChainPoint(1, BlockHash.fromHex(blockHash)), 1)
        assert(await(new BlockfrostRestChainApi(client).blocksAfter(ref)).isEmpty)
    }

    test("an empty /next page means the tip, and is not a reorg") {
        given Backend[Future] = BackendStub.asynchronousFuture.whenAnyRequest.thenRespondAdjust("[]")
        val client = new BlockfrostProvider("k", "http://stub.invalid", 5, CardanoInfo.mainnet)
        val ref = BlockRef(ChainPoint(1, BlockHash.fromHex(blockHash)), 1)
        assert(
          await(new BlockfrostRestChainApi(client).blocksAfter(ref)).contains(Seq.empty),
          "reading 'nothing new yet' as a reorg would fail every subscription every time the " +
              "chain paused"
        )
    }

    test("a block is placed by its own height and slot") {
        given Backend[Future] =
            BackendStub.asynchronousFuture.whenAnyRequest.thenRespondAdjust(blockJson())
        val client = new BlockfrostProvider("k", "http://stub.invalid", 5, CardanoInfo.mainnet)
        val ref = await(new BlockfrostRestChainApi(client).latestBlock())
        assert(ref.blockNo == 1234L)
        assert(ref.point == ChainPoint(9999L, BlockHash.fromHex(blockHash)))
    }

    test("a block with no height is refused rather than placed at zero") {
        // Only Byron boundary blocks lack one, and the follower walks forward from the current tip
        // so it can never meet them. Defaulting would put a block at height 0 and drag every
        // watermark back with it.
        given Backend[Future] = BackendStub.asynchronousFuture.whenAnyRequest
            .thenRespondAdjust(blockJson(height = "null"))
        val client = new BlockfrostProvider("k", "http://stub.invalid", 5, CardanoInfo.mainnet)
        assertThrows[IllegalStateException](await(new BlockfrostRestChainApi(client).latestBlock()))
    }

    test("an address's transactions are asked for by block-height range") {
        // The range is what makes watching an address one request per block instead of a scan, so
        // losing it would be a silent change in cost rather than in behaviour.
        @volatile var seen: Option[String] = None
        given Backend[Future] = BackendStub.asynchronousFuture
            .whenRequestMatches { req => seen = Some(req.uri.toString); true }
            .thenRespondAdjust("[]")
        val client = new BlockfrostProvider("k", "http://stub.invalid", 5, CardanoInfo.mainnet)
        await(new BlockfrostRestChainApi(client).addressTransactionsIn(Party.Alice.address, 7, 7))
        val url = seen.getOrElse(fail("no request was made"))
        assert(url.contains("from=7") && url.contains("to=7"), s"got $url")
        assert(
          url.contains(Party.Alice.address.asInstanceOf[ShelleyAddress].toBech32.get),
          s"got $url"
        )
    }

    test("only Shelley addresses are watchable") {
        assert(BlockfrostRestChainApi.isWatchable(Party.Alice.address))
        val byron: Address =
            ByronAddress.fromBase58("Ae2tdPwUPEZDoUnyXuAgqzhkjNXNJeiZ5nqwprg9sArZmRNjySfJ5uz4FjB").get
        assert(!BlockfrostRestChainApi.isWatchable(byron))
        assertThrows[IllegalArgumentException](BlockfrostRestChainApi.bech32(byron))
    }
}
