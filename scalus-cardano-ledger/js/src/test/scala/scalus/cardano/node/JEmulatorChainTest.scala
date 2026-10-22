package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.uplc.builtin.ByteString
import scalus.utils.scalajs.internal.*

import java.time.Instant
import scala.scalajs.js

/** The chain surface of the JavaScript `Emulator`, spec section 13.2. */
class JEmulatorChainTest extends AnyFunSuite {

    private given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
    private val alice = Alice.address(Network.Mainnet)
    private val bob = Bob.address(Network.Mainnet)
    private val initialUtxos: Utxos = Map(Input(genesisHash, 0) -> Output(alice, Value.ada(1000)))

    // Read a `js.UndefOr` through `.toOption` inside `assert`, see EmulatorJsTest.

    /** A `JEmulator` at slot 0 with one funded UTxO, and `chain` as the extra options. */
    private def create(chain: (String, js.Any)*): JEmulator = {
        val options = js.Dynamic.literal(
          utxos = js.Array(initialUtxos.toSeq.map((i, o) => JsUtxo.wrap(i, o))*),
          slot = 0.0
        )
        chain.foreach((k, v) => options.updateDynamic(k)(v))
        JEmulator.create(JsCardanoInfo.mainnet(), options.asInstanceOf[JsEmulatorOptions])
    }

    private def payment(utxos: Utxos, lovelace: Long): Transaction =
        TxBuilder(testEnv)
            .payTo(bob, Value.lovelace(lovelace))
            .complete(utxos, alice)
            .sign(Alice.signer)
            .transaction

    private def bytes(tx: Transaction) = tx.toCbor.toUint8Array

    private def bobFilter: JsUtxoFilter =
        js.Dynamic.literal(address = bob.encode.get).asInstanceOf[JsUtxoFilter]

    private def point(p: JsChainPoint): (Long, String) = (p.slot.toLong, p.blockHash)

    test("blockProduction manual: submit waits in the mempool until produceBlock") {
        // spec [SF-6], [SF-7], [SF-8], [TE-5]
        val e = create("blockProduction" -> "manual")
        val tx = payment(initialUtxos, 2_000_000)
        assert(e.submitTx(bytes(tx)).isSuccess)

        assert(e.getUtxos(bobFilter).isEmpty)
        assert(e.getMempool().toSeq == Seq(tx.id.toHex))
        assert(e.getTransactionStatus(tx.id.toHex) == "Pending")
        assert(e.getTxStatus(tx.id.toHex).status == "Pending")
        assert(point(e.getTip()) == (0L, "00" * 32))
        assert(e.getTip().height == 0)

        val p = e.produceBlock()
        assert(p.slot == 1)
        assert(point(e.getTip()) == point(p))
        assert(e.getTip().height == 1)
        assert(e.getUtxos(bobFilter).length == 1)
        assert(e.getMempool().isEmpty)
        val status = e.getTxStatus(tx.id.toHex)
        assert(status.status == "Confirmed")
        assert(status.depth.toOption.contains(1.0))
        assert(status.point.toOption.map(point).contains(point(p)))
        assert(e.getTransactionStatus(tx.id.toHex) == "Confirmed")
        assert(e.getTransactionStatus("11" * 32) == "NotFound")
    }

    test("the default stays instant: submit is visible at once") {
        // spec [CF-4]
        val e = create()
        assert(e.submitTx(bytes(payment(initialUtxos, 2_000_000))).isSuccess)
        assert(e.getUtxos(bobFilter).length == 1)
        assert(e.getTip().height == 1)
    }

    test("awaitTx forges until the tx is confirmed; waitOnly forges nothing") {
        // spec [SF-3], [SF-4], [SF-7]
        val forging = create("blockProduction" -> "manual")
        val tx = payment(initialUtxos, 2_000_000)
        forging.submitTx(bytes(tx))
        assert(forging.awaitTx(tx.id.toHex))
        assert(forging.getUtxos(bobFilter).length == 1)

        val waiting = create("blockProduction" -> "manual", "awaitTx" -> "waitOnly")
        waiting.submitTx(bytes(tx))
        assert(!waiting.awaitTx(tx.id.toHex))
        assert(waiting.getTip().height == 0)
        assert(!waiting.awaitTx("11" * 32))
    }

    test("getUtxos reads the view: tip, mempool or stable") {
        // spec [SF-9], [VW-3]
        val e = create("blockProduction" -> "manual")
        val tx = payment(initialUtxos, 2_000_000)
        e.submitTx(bytes(tx))
        assert(e.getUtxos(bobFilter, "tip").isEmpty)
        assert(e.getUtxos(bobFilter, "mempool").length == 1)
        e.produceBlock()
        assert(e.getUtxos(bobFilter, js.Dynamic.literal(stable = 1)).length == 1)
        assert(e.getUtxos(bobFilter, js.Dynamic.literal(stable = 2)).isEmpty)

        for view <- Seq[js.Any]("latest", js.Dynamic.literal(), js.Dynamic.literal(stable = "1"))
        do
            val thrown = intercept[js.JavaScriptException](e.getUtxos(bobFilter, view))
            assert(thrown.exception.isInstanceOf[js.TypeError], thrown)
        val outOfRange =
            intercept[js.JavaScriptException](e.getUtxos(bobFilter, js.Dynamic.literal(stable = 0)))
        assert(outOfRange.exception.isInstanceOf[js.RangeError], outOfRange)
    }

    test("produceBlockOn forges a rival branch; rollback undoes blocks") {
        // spec [SF-7], [FG-11], [FG-12], [TE-7], [RB-7]
        val e = create("blockProduction" -> "manual", "orphanTxs" -> "drop")
        val tx = payment(initialUtxos, 2_000_000)
        val rival = payment(initialUtxos, 3_000_000)
        e.submitTx(bytes(tx))
        e.produceBlock()
        val origin = js.Dynamic.literal(slot = 0, blockHash = "00" * 32).asInstanceOf[JsChainPoint]

        val tooEarly = e.produceBlockOn(origin, js.Array(bytes(rival)), 0)
        assert(!tooEarly.isSuccess)
        assert(tooEarly.errorKind.toOption.contains("SlotNotAfterParent"))

        val b1 = e.produceBlockOn(origin, js.Array(bytes(rival)), 5)
        assert(b1.isSuccess)
        val b2 = e.produceBlockOn(b1.point.get, js.Array[js.typedarray.Uint8Array]())
        assert(b2.isSuccess && point(b2.point.get) == point(e.getTip()))
        assert(e.getTxStatus(tx.id.toHex).status == "RolledBack")
        assert(e.getTransactionStatus(tx.id.toHex) == "RolledBack")
        assert(e.getTxStatus(rival.id.toHex).depth.toOption.contains(2.0))

        val rolled = e.rollback(1)
        assert(rolled.isSuccess && rolled.point.toOption.map(point).contains(point(b1.point.get)))
        val tooFar = e.rollback(3)
        assert(tooFar.errorKind.toOption.contains("PastGenesis"))
        assert(tooFar.error.isDefined)
    }

    test("a tx dropped by revalidation reads Dropped with its reason") {
        // spec [SF-8], [RV-4]
        val e = create("blockProduction" -> "manual")
        val tx = TxBuilder(testEnv)
            .payTo(bob, Value.lovelace(2_000_000))
            .validTo(Instant.ofEpochMilli(testEnv.slotConfig.slotToTime(100)))
            .complete(initialUtxos, alice)
            .sign(Alice.signer)
            .transaction
        e.submitTx(bytes(tx))
        e.setSlot(200)
        val status = e.getTxStatus(tx.id.toHex)
        assert(status.status == "Dropped")
        assert(status.reason.toOption.contains("Expired"))
        assert(e.getTransactionStatus(tx.id.toHex) == "Dropped")
    }

    test("securityParam bounds the rollback; snapshot keeps the preset") {
        // spec [SF-6], [RB-2]
        val e = create("blockProduction" -> "manual", "securityParam" -> 1)
        e.produceBlock()
        e.produceBlock()
        assert(e.rollback(2).errorKind.toOption.contains("TooDeep"))

        val copy = create("blockProduction" -> "manual").snapshot()
        copy.submitTx(bytes(payment(initialUtxos, 2_000_000)))
        assert(copy.getMempool().length == 1, "a snapshot of a Manual emulator stays Manual")
    }

    test("invalid chain options throw a TypeError") {
        // spec [SF-6]
        for option <- Seq(
              "blockProduction" -> ("hourly": js.Any),
              "awaitTx" -> ("never": js.Any),
              "orphanTxs" -> ("keep": js.Any),
              "securityParam" -> (0: js.Any)
            )
        do
            val thrown = intercept[js.JavaScriptException](create(option))
            assert(thrown.exception.isInstanceOf[js.TypeError], option)
    }

    test("securityParam below 1 names the option in its TypeError") {
        // spec [SF-6]: checked here, not by the require of ChainConfig
        val thrown = intercept[js.JavaScriptException](create("securityParam" -> 0))
        val error = thrown.exception.asInstanceOf[js.Error]
        assert(error.isInstanceOf[js.TypeError])
        assert(error.message == "securityParam must be at least 1, got: 0")
    }
}
