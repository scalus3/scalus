package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network, StakeAddress, StakePayload}
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

    test("ledgerFailures lists every ledger failure by its ledger names, spec [FL-8]") {
        val e = create()
        val tx = TxBuilder(testEnv)
            .payTo(bob, Value.lovelace(2_000_000))
            .validTo(Instant.ofEpochMilli(testEnv.slotConfig.slotToTime(100)))
            .complete(initialUtxos, alice)
            .sign(Alice.signer)
            .transaction
        assert(e.ledgerFailures(bytes(tx)).isEmpty)
        assert(e.submitTx(bytes(tx)).isSuccess)
        e.setSlot(200)
        // the spent input and the expired interval, BadInputsUTxO first, spec [FL-7]
        assert(
          e.ledgerFailures(bytes(tx)).toSeq == Seq("BadInputsUTxO", "OutsideValidityIntervalUTxO")
        )
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

    test("a failed forge or rollback says what went wrong in prose") {
        // spec [SF-7]: error is for people, errorKind for code
        val e = create("blockProduction" -> "manual", "securityParam" -> 1)
        val origin = js.Dynamic.literal(slot = 0, blockHash = "00" * 32).asInstanceOf[JsChainPoint]
        val unknown =
            js.Dynamic.literal(slot = 7, blockHash = "ff" * 32).asInstanceOf[JsChainPoint]
        val none = js.Array[js.typedarray.Uint8Array]()
        val spent = payment(initialUtxos, 2_000_000)
        def error(result: JChainResult): Option[String] = result.error.toOption

        assert(
          error(e.produceBlockOn(origin, none, 0)) ==
              Some("slot 0 is not after slot 0 of the parent block")
        )
        assert(
          error(e.produceBlockOn(unknown, none)) ==
              Some(s"no block at slot 7 with hash ${"ff" * 32} in the block tree")
        )
        assert(e.rollback(0).error.toOption == Some("rollback(0): n must be at least 1"))
        assert(
          e.rollback(1).error.toOption ==
              Some("rollback(1): n is above the tip height 0")
        )
        e.produceBlock()
        e.produceBlock()
        e.produceBlock()
        assert(
          error(e.produceBlockOn(origin, none)) ==
              Some("the fork point is at depth 4, deeper than securityParam 1")
        )
        assert(
          e.rollback(2).error.toOption ==
              Some("rollback(2): n is above securityParam 1, or past the oldest snapshot kept")
        )
        val invalid = error(e.produceBlockOn(e.getTip(), js.Array(bytes(spent), bytes(spent))))
        assert(invalid.exists(_.startsWith("the tx at index 1 is invalid: ")), invalid)
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

    /** The `deltaT` of the reward update after one block in the first epoch and its boundary; with
      * no fee it is above 0 exactly when eta is.
      */
    private def deltaTAfterOneBlock(chain: (String, js.Any)*): BigInt = {
        val e = create(chain*)
        e.produceBlock()
        val slotConfig = testEnv.slotConfig
        e.setSlot(slotConfig.firstSlotOfEpoch(slotConfig.epochOf(0L) + 1).toDouble)
        val update = e.getPendingRewards().toOption.getOrElse(fail("nothing pending"))
        BigInt(update.deltaT.toString)
    }

    private val measured: (String, js.Any) = "poolPerformance" -> "measured"

    test("blockIssuer takes the hex id of a pool; another value is a TypeError") {
        // ledger-rules spec [ES-10a], [ES-1], [RW-14]: under Measured a block counts only with
        // a Pool issuer, and eta = 0 makes deltaT = 0
        assert(deltaTAfterOneBlock(measured) == 0)
        assert(deltaTAfterOneBlock(measured, "blockIssuer" -> "b7" * 28) > 0)
        for bad <- Seq("b7" * 27, "zz" * 28) do
            val thrown = intercept[js.JavaScriptException](create("blockIssuer" -> bad))
            val error = thrown.exception.asInstanceOf[js.Error]
            assert(error.isInstanceOf[js.TypeError], bad)
            assert(error.message == s"blockIssuer must be the hex of 28 bytes, got: \"$bad\"")
    }

    test("poolPerformance and activeSlotsCoeff are taken; a bad value is a TypeError") {
        // ledger-rules spec [ES-10a], [RW-14], [RW-15]: Ideal has eta = 1 with no counted block,
        // Measured eta = 0; with one block, eta = 1 / floor(f · epochLength), so a larger f
        // gives a smaller deltaT
        assert(deltaTAfterOneBlock("poolPerformance" -> "ideal") > 0)
        val issuer: (String, js.Any) = "blockIssuer" -> "b7" * 28
        def coeff(d: Double): (String, js.Any) =
            "activeSlotsCoeff" -> js.Dynamic.literal(numerator = 1.0, denominator = d)
        val tenth = deltaTAfterOneBlock(measured, issuer, coeff(10.0))
        val twentieth = deltaTAfterOneBlock(measured, issuer, coeff(20.0))
        assert(tenth > 0)
        assert(tenth < twentieth)
        def typeError(option: (String, js.Any)): String = {
            val thrown = intercept[js.JavaScriptException](create(option))
            val error = thrown.exception.asInstanceOf[js.Error]
            assert(error.isInstanceOf[js.TypeError], option)
            error.message
        }
        assert(
          typeError("poolPerformance" -> "perfect") ==
              "poolPerformance must be \"ideal\" or \"measured\", got: \"perfect\""
        )
        for (n, d) <- Seq((0.0, 1.0), (2.0, 1.0), (1.0, 0.0), (0.5, 1.0)) do
            val message =
                typeError("activeSlotsCoeff" -> js.Dynamic.literal(numerator = n, denominator = d))
            assert(
              message.startsWith("activeSlotsCoeff must be a fraction in (0, 1], got: "),
              message
            )
    }

    test("securityParam below 1 names the option in its TypeError") {
        // spec [SF-6]: checked here, not by the require of ChainConfig
        val thrown = intercept[js.JavaScriptException](create("securityParam" -> 0))
        val error = thrown.exception.asInstanceOf[js.Error]
        assert(error.isInstanceOf[js.TypeError])
        assert(error.message == "securityParam must be at least 1, got: 0")
    }

    test("getStakeSnapshots and getPendingRewards read the stake snapshots and the reward update") {
        // ledger-rules spec [ES-9a] (D82)
        val pool = "c3" * 28
        val bobStake = Bob.account.stakeAddress(Network.Mainnet)
        val stakeHash = bobStake.credential match
            case Credential.KeyHash(hash) => hash.toHex
            case other                    => fail(s"not a key credential: $other")
        val bobBase =
            Address(Network.Mainnet, Credential.KeyHash(Bob.addrKeyHash), bobStake.credential)
        val bobUtxo = Input(genesisHash, 1) -> Output(bobBase, Value.ada(500))
        val options = js.Dynamic.literal(
          utxos = js.Array((initialUtxos + bobUtxo).toSeq.map((i, o) => JsUtxo.wrap(i, o))*),
          slot = 0.0,
          stakeRegistrations = js.Array(
            js.Dynamic.literal(
              credentialType = "key",
              credentialHash = stakeHash,
              rewards = js.BigInt(3),
              delegatedTo = pool
            )
          ),
          poolRegistrations = js.Array(js.Dynamic.literal(poolId = pool))
        )
        val emulator =
            JEmulator.create(JsCardanoInfo.mainnet(), options.asInstanceOf[JsEmulatorOptions])
        assert(emulator.getPendingRewards().toOption.isEmpty)
        val snapshots = emulator.getStakeSnapshots()
        assert(snapshots.fee.toString == "0")
        for entries <- Seq(snapshots.mark, snapshots.set, snapshots.go) do
            assert(entries.length == 1)
            val entry = entries(0)
            assert(entry.credential == stakeHash)
            assert(entry.rewardAddress == bobStake.toBech32.get)
            assert(entry.pool == pool)
            assert(entry.stake.toString == (Coin.ada(500) + Coin(3)).value.toString)

        val slotConfig = testEnv.slotConfig
        emulator.setSlot(slotConfig.firstSlotOfEpoch(slotConfig.epochOf(0L) + 1).toDouble)
        val update = emulator.getPendingRewards().toOption.getOrElse(fail("nothing pending"))
        // the pool's 500 ADA earns less than its cost, so its operator, the reward account of a
        // pool given by poolId, takes the whole pool reward; nothing pays fees
        assert(update.rewards.length == 1)
        val reward = update.rewards(0)
        assert(reward.credential == pool)
        val operatorStake =
            StakeAddress(Network.Mainnet, StakePayload.Stake(StakeKeyHash.fromHex(pool)))
        assert(reward.rewardAddress == operatorStake.toBech32.get)
        assert(BigInt(reward.amount.toString) > 0)
        assert(update.deltaF.toString == "0")
        // spec [RW-9]: the four parts sum to 0
        val parts = Seq(update.deltaT, update.deltaR, reward.amount, update.deltaF)
        assert(parts.map(p => BigInt(p.toString)).sum == 0)
        assert(BigInt(update.deltaT.toString) > 0)
    }
}
