package scalus.cardano.node

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.STS
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.uplc.builtin.{ByteString, Data}

import java.time.Instant

/** The `Manual` preset with its mempool, milestone M2 of the spec: revalidation, block limits,
  * mempool capacity, views and tx status.
  */
class EmulatorMempoolTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    private given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private val alice = Alice.address(Network.Mainnet)
    private val bob = Bob.address(Network.Mainnet)

    private val initialUtxos: Utxos = (0 until 4).map { i =>
        Input(genesisHash, i) -> Output(alice, Value.ada(1000))
    }.toMap

    private def emulator(config: ChainConfig): Emulator =
        Emulator.withState(EmulatorInitialState(initialUtxos), rules.Context.testMainnet(), config)

    private def stateOf(emulator: Emulator): EmulatorState = EmulatorBase.stateOf(emulator)

    private def payment(utxos: Utxos, lovelace: Long): Transaction =
        TxBuilder(testEnv)
            .payTo(bob, Value.lovelace(lovelace))
            .complete(utxos.filter(_._2.address == alice), alice)
            .sign(Alice.signer)
            .transaction

    private def paymentValidTo(utxos: Utxos, lovelace: Long, slot: SlotNo): Transaction =
        TxBuilder(testEnv)
            .payTo(bob, Value.lovelace(lovelace))
            .validTo(Instant.ofEpochMilli(testEnv.slotConfig.slotToTime(slot)))
            .complete(utxos.filter(_._2.address == alice), alice)
            .sign(Alice.signer)
            .transaction

    private def toBob(e: Emulator): Utxos = e.findUtxosSync(UtxoQuery(UtxoSource.FromAddress(bob)))

    /** The UTxO set after the mempool txs, which is what a chained tx spends from. */
    private def mempoolUtxos(emulator: Emulator): Utxos =
        stateOf(emulator).mempool.lastOption.fold(emulator.utxos)(_.snapshot.ledger.utxos)

    /** `==` on two snapshots, except the gov state: an `Array`, so `==` compares its identity. */
    private def assertSameSnapshot(a: LedgerSnapshot, b: LedgerSnapshot): Unit = {
        assert(a.ledger.govState.sameElements(b.ledger.govState))
        val gov = GovState.empty
        def normal(s: LedgerSnapshot) = s.copy(ledger = s.ledger.copy(govState = gov))
        assert(normal(a) == normal(b))
    }

    private enum Op {
        case Submit(lovelace: Long)
        case SubmitChained(lovelace: Long)
        case Tick(slots: Long)
        case AddUtxo(index: Int)
        case RemoveUtxo(index: Int)
        case AddDatum(value: Int)
        case ProduceBlock
    }

    private val genOp: Gen[Op] = Gen.frequency(
      3 -> Gen.choose(1_000_000L, 50_000_000L).map(Op.Submit(_)),
      3 -> Gen.choose(1_000_000L, 50_000_000L).map(Op.SubmitChained(_)),
      2 -> Gen
          .oneOf(Gen.choose(0L, 100L), Gen.const(testEnv.slotConfig.epochLength))
          .map(Op.Tick(_)),
      1 -> Gen.choose(10, 12).map(Op.AddUtxo(_)),
      1 -> Gen.choose(0, 3).map(Op.RemoveUtxo(_)),
      1 -> Gen.choose(0, 1000).map(Op.AddDatum(_)),
      1 -> Gen.const(Op.ProduceBlock)
    )

    private def run(ops: Seq[Op]): Emulator = {
        val e = emulator(ChainConfig.manual)
        ops.foreach {
            case Op.Submit(lovelace)        => e.submitSync(payment(e.utxos, lovelace))
            case Op.SubmitChained(lovelace) => e.submitSync(payment(mempoolUtxos(e), lovelace))
            case Op.Tick(n)                 => e.tick(n)
            case Op.AddUtxo(i)    => e.addUtxo(Input(genesisHash, i), Output(alice, Value.ada(7)))
            case Op.RemoveUtxo(i) => e.removeUtxo(Input(genesisHash, i))
            case Op.AddDatum(n)   => e.addDatum(Data.I(n))
            case Op.ProduceBlock  => e.produceBlock()
        }
        e
    }

    test("each mempool snapshot is the tip snapshot plus the mempool txs, applied in order") {
        // spec [TE-2], [RV-1], [RV-7], [DE-3]
        forAll(Gen.listOfN(8, genOp), minSuccessful(15)) { ops =>
            val e = run(ops)
            val state = stateOf(e)
            state.mempool.foldLeft(state.tipSnapshot) { (base, entry) =>
                val ticked = base.atSlot(e.currentSlotSync)
                val ledger = STS.Mutator
                    .transit(
                      e.validators,
                      e.mutators,
                      ticked.context,
                      ticked.ledger,
                      entry.applied.tx
                    )
                    .fold(err => fail(s"a mempool tx fails on the tip: $err"), identity)
                val expected = ticked.after(ledger, entry.applied.tx)
                assertSameSnapshot(entry.snapshot, expected)
                assert(entry.applied.slot == e.currentSlotSync, "spec [TS-6]")
                expected
            }
        }
    }

    test("Manual: a submitted tx is invisible to queries until produceBlock includes it") {
        // spec [TE-5], [VW-1], [FG-7], [MP-7]
        val e = emulator(ChainConfig.manual)
        val tx = payment(e.utxos, 2_000_000)
        assert(e.submitSync(tx) == Right(tx.id))
        assert(toBob(e).isEmpty)
        assert(e.utxos == initialUtxos)
        assert(e.txStatus(tx.id) == TxStatus.Pending)
        assert(e.mempoolTxs == Seq(tx))
        val point = e.produceBlock()
        assert(toBob(e).keySet.exists(_.transactionId == tx.id))
        assert(e.mempoolTxs.isEmpty)
        assert(e.tip.point == point)
        assert(e.txStatus(tx.id) == TxStatus.Confirmed(1, point))
    }

    test("Manual: produceBlock forges at max(current slot, tip slot + 1) and moves the slot") {
        // spec [FG-1], [FG-3], section 8.1 example
        val e = emulator(ChainConfig.manual)
        e.setSlot(50)
        assert(e.produceBlock().slot == 50L)
        assert(e.produceBlock().slot == 51L)
        assert(e.currentSlotSync == 51L)
        e.setSlot(70)
        assert(e.produceBlock().slot == 70L)
        assert(e.tip.blockNo == 3L)
    }

    test("Manual: produceBlock drops a tx that expires at the forge slot") {
        // spec [FG-3]: the slot moves, then revalidation, then the forge
        val e = emulator(ChainConfig.manual)
        e.setSlot(99)
        e.produceBlock()
        val tx = paymentValidTo(e.utxos, 2_000_000, 100)
        assert(e.submitSync(tx).isRight)
        val point = e.produceBlock()
        assert(point.slot == 100L)
        assert(e.txStatus(tx.id) == TxStatus.Dropped(DropReason.Expired))
        assert(stateOf(e).selectedChain.last.txs.isEmpty)
    }

    test("Manual: produceBlock revalidates at the forge slot even when the slot does not move") {
        // spec [FG-3], [RV-2]: a tx on the selected chain leaves the mempool before the forge, so
        // a confirmed tx submitted again after a direct edit is not forged a second time
        val e = emulator(ChainConfig.manual)
        val tx = payment(e.utxos, 2_000_000)
        val inputs = tx.body.value.inputs.toSeq.map(i => i -> e.utxos(i))
        assert(e.submitSync(tx).isRight)
        val first = e.produceBlock()
        e.setSlot(10)
        inputs.foreach(e.addUtxo)
        assert(e.submitSync(tx).isRight)
        assert(e.produceBlock().slot == 10L, "the forge slot is the current slot")
        assert(stateOf(e).selectedChain.last.txs.isEmpty)
        assert(e.mempoolTxs.isEmpty)
        assert(e.txStatus(tx.id) == TxStatus.Confirmed(2, first))
    }

    test("Manual: produceBlock with an empty mempool forges an empty block") {
        // spec [FG-8]
        val e = emulator(ChainConfig.manual)
        val point = e.produceBlock()
        val tip = stateOf(e).selectedChain.last
        assert(tip.id == point.blockHash)
        assert(tip.block.transactionBodies.isEmpty)
    }

    test("Instant: an explicit produceBlock forges an empty block at the FG-2 slot") {
        // spec [FG-8], [FG-2], [FG-3a]
        val e = emulator(ChainConfig.instant)
        e.setSlot(30)
        assert(e.produceBlock() == e.tip.point)
        assert(e.tip.blockNo == 1L)
        assert(stateOf(e).selectedChain.last.txs.isEmpty)
        assert(e.tip.slot == 30L)
        assert(e.currentSlotSync == 30L)
    }

    test("Confirmed.depth is the depth of the block at the moment of the call") {
        // spec [TS-2], [TS-7]
        val e = emulator(ChainConfig.manual)
        val tx = payment(e.utxos, 2_000_000)
        assert(e.submitSync(tx).isRight)
        val point = e.produceBlock()
        e.produceBlock()
        e.produceBlock()
        assert(e.txStatus(tx.id) == TxStatus.Confirmed(3, point))
    }

    test("txStatus is NotFound for a tx never seen") {
        // spec [TS-1]
        val e = emulator(ChainConfig.manual)
        assert(e.txStatus(payment(e.utxos, 2_000_000).id) == TxStatus.NotFound)
    }

    test("a TTL drop at the boundary: kept at slot 99, Expired at slot 100") {
        // spec [TE-6], [RV-3], [RV-4]
        val e = emulator(ChainConfig.manual)
        val tx = paymentValidTo(e.utxos, 2_000_000, 100)
        assert(tx.body.value.ttl.contains(100L), "the builder sets invalidHereafter = 100")
        assert(e.submitSync(tx).isRight)
        e.setSlot(99)
        assert(e.txStatus(tx.id) == TxStatus.Pending)
        e.setSlot(100)
        assert(e.txStatus(tx.id) == TxStatus.Dropped(DropReason.Expired))
        assert(e.mempoolTxs.isEmpty)
    }

    test("a removeUtxo under a pending tx drops it with InputsSpent") {
        // spec [DE-3], [RV-5]
        val e = emulator(ChainConfig.manual)
        val tx = payment(e.utxos, 2_000_000)
        assert(e.submitSync(tx).isRight)
        tx.body.value.inputs.toSeq.foreach(e.removeUtxo)
        assert(e.txStatus(tx.id) == TxStatus.Dropped(DropReason.InputsSpent))
    }

    test("a tx that fails another rule after a direct edit drops with Invalid") {
        // spec [RV-6]: the tx spends a UTxO that a direct edit makes worth less than it pays
        val e = emulator(ChainConfig.manual)
        val tx = payment(e.utxos, 2_000_000)
        assert(e.submitSync(tx).isRight)
        val input = tx.body.value.inputs.toSeq.head
        e.addUtxo(input, Output(alice, Value.ada(500)))
        e.txStatus(tx.id) match
            case TxStatus.Dropped(DropReason.Invalid(error)) =>
                assert(error.rule == "ValueNotConserved")
            case other => fail(s"expected Dropped(Invalid), got $other")
    }

    test("a submit after addUtxo validates on the edited mempool tail") {
        // spec [DE-3], [MP-1]; M1 review finding R9
        val e = emulator(ChainConfig.manual)
        assert(e.submitSync(payment(e.utxos, 2_000_000)).isRight)
        val extra = Input(genesisHash, 20) -> Output(bob, Value.ada(50))
        e.addUtxo(extra._1, extra._2)
        val spendsExtra = TxBuilder(testEnv)
            .spend(Utxo(extra))
            .payTo(alice, Value.ada(10))
            .complete(Map(extra), bob)
            .sign(Bob.signer)
            .transaction
        assert(e.submitSync(spendsExtra) == Right(spendsExtra.id))
    }

    test("a dropped tx that is submitted again is Pending") {
        // spec [TS-1]: the mempool wins over a stored drop
        val e = emulator(ChainConfig.manual)
        val tx = payment(e.utxos, 2_000_000)
        assert(e.submitSync(tx).isRight)
        val input = tx.body.value.inputs.toSeq.head
        val output = e.utxos(input)
        e.removeUtxo(input)
        e.addUtxo(input, output)
        assert(e.submitSync(tx).isRight)
        assert(e.txStatus(tx.id) == TxStatus.Pending)
    }

    test("Manual: setSlot below the tip slot is rejected") {
        // spec [RV-8]
        val e = emulator(ChainConfig.manual)
        e.setSlot(50)
        e.produceBlock()
        e.setSlot(50)
        assertThrows[IllegalArgumentException](e.setSlot(49))
        assert(e.currentSlotSync == 50L)
    }

    test("checkTransaction maps Pending and Confirmed, and every other status to NotFound") {
        // spec [TS-3], [TS-4]
        val e = emulator(ChainConfig.manual)
        def check(hash: TransactionHash) = e.checkTransaction(hash).value.get.get
        val confirmed = payment(e.utxos, 2_000_000)
        assert(e.submitSync(confirmed).isRight)
        assert(check(confirmed.id) == TransactionStatus.Pending)
        e.produceBlock()
        assert(check(confirmed.id) == TransactionStatus.Confirmed)
        val dropped = paymentValidTo(e.utxos, 3_000_000, 100)
        assert(e.submitSync(dropped).isRight)
        e.setSlot(100)
        assert(e.txStatus(dropped.id) == TxStatus.Dropped(DropReason.Expired))
        assert(check(dropped.id) == TransactionStatus.NotFound)
    }

    test("checkTransaction reports a tx of the imported log as Confirmed, as before") {
        // spec [TS-4] implementation status: the imported log has no block
        val source = emulator(ChainConfig.instant)
        val tx = payment(source.utxos, 2_000_000)
        assert(source.submitSync(tx).isRight)
        val restored = new Emulator(
          source.utxos,
          rules.Context.testMainnet(),
          Emulator.defaultValidators,
          Emulator.defaultMutators,
          CertState.empty,
          Map.empty,
          source.appliedTxLog,
          ChainConfig.manual
        )
        assert(restored.checkTransaction(tx.id).value.get.get == TransactionStatus.Confirmed)
        assert(restored.txStatus(tx.id) == TxStatus.NotFound)
    }

    private val toBobQuery = UtxoQuery(UtxoSource.FromAddress(bob))

    private def seesOutputsOf(e: Emulator, view: ChainView, tx: Transaction): Boolean =
        e.findUtxosSync(toBobQuery, view)
            .fold(
              err => fail(s"view $view fails: $err"),
              _.keySet.exists(_.transactionId == tx.id)
            )

    test("the views read the tip, the mempool and the block of a given depth") {
        // spec [VW-3], [VW-4], [VW-5], section 9 example: tx A in block 4 of 5, tx B in the mempool
        val e = emulator(ChainConfig.manual)
        (1 to 3).foreach(_ => e.produceBlock())
        val a = payment(e.utxos, 2_000_000)
        assert(e.submitSync(a).isRight)
        e.produceBlock()
        e.produceBlock()
        val b = payment(mempoolUtxos(e), 3_000_000)
        assert(e.submitSync(b).isRight)
        assert(e.tip.blockNo == 5L)
        val expected = Seq(
          ChainView.Tip -> (true, false),
          ChainView.Mempool -> (true, true),
          ChainView.Stable(1) -> (true, false),
          ChainView.Stable(2) -> (true, false),
          ChainView.Stable(3) -> (false, false)
        )
        for (view, (seesA, seesB)) <- expected do
            assert(seesOutputsOf(e, view, a) == seesA, s"$view and A")
            assert(seesOutputsOf(e, view, b) == seesB, s"$view and B")
        assert(e.findUtxos(toBobQuery, ChainView.Tip).value.get.get == Right(toBob(e)))
    }

    test("Stable(d) reads genesis at the tip height plus 1, and fails outside 1 .. height + 1") {
        // spec [VW-6], [VW-8]
        val e = emulator(ChainConfig.manual)
        assert(e.submitSync(payment(e.utxos, 2_000_000)).isRight)
        e.produceBlock()
        val all = UtxoQuery(UtxoSource.FromAddress(alice))
        assert(e.findUtxosSync(all, ChainView.Stable(2)) == Right(initialUtxos))
        assert(
          e.findUtxosSync(all, ChainView.Stable(0)) == Left(ChainViewError.DepthOutOfRange(0, 1L))
        )
        assert(
          e.findUtxosSync(all, ChainView.Stable(3)) == Left(ChainViewError.DepthOutOfRange(3, 1L))
        )
    }

    test("Stable(d) fails for a block deeper than k + 1, whose snapshot is dropped") {
        // spec [VW-7], [ST-4]
        val e = emulator(ChainConfig.manual.withSecurityParam(2))
        (1 to 3).foreach(_ => e.produceBlock())
        assert(e.findUtxosSync(toBobQuery, ChainView.Stable(3)).isRight, "the anchor")
        assert(
          e.findUtxosSync(toBobQuery, ChainView.Stable(4)) ==
              Left(ChainViewError.SnapshotDropped(4))
        )
    }

    test("currentSlot is the current slot, which can be ahead of the tip slot") {
        // spec [VW-9]
        val e = emulator(ChainConfig.manual)
        e.produceBlock()
        e.setSlot(40)
        assert(e.currentSlot.value.get.get == 40L)
        assert(e.tip.slot == 1L)
    }

    test("pollForConfirmation with ProduceBlock forges until the tx is confirmed") {
        // spec [SF-2], [SF-3]
        val e = emulator(ChainConfig.manual)
        val tx = payment(e.utxos, 2_000_000)
        assert(e.submitSync(tx).isRight)
        val status = e.pollForConfirmation(tx.id, 5, 60_000L)
        assert(status.isCompleted, "no sleep")
        assert(status.value.get.get == TransactionStatus.Confirmed)
        assert(e.tip.blockNo == 1L, "one block was enough")
    }

    test("pollForConfirmation with ProduceBlock forges at most maxAttempts blocks") {
        // spec [SF-3]
        val e = emulator(ChainConfig.manual)
        val unknown = payment(e.utxos, 2_000_000).id
        assert(e.pollForConfirmation(unknown, 3, 0L).value.get.get == TransactionStatus.NotFound)
        assert(e.tip.blockNo == 0L, "a tx that is not pending forges nothing")
        val tx = paymentValidTo(e.utxos, 2_000_000, 100)
        assert(e.submitSync(tx).isRight)
        assert(e.pollForConfirmation(tx.id, 0, 0L).value.get.get == TransactionStatus.Pending)
        assert(e.tip.blockNo == 0L)
    }

    test("pollForConfirmation with WaitOnly reports the status and forges nothing") {
        // spec [SF-4]
        val e = emulator(ChainConfig.manual.withAwaitTx(AwaitTxPolicy.WaitOnly))
        val tx = payment(e.utxos, 2_000_000)
        assert(e.submitSync(tx).isRight)
        assert(e.pollForConfirmation(tx.id, 5, 0L).value.get.get == TransactionStatus.Pending)
        assert(e.tip.blockNo == 0L)
    }

    test("submitAndPoll confirms a tx in the Manual preset") {
        // spec [SF-3]: off-the-shelf dApp code awaits a tx
        val e = emulator(ChainConfig.manual)
        val tx = payment(e.utxos, 2_000_000)
        assert(e.submitAndPoll(tx).value.get.get == Right(tx.id))
    }

    /** A payment from the genesis input `index` alone, so the txs do not conflict. */
    private def paymentFrom(index: Int, lovelace: Long): Transaction = {
        val utxo = Input(genesisHash, index) -> initialUtxos(Input(genesisHash, index))
        TxBuilder(testEnv)
            .spend(Utxo(utxo))
            .payTo(bob, Value.lovelace(lovelace))
            .complete(Map(utxo), alice)
            .sign(Alice.signer)
            .transaction
    }

    private def manualWithBlockBodySize(maxBlockBodySize: Long): Emulator = {
        val context = rules.Context.testMainnet()
        val params = context.env.params.copy(maxBlockBodySize = maxBlockBodySize)
        Emulator.withState(
          EmulatorInitialState(initialUtxos),
          context.copy(env = context.env.copy(params = params)),
          ChainConfig.manual
        )
    }

    test("a mempool that needs two blocks: the first takes the prefix that fits") {
        // spec [TE-8], [FG-4], [FG-7]
        val txs = (0 until 3).map(i => paymentFrom(i, 2_000_000L + i))
        val e = manualWithBlockBodySize(BlockForge.bodySize(txs.take(2)))
        txs.foreach(tx => assert(e.submitSync(tx) == Right(tx.id)))
        e.produceBlock()
        assert(stateOf(e).selectedChain.last.txs.map(_.tx) == txs.take(2))
        assert(e.mempoolTxs == txs.drop(2))
        e.produceBlock()
        assert(stateOf(e).selectedChain.last.txs.map(_.tx) == txs.drop(2))
        assert(e.mempoolTxs.isEmpty)
    }

    test("pollForConfirmation forges as many blocks as the tx needs") {
        // spec [SF-3], [FG-4]
        val txs = (0 until 3).map(i => paymentFrom(i, 2_000_000L + i))
        val e = manualWithBlockBodySize(BlockForge.bodySize(txs.take(2)))
        txs.foreach(tx => assert(e.submitSync(tx).isRight))
        assert(e.pollForConfirmation(txs(2).id, 5, 0L).value.get.get == TransactionStatus.Confirmed)
        assert(e.tip.blockNo == 2L)
    }

    test("a valid tx that would push the mempool past two blocks is rejected with MempoolFull") {
        // spec [MP-4]: capacity 2 * limit holds 3 txs, the 4th does not fit
        val txs = (0 until 4).map(i => paymentFrom(i, 2_000_000L + i))
        val limit = (BlockForge.bodySize(txs.take(3)) + 1) / 2
        assert(BlockForge.bodySize(txs) > 2 * limit)
        val e = manualWithBlockBodySize(limit)
        txs.take(3).foreach(tx => assert(e.submitSync(tx).isRight))
        assert(e.submitSync(txs(3)).left.map(_.rule) == Left("MempoolFull"))
        assert(e.mempoolTxs == txs.take(3))
    }

    test("an invalid tx gets the error of its rule even when the mempool is full") {
        // spec [MP-3], [MP-4]
        val txs = (0 until 3).map(i => paymentFrom(i, 2_000_000L + i))
        val e = manualWithBlockBodySize((BlockForge.bodySize(txs.take(2)) + 1) / 2)
        txs.take(2).foreach(tx => assert(e.submitSync(tx).isRight))
        assert(e.submitSync(txs(0)).left.map(_.rule) == Left("UtxoNotAvailable"))
    }
}
