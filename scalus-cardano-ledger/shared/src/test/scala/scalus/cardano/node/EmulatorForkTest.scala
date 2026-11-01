package scalus.cardano.node

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.node.emulator.{BlockForge, ChainConfig, DropReason, ForgeError, LedgerSnapshot, OrphanTxPolicy, RollbackError, TxStatus}
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.uplc.builtin.ByteString

import scala.annotation.nowarn

/** Forks, milestone M3 of the spec: branch forging, chain selection, orphan txs, rollback and
  * branch pruning.
  */
class EmulatorForkTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    private given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private val alice = Alice.address(Network.Mainnet)
    private val bob = Bob.address(Network.Mainnet)

    private val initialUtxos: Utxos = (0 until 4).map { i =>
        Input(genesisHash, i) -> Output(alice, Value.ada(1000))
    }.toMap

    private val manual = ChainConfig.manual

    private def emulator(config: ChainConfig): Emulator =
        Emulator.withState(EmulatorInitialState(initialUtxos), rules.Context.testMainnet(), config)

    private def stateOf(emulator: Emulator): EmulatorState = EmulatorBase.stateOf(emulator)

    private def payment(utxos: Utxos, lovelace: Long): Transaction =
        TxBuilder(testEnv)
            .payTo(bob, Value.lovelace(lovelace))
            .complete(utxos.filter(_._2.address == alice), alice)
            .sign(Alice.signer)
            .transaction

    /** A payment that spends exactly the genesis UTxO `i`, so two of them can double-spend. */
    private def pay(i: Int, lovelace: Long): Transaction =
        payment(initialUtxos.filter(_._1.index == i), lovelace)

    private def ok[A](result: Either[?, A]): A = result.fold(e => fail(s"unexpected $e"), identity)

    private def chainIds(e: Emulator): Vector[BlockHash] = stateOf(e).selectedChain.map(_.id)

    private def ids(points: ChainPoint*): Vector[BlockHash] = points.map(_.blockHash).toVector

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

    test("a block on the tip is forged at on.slot + 1 and extends the selected chain") {
        // spec [FG-11], [FG-15], [FG-19], [CS-1], [TS-6]
        val e = emulator(manual)
        val first = e.produceBlock()
        val tx = pay(0, 2_000_000)
        val point = ok(e.produceBlock(first, Seq(tx)))
        assert(point.slot == first.slot + 1)
        assert(e.tip.point == point)
        assert(e.txStatus(tx.id) == TxStatus.Confirmed(1, point))
        assert(stateOf(e).selectedChain.last.txs.map(_.slot) == Vector(point.slot))
        assert(e.utxos.keySet.exists(_.transactionId == tx.id))
    }

    test("a block on genesis is forged at slot 1, and with an explicit slot at that slot") {
        // spec [FG-11], genesis is ChainPoint.origin
        val e = emulator(manual)
        val point = ok(e.produceBlock(ChainPoint.origin, Seq(pay(0, 2_000_000))))
        assert(point.slot == 1L)
        assert(e.tip.blockNo == 1L)
        val e2 = emulator(manual)
        assert(ok(e2.produceBlock(ChainPoint.origin, 7L, Seq.empty)).slot == 7L)
    }

    test("forging the same block again is a no-op that returns its point") {
        // spec section 4 "Consequence": same parent, slot and txs make the same block
        val e = emulator(manual)
        val tx = pay(0, 2_000_000)
        val point = ok(e.produceBlock(ChainPoint.origin, Seq(tx)))
        val before = stateOf(e)
        assert(e.produceBlock(ChainPoint.origin, Seq(tx)) == Right(point))
        assert(stateOf(e) eq before)
    }

    test("produceBlock(on, ...) rejects a bad slot, an unknown or deep point, an invalid tx") {
        // spec [FG-12], [FG-13], [FG-14], [FG-16], [FG-17]
        val e = emulator(manual.withSecurityParam(2))
        val points = (1 to 4).map(_ => e.produceBlock())
        val (p2, p3, p4) = (points(1), points(2), points(3))
        val before = stateOf(e)
        assert(
          e.produceBlock(p4, p4.slot, Seq.empty) ==
              Left(ForgeError.SlotNotAfterParent(p4.slot, p4.slot))
        )
        val wrongSlot = ChainPoint(p4.slot + 1, p4.blockHash)
        assert(e.produceBlock(wrongSlot, Seq.empty) == Left(ForgeError.UnknownPoint(wrongSlot)))
        val unknown = ChainPoint(3L, BlockHash.fromByteString(ByteString.fromHex("ff" * 32)))
        assert(e.produceBlock(unknown, Seq.empty) == Left(ForgeError.UnknownPoint(unknown)))
        assert(ok(e.produceBlock(p3, Seq(pay(3, 2_000_000)))).slot == p3.slot + 1, "depth 2 = k")
        assert(e.produceBlock(p2, Seq.empty) == Left(ForgeError.TooDeep(3, 2)), "the anchor")
        assert(e.produceBlock(ChainPoint.origin, Seq.empty) == Left(ForgeError.TooDeep(5, 2)))
        val afterBranch = stateOf(e)
        e.produceBlock(p4, Seq(pay(0, 2_000_000), pay(0, 3_000_000))) match
            case Left(ForgeError.InvalidTx(1, _: NodeSubmitError.UtxoNotAvailable)) =>
            case other => fail(s"expected InvalidTx(1, UtxoNotAvailable), got $other")
        assert(stateOf(e) eq afterBranch)
        assert(before ne afterBranch)
    }

    test("an unknown point is UnknownPoint, even with a slot not after it") {
        // spec [FG-13] before [FG-12]: the slot of a point that names no block means nothing
        val e = emulator(manual)
        val unknown = ChainPoint(100L, BlockHash.fromByteString(ByteString.fromHex("ff" * 32)))
        assert(e.produceBlock(unknown, 50L, Seq.empty) == Left(ForgeError.UnknownPoint(unknown)))
    }

    test("produceBlock(on, ...) rejects txs over the block limits, and leaves the state") {
        // spec [FG-18], [FG-17]
        val txs = Vector(pay(0, 2_000_000), pay(1, 2_000_000))
        val context = rules.Context.testMainnet()
        val params = context.env.params.copy(maxBlockBodySize = BlockForge.bodySize(txs.take(1)))
        val e = Emulator.withState(
          EmulatorInitialState(initialUtxos),
          context.copy(env = context.env.copy(params = params)),
          manual
        )
        val before = stateOf(e)
        assert(e.produceBlock(ChainPoint.origin, txs) == Left(ForgeError.BlockTooBig))
        assert(
          ForgeError.BlockTooBig.message ==
              "the txs exceed the block body size or execution unit limits"
        )
        assert(stateOf(e) eq before)
        assert(e.produceBlock(ChainPoint.origin, txs.take(1)).isRight)
    }

    test("chain selection follows the example of section 11.1") {
        // spec [CS-1], [CS-2], [FG-19], [FG-21], [CS-4]
        val e = emulator(manual)
        val (p1, p2, p3) = (e.produceBlock(), e.produceBlock(), e.produceBlock())
        val b2 = ok(e.produceBlock(p1, Seq(pay(0, 2_000_000))))
        assert(chainIds(e) == ids(p1, p2, p3), "the branch is shorter")
        assert(e.currentSlotSync == 3L, "a branch block below the current slot does not move it")
        val b3 = ok(e.produceBlock(b2, Seq.empty))
        assert(chainIds(e) == ids(p1, p2, p3), "a tie keeps the current chain")
        val b4 = ok(e.produceBlock(b3, Seq.empty))
        assert(chainIds(e) == ids(p1, b2, b3, b4), "the longer branch wins")
        assert(e.tip.point == b4)
        assert(e.currentSlotSync == b4.slot)
        assert(stateOf(e).tipSnapshot.context.env.slot == b4.slot)
    }

    test("after a switch the current slot is max(current slot, new tip slot)") {
        // spec [CS-4], [FG-21]
        val e = emulator(manual)
        val p1 = e.produceBlock()
        e.setSlot(50)
        val b = ok(e.produceBlock(p1, Seq.empty))
        assert(b.slot == 2L)
        assert(e.tip.point == b)
        assert(e.currentSlotSync == 50L)
        assert(stateOf(e).tipSnapshot.context.env.slot == 50L)
    }

    test("a branch block does not touch the mempool unless the chain switches") {
        // spec [FG-20]
        val e = emulator(manual)
        val p1 = e.produceBlock()
        e.produceBlock()
        val pending = pay(0, 2_000_000)
        assert(e.submitSync(pending).isRight)
        val before = stateOf(e).mempool
        ok(e.produceBlock(p1, Seq(pay(0, 3_000_000))))
        assert(stateOf(e).mempool == before)
        assert(e.txStatus(pending.id) == TxStatus.Pending)
    }

    for policy <- OrphanTxPolicy.values do
        test(s"rival double spend on a longer branch, $policy: the table of section 11.2") {
            // spec [TE-7], [CS-5], [CS-6], [OR-1] .. [OR-4], [TS-2], [TS-7]
            val e = emulator(manual.withOrphanTxs(policy))
            val p1 = e.produceBlock()
            val (a, b, c) = (pay(0, 2_000_000), pay(1, 2_000_000), pay(2, 2_000_000))
            assert(e.submitSync(a).isRight)
            assert(e.submitSync(b).isRight)
            val p2 = e.produceBlock()
            assert(e.submitSync(c).isRight)
            val rival = pay(1, 3_000_000)
            val r2 = ok(e.produceBlock(p1, Seq(rival)))
            assert(e.tip.point == p2)
            assert(e.txStatus(b.id) == TxStatus.Confirmed(1, p2))
            val r3 = ok(e.produceBlock(r2, Seq.empty))
            assert(e.tip.point == r3)
            assert(e.txStatus(rival.id) == TxStatus.Confirmed(2, r2))
            policy match
                case OrphanTxPolicy.ReturnToMempool =>
                    assert(e.mempoolTxs == Seq(a, c))
                    assert(e.txStatus(a.id) == TxStatus.Pending)
                    assert(e.txStatus(b.id) == TxStatus.Dropped(DropReason.InputsSpent))
                case OrphanTxPolicy.Drop =>
                    assert(e.mempoolTxs == Seq(c))
                    assert(e.txStatus(a.id) == TxStatus.RolledBack)
                    assert(e.txStatus(b.id) == TxStatus.RolledBack)
            assert(e.txStatus(c.id) == TxStatus.Pending)
            assert(e.appliedTxLog.map(_.txHash) == Vector(rival.id))
        }

    test("Instant: orphan txs return to the mempool and are forged on the new tip") {
        // spec [OR-5], [CS-5], [FG-9]
        val e = emulator(ChainConfig.instant)
        val (a, b) = (pay(0, 2_000_000), pay(1, 2_000_000))
        assert(e.submitSync(a).isRight)
        val p1 = e.tip.point
        assert(e.submitSync(b).isRight)
        val rival = pay(2, 3_000_000)
        val r2 = ok(e.produceBlock(p1, Seq(rival)))
        val r3 = ok(e.produceBlock(r2, Seq.empty))
        assert(e.tip.blockNo == 4L)
        assert(chainIds(e).take(3) == ids(p1, r2, r3))
        assert(e.mempoolTxs.isEmpty)
        assert(e.txStatus(b.id) == TxStatus.Confirmed(1, e.tip.point))
        assert(e.tip.slot == r3.slot, "forged at max(current slot, tip slot)")
    }

    test("Instant with Drop: orphan txs are rolled back and nothing is forged") {
        // spec [OR-4], [OR-5]
        val e = emulator(ChainConfig.instant.withOrphanTxs(OrphanTxPolicy.Drop))
        val (a, b) = (pay(0, 2_000_000), pay(1, 2_000_000))
        assert(e.submitSync(a).isRight)
        val p1 = e.tip.point
        assert(e.submitSync(b).isRight)
        val r2 = ok(e.produceBlock(p1, Seq.empty))
        val r3 = ok(e.produceBlock(r2, Seq.empty))
        assert(e.tip.point == r3)
        assert(e.txStatus(b.id) == TxStatus.RolledBack)
        assert(!e.utxos.keySet.exists(_.transactionId == b.id))
        assert(e.utxos.contains(Input(genesisHash, 1)))
    }

    test("rollback rejects n < 1, n > k and n above the tip height") {
        // spec [RB-1], [RB-2], [RB-3]
        val e = emulator(manual.withSecurityParam(2))
        e.produceBlock()
        val before = stateOf(e)
        assert(e.rollback(0) == Left(RollbackError.NotPositive(0)))
        assert(e.rollback(-1) == Left(RollbackError.NotPositive(-1)))
        assert(e.rollback(3) == Left(RollbackError.TooDeep(3, 2)))
        assert(e.rollback(2) == Left(RollbackError.PastGenesis(2, 1)))
        assert(stateOf(e) eq before)
    }

    test("rollback(k) reaches the anchor, and rollback to genesis works") {
        // spec [RB-2], [RB-5], [ST-4] anchor
        val e = emulator(manual.withSecurityParam(2))
        val p1 = e.produceBlock()
        e.produceBlock()
        e.produceBlock()
        assert(e.rollback(2) == Right(p1))
        val g = emulator(manual.withSecurityParam(2))
        g.produceBlock()
        g.produceBlock()
        assert(g.rollback(2) == Right(ChainPoint.origin))
        assert(g.tip == ChainTip.origin)
        assert(g.utxos == initialUtxos)
    }

    test("a second rollback past the dropped snapshots is TooDeep") {
        // spec [RB-2]: the anchor is gone, as the node cannot roll back past its immutable tip
        val e = emulator(manual.withSecurityParam(2))
        (1 to 5).foreach(_ => e.produceBlock())
        assert(e.rollback(2).isRight)
        val before = stateOf(e)
        assert(e.rollback(2) == Left(RollbackError.TooDeep(2, 2)))
        assert(stateOf(e) eq before)
        assert(e.rollback(1) == Left(RollbackError.TooDeep(1, 2)), "block 2 is past the anchor too")
    }

    test("rollback follows the example of section 11.3: it returns the tip after selection") {
        // spec [RB-4], [RB-5], [RB-9], [CS-7]
        val e = emulator(manual)
        val (p1, p2, p3) = (e.produceBlock(), e.produceBlock(), e.produceBlock())
        e.produceBlock()
        val b2 = ok(e.produceBlock(p1, Seq(pay(0, 2_000_000))))
        val onRemoved = ok(e.produceBlock(p2, Seq(pay(1, 2_000_000))))
        assert(e.rollback(3) == Right(b2))
        assert(chainIds(e) == ids(p1, b2))
        val blocks = stateOf(e).blocks
        assert(!blocks.contains(p3.blockHash))
        assert(!blocks.contains(onRemoved.blockHash), "a descendant of a removed block")
        assert(e.produceBlock(p3, Seq.empty) == Left(ForgeError.UnknownPoint(p3)))
    }

    test("rollback keeps the current slot, and the new tip snapshot is ticked to it") {
        // spec [RB-6], [CS-4]
        val e = emulator(manual)
        val (p1, _, _) = (e.produceBlock(), e.produceBlock(), e.produceBlock())
        e.setSlot(40)
        assert(e.rollback(2) == Right(p1))
        assert(e.currentSlotSync == 40L)
        assert(e.tip.slot == p1.slot)
    }

    for policy <- OrphanTxPolicy.values do
        test(s"rollback handles the removed txs by the policy, then revalidates: $policy") {
            // spec [RB-7], [RB-8], [OR-1], [OR-2], [OR-3], [OR-4]
            val e = emulator(manual.withOrphanTxs(policy))
            val a = pay(0, 2_000_000)
            assert(e.submitSync(a).isRight)
            e.produceBlock()
            val chained = payment(e.utxos.filter(_._1.transactionId == a.id), 1_500_000)
            assert(e.submitSync(chained).isRight)
            assert(e.rollback(1) == Right(ChainPoint.origin))
            policy match
                case OrphanTxPolicy.ReturnToMempool =>
                    assert(e.mempoolTxs == Seq(a, chained))
                    assert(e.txStatus(a.id) == TxStatus.Pending)
                    assert(e.txStatus(chained.id) == TxStatus.Pending)
                case OrphanTxPolicy.Drop =>
                    assert(e.mempoolTxs.isEmpty)
                    assert(e.txStatus(a.id) == TxStatus.RolledBack)
                    assert(e.txStatus(chained.id) == TxStatus.Dropped(DropReason.InputsSpent))
            assert(e.utxos == initialUtxos)
        }

    test("Instant, rollback(1) with ReturnToMempool forges the orphan tx again at once") {
        // spec [OR-5], [RB-7], example under section 11.3
        val e = emulator(ChainConfig.instant)
        val a = pay(0, 2_000_000)
        assert(e.submitSync(a).isRight)
        e.setSlot(5)
        val point = ok(e.rollback(1))
        assert(point.slot == 5L)
        assert(e.tip.point == point)
        assert(e.tip.blockNo == 1L)
        assert(e.txStatus(a.id) == TxStatus.Confirmed(1, point))
        assert(e.mempoolTxs.isEmpty)
    }

    test("Instant, rollback(1) at an unchanged slot forges the same block again") {
        // spec [OR-5], [RB-7]: what the docs warn about; a tick first makes a new block
        val e = emulator(ChainConfig.instant)
        assert(e.submitSync(pay(0, 2_000_000)).isRight)
        val before = e.tip
        assert(ok(e.rollback(1)) == before.point)
        assert(e.tip == before)
        e.tick(1)
        assert(ok(e.rollback(1)).slot == before.slot + 1)
    }

    test("after clearAppliedTxs and a rollback, the blocks forged again join the log") {
        // spec [DE-4], [TS-5]: the log cursor moves down to the fork point
        val e = emulator(manual)
        e.produceBlock()
        e.produceBlock()
        clearAppliedTxs(e)
        val p1 = ok(e.rollback(1))
        val tx = pay(0, 2_000_000)
        ok(e.produceBlock(p1, Seq(tx)))
        assert(e.appliedTxLog.map(_.txHash) == Vector(tx.id))
    }

    // the deprecated clearAppliedTxs is the only way to move the log cursor
    @nowarn("msg=clearAppliedTxs")
    private def clearAppliedTxs(e: Emulator): Unit = e.clearAppliedTxs()

    test("a branch keeps its snapshots, and is removed when its fork point is deeper than k") {
        // spec [ST-3], [ST-6], [CS-7]
        val e = emulator(manual.withSecurityParam(2))
        val p1 = e.produceBlock()
        e.produceBlock()
        val b = ok(e.produceBlock(p1, Seq(pay(0, 2_000_000))))
        assert(stateOf(e).blocks.get(b.blockHash).exists(_.snapshot.isDefined))
        e.produceBlock()
        assert(!stateOf(e).blocks.contains(b.blockHash))
        assert(e.produceBlock(b, Seq.empty) == Left(ForgeError.UnknownPoint(b)))
    }

    test("a switch drops the snapshots deeper than k + 1 and prunes the old chain's branch") {
        // spec [ST-4], [ST-6], [CS-7]
        val e = emulator(manual.withSecurityParam(2))
        val (p1, p2, p3) = (e.produceBlock(), e.produceBlock(), e.produceBlock())
        assert(stateOf(e).blocks(p1.blockHash).snapshot.isDefined, "the anchor")
        val b3 = ok(e.produceBlock(p2, Seq(pay(0, 2_000_000))))
        val b4 = ok(e.produceBlock(b3, Seq.empty))
        assert(chainIds(e) == ids(p1, p2, b3, b4))
        val state = stateOf(e)
        assert(state.blocks(p1.blockHash).snapshot.isEmpty)
        assert(state.blocks(p2.blockHash).snapshot.isDefined)
        assert(!state.blocks.contains(p3.blockHash))
    }

    test("among several longer branches, selection takes the highest, then the lowest id") {
        // spec [CS-1]; ties between candidates: implementation status under [CS-2]
        val e = emulator(manual)
        val (p1, _, _) = (e.produceBlock(), e.produceBlock(), e.produceBlock())
        val x = ok(e.produceBlock(p1, Seq(pay(0, 2_000_000))))
        val y = ok(e.produceBlock(p1, Seq(pay(1, 2_000_000))))
        assert(e.rollback(2) == Right(Seq(x, y).minBy(_.blockHash.toHex)))
    }

    /** A donation of `lovelace` that states the treasury of the current slot, from `utxos`. */
    private def donation(utxos: Utxos, lovelace: Long, treasury: Coin): Transaction =
        TxBuilder(testEnv)
            .donateToTreasury(Coin(lovelace))
            .setCurrentTreasuryValue(treasury)
            .complete(utxos.filter(_._2.address == alice), alice)
            .sign(Alice.signer)
            .transaction

    test("a branch block before an epoch boundary sees the treasury of its own epoch") {
        // spec [FG-15], [NL-2]: a later slot change does not tick the stored snapshot of `on`
        val start = testEnv.slotConfig.firstSlotOfEpoch(500) + 10
        val context = rules.Context.testMainnet(start)
        val e = Emulator.withState(
          EmulatorInitialState(initialUtxos),
          context.copy(env = context.env.copy(treasury = Coin(1000))),
          manual
        )
        assert(e.submitSync(donation(e.utxos, 5, Coin(1000))).isRight)
        val p1 = e.produceBlock()
        e.setSlot(testEnv.slotConfig.firstSlotOfEpoch(501))
        assert(e.treasury == Coin(1005))
        val rival = donation(e.utxos, 7, Coin(1000))
        val b = ok(e.produceBlock(p1, Seq(rival)))
        assert(e.tip.point == b)
        assert(e.treasury == Coin(1012), "the tip view ticks the branch block across the boundary")
        assert(e.donation == Coin.zero)
    }

    test("Instant: a move back before an epoch boundary undoes the move of the donations") {
        // spec [NL-2]: the tip view is the tip block's ledger ticked to the current slot
        val start = testEnv.slotConfig.firstSlotOfEpoch(500) + 10
        val context = rules.Context.testMainnet(start)
        val e = Emulator(
          initialUtxos = initialUtxos,
          initialContext = context.copy(env = context.env.copy(treasury = Coin(1000)))
        )
        assert(e.submitSync(donation(e.utxos, 5, Coin(1000))).isRight)
        e.setSlot(testEnv.slotConfig.firstSlotOfEpoch(501))
        assert((e.treasury, e.donation) == (Coin(1005), Coin.zero))
        e.setSlot(start + 1)
        assert((e.treasury, e.donation) == (Coin(1000), Coin(5)))
    }

    test("a re-forge after a rollback across an epoch boundary sees the treasury of its epoch") {
        // spec [TE-3], [RB-6], [NL-2]: the rollback does not tick the stored snapshot of the tip
        val start = testEnv.slotConfig.firstSlotOfEpoch(500) + 10
        val context = rules.Context.testMainnet(start)
        val e = Emulator.withState(
          EmulatorInitialState(initialUtxos),
          context.copy(env = context.env.copy(treasury = Coin(1000))),
          manual.withOrphanTxs(OrphanTxPolicy.Drop)
        )
        assert(e.submitSync(donation(mempoolUtxos(e), 5, Coin(1000))).isRight)
        val p1 = e.produceBlock()
        val second = donation(e.utxos, 7, Coin(1000))
        assert(e.submitSync(second).isRight)
        val p2 = e.produceBlock()
        e.setSlot(testEnv.slotConfig.firstSlotOfEpoch(501))
        e.produceBlock()
        assert(e.rollback(2) == Right(p1))
        assert(e.produceBlock(p1, p2.slot, Seq(second)) == Right(p2))
        assert(e.treasury == Coin(1012))
    }

    private case class BlockPlan(lovelace: Seq[Long], donation: Option[Long], tick: Long)

    private val genPlan: Gen[BlockPlan] = for
        count <- Gen.choose(0, 2)
        lovelace <- Gen.listOfN(count, Gen.choose(1_000_000L, 20_000_000L))
        donation <- Gen.option(Gen.choose(1L, 5_000_000L))
        tick <- Gen.oneOf(Gen.choose(0L, 50L), Gen.const(testEnv.slotConfig.epochLength))
    yield BlockPlan(lovelace, donation, tick)

    test("rollback(n) and a re-forge of the same txs at the same slots give the same state") {
        // spec [TE-3], with donations and ticks across epoch boundaries, spec [NL-2]
        val gen = for
            plans <- Gen.choose(1, 5).flatMap(Gen.listOfN(_, genPlan))
            n <- Gen.choose(1, plans.size)
            policy <- Gen.oneOf(OrphanTxPolicy.values.toSeq)
        yield (plans, n, policy)
        forAll(gen, minSuccessful(20)) { (plans, n, policy) =>
            val e = emulator(manual.withOrphanTxs(policy))
            for plan <- plans do
                plan.lovelace.foreach(l =>
                    assert(e.submitSync(payment(mempoolUtxos(e), l)).isRight)
                )
                plan.donation.foreach(l =>
                    assert(e.submitSync(donation(mempoolUtxos(e), l, e.treasury)).isRight)
                )
                e.tick(plan.tick)
                e.produceBlock()
            val before = stateOf(e)
            val removed = before.selectedChain.takeRight(n)
            assert(e.rollback(n).isRight)
            for node <- removed do
                ok(e.produceBlock(e.tip.point, node.block.slot, node.txs.map(_.tx)))
            val after = stateOf(e)
            assert(after.chain == before.chain)
            assertSameSnapshot(after.tipSnapshot, before.tipSnapshot)
            assert(after.mempool.isEmpty)
            assert(after.appliedTxLog == before.appliedTxLog)
            for applied <- before.appliedTxLog do
                assert(after.txStatus(applied.txHash) == before.txStatus(applied.txHash))
        }
    }
}
