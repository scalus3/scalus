package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.node.emulator.ChainConfig
import scalus.cardano.node.stream.internal.ChainEvent
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.uplc.builtin.ByteString

/** ChainSync, milestone M4 of the spec: `diff` between two tips of the block tree, section 12.1. */
class EmulatorDiffTest extends AnyFunSuite {

    private given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private val alice = Alice.address(Network.Mainnet)
    private val bob = Bob.address(Network.Mainnet)

    private val initialUtxos: Utxos = (0 until 4).map { i =>
        Input(genesisHash, i) -> Output(alice, Value.ada(1000))
    }.toMap

    private def manual(): Emulator =
        Emulator.withState(
          EmulatorInitialState.empty.withUtxos(initialUtxos),
          rules.Context.testMainnet(),
          ChainConfig.manual
        )

    /** A payment that spends exactly the genesis UTxO `i`. */
    private def pay(i: Int, lovelace: Long): Transaction =
        TxBuilder(testEnv)
            .payTo(bob, Value.lovelace(lovelace))
            .complete(initialUtxos.filter(_._1.index == i), alice)
            .sign(Alice.signer)
            .transaction

    private def ok[A](result: Either[?, A]): A = result.fold(e => fail(s"unexpected $e"), identity)

    /** A forge on `on` that returns the new tip, which must be the forged block. */
    private def forge(e: Emulator, on: ChainPoint, txs: Transaction*): ChainPoint =
        ok(e.produceBlock(on, txs))

    /** An event without the block body: `B` for a rollback target, `F` for a block forward. */
    private def shape(events: Seq[ChainEvent]): Seq[(String, ChainTip)] = events.map {
        case ChainEvent.RollBackward(to) => "B" -> to
        case ChainEvent.RollForward(b)   => "F" -> ChainTip(b.point, b.blockNo)
    }

    test("diff of a tip with itself is empty; from genesis it rolls forward every block") {
        // spec [SY-1], [SY-3]
        val e = manual()
        val tips = (1 to 3).map { _ =>
            e.produceBlock(); e.tip
        }
        assert(e.diff(e.tip, e.tip).isEmpty)
        assert(e.diff(ChainTip.origin, ChainTip.origin).isEmpty)
        assert(shape(e.diff(ChainTip.origin, e.tip)) == tips.map("F" -> _))
        assert(shape(e.diff(tips(0), tips(2))) == tips.drop(1).map("F" -> _))
    }

    test("a RollForward carries the real block and the txs with their UTxO effects") {
        // spec [SY-4]
        val e = manual()
        val tx = pay(0, 2_000_000)
        val before = e.tip
        val point = forge(e, ChainPoint.origin, tx)
        e.diff(before, e.tip) match
            case Seq(ChainEvent.RollForward(applied)) =>
                assert(applied.point == point)
                assert(applied.blockNo == 1L)
                assert(applied.block.map(_.hash).contains(point.blockHash))
                assert(applied.block.map(_.txCount).contains(1))
                val Seq(appliedTx) = applied.txs
                assert(appliedTx.tx == tx)
                assert(appliedTx.spent == initialUtxos.filter(_._1.index == 0))
                assert(appliedTx.created.keySet.map(_.transactionId) == Set(tx.id))
                assert(appliedTx.created.size == tx.body.value.outputs.size)
            case other => fail(s"expected one RollForward, got $other")
    }

    test("a branch switch rolls back to the common ancestor, then forward in height order") {
        // spec [SY-2], [SY-3]; the selected chain G-1-2-3 switches to G-1-2'-3'-4'
        val e = manual()
        val p1 = e.produceBlock()
        val t1 = e.tip
        e.produceBlock()
        e.produceBlock()
        val t3 = e.tip
        val b2 = forge(e, p1, pay(0, 2_000_000))
        val b3 = forge(e, b2, pay(1, 2_000_000))
        assert(e.tip == t3, "a branch of the same height does not switch, spec [CS-2]")
        val b4 = forge(e, b3)
        assert(e.tip.point == b4)
        val forward = Seq(b2 -> 2L, b3 -> 3L, b4 -> 4L).map((p, h) => "F" -> ChainTip(p, h))
        assert(shape(e.diff(t3, e.tip)) == ("B" -> t1) +: forward)
        assert(p1 == t1.point)
    }

    test("when the common ancestor is genesis, the rollback target is ChainTip.origin") {
        // spec [SY-2]
        val e = manual()
        e.produceBlock()
        val t1 = e.tip
        val b1 = forge(e, ChainPoint.origin, pay(0, 2_000_000))
        val b2 = forge(e, b1)
        assert(e.tip.point == b2)
        assert(
          shape(e.diff(t1, e.tip)) ==
              Seq("B" -> ChainTip.origin, "F" -> ChainTip(b1, 1L), "F" -> ChainTip(b2, 2L))
        )
    }

    test("diff rejects a tip that is not in the block tree") {
        // spec [SY-1]: the tips must name blocks of the tree; a known hash with a wrong slot or
        // height is not that block
        val e = manual()
        e.produceBlock()
        val tip = e.tip
        val unknown = ChainTip(
          ChainPoint(tip.slot, BlockHash.fromByteString(ByteString.fromHex("ff" * 32))),
          tip.blockNo
        )
        val wrongSlot = ChainTip(ChainPoint(tip.slot + 1, tip.blockHash), tip.blockNo)
        val wrongHeight = ChainTip(tip.point, tip.blockNo + 1)
        for bad <- Seq(unknown, wrongSlot, wrongHeight) do
            assertThrows[IllegalArgumentException](e.diff(bad, tip))
            assertThrows[IllegalArgumentException](e.diff(tip, bad))
    }
}
