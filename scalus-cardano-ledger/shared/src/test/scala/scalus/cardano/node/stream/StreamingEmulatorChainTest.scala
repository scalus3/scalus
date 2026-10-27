package scalus.cardano.node.stream

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.node.*
import scalus.cardano.node.emulator.ChainConfig
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.uplc.builtin.ByteString

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

/** ChainSync, milestone M4 of the spec: `StreamingEmulator` on the real block tree, section 12.1.
  */
class StreamingEmulatorChainTest extends AnyFunSuite {

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

    /** A payment to Bob that spends exactly the genesis UTxO `i`. */
    private def pay(i: Int, lovelace: Long): Transaction =
        TxBuilder(testEnv)
            .payTo(bob, Value.lovelace(lovelace))
            .complete(initialUtxos.filter(_._1.index == i), alice)
            .sign(Alice.signer)
            .transaction

    private def ok[A](result: Either[?, A]): A = result.fold(e => fail(s"unexpected $e"), identity)

    private def submitted(stream: StreamingEmulator, tx: Transaction): TransactionHash =
        ok(stream.submit(tx).value.get.get)

    private def statusOf(stream: StreamingEmulator, hash: TransactionHash): TransactionStatus =
        stream.checkTransaction(hash).value.get.get

    /** Reads what a source has already delivered; delivery is synchronous, so this runs on JS too.
      * It keeps an unanswered pull for the next call: a pull is a commitment, and a dropped one
      * would take the next event with it.
      */
    private final class Reader[A](source: ScalusAsyncSource[A]) {
        private var outstanding: Option[Future[Option[A]]] = None

        def drain(): List[A] = {
            val buf = List.newBuilder[A]
            var more = true
            while more do
                val next = outstanding.getOrElse(source.pull())
                next.value match
                    case Some(Success(Some(a))) =>
                        outstanding = None
                        buf += a
                    case _ =>
                        outstanding = Some(next)
                        more = false
            buf.result()
        }
    }

    private def bobEvents(stream: StreamingEmulator): Reader[UtxoEvent] =
        Reader(
          stream.subscribeUtxoQuery[ScalusAsyncSource](
            UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(bob))),
            SubscriptionOptions(includeExistingUtxos = false)
          )
        )

    /** The Created and RolledBack events, as (kind, tx or point). */
    private def shape(events: List[UtxoEvent]): List[(String, Any)] = events.collect {
        case UtxoEvent.Created(_, by, _) => "created" -> by
        case UtxoEvent.RolledBack(to)    => "rolledBack" -> to
    }

    test("a branch switch delivers RollBackward to a subscriber, then the new chain") {
        // spec [TE-9], [SY-5], [SY-2]
        val e = emulator(ChainConfig.manual)
        val stream = new StreamingEmulator(e)
        val events = bobEvents(stream)
        val p1 = stream.produceBlock()
        val a = submitted(stream, pay(0, 2_000_000))
        stream.produceBlock()
        assert(shape(events.drain()) == List("created" -> a))
        val rival = pay(1, 3_000_000)
        val b2 = ok(stream.produceBlock(p1, Seq(rival)))
        assert(events.drain().isEmpty, "a branch of the same height does not switch")
        val b3 = ok(stream.produceBlock(b2, Seq.empty))
        assert(e.tip.point == b3)
        assert(shape(events.drain()) == List("rolledBack" -> p1, "created" -> rival.id))
        assert(stream.currentTip == e.tip)
    }

    test("a mempool tx reads Pending, also when a rollback returns it to the mempool") {
        // spec [SY-8], [OR-3]
        val e = emulator(ChainConfig.manual)
        val stream = new StreamingEmulator(e)
        val hash = submitted(stream, pay(0, 2_000_000))
        assert(statusOf(stream, hash) == TransactionStatus.Pending)
        stream.produceBlock()
        assert(statusOf(stream, hash) == TransactionStatus.Confirmed)
        assert(ok(stream.rollback(1)) == ChainPoint.origin)
        assert(e.mempoolTxs.map(_.id) == Seq(hash))
        assert(statusOf(stream, hash) == TransactionStatus.Pending)
    }

    test("a tx that leaves the mempool without a block reads NotFound, as in the emulator") {
        // spec [SY-8]: only a mempool tx is Pending; a rival block on genesis spends the input of
        // the pending tx, so the switch revalidates it out of the mempool
        val e = emulator(ChainConfig.manual)
        val stream = new StreamingEmulator(e)
        val hash = submitted(stream, pay(0, 2_000_000))
        val b1 = ok(stream.produceBlock(ChainPoint.origin, Seq(pay(0, 3_000_000))))
        assert(e.tip.point == b1)
        assert(e.mempoolTxs.isEmpty)
        assert(statusOf(stream, hash) == TransactionStatus.NotFound)
        assert(statusOf(stream, hash) == e.checkTransaction(hash).value.get.get)
    }

    test("in Instant, a status subscriber sees a submitted tx Pending, then Confirmed") {
        // spec [SY-8], [CF-4]: the sequence StreamingEmulator gave before the real chain
        val e = emulator(ChainConfig.instant)
        val stream = new StreamingEmulator(e)
        val tx = pay(0, 2_000_000)
        val statuses = Reader(stream.subscribeTransactionStatus[ScalusAsyncSource](tx.id))
        assert(statuses.drain() == List(TransactionStatus.NotFound))
        submitted(stream, tx)
        assert(statuses.drain() == List(TransactionStatus.Pending, TransactionStatus.Confirmed))
    }

    test("newEmptyBlock advances the slot by one and adds one block") {
        // spec [FG-8], [CF-4]: as before the real chain, the empty block is one slot later
        val e = emulator(ChainConfig.instant)
        val stream = new StreamingEmulator(e)
        submitted(stream, pay(0, 2_000_000))
        val before = e.tip
        stream.newEmptyBlock()
        assert(e.currentSlotSync == before.slot + 1)
        assert(e.tip.slot == before.slot + 1)
        assert(e.tip.blockNo == before.blockNo + 1)
        assert(stream.currentTip == e.tip)
    }

    test("the advertised rollback horizon is the securityParam argument, Some(0) by default") {
        // spec [SY-6]: split horizon, the emulator's k does not reach the capabilities
        val config = ChainConfig.manual.withSecurityParam(5)
        def horizon(stream: StreamingEmulator) = stream.streamCapabilities.rollbackHorizon
        assert(horizon(new StreamingEmulator(emulator(config))) == Some(0))
        assert(horizon(new StreamingEmulator(emulator(config), 9)) == Some(9))
        assert(horizon(new StreamingEmulator(emulator(config), 1)) == Some(1))
    }

    test("a rollback deeper than the securityParam argument, within k, reaches subscribers") {
        // spec [SY-6]: the hub keeps max(securityParam, k) blocks
        val e = emulator(ChainConfig.manual.withSecurityParam(5))
        val stream = new StreamingEmulator(e, 1)
        val events = bobEvents(stream)
        val p1 = stream.produceBlock()
        val a = submitted(stream, pay(0, 2_000_000))
        stream.produceBlock()
        stream.produceBlock()
        assert(ok(stream.rollback(2)) == p1)
        assert(shape(events.drain()) == List("created" -> a, "rolledBack" -> p1))
    }

    test("by default a noRollback subscriber gets an event at once, and a rollback still arrives") {
        // spec [SY-6]: the default horizon is Some(0): noRollback waits for nothing, and the
        // capability does not promise that RolledBack never arrives, since it does
        val e = emulator(ChainConfig.manual)
        val stream = new StreamingEmulator(e)
        assert(stream.streamCapabilities.rollbackHorizon == Some(0))
        val events = Reader(
          stream.subscribeUtxoQuery[ScalusAsyncSource](
            UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(bob))),
            SubscriptionOptions(includeExistingUtxos = false, noRollback = true)
          )
        )
        val p1 = stream.produceBlock()
        val a = submitted(stream, pay(0, 2_000_000))
        stream.produceBlock()
        assert(shape(events.drain()) == List("created" -> a))
        assert(ok(stream.rollback(1)) == p1)
        assert(shape(events.drain()) == List("rolledBack" -> p1))
    }

    test("the stream delivers the emulator's real blocks") {
        // spec [SY-4], [SY-7]
        val e = emulator(ChainConfig.instant)
        val stream = new StreamingEmulator(e)
        val blocks = Reader(
          stream.subscribeBlockQuery[ScalusAsyncSource](BlockQuery.All, SubscriptionOptions())
        )
        val hash = submitted(stream, pay(0, 2_000_000))
        val t1 = e.tip
        stream.newEmptyBlock()
        val t2 = e.tip
        val applied = blocks.drain().collect { case BlockEvent.Applied(block, at) => block -> at }
        assert(applied.map(_._2) == List(t1.point, t2.point))
        assert(t2.blockNo == 2L)
        assert(applied.forall((block, at) => block.hash == at.blockHash))
        assert(applied.map(_._1.txCount) == List(1, 0))
        assert(stream.currentTip == e.tip)
        assert(statusOf(stream, hash) == TransactionStatus.Confirmed)
    }

    test("in Manual, submitAndPoll forges through the stream, and subscribers get the block") {
        // spec [SF-3], [SY-5]
        val e = emulator(ChainConfig.manual)
        val stream = new StreamingEmulator(e)
        val blocks = Reader(
          stream.subscribeBlockQuery[ScalusAsyncSource](BlockQuery.All, SubscriptionOptions())
        )
        val tx = pay(0, 2_000_000)
        assert(stream.submitAndPoll(tx, 5, 0L).value.get.get == Right(tx.id))
        val applied = blocks.drain().collect { case BlockEvent.Applied(_, at) => at }
        assert(applied == List(e.tip.point))
        assert(stream.currentTip == e.tip)
    }

    test("a transition made on the wrapped emulator reaches subscribers at the next operation") {
        // spec [SY-5]
        val e = emulator(ChainConfig.manual)
        val stream = new StreamingEmulator(e)
        val events = bobEvents(stream)
        val p1 = stream.produceBlock()
        val a = submitted(stream, pay(0, 2_000_000))
        stream.produceBlock()
        assert(ok(e.rollback(1)) == p1)
        assert(shape(events.drain()) == List("created" -> a))
        // a later slot, else the forge makes the removed block again, and the tips are equal
        e.tick(1)
        stream.produceBlock()
        assert(shape(events.drain()) == List("rolledBack" -> p1, "created" -> a))
        assert(stream.currentTip == e.tip)
    }

    test("a subscriber that forges while a switch is delivered sees the blocks in order") {
        // spec [SY-5]: a delivery continuation may run inline and call the stream again
        val e = emulator(ChainConfig.manual)
        val stream = new StreamingEmulator(e)
        val p1 = stream.produceBlock()
        stream.produceBlock()
        val tips = stream.subscribeTip[ScalusAsyncSource]()
        val seen = List.newBuilder[ChainTip]
        def follow(): Unit =
            tips.pull()
                .onComplete { result =>
                    result.toOption.flatten.foreach { tip =>
                        seen += tip
                        if tip.point == p1 then stream.newEmptyBlock()
                        follow()
                    }
                }(using ExecutionContext.parasitic)
        follow()
        val b2 = ok(stream.produceBlock(p1, Seq(pay(0, 2_000_000))))
        ok(stream.produceBlock(b2, Seq.empty))
        assert(e.tip.blockNo == 4L)
        assert(stream.currentTip == e.tip)
        assert(seen.result().map(_.blockNo) == List(2L, 1L, 2L, 3L, 4L))
    }
}
