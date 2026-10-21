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
import scala.annotation.nowarn

/** The Emulator as a chain of real blocks with a mempool, milestone M1 of the spec: the `Instant`
  * preset forges one block per submit, and `Manual` keeps a tx in the mempool.
  */
class EmulatorChainTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    private given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private val alice = Alice.address(Network.Mainnet)
    private val bob = Bob.address(Network.Mainnet)

    private val initialUtxos: Utxos = (0 until 3).map { i =>
        Input(genesisHash, i) -> Output(alice, Value.ada(1000))
    }.toMap

    private def emulator(config: ChainConfig): Emulator =
        Emulator(initialUtxos = initialUtxos, chainConfig = config)

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

    /** The UTxO set after the mempool txs, which is what a chained tx spends from. */
    private def mempoolUtxos(emulator: Emulator): Utxos =
        stateOf(emulator).mempool.lastOption.fold(emulator.utxos)(_.snapshot.ledger.utxos)

    test("every existing factory starts with the Instant preset") {
        // spec [CF-4]
        assert(stateOf(Emulator(initialUtxos = initialUtxos)).config == ChainConfig.instant)
        assert(stateOf(Emulator.withAddresses(Seq(alice))).config == ChainConfig.instant)
        assert(stateOf(Emulator.withState(EmulatorInitialState())).config == ChainConfig.instant)
    }

    test("every factory has an overload that takes a ChainConfig") {
        // spec [CF-5]
        val manual = ChainConfig.manual
        val context = rules.Context.testMainnet()
        val built = Seq(
          Emulator.withAddresses(Seq(alice), Value.ada(10), manual),
          Emulator.withState(EmulatorInitialState(), context, manual),
          Emulator.withRegisteredStakeCredentials(initialUtxos, Map.empty, context, manual)
        )
        for e <- built do assert(stateOf(e).config == manual)
    }

    test("Instant: a submit forges one block with the tx, and the mempool is empty after it") {
        // spec [MP-6], [MP-8], [FG-6], [FG-7], [FG-9]
        val e = emulator(ChainConfig.instant)
        val tx = payment(e.utxos, 2_000_000)
        assert(e.submitSync(tx) == Right(tx.id))
        val state = stateOf(e)
        assert(state.mempool.isEmpty)
        assert(state.selectedChain.map(_.txs.map(_.tx.id)) == Vector(Vector(tx.id)))
        assert(state.selectedChain.head.snapshot.map(_.ledger.utxos).contains(e.utxos))
        assert(e.utxos.keySet.exists(_.transactionId == tx.id))
    }

    test("Instant: each submit forges the next block on the previous one") {
        // spec [BH-5], [BH-6], [ST-1]
        val e = emulator(ChainConfig.instant)
        for amount <- Seq(2_000_000L, 3_000_000L, 4_000_000L) do
            val tx = payment(e.utxos, amount)
            assert(e.submitSync(tx) == Right(tx.id))
        val chain = stateOf(e).selectedChain
        assert(chain.map(_.height) == Vector(1L, 2L, 3L))
        assert(chain.map(_.block.blockNumber) == Vector(1L, 2L, 3L))
        assert(chain.map(_.parent) == Vector(None, Some(chain(0).id), Some(chain(1).id)))
        assert(chain.forall(n => n.id == n.block.hash))
    }

    test("Instant: a rejected submit, a slot change and a direct edit forge no block") {
        // spec [FG-10], [DE-2]
        val e = emulator(ChainConfig.instant)
        val tx = payment(e.utxos, 2_000_000)
        assert(e.submitSync(tx).isRight)
        assert(e.submitSync(tx).isLeft, "a duplicate")
        e.setSlot(10)
        e.tick(5)
        e.addUtxo(Input(genesisHash, 9), Output(alice, Value.ada(5)))
        e.removeUtxo(Input(genesisHash, 9))
        e.addDatum(Data.I(1))
        assert(stateOf(e).selectedChain.size == 1)
    }

    test("Instant: the forge uses max(current slot, tip slot) and keeps the current slot") {
        // spec [FG-2], [FG-3a], [TS-6], section 8.1 example
        val e = emulator(ChainConfig.instant)
        e.setSlot(50)
        val first = payment(e.utxos, 2_000_000)
        assert(e.submitSync(first).isRight)
        e.setSlot(40)
        val second = payment(e.utxos, 3_000_000)
        assert(e.submitSync(second).isRight)
        assert(stateOf(e).selectedChain.map(_.block.slot) == Vector(50L, 50L))
        assert(e.currentSlotSync == 40L)
        assert(e.getAppliedTx(second.id).map(_.slot).contains(40L))
        e.setSlot(70)
        val third = payment(e.utxos, 4_000_000)
        assert(e.submitSync(third).isRight)
        assert(stateOf(e).selectedChain.last.block.slot == 70L)
    }

    test("the block carries the protocol version of the current protocol parameters") {
        // spec [BH-7]
        val context = rules.Context.testMainnet()
        val version = ProtocolVersion(context.env.params.protocolVersion.major, 7)
        val params = context.env.params.copy(protocolVersion = version)
        val e = Emulator(
          initialUtxos = initialUtxos,
          initialContext = context.copy(env = context.env.copy(params = params))
        )
        assert(e.submitSync(payment(e.utxos, 2_000_000)).isRight)
        assert(stateOf(e).selectedChain.head.block.header.headerBody.protocolVersion == version)
    }

    test("Manual: a submit leaves the tx in the mempool, and the tip does not move") {
        // spec [MP-6], [VW-1], [VW-2]; M1 Manual forges nothing on its own
        val e = emulator(ChainConfig.manual)
        val tx = payment(e.utxos, 2_000_000)
        assert(e.submitSync(tx) == Right(tx.id))
        val state = stateOf(e)
        assert(state.selectedChain.isEmpty)
        assert(state.mempool.map(_.applied.tx.id) == Vector(tx.id))
        assert(e.utxos == initialUtxos, "findUtxos and utxos read the tip")
        assert(e.findUtxosSync(UtxoQuery(UtxoSource.FromAddress(bob))).isEmpty)
        assert(e.fees == Coin.zero)
        assert(!e.hasTx(tx.id))
    }

    test("Manual: a tx can spend the output of a pending tx") {
        // spec [MP-1], [ST-7]
        val e = emulator(ChainConfig.manual)
        val first = payment(e.utxos, 2_000_000)
        assert(e.submitSync(first).isRight)
        val second = payment(mempoolUtxos(e), 2_500_000_000L)
        assert(second.body.value.inputs.toSeq.exists(_.transactionId == first.id))
        assert(e.submitSync(second) == Right(second.id))
        val entries = stateOf(e).mempool
        assert(entries.map(_.applied.tx.id) == Vector(first.id, second.id))
        assert(entries(1).snapshot.ledger.utxos.keySet.exists(_.transactionId == second.id))
    }

    test("Manual: submit validates at the current slot, not the slot of the mempool tail") {
        // spec [MP-2]
        val e = emulator(ChainConfig.manual)
        assert(e.submitSync(payment(e.utxos, 2_000_000)).isRight)
        e.setSlot(200)
        val late = paymentValidTo(mempoolUtxos(e), 3_000_000, 100)
        assert(e.submitSync(late).left.map(_.rule) == Left("TransactionExpired"))
        assert(e.getAppliedTx(late.id).isEmpty)
    }

    test("a duplicate tx is rejected by the ledger rules, in the mempool and on the chain") {
        // spec [MP-5]: its inputs are spent
        val manual = emulator(ChainConfig.manual)
        val pending = payment(manual.utxos, 2_000_000)
        assert(manual.submitSync(pending).isRight)
        assert(manual.submitSync(pending).left.map(_.rule) == Left("UtxoNotAvailable"))
        assert(stateOf(manual).mempool.size == 1)

        val instant = emulator(ChainConfig.instant)
        val confirmed = payment(instant.utxos, 2_000_000)
        assert(instant.submitSync(confirmed).isRight)
        assert(instant.submitSync(confirmed).left.map(_.rule) == Left("UtxoNotAvailable"))
        assert(stateOf(instant).selectedChain.size == 1)
    }

    test("withApplied forges in Instant and leaves the tx in the mempool in Manual") {
        // spec [MP-6], [MP-8], section 5: Manual forges only on an explicit call
        def applied(config: ChainConfig): EmulatorState = {
            val state = stateOf(emulator(config))
            val tx = payment(initialUtxos, 2_000_000)
            state.withApplied(state.ledger, AppliedTx(tx, state.currentSlot, Map.empty))
        }
        val instant = applied(ChainConfig.instant)
        assert(instant.selectedChain.size == 1)
        assert(instant.mempool.isEmpty)
        val manual = applied(ChainConfig.manual)
        assert(manual.selectedChain.isEmpty)
        assert(manual.mempool.size == 1)
    }

    test("a direct edit changes the tip snapshot in place") {
        // spec [DE-1], [ST-8]
        val e = emulator(ChainConfig.instant)
        assert(e.submitSync(payment(e.utxos, 2_000_000)).isRight)
        val extra = Input(genesisHash, 9) -> Output(alice, Value.ada(5))
        e.addUtxo(extra._1, extra._2)
        val tip = stateOf(e).selectedChain.last
        assert(tip.snapshot.exists(_.ledger.utxos.get(extra._1).contains(extra._2)))
        assert(stateOf(e).ledger == tip.snapshot.get.ledger)
    }

    test("snapshots older than k are dropped, the blocks are kept") {
        // spec [ST-2], [ST-4], [ST-5]
        val e = emulator(ChainConfig.instant.withSecurityParam(2))
        val txs = (1 to 4).map { i =>
            val tx = payment(e.utxos, 1_000_000L * (i + 1))
            assert(e.submitSync(tx).isRight)
            tx
        }
        val state = stateOf(e)
        assert(state.selectedChain.map(_.snapshot.isDefined) == Vector(false, false, true, true))
        assert(state.genesisSnapshot.isEmpty)
        assert(state.selectedChain.size == 4)
        assert(e.getTransaction(txs.head.id).contains(txs.head))
    }

    test("with k above the height, the genesis snapshot is kept") {
        // spec [ST-2]
        val e = emulator(ChainConfig.instant.withSecurityParam(2))
        assert(e.submitSync(payment(e.utxos, 2_000_000)).isRight)
        assert(stateOf(e).genesisSnapshot.map(_.ledger.utxos).contains(initialUtxos))
    }

    test("the applied-tx log is the imported log, then the txs of the selected chain") {
        // spec [TS-5]
        val source = emulator(ChainConfig.instant)
        val imported = payment(source.utxos, 2_000_000)
        assert(source.submitSync(imported).isRight)
        val restored = Emulator(
          initialUtxos = source.utxos,
          initialAppliedTxLog = source.appliedTxLog,
          chainConfig = ChainConfig.instant
        )
        val next = payment(restored.utxos, 3_000_000)
        assert(restored.submitSync(next).isRight)
        assert(restored.appliedTxLog.map(_.txHash) == Vector(imported.id, next.id))
        assert(stateOf(restored).selectedChain.flatMap(_.txs.map(_.txHash)) == Vector(next.id))
    }

    test("clearAppliedTxs empties the log up to the tip and keeps the blocks") {
        // spec [DE-4]: the log-start cursor moves to the tip
        val e = emulator(ChainConfig.instant)
        val before = payment(e.utxos, 2_000_000)
        assert(e.submitSync(before).isRight)
        clear(e)
        val after = payment(e.utxos, 3_000_000)
        assert(e.submitSync(after).isRight)
        assert(e.appliedTxLog.map(_.txHash) == Vector(after.id))
        assert(e.appliedTxs == Set(after.id))
        assert(!e.hasTx(before.id))
        assert(stateOf(e).selectedChain.size == 2)
    }

    @nowarn("msg=clearAppliedTxs") // deprecated, spec [DE-5]; this test pins what it still does
    private def clear(e: Emulator): Unit = e.clearAppliedTxs()

    private enum Op {
        case Submit(lovelace: Long)
        case Resubmit
        case Tick(slots: Long)
    }

    private val genOp: Gen[Op] = Gen.frequency(
      4 -> Gen.choose(1_000_000L, 50_000_000L).map(Op.Submit(_)),
      1 -> Gen.const(Op.Resubmit),
      2 -> Gen
          .oneOf(Gen.choose(0L, 100L), Gen.const(testEnv.slotConfig.epochLength))
          .map(Op.Tick(_))
    )

    private def run(ops: Seq[Op]): Emulator = {
        val e = emulator(ChainConfig.instant)
        var last: Option[Transaction] = None
        ops.foreach {
            case Op.Submit(lovelace) =>
                val tx = payment(e.utxos, lovelace)
                last = Some(tx)
                e.submitSync(tx)
            case Op.Resubmit => last.foreach(e.submitSync)
            case Op.Tick(n)  => e.tick(n)
        }
        e
    }

    /** `==` on two snapshots, except the gov state: an `Array`, so `==` compares its identity. */
    private def assertSameSnapshot(a: LedgerSnapshot, b: LedgerSnapshot): Unit = {
        assert(a.ledger.govState.sameElements(b.ledger.govState))
        val gov = GovState.empty
        def normal(s: LedgerSnapshot) = s.copy(ledger = s.ledger.copy(govState = gov))
        assert(normal(a) == normal(b))
    }

    test("the tip snapshot is the fold of the selected chain's blocks from genesis") {
        // spec [TE-1], [NL-1], [NL-3]: the fold ticks to the slot each tx was validated at
        val genesis = stateOf(emulator(ChainConfig.instant)).tipSnapshot
        forAll(Gen.listOfN(6, genOp), minSuccessful(10)) { ops =>
            val e = run(ops)
            val state = stateOf(e)
            val folded =
                state.selectedChain.flatMap(_.txs).foldLeft(genesis) { (snapshot, applied) =>
                    val ticked = snapshot.atSlot(applied.slot)
                    val ledger = STS.Mutator
                        .transit(
                          e.validators,
                          e.mutators,
                          ticked.context,
                          ticked.ledger,
                          applied.tx
                        )
                        .fold(err => fail(s"a chain tx fails the fold: $err"), identity)
                    ticked.copy(
                      ledger = ledger,
                      binaryDatums = ticked.binaryDatums ++ EmulatorBase.binaryDatumsOf(applied.tx)
                    )
                }
            assertSameSnapshot(folded.atSlot(e.currentSlotSync), state.tipSnapshot)
            assert(state.context.fee == Coin.zero, "no transition writes Context.fee")
        }
    }

    test("two runs of the same operations forge the same block ids") {
        // spec [TE-4]
        forAll(Gen.listOfN(6, genOp), minSuccessful(10)) { ops =>
            val ids = stateOf(run(ops)).selectedChain.map(_.id)
            assert(stateOf(run(ops)).selectedChain.map(_.id) == ids)
        }
    }
}
