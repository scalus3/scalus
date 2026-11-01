package scalus.testing

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.PlutusScriptsTransactionMutator
import scalus.cardano.node.*
import scalus.cardano.node.emulator.{ChainConfig, DropReason, ForgeError, RollbackError, TxStatus}
import scalus.cardano.node.stream.ChainPoint
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob}

import java.time.Instant

/** `ImmutableEmulator` on the shared `EmulatorState`, spec section 13.3. */
class ImmutableEmulatorChainTest extends AnyFunSuite {

    private given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val alice = Alice.address(Network.Mainnet)
    private val bob = Bob.address(Network.Mainnet)

    private def immutable(config: ChainConfig): ImmutableEmulator =
        ImmutableEmulator.fromEmulator(Emulator.withAddresses(Seq(alice), Value.ada(1000), config))

    private def payment(emu: ImmutableEmulator, lovelace: Long): Transaction =
        TxBuilder(testEnv)
            .payTo(bob, Value.lovelace(lovelace))
            .complete(emu.utxos, alice)
            .sign(Alice.signer)
            .transaction

    private def toBob(emu: ImmutableEmulator): Utxos = emu.utxos.filter(_._2.address == bob)

    private def submitted(emu: ImmutableEmulator, tx: Transaction): ImmutableEmulator =
        emu.submit(tx) match
            case Right((_, next)) => next
            case Left(error)      => fail(s"submit failed: $error")

    test("Manual: a submitted tx is pending until produceBlock forges it") {
        // spec [SF-11], [SF-14], [TE-5]
        val start = immutable(ChainConfig.manual)
        val tx = payment(start, 2_000_000)
        val pending = submitted(start, tx)
        assert(toBob(pending).isEmpty, "the tip does not show a pending tx")
        assert(pending.txStatus(tx.id) == TxStatus.Pending)

        val (point, forged) = pending.produceBlock()
        assert(toBob(forged).keySet == Set(Input(tx.id, 0)))
        assert(forged.txStatus(tx.id) == TxStatus.Confirmed(1, point))
        assert(toBob(pending).isEmpty, "produceBlock is pure: the receiver is unchanged")
    }

    test("withAddresses with a ChainConfig starts in that preset") {
        // spec [CF-5], [SF-11]
        val start = ImmutableEmulator.withAddresses(Seq(alice), Value.ada(1000), ChainConfig.manual)
        val tx = payment(start, 2_000_000)
        assert(submitted(start, tx).txStatus(tx.id) == TxStatus.Pending)
    }

    test("rollback is pure and returns the tx to the mempool") {
        // spec [SF-14], [RB-7], [OR-1]
        val start = immutable(ChainConfig.manual)
        val tx = payment(start, 2_000_000)
        val (_, forged) = submitted(start, tx).produceBlock()

        forged.rollback(1) match
            case Right((tip, rolled)) =>
                assert(tip == ChainPoint.origin)
                assert(toBob(rolled).isEmpty)
                assert(rolled.txStatus(tx.id) == TxStatus.Pending)
            case Left(error) => fail(s"rollback failed: $error")
        assert(toBob(forged).nonEmpty, "the receiver is unchanged")
        assert(forged.rollback(2) == Left(RollbackError.PastGenesis(2, 1)))
    }

    test("produceBlock on a branch forges the given txs, or reports why not") {
        // spec [SF-14], [FG-11], [FG-12]
        val start = immutable(ChainConfig.manual)
        val tx = payment(start, 2_000_000)
        start.produceBlock(ChainPoint.origin, Seq(tx)) match
            case Right((point, next)) =>
                assert(point.slot == 1L)
                assert(next.txStatus(tx.id) == TxStatus.Confirmed(1, point))
            case Left(error) => fail(s"forge failed: $error")
        assert(
          start.produceBlock(ChainPoint.origin, 0L, Seq(tx)) ==
              Left(ForgeError.SlotNotAfterParent(0L, 0L))
        )
    }

    test("setSlot revalidates the mempool") {
        // spec [DE-3], [RV-4], M2 review R5
        val start = immutable(ChainConfig.manual)
        val tx = TxBuilder(testEnv)
            .payTo(bob, Value.lovelace(2_000_000))
            .validTo(Instant.ofEpochMilli(testEnv.slotConfig.slotToTime(100)))
            .complete(start.utxos, alice)
            .sign(Alice.signer)
            .transaction
        val later = submitted(start, tx).setSlot(200)
        assert(later.txStatus(tx.id) == TxStatus.Dropped(DropReason.Expired))
    }

    test("fromEmulator and toEmulator keep the preset, the chain and the mempool") {
        // spec [SF-11]
        val start = immutable(ChainConfig.manual)
        val first = payment(start, 2_000_000)
        val (point, forged) = submitted(start, first).produceBlock()
        val second = payment(forged, 3_000_000)
        val pending = submitted(forged, second)

        val emulator = pending.toEmulator
        assert(emulator.tip.point == point)
        assert(emulator.mempoolTxs == Seq(second))
        assert(ImmutableEmulator.fromEmulator(emulator) == pending)
    }

    test("Instant: a submit is visible at once, in a block") {
        // spec [CF-4], [MP-8]
        val start = immutable(ChainConfig.instant)
        val tx = payment(start, 2_000_000)
        val next = submitted(start, tx)
        assert(toBob(next).keySet == Set(Input(tx.id, 0)))
        assert(next.txStatus(tx.id).isInstanceOf[TxStatus.Confirmed])
    }

    test("the factories keep running only PlutusScriptsTransactionMutator") {
        // spec [SF-13]
        for emu <- Seq(
              ImmutableEmulator.withAddresses(Seq(alice)),
              ImmutableEmulator.fromJson("""{ "utxo": {} }""")
            )
        do assert(emu.mutators == Set(PlutusScriptsTransactionMutator))
    }
}
