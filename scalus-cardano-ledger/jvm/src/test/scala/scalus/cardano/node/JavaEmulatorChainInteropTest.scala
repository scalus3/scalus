package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.PoolPerformance
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.emulator.{BlockIssuer, ChainConfig, ChainViewError, ForgeError, OrphanTxPolicy, RollbackError}
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.cardano.node.stream.internal.ChainEvent
import scalus.cardano.txbuilder.{TransactionBuilderStep, TxBuilder}
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.uplc.builtin.ByteString

import java.time.Instant
import scala.jdk.CollectionConverters.*

/** Drives [[JavaEmulatorChainInterop]], the proof that the chain members of the Emulator are usable
  * from real Java, spec [SF-5], and checks the results at runtime.
  */
class JavaEmulatorChainInteropTest extends AnyFunSuite {

    private given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val alice = Alice.address(Network.Mainnet)
    private val bob = Bob.address(Network.Mainnet)

    private def manual(): Emulator =
        JavaEmulatorChainInterop.createManual(java.util.List.of(alice), Value.ada(1000))

    private def payment(utxos: Utxos, lovelace: Long): Transaction =
        TxBuilder(testEnv)
            .payTo(bob, Value.lovelace(lovelace))
            .complete(utxos.filter(_._2.address == alice), alice)
            .sign(Alice.signer)
            .transaction

    test("Java builds a ChainConfig with a pool block issuer") {
        // ledger-rules spec [ES-1a], [ES-3]
        val pool = PoolKeyHash.fromHex("b7" * 28)
        assert(JavaEmulatorChainInterop.issuedBy(pool).blockIssuer == BlockIssuer.Pool(pool))
        assert(JavaEmulatorChainInterop.unattributed() == BlockIssuer.Unattributed)
    }

    test("Java sets the pool performance and the active slots coefficient") {
        // ledger-rules spec [ES-1], [ES-2a], [ES-1a]
        val config = JavaEmulatorChainInterop.measured(1, 10)
        assert(config.poolPerformance == PoolPerformance.Measured)
        assert(config.activeSlotsCoeff == UnitInterval(1, 10))
        assert(JavaEmulatorChainInterop.ideal() == PoolPerformance.Ideal)
    }

    test("Java switches over PoolPerformance as over a Java enum") {
        assert(JavaEmulatorChainInterop.performanceWord(PoolPerformance.Measured) == "measured")
        assert(JavaEmulatorChainInterop.performanceCount() == PoolPerformance.values.length)
    }

    test("Java submits to the mempool, reads the views, and forges") {
        val e = manual()
        val tx = payment(e.utxos, 2_000_000)
        assert(e.trySubmit(tx).isSuccess)

        assert(JavaEmulatorChainInterop.isPending(e, tx.id))
        assert(JavaEmulatorChainInterop.mempool(e) == java.util.List.of(tx))
        val atTip = JavaEmulatorChainInterop.utxosAtTip(e, bob)
        assert(atTip.isSuccess && atTip.getValueOrNull.isEmpty)
        assert(JavaEmulatorChainInterop.utxosInMempool(e, bob).getValueOrNull.size == 1)

        val point = JavaEmulatorChainInterop.produceBlock(e)
        assert(JavaEmulatorChainInterop.tip(e).point == point)
        assert(JavaEmulatorChainInterop.confirmedDepth(e, tx.id) == 1)
        assert(JavaEmulatorChainInterop.mempool(e).isEmpty)
        assert(JavaEmulatorChainInterop.utxosAtDepth(e, bob, 1).getValueOrNull.size == 1)

        val tooDeep = JavaEmulatorChainInterop.utxosAtDepth(e, bob, 5)
        assert(!tooDeep.isSuccess)
        assert(tooDeep.getValueOrNull == null)
        assert(tooDeep.getErrorOrNull == ChainViewError.DepthOutOfRange(5, 1))
    }

    test("Java reads a dropped tx's reason through the static DropReason forwarders") {
        val e = manual()
        e.setSlot(99)
        val tx = TxBuilder(testEnv)
            .payTo(bob, Value.lovelace(2_000_000))
            .validTo(Instant.ofEpochMilli(testEnv.slotConfig.slotToTime(100)))
            .complete(e.utxos, alice)
            .sign(Alice.signer)
            .transaction
        assert(e.trySubmit(tx).isSuccess)
        assert(!JavaEmulatorChainInterop.isExpired(e, tx.id))
        e.setSlot(100)
        assert(JavaEmulatorChainInterop.isExpired(e, tx.id))
    }

    test("Java names every TxStatus and DropReason case through the static forwarders") {
        val config = ChainConfig.manual.withOrphanTxs(OrphanTxPolicy.Drop)
        val e = Emulator.withAddresses(Seq(alice), Value.ada(1000), config)
        def word(tx: Transaction) = JavaEmulatorChainInterop.statusWord(e, tx.id)
        val tx = payment(e.utxos, 2_000_000)
        assert(word(tx) == "notFound")
        assert(e.trySubmit(tx).isSuccess)
        assert(word(tx) == "pending")
        e.produceBlock()
        assert(word(tx) == "confirmed")
        assert(e.rollback(1).isRight)
        assert(word(tx) == "rolledBack")

        val other = Emulator.withAddresses(Seq(alice), Value.ada(1000), ChainConfig.manual)
        val pending = payment(other.utxos, 2_000_000)
        assert(other.trySubmit(pending).isSuccess)
        val rival = payment(other.utxos, 3_000_000)
        assert(other.produceBlock(ChainPoint.origin, Seq(rival)).isRight)
        assert(JavaEmulatorChainInterop.statusWord(other, pending.id) == "inputsSpent")
    }

    test("Java forges on a branch, rolls back, and reads the ChainSync diff") {
        val e = manual()
        val tx = payment(e.utxos, 2_000_000)
        val genesis = JavaEmulatorChainInterop.genesis()

        val forged = JavaEmulatorChainInterop.produceBlockOn(e, genesis, java.util.List.of(tx))
        assert(forged.isSuccess && forged.getErrorOrNull == null)
        val point = forged.getValueOrNull
        assert(point.slot == 1L)
        assert(forged.toEither == Right(point))

        val sameSlot =
            JavaEmulatorChainInterop.produceBlockAt(e, genesis, 0L, java.util.List.of(tx))
        assert(sameSlot.getErrorOrNull == ForgeError.SlotNotAfterParent(0L, 0L))

        val events = JavaEmulatorChainInterop.diff(e, ChainTip.origin, e.tip).asScala
        assert(events.collect { case ChainEvent.RollForward(b) => b.point } == Seq(point))

        assert(JavaEmulatorChainInterop.rollback(e, 1).getValueOrNull == genesis)
        assert(
          JavaEmulatorChainInterop.rollback(e, 1).getErrorOrNull ==
              RollbackError.PastGenesis(1, 0)
        )
    }

    test("Java reads the stake snapshots and the pending reward update") {
        // ledger-rules spec [ES-9a] (D82): getPendingRewardsOrNull is null at the genesis
        val e = Emulator.withAddresses(Seq(alice), Value.ada(1000), ChainConfig.instant)
        assert(JavaEmulatorChainInterop.snapshots(e) == e.snapshots)
        assert(JavaEmulatorChainInterop.pendingRewards(e) == null)
        val slotConfig = e.cardanoInfo.slotConfig
        e.setSlot(slotConfig.firstSlotOfEpoch(e.currentEpoch + 1))
        val update = JavaEmulatorChainInterop.pendingRewards(e)
        assert(update != null)
        assert(e.pendingRewards.contains(update))
    }

    test("Java reads the pending ratify result") {
        // ledger-rules spec [ES-9b] (D94): the genesis result enacts nothing, spec [DM-9c]
        val e = Emulator.withAddresses(Seq(alice), Value.ada(1000), ChainConfig.instant)
        val ratified = JavaEmulatorChainInterop.pendingRatify(e)
        assert(ratified == e.pendingRatify)
        assert(ratified.enacted.isEmpty && ratified.expired.isEmpty && !ratified.delayed)
    }

    test("Java reads the pending ratify result of the tip after a boundary") {
        // ledger-rules spec [ES-9b], [RT-10]: the getter reads the tip, not the genesis snapshot
        val slotConfig = testEnv.slotConfig
        val startEpoch = 500L
        val aliceStake = Alice.account.stakeAddress(Network.Mainnet)
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val e = Emulator.withState(
          EmulatorInitialState.empty
              .withUtxos(Map(Input(genesisHash, 0) -> Output(alice, Value.ada(200_000))))
              .withStakeRegistrations(Seq(EmulatorStakeRegistration(aliceStake.credential))),
          Context.testMainnet(slotConfig.firstSlotOfEpoch(startEpoch) + 10)
        )
        val params = testEnv.protocolParams
        val proposal = ProposalProcedure(
          Coin(params.govActionDeposit),
          RewardAccount(aliceStake),
          GovAction.InfoAction,
          Anchor("https://example.com", DataHash.fromHex("0" * 64))
        )
        val tx = TxBuilder(testEnv)
            .addSteps(TransactionBuilderStep.SubmitProposal(proposal))
            .complete(e.utxos, alice)
            .sign(Alice.signer)
            .transaction
        assert(e.submitSync(tx) == Right(tx.id))
        // expiresAfter is startEpoch + lifetime; the boundary after it marks the action expired
        e.setSlot(slotConfig.firstSlotOfEpoch(startEpoch + params.govActionLifetime + 1))
        val ratified = JavaEmulatorChainInterop.pendingRatify(e)
        assert(ratified.expired == Set(GovActionId(tx.id, 0)))
        assert(ratified.enacted.isEmpty)
    }
}
