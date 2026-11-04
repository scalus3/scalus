package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.{FuturePParams, PoolPerformance}
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.emulator.{AwaitTxPolicy, BlockIssuer, BlockProduction, ChainConfig, ChainViewError, ForgeError, OrphanTxPolicy, RollbackError}
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.cardano.node.stream.internal.ChainEvent
import scalus.cardano.txbuilder.{TransactionBuilderStep, TransactionSigner, TxBuilder}
import scalus.testing.kit.Party
import scalus.testing.kit.Party.{Alice, Bob, Charles, Dave}
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

    test("Java reads the UTxO set and the future pparams through ledgerState") {
        // ledger-rules spec D122, D125: no ParameterChange is ratified, so none is adopted
        val e = Emulator.withAddresses(Seq(alice), Value.ada(1000), ChainConfig.instant)
        assert(JavaEmulatorChainInterop.ledgerStateUtxos(e) == e.utxos)
        assert(JavaEmulatorChainInterop.ledgerStateUtxos(e).nonEmpty)
        assert(JavaEmulatorChainInterop.futureParamsOrNull(e) == null)
    }

    test("Java puts the chain policies in an EnumSet") {
        // interop rule 8: a new enum with parameterless cases only extends java.lang.Enum
        assert(
          JavaEmulatorChainInterop.blockProductions().asScala.toSet == BlockProduction.values.toSet
        )
        assert(
          JavaEmulatorChainInterop.awaitTxPolicies().asScala.toSet == AwaitTxPolicy.values.toSet
        )
        assert(
          JavaEmulatorChainInterop.orphanTxPolicies().asScala.toSet == OrphanTxPolicy.values.toSet
        )
    }

    test("Java builds each FuturePParams case through its static forwarder") {
        // interop rule 9, D125
        val params = testEnv.protocolParams
        assert(JavaEmulatorChainInterop.noPParamsUpdate() == FuturePParams.NoPParamsUpdate)
        assert(
          JavaEmulatorChainInterop.definitePParamsUpdate(params) ==
              FuturePParams.DefinitePParamsUpdate(params)
        )
    }

    test("Java reads the pparams of a ratified ParameterChange as the future pparams") {
        // ledger-rules spec D122, D125: after the boundary that ratifies the change, the future
        // pparams are definite until the next boundary adopts them
        val slotConfig = testEnv.slotConfig
        val startEpoch = 500L
        val aliceStake = Alice.account.stakeAddress(Network.Mainnet)
        val drep = Credential.KeyHash(Bob.addrKeyHash)
        val coldKey = Credential.KeyHash(Charles.addrKeyHash)
        val deposit = Coin.ada(1)
        val context = Context.testMainnet(slotConfig.firstSlotOfEpoch(startEpoch) + 10)
        val params = context.env.params
        // one DRep and one committee member decide the change of txFeePerByte, an economic and
        // security parameter; the pools need no vote
        val govParams = params.copy(
          govActionLifetime = 2,
          govActionDeposit = deposit.value,
          committeeMinSize = 1,
          dRepVotingThresholds =
              params.dRepVotingThresholds.copy(ppEconomicGroup = UnitInterval.one),
          poolVotingThresholds =
              params.poolVotingThresholds.copy(ppSecurityGroup = UnitInterval.zero)
        )
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val e = Emulator.withState(
          EmulatorInitialState.empty
              .withUtxos(Map(Input(genesisHash, 0) -> Output(alice, Value.ada(1000))))
              .withStakeRegistrations(
                Seq(EmulatorStakeRegistration(aliceStake.credential, Coin.ada(20), None))
              )
              .withDRepRegistrations(Seq(EmulatorDRepRegistration(drep, Coin(params.dRepDeposit))))
              .withCommittee(Committee(Map(coldKey -> (startEpoch + 100)), UnitInterval.half))
              .withCommitteeHotKeys(Map(coldKey -> Credential.KeyHash(Dave.addrKeyHash))),
          context.copy(env = context.env.copy(params = govParams)),
          ChainConfig.instant
        )
        def submit(builder: TxBuilder, signers: Party*): Transaction = {
            val keys = (Alice +: signers).map(_.account.paymentKeyPair) :+
                Alice.account.stakeKeyPair
            val tx =
                builder.complete(e.utxos, alice).sign(new TransactionSigner(keys.toSet)).transaction
            assert(e.submitSync(tx) == Right(tx.id))
            tx
        }
        submit(TxBuilder(testEnv).delegateVoteToDRep(aliceStake, DRep.KeyHash(Bob.addrKeyHash)))
        val newFee = Coin(govParams.txFeePerByte + 300)
        val proposal = ProposalProcedure(
          deposit,
          RewardAccount(aliceStake),
          GovAction.ParameterChange(None, ProtocolParamUpdate(minFeeA = Some(newFee)), None),
          Anchor("https://example.com", DataHash.fromHex("0" * 64))
        )
        val proposed =
            submit(TxBuilder(testEnv).addSteps(TransactionBuilderStep.SubmitProposal(proposal)))
        val id = GovActionId(proposed.id, 0)
        val yes = Map(id -> VotingProcedure(Vote.Yes, None))
        for (voter, signer) <- Seq(
              Voter.DRepKey(Bob.addrKeyHash) -> Bob,
              Voter.ConstitutionalCommitteeHotKey(Dave.addrKeyHash) -> Dave
            )
        do
            submit(
              TxBuilder(testEnv)
                  .addSteps(TransactionBuilderStep.SubmitVotingProcedure(voter, yes)),
              signer
            )
        assert(JavaEmulatorChainInterop.futureParamsOrNull(e) == null)
        e.setSlot(slotConfig.firstSlotOfEpoch(startEpoch + 1))
        assert(e.pendingRatify.enacted == Vector(id))
        val future = JavaEmulatorChainInterop.futureParamsOrNull(e)
        assert(future != null)
        assert(future.txFeePerByte == newFee.value)
        assert(future == e.pendingRatify.enactState.params)
    }

    test("Java reads the ledger state's maps and reward update through its getters") {
        // ledger-rules spec D122, D128: a pool with Alice's stake, every block counting for it
        val slotConfig = testEnv.slotConfig
        val startEpoch = 500L
        val aliceStake = Alice.account.stakeAddress(Network.Mainnet)
        val registration: Certificate.PoolRegistration = Certificate.PoolRegistration(
          operator = Alice.addrKeyHash,
          vrfKeyHash = VrfKeyHash.fromHex("a1" * 32),
          pledge = Coin.zero,
          cost = Coin(testEnv.protocolParams.minPoolCost),
          margin = UnitInterval.zero,
          rewardAccount = RewardAccount(aliceStake),
          poolOwners = Set(Alice.addrKeyHash),
          relays = IndexedSeq.empty,
          poolMetadata = None
        )
        val poolId = PoolKeyHash.fromByteString(registration.operator)
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val e = Emulator.withState(
          EmulatorInitialState.empty
              .withUtxos(Map(Input(genesisHash, 0) -> Output(alice, Value.ada(1000))))
              .withStakeRegistrations(
                Seq(EmulatorStakeRegistration(aliceStake.credential, Coin.ada(20), Some(poolId)))
              )
              .withPoolRegistrations(Seq(EmulatorPoolRegistration(registration))),
          Context.testMainnet(slotConfig.firstSlotOfEpoch(startEpoch) + 10),
          ChainConfig.instant.withBlockIssuer(BlockIssuer.pool(poolId))
        )
        val genesis = JavaEmulatorChainInterop.ledgerState(e)
        assert(genesis == e.ledgerState)
        assert(JavaEmulatorChainInterop.ledgerStatePendingRewards(genesis) == null)
        // the genesis seeds the snapshots, so the pool distribution holds the pool, spec [ES-8a]
        val pools = JavaEmulatorChainInterop.poolDistr(genesis).asScala.toMap
        assert(pools.keySet == Set(poolId))
        assert(pools == genesis.poolDistr.pools)
        assert(JavaEmulatorChainInterop.likelihoods(genesis).isEmpty)
        val tx = payment(e.utxos, 2_000_000L)
        assert(e.submitSync(tx) == Right(tx.id))
        val forged = JavaEmulatorChainInterop.ledgerState(e)
        assert(JavaEmulatorChainInterop.blocksMadeCur(forged).asScala.toMap == Map(poolId -> 1L))
        e.setSlot(slotConfig.firstSlotOfEpoch(startEpoch + 1))
        val after = JavaEmulatorChainInterop.ledgerState(e)
        assert(JavaEmulatorChainInterop.blocksMadePrev(after).asScala.toMap == Map(poolId -> 1L))
        assert(JavaEmulatorChainInterop.blocksMadeCur(after).isEmpty)
        val rewards = JavaEmulatorChainInterop.ledgerStatePendingRewards(after)
        assert(rewards != null && after.pendingRewards.contains(rewards))
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
