package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.{AccountState, FuturePParams, IndividualPoolStake, NewEpochState, NonMyopic, PoolDistr}
import scalus.cardano.node.emulator.{BlockIssuer, ChainConfig}
import scalus.cardano.txbuilder.{TransactionBuilderStep, TransactionSigner, TxBuilder}
import scalus.testing.kit.Party
import scalus.testing.kit.Party.{Alice, Bob, Charles, Dave, Eve}
import scalus.uplc.builtin.ByteString

/** `ledgerState`, the node-shaped view of the Emulator's ledger, D122 to D126. */
class EmulatorLedgerStateTest extends AnyFunSuite {

    private given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val slotConfig = testEnv.slotConfig
    private val startEpoch = 500L

    private val alice = Alice.address(Network.Mainnet)
    private val aliceStake: StakeAddress = Alice.account.stakeAddress(Network.Mainnet)
    private val charlesStake: Credential = Charles.account.stakeAddress(Network.Mainnet).credential

    // Bob is the DRep, Charles the cold key of the one committee member, Dave its hot key
    private val drep = Credential.KeyHash(Bob.addrKeyHash)
    private val coldKey = Credential.KeyHash(Charles.addrKeyHash)
    private val hotKey = Credential.KeyHash(Dave.addrKeyHash)
    private val deposit = Coin.ada(1)

    private def pool(owner: Party, vrf: String): Certificate.PoolRegistration =
        Certificate.PoolRegistration(
          operator = owner.addrKeyHash,
          vrfKeyHash = VrfKeyHash.fromHex(vrf * 32),
          pledge = Coin.zero,
          cost = Coin(testEnv.protocolParams.minPoolCost),
          margin = UnitInterval.zero,
          rewardAccount = RewardAccount(owner.account.stakeAddress(Network.Mainnet)),
          poolOwners = Set(owner.addrKeyHash),
          relays = IndexedSeq.empty,
          poolMetadata = None
        )

    private val pool1 = pool(Alice, "a1")
    private val pool2 = pool(Charles, "a2")
    // no account delegates to it, so the pool distribution leaves it out
    private val pool3 = pool(Eve, "a3")
    private def idOf(p: Certificate.PoolRegistration) = PoolKeyHash.fromByteString(p.operator)

    private val initialUtxos: Utxos = (0 until 2).map { i =>
        val hash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        Input(hash, i) -> Output(alice, Value.ada(1000))
    }.toMap

    /** Alice's address has no stake part, so the stake is the reward balances only: 20 ADA of
      * Alice's account in pool 1 and 10 ADA of Charles's in pool 2. One committee member with a hot
      * key, Bob a DRep, gov action lifetime 2, and every block counts for pool 1.
      */
    private def emulator(): Emulator = {
        val context = rules.Context.testMainnet(slotConfig.firstSlotOfEpoch(startEpoch) + 10)
        val params = context.env.params
        val newParams = params.copy(
          govActionLifetime = 2,
          govActionDeposit = deposit.value,
          committeeMinSize = 1,
          dRepVotingThresholds =
              params.dRepVotingThresholds.copy(ppEconomicGroup = UnitInterval.one),
          poolVotingThresholds =
              params.poolVotingThresholds.copy(ppSecurityGroup = UnitInterval.zero)
        )
        Emulator.withState(
          EmulatorInitialState.empty
              .withUtxos(initialUtxos)
              .withStakeRegistrations(
                Seq(
                  EmulatorStakeRegistration(aliceStake.credential, Coin.ada(20), Some(idOf(pool1))),
                  EmulatorStakeRegistration(charlesStake, Coin.ada(10), Some(idOf(pool2)))
                )
              )
              .withPoolRegistrations(Seq(pool1, pool2, pool3).map(EmulatorPoolRegistration(_)))
              .withDRepRegistrations(Seq(EmulatorDRepRegistration(drep, Coin(params.dRepDeposit))))
              .withCommittee(Committee(Map(coldKey -> (startEpoch + 100)), UnitInterval.half))
              .withCommitteeHotKeys(Map(coldKey -> hotKey)),
          context.copy(env = context.env.copy(params = newParams)),
          ChainConfig.instant.withBlockIssuer(BlockIssuer.pool(idOf(pool1)))
        )
    }

    private def submit(e: Emulator, builder: TxBuilder, signers: Party*): Transaction = {
        val keys = (Alice +: signers).map(_.account.paymentKeyPair) :+ Alice.account.stakeKeyPair
        val tx =
            builder.complete(e.utxos, alice).sign(new TransactionSigner(keys.toSet)).transaction
        assert(e.submitSync(tx) == Right(tx.id))
        tx
    }

    private def vote(e: Emulator, voter: Voter, id: GovActionId, signer: Party): Unit = {
        val votes = Map(id -> VotingProcedure(Vote.Yes, None))
        submit(
          e,
          TxBuilder(testEnv).addSteps(TransactionBuilderStep.SubmitVotingProcedure(voter, votes)),
          signer
        )
    }

    /** A ParameterChange of `txFeePerByte`, voted Yes by the DRep and the committee. */
    private def votedFeeChange(e: Emulator): GovActionId = {
        submit(e, TxBuilder(testEnv).delegateVoteToDRep(aliceStake, DRep.KeyHash(Bob.addrKeyHash)))
        val fee = EmulatorBase.stateOf(e).context.env.params.txFeePerByte
        val proposal = ProposalProcedure(
          deposit,
          RewardAccount(aliceStake),
          GovAction
              .ParameterChange(None, ProtocolParamUpdate(minFeeA = Some(Coin(fee + 300))), None),
          Anchor("https://example.com", DataHash.fromHex("0" * 64))
        )
        val proposed =
            submit(e, TxBuilder(testEnv).addSteps(TransactionBuilderStep.SubmitProposal(proposal)))
        val id = GovActionId(proposed.id, 0)
        vote(e, Voter.DRepKey(Bob.addrKeyHash), id, Bob)
        vote(e, Voter.ConstitutionalCommitteeHotKey(Dave.addrKeyHash), id, Dave)
        id
    }

    private def boundary(e: Emulator, n: Long): Unit =
        e.setSlot(slotConfig.firstSlotOfEpoch(startEpoch + n))

    /** Every field of `ledgerState` against the getter or the internal value it views. */
    private def assertViews(e: Emulator): NewEpochState = {
        val view = e.ledgerState
        val state = EmulatorBase.stateOf(e)
        val book = state.tipSnapshot.epoch
        val ledger = state.ledger
        assert(view.lastEpoch == book.lastEpoch)
        assert(view.blocksMadePrev == book.blocksMadePrev)
        assert(view.blocksMadeCur == book.blocksMadeCur)
        assert(view.pendingRewards == e.pendingRewards)
        val es = view.epochState
        assert(es.accountState == AccountState(e.treasury, e.reserves))
        assert(es.accountState == book.accountState)
        assert(es.snapshots == e.snapshots)
        assert(es.nonMyopic == NonMyopic.empty)
        assert(es.params == state.context.env.params)
        assert(es.prevParams == book.prevParams)
        assert(es.pendingRatify == e.pendingRatify)
        val utxoState = es.ledgerState.utxoState
        assert(utxoState.utxo == e.utxos)
        assert(utxoState.deposited == ledger.deposited)
        assert(utxoState.fees == e.fees)
        assert(utxoState.govState == e.govState)
        assert(utxoState.stakeDistribution == ledger.stakeDistribution)
        assert(utxoState.donation == e.donation)
        assert(es.ledgerState.certState == e.certState)
        view
    }

    /** The pool distribution of the set snapshot, shares in lowest terms, D123. */
    private def assertPoolDistrOfSet(view: NewEpochState): Unit = {
        val set = view.epochState.snapshots.set
        val expected = set.pools.collect {
            case (id, p) if p.numDelegators > 0 =>
                val share = NonNegativeInterval.reduceBigInt(
                  BigInt(p.stake.value),
                  BigInt(set.totalActiveStake.value)
                )
                id -> IndividualPoolStake(
                  UnitInterval(share.numerator, share.denominator),
                  p.stake,
                  p.vrfKeyHash
                )
        }
        assert(view.poolDistr == PoolDistr(expected, set.totalActiveStake))
    }

    test("at genesis the pool distribution is the share of each pool with a delegator") {
        // NewEpoch.hs:182 takes the pool distribution of the mark before SNAP, the set after it;
        // SnapShots.hs:449-463 leaves out a pool with no delegator. 20 + 10 ADA of stake.
        val view = assertViews(emulator())
        val pd = view.poolDistr
        assert(
          pd == PoolDistr(
            Map(
              idOf(pool1) -> IndividualPoolStake(
                UnitInterval(2, 3),
                Coin.ada(20),
                pool1.vrfKeyHash
              ),
              idOf(pool2) -> IndividualPoolStake(
                UnitInterval(1, 3),
                Coin.ada(10),
                pool2.vrfKeyHash
              )
            ),
            Coin.ada(30)
          )
        )
        assert(view.lastEpoch == startEpoch)
        assert(view.epochState.futureParams == FuturePParams.NoPParamsUpdate)
    }

    test(
      "after boundaries with rewards and an enacted ParameterChange the view equals the getters"
    ) {
        val e = emulator()
        val id = votedFeeChange(e)
        boundary(e, 1)
        val ratified = assertViews(e)
        assert(ratified.lastEpoch == e.currentEpoch)
        assert(ratified.blocksMadePrev == Map(idOf(pool1) -> 4L))
        // RATIFY accepted the ParameterChange; the next boundary adopts its pparams
        assert(e.pendingRatify.enacted == Vector(id))
        assert(
          ratified.epochState.futureParams ==
              FuturePParams.DefinitePParamsUpdate(e.pendingRatify.enactState.params)
        )
        boundary(e, 2)
        val enacted = assertViews(e)
        assertPoolDistrOfSet(enacted)
        // boundary 2 paid pool 1's leader reward to Alice's account before SNAP took mark; the
        // pool distribution is that of set, the stake of boundary 1, so it leaves the reward out
        val mark = enacted.epochState.snapshots.mark
        assert(mark.pools(idOf(pool1)).stake > Coin.ada(20))
        assert(enacted.poolDistr.pools(idOf(pool1)).totalStake == Coin.ada(20))
        assert(enacted.epochState.futureParams == FuturePParams.NoPParamsUpdate)
        assert(
          FuturePParams.DefinitePParamsUpdate(enacted.epochState.params) ==
              ratified.epochState.futureParams
        )
        assert(enacted.epochState.prevParams == ratified.epochState.params)
        boundary(e, 3)
        val paid = assertViews(e)
        assert(paid.pendingRewards.exists(_.rewards.nonEmpty))
        assertPoolDistrOfSet(paid)
        assert(paid.poolDistr.pools.keySet == Set(idOf(pool1), idOf(pool2)))
    }
}
