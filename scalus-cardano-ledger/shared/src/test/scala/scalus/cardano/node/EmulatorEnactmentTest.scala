package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.RewardUpdate
import scalus.cardano.node.emulator.{ChainConfig, DropReason, TxStatus}
import scalus.cardano.node.stream.{ScalusAsyncSource, StreamingEmulator}
import scalus.cardano.txbuilder.{TransactionBuilderStep, TransactionSigner, TxBuilder}
import scalus.testing.kit.Party
import scalus.testing.kit.Party.{Alice, Bob, Charles, Dave}
import scalus.uplc.builtin.ByteString

import scala.concurrent.Future

/** The ratify and enact cycle of the Emulator across epoch boundaries, ledger-rules spec [EB-10] to
  * [EB-14], [EB-20], [ES-9b], phase 5 acceptance 14. The Imp-timed acceptance 9 and the lifecycle
  * of acceptance 10 are in [[EmulatorGovLifecycleTest]].
  */
class EmulatorEnactmentTest extends AnyFunSuite {

    private given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val slotConfig = testEnv.slotConfig
    private val startEpoch = 500L

    private val alice = Alice.address(Network.Mainnet)
    private val aliceStake: StakeAddress = Alice.account.stakeAddress(Network.Mainnet)
    private val aliceAccount: Credential = aliceStake.credential

    // Bob is the DRep, Charles the cold key of the one committee member, Dave its hot key
    private val drep = Credential.KeyHash(Bob.addrKeyHash)
    private val coldKey = Credential.KeyHash(Charles.addrKeyHash)
    private val hotKey = Credential.KeyHash(Dave.addrKeyHash)

    private val deposit = Coin.ada(1)

    private val initialUtxos: Utxos = (0 until 2).map { i =>
        val hash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        Input(hash, i) -> Output(alice, Value.ada(1000))
    }.toMap

    /** An emulator at slot 10 of `startEpoch` with one committee member and its hot key, Bob as a
      * DRep, Alice's stake account registered with a reward balance, and gov action lifetime 2. The
      * DRep threshold of the economic group is 1 (Imp "proposal is accepted after two epochs",
      * EpochSpec.hs:354-391). The SPO security-group threshold is 0, so no pool needs to vote
      * ([RT-11]).
      */
    private def emulator(config: ChainConfig): Emulator = {
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
              .withStakeRegistrations(Seq(EmulatorStakeRegistration(aliceAccount, Coin.ada(10))))
              .withDRepRegistrations(Seq(EmulatorDRepRegistration(drep, Coin(params.dRepDeposit))))
              .withCommittee(Committee(Map(coldKey -> (startEpoch + 100)), UnitInterval.half))
              .withCommitteeHotKeys(Map(coldKey -> hotKey)),
          context.copy(env = context.env.copy(params = newParams)),
          config
        )
    }

    /** Submits the tx of `builder`, and forges it at once under `Manual` block production. */
    private def submit(e: Emulator, builder: TxBuilder, signers: Party*): Transaction = {
        val tx = submitted(e, builder, signers*)
        if e.txStatus(tx.id) == TxStatus.Pending then e.produceBlock()
        tx
    }

    /** Submits the tx of `builder`, which stays in the mempool under `Manual` block production. */
    private def submitted(e: Emulator, builder: TxBuilder, signers: Party*): Transaction = {
        val keys = (Alice +: signers).map(_.account.paymentKeyPair) :+ Alice.account.stakeKeyPair
        val signer = new TransactionSigner(keys.toSet)
        val tx = builder.complete(e.utxos, alice).sign(signer).transaction
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

    private def boundary(e: Emulator, n: Long): Unit =
        e.setSlot(slotConfig.firstSlotOfEpoch(startEpoch + n))

    private def txFeePerByte(e: Emulator): Long =
        EmulatorBase.stateOf(e).context.env.params.txFeePerByte

    /** A ParameterChange of `txFeePerByte` to `initialFee + 300`, voted Yes by the DRep and the
      * committee in the first epoch, with Alice's stake delegated to the DRep. Returns its id.
      */
    private def votedFeeChange(e: Emulator, initialFee: Long): GovActionId = {
        submit(e, TxBuilder(testEnv).delegateVoteToDRep(aliceStake, DRep.KeyHash(Bob.addrKeyHash)))
        val update = ProtocolParamUpdate(minFeeA = Some(Coin(initialFee + 300)))
        val proposal = ProposalProcedure(
          deposit,
          RewardAccount(aliceStake),
          GovAction.ParameterChange(None, update, None),
          Anchor("https://example.com", DataHash.fromHex("0" * 64))
        )
        val proposed =
            submit(e, TxBuilder(testEnv).addSteps(TransactionBuilderStep.SubmitProposal(proposal)))
        val id = GovActionId(proposed.id, 0)
        vote(e, Voter.DRepKey(Bob.addrKeyHash), id, Bob)
        vote(e, Voter.ConstitutionalCommitteeHotKey(Dave.addrKeyHash), id, Dave)
        id
    }

    test("a voted ParameterChange is ratified at the next boundary and enacted at the one after") {
        // spec [EB-20], [EB-10] to [EB-14], [ES-9b], [ES-15]: votes of epoch 0,
        // ratified at boundary 1, enacted at 2
        val e = emulator(ChainConfig.instant)
        val initialFee = txFeePerByte(e)
        val id = votedFeeChange(e, initialFee)
        // the genesis ratify result enacts nothing, spec [DM-9c]
        assert(e.pendingRatify.enacted.isEmpty)
        val balance = e.certState.dstate.accounts(aliceAccount).balance

        boundary(e, 1)
        // RATIFY at boundary 1 accepts it with the votes of epoch 0; nothing is applied yet
        assert(e.pendingRatify.enacted == Vector(id))
        assert(e.pendingRatify.enactState.params.txFeePerByte == initialFee + 300)
        assert(txFeePerByte(e) == initialFee)
        assert(e.govState.proposal(id).isDefined)

        boundary(e, 2)
        // [EB-12c], [EB-13a], [EB-14a]: the action is the root, the pparams change, the deposit
        // returns to its account
        assert(txFeePerByte(e) == initialFee + 300)
        assert(e.govState.proposals.isEmpty)
        assert(e.govState.roots.pparamUpdate == Some(id))
        assert(e.certState.dstate.accounts(aliceAccount).balance == balance + deposit)
        assert(e.pendingRatify.enacted.isEmpty)
    }

    test("an enacted txFeePerByte drops a mempool tx that pays the old fee") {
        // spec [ES-15], acceptance 14: revalidation after the boundary reads the new pparams
        def run(voted: Boolean): (Emulator, Transaction) = {
            val e = emulator(ChainConfig.manual)
            if voted then votedFeeChange(e, txFeePerByte(e))
            boundary(e, 1)
            val oldFee = submitted(e, TxBuilder(testEnv).payTo(alice, Value.ada(5)))
            assert(e.txStatus(oldFee.id) == TxStatus.Pending)
            boundary(e, 2)
            (e, oldFee)
        }
        val (e, oldFee) = run(voted = true)
        assert(txFeePerByte(e) == txFeePerByte(emulator(ChainConfig.manual)) + 300)
        e.txStatus(oldFee.id) match
            case TxStatus.Dropped(DropReason.Invalid(error)) => assert(error.rule == "FeesOk")
            case other => fail(s"not dropped as invalid: $other")
        // control: with no enactment the same tx stays in the mempool
        val (control, kept) = run(voted = false)
        assert(control.txStatus(kept.id) == TxStatus.Pending)
    }

    test("one move over both boundaries gives the pparams and rewards of two moves") {
        // spec [EB-25], acceptance 14: boundary 2 reads the pparams boundary 1 left
        val initialFee = txFeePerByte(emulator(ChainConfig.instant))
        def run(moves: Seq[Long]): (ProtocolParams, ProtocolParams, Option[RewardUpdate]) = {
            val e = emulator(ChainConfig.instant)
            votedFeeChange(e, initialFee)
            moves.foreach(boundary(e, _))
            val state = EmulatorBase.stateOf(e)
            (state.context.env.params, state.tipSnapshot.epoch.prevParams, e.pendingRewards)
        }
        val twoMoves = run(Seq(1, 2))
        assert(twoMoves._1.txFeePerByte == initialFee + 300)
        assert(run(Seq(2)) == twoMoves)
    }

    test("a protocol-params subscriber of a StreamingEmulator sees the enacted pparams") {
        // spec [ES-15], [EB-25]: the stream reads the pparams a boundary enacts, as
        // fetchLatestParams does
        val e = emulator(ChainConfig.instant)
        val initialFee = txFeePerByte(e)
        votedFeeChange(e, initialFee)
        val stream = new StreamingEmulator(e)
        def fee(next: Future[Option[ProtocolParams]]): Option[Long] =
            next.value.flatMap(_.toOption).flatten.map(_.txFeePerByte)
        val early = stream.subscribeProtocolParams[ScalusAsyncSource]()
        assert(fee(early.pull()) == Some(initialFee))
        val update = early.pull()
        boundary(e, 2)
        // a subscription made after an enactment through the emulator, with no stream operation
        // between, starts at the enacted pparams, as fetchLatestParams reads them
        val late = stream.subscribeProtocolParams[ScalusAsyncSource]()
        assert(fee(late.pull()) == Some(initialFee + 300))
        assert(fee(update) == Some(initialFee + 300))
        // a rollback of the committee vote, a block before both boundaries, leaves the action
        // unratified at the current slot, so the stream emits the old pparams again
        val restored = early.pull()
        assert(stream.rollback(1).isRight)
        assert(txFeePerByte(e) == initialFee)
        assert(fee(restored) == Some(initialFee))
    }
}
