package scalus.cardano.node

import org.scalatest.EitherValues
import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.node.SubmitError.ValidationError
import scalus.cardano.node.emulator.{ChainConfig, DropReason, OrphanTxPolicy, TxStatus}
import scalus.cardano.txbuilder.TransactionBuilderStep.{IssueCertificate, SubmitProposal, SubmitVotingProcedure}
import scalus.cardano.txbuilder.{PubKeyWitness, TxBuilder}
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import scalus.uplc.builtin.ByteString

/** The GOV rule in the Emulator, phase 2 acceptance 4 of the ledger rules spec: votes and proposals
  * go through [[scalus.cardano.ledger.rules.GovMutator]], spec [GV-10b]. Also the gov surface of
  * the Emulator: the genesis committee, hot keys and constitution, spec [ES-6], [ES-6a], [ES-13],
  * [ES-14], and the gov reads, spec [ES-9].
  */
class EmulatorGovTest extends AnyFunSuite with Matchers with EitherValues with OptionValues {

    private given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
    private val alice = Alice.address(Network.Mainnet)
    private val aliceKey = Credential.KeyHash(Alice.addrKeyHash)

    private val initialUtxos: Utxos = (0 until 4).map { i =>
        Input(genesisHash, i) -> Output(alice, Value.ada(200_000))
    }.toMap

    // Charles is the cold key of the seeded committee member, Bob its hot key.
    private val coldKey = Credential.KeyHash(Charles.addrKeyHash)
    private val hotKey = Credential.KeyHash(Bob.addrKeyHash)
    private val committee = Committee(Map(coldKey -> 10_000L), UnitInterval(1, 1))
    private val constitution = Constitution(
      Anchor("https://example.com/constitution", DataHash.fromHex("2" * 64)),
      Some(ScriptHash.fromHex("3" * 56))
    )

    private def initialState(
        committee: Option[Committee],
        hotKeys: Map[Credential, Credential],
        constitution: Option[Constitution]
    ): EmulatorInitialState =
        EmulatorInitialState(
          utxos = initialUtxos,
          stakeRegistrations = Seq(EmulatorStakeRegistration.of(aliceKey)),
          drepRegistrations = Seq(
            EmulatorDRepRegistration.of(aliceKey, Coin(testEnv.protocolParams.dRepDeposit))
          ),
          committee = committee,
          committeeHotKeys = hotKeys,
          constitution = constitution
        )

    private def emulator(config: ChainConfig = ChainConfig.instant): Emulator =
        Emulator.withState(initialState(None, Map.empty, None), rules.Context.testMainnet(), config)

    private def emulatorWithCommittee(
        hotKeys: Map[Credential, Credential],
        config: ChainConfig = ChainConfig.instant
    ): Emulator =
        Emulator.withState(
          initialState(Some(committee), hotKeys, Some(constitution)),
          rules.Context.testMainnet(),
          config
        )

    private def voteTx(e: Emulator, voter: Voter, id: GovActionId): Transaction =
        voteTx(e.utxos, voter, id)

    private def voteTx(utxos: Utxos, voter: Voter, id: GovActionId): Transaction = {
        val built = TxBuilder(testEnv)
            .addSteps(SubmitVotingProcedure(voter, Map(id -> VotingProcedure(Vote.Yes, None))))
            .complete(utxos, alice)
            .sign(Alice.signer)
        val needsBob = voter == Voter.DRepKey(Bob.addrKeyHash) ||
            voter == Voter.ConstitutionalCommitteeHotKey(Bob.addrKeyHash)
        (if needsBob then built.sign(Bob.signer) else built).transaction
    }

    private val returnAccount = RewardAccount(
      StakeAddress(
        Network.Mainnet,
        StakePayload.Stake(StakeKeyHash.fromByteString(Alice.addrKeyHash))
      )
    )

    private val infoProposal = ProposalProcedure(
      Coin(testEnv.protocolParams.govActionDeposit),
      returnAccount,
      GovAction.InfoAction,
      Anchor("https://example.com", DataHash.fromHex("0" * 64))
    )

    private def proposalTx(utxos: Utxos): Transaction =
        TxBuilder(testEnv)
            .addSteps(SubmitProposal(infoProposal))
            .complete(utxos, alice)
            .sign(Alice.signer)
            .transaction

    /** The genesis UTxO of index `i` alone, so two txs built from different ones do not conflict.
      */
    private def utxo(i: Int): Utxos = initialUtxos.filter((input, _) => input.index == i)

    private def ruleOf(result: Either[SubmitError, TransactionHash]): Option[String] =
        result.left.value match
            case e: ValidationError => e.errorCode
            case other              => fail(s"not a ValidationError: $other")

    test("spec [GV-10b]: GovActionsDoNotExist for a seeded DRep, VotersDoNotExist for an unknown") {
        val e = emulator()
        val missing = GovActionId(TransactionHash.fromByteString(ByteString.fromHex("1" * 64)), 0)
        ruleOf(e.submitSync(voteTx(e, Voter.DRepKey(Alice.addrKeyHash), missing))) shouldBe
            Some("GovActionsDoNotExist")
        ruleOf(e.submitSync(voteTx(e, Voter.DRepKey(Bob.addrKeyHash), missing))) shouldBe
            Some("VotersDoNotExist")
    }

    test("spec [GV-4], [GV-14], [ES-9]: a proposal and votes on it are read back") {
        val e = emulatorWithCommittee(Map(coldKey -> hotKey))
        val tx = proposalTx(e.utxos)
        e.submitSync(tx).value
        val id = GovActionId(tx.id, 0)
        e.submitSync(voteTx(e, Voter.DRepKey(Alice.addrKeyHash), id)).value
        e.submitSync(voteTx(e, Voter.ConstitutionalCommitteeHotKey(Bob.addrKeyHash), id)).value
        e.govState.proposals.map(_.id) shouldBe Vector(id)
        val stored = e.govState.proposal(id).value
        stored.procedure shouldBe infoProposal
        stored.drepVotes shouldBe Map(aliceKey -> Vote.Yes)
        stored.committeeVotes shouldBe Map(hotKey -> Vote.Yes)
        // spec [ES-9]: one DRep state is read from the cert state, D35
        e.certState.vstate.dreps.get(aliceKey).map(_.deposit) shouldBe
            Some(Coin(testEnv.protocolParams.dRepDeposit))
    }

    test("spec [ES-13], [ES-14]: with nothing seeded there is no committee and no guardrails") {
        val gov = emulator().govState
        gov.committee shouldBe None
        gov.constitution.scriptHash shouldBe None
        gov shouldBe GovState.empty
    }

    test("spec [ES-6], [ES-6a]: the seeded committee, hot key and constitution are at genesis") {
        val e = emulatorWithCommittee(Map(coldKey -> hotKey))
        e.govState.committee shouldBe Some(committee)
        e.govState.constitution shouldBe constitution
        e.certState.vstate.committeeState shouldBe
            Map(coldKey -> CommitteeAuthorization.HotCredential(hotKey))
    }

    test("spec [ES-6a]: a hot key for a cold credential that is not a member is rejected") {
        val stranger = Credential.KeyHash(Alice.addrKeyHash)
        val thrown = intercept[IllegalArgumentException] {
            emulatorWithCommittee(Map(stranger -> hotKey))
        }
        thrown.getMessage should include("not a member")
        intercept[IllegalArgumentException] {
            Emulator.withState(initialState(None, Map(coldKey -> hotKey), None))
        }
    }

    test(
      "spec [ES-6], [GC-3]: a seeded member authorizes a hot key by tx, then the hot key votes"
    ) {
        val e = emulatorWithCommittee(Map.empty)
        val auth = TxBuilder(testEnv)
            .addSteps(
              IssueCertificate(Certificate.AuthCommitteeHotCert(coldKey, hotKey), PubKeyWitness)
            )
            // TxBuilder does not collect the cold key witness of a committee cert, so the test
            // names it as a required signer; else `sign` drops Charles's signature
            .requireSignature(Charles.addrKeyHash)
            .complete(e.utxos, alice)
            .sign(Alice.signer)
            .sign(Charles.signer)
            .transaction
        e.submitSync(auth).value
        e.certState.vstate.committeeState shouldBe
            Map(coldKey -> CommitteeAuthorization.HotCredential(hotKey))
        val tx = proposalTx(e.utxos)
        e.submitSync(tx).value
        val id = GovActionId(tx.id, 0)
        e.submitSync(voteTx(e, Voter.ConstitutionalCommitteeHotKey(Bob.addrKeyHash), id)).value
        e.govState.proposal(id).value.committeeVotes shouldBe Map(hotKey -> Vote.Yes)
    }

    test("spec [ES-6]: a rollback removes a proposal and keeps the seeded committee") {
        val config = ChainConfig.manual.withOrphanTxs(OrphanTxPolicy.Drop)
        val e = emulatorWithCommittee(Map(coldKey -> hotKey), config)
        val genesis = e.govState
        val tx = proposalTx(utxo(0))
        e.submitSync(tx).value
        e.produceBlock()
        val id = GovActionId(tx.id, 0)
        e.govState.proposal(id) shouldBe defined
        e.rollback(1).value
        e.govState.proposal(id) shouldBe None
        e.txStatus(tx.id) shouldBe TxStatus.RolledBack
        e.govState shouldBe genesis
        e.govState.committee shouldBe Some(committee)
        e.certState.vstate.committeeState shouldBe
            Map(coldKey -> CommitteeAuthorization.HotCredential(hotKey))
    }

    test("spec [GV-10b]: mempool revalidation drops a vote whose action vanished") {
        val e = emulator(ChainConfig.manual.withOrphanTxs(OrphanTxPolicy.Drop))
        val tx = proposalTx(utxo(0))
        e.submitSync(tx).value
        e.produceBlock()
        val id = GovActionId(tx.id, 0)
        val vote = voteTx(utxo(1), Voter.DRepKey(Alice.addrKeyHash), id)
        e.submitSync(vote).value
        e.txStatus(vote.id) shouldBe TxStatus.Pending
        e.rollback(1).value
        e.mempoolTxs shouldBe empty
        e.txStatus(vote.id) match
            case TxStatus.Dropped(DropReason.Invalid(error: ValidationError)) =>
                error.errorCode shouldBe Some("GovActionsDoNotExist")
            case other => fail(s"expected a vote dropped as GovActionsDoNotExist, got $other")
    }
}
