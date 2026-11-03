package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{NativeScriptWitness, TxBuilder}
import scalus.cardano.txbuilder.TransactionBuilderStep.{SubmitProposal, SubmitVotingProcedure}
import scalus.testing.kit.Party.{Alice, Bob, Charles, Dave}
import scalus.uplc.builtin.ByteString
import scalus.utils.scalajs.internal.*

import scala.scalajs.js

/** The gov surface of the JavaScript `Emulator`: the committee, hot keys and constitution options,
  * ledger-rules spec [ES-6], [ES-6a], [ES-13], [ES-14], and `getGovState`, spec [ES-9].
  */
class JEmulatorGovTest extends AnyFunSuite {

    private given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
    private val alice = Alice.address(Network.Mainnet)
    private val initialUtxos: Utxos = (0 until 2).map { i =>
        Input(genesisHash, i) -> Output(alice, Value.ada(200_000))
    }.toMap

    private val committee = js.Dynamic.literal(
      members = js.Array(
        js.Dynamic.literal(
          credentialType = "key",
          credentialHash = Charles.addrKeyHash.toHex,
          expiresAfter = 10_000.0
        )
      ),
      threshold = js.Dynamic.literal(numerator = 1.0, denominator = 1.0)
    )

    private def hotKey(cold: AddrKeyHash) = js.Dynamic.literal(
      coldCredentialType = "key",
      coldCredentialHash = cold.toHex,
      hotCredentialType = "key",
      hotCredentialHash = Bob.addrKeyHash.toHex
    )

    private val constitution = js.Dynamic.literal(
      anchorUrl = "https://example.com/constitution",
      anchorDataHash = "2" * 64,
      guardrailsScriptHash = "3" * 56
    )

    /** A `JEmulator` at slot 0 with funded UTxOs, a stake registration for Alice, and `extra`. */
    private def create(extra: (String, js.Any)*): JEmulator = {
        val options = js.Dynamic.literal(
          utxos = js.Array(initialUtxos.toSeq.map((i, o) => JsUtxo.wrap(i, o))*),
          slot = 0.0,
          stakeRegistrations = js.Array(
            js.Dynamic.literal(
              credentialType = "key",
              credentialHash = Alice.addrKeyHash.toHex,
              rewards = js.BigInt(0)
            )
          )
        )
        extra.foreach((k, v) => options.updateDynamic(k)(v))
        JEmulator.create(JsCardanoInfo.mainnet(), options.asInstanceOf[JsEmulatorOptions])
    }

    private def bytes(tx: Transaction) = tx.toCbor.toUint8Array

    /** The genesis UTxO of index `i` alone, so two txs built from different ones do not conflict.
      */
    private def utxo(i: Int): Utxos = initialUtxos.filter((input, _) => input.index == i)

    /** A proposal of `action` with the deposit returned to Alice's registered stake credential. */
    private def proposal(action: GovAction): ProposalProcedure = ProposalProcedure(
      Coin(testEnv.protocolParams.govActionDeposit),
      RewardAccount(
        StakeAddress(
          Network.Mainnet,
          StakePayload.Stake(StakeKeyHash.fromByteString(Alice.addrKeyHash))
        )
      ),
      action,
      Anchor("https://example.com", DataHash.fromHex("0" * 64))
    )

    private def drepRegistration(credentialType: String, credentialHash: String) =
        js.Dynamic.literal(
          credentialType = credentialType,
          credentialHash = credentialHash,
          deposit = js.BigInt(testEnv.protocolParams.dRepDeposit.toString)
        )

    test("spec [ES-13], [ES-14]: no committee and no guardrails script by default") {
        val gov = create().getGovState()
        assert(gov.proposals.isEmpty)
        assert(gov.committee.toOption.isEmpty)
        assert(gov.constitution.anchorUrl == "")
        assert(gov.constitution.guardrailsScriptHash.toOption.isEmpty)
    }

    test("spec [ES-6]: the seeded committee and constitution are read back") {
        val gov = create(
          "committee" -> committee,
          "committeeHotKeys" -> js.Array(hotKey(Charles.addrKeyHash)),
          "constitution" -> constitution
        ).getGovState()
        val c = gov.committee.toOption.get
        assert(c.members.length == 1)
        assert(c.members(0).credentialHash == Charles.addrKeyHash.toHex)
        assert(c.members(0).expiresAfter == 10_000.0)
        assert((c.threshold.numerator, c.threshold.denominator) == (1.0, 1.0))
        assert(gov.constitution.anchorDataHash == "2" * 64)
        assert(gov.constitution.guardrailsScriptHash.toOption.contains("3" * 56))
    }

    test("spec [ES-6a]: a hot key for a cold credential that is not a member is a TypeError") {
        val thrown = intercept[js.JavaScriptException](
          create(
            "committee" -> committee,
            "committeeHotKeys" -> js.Array(hotKey(Alice.addrKeyHash))
          )
        )
        assert(thrown.exception.isInstanceOf[js.TypeError], thrown)
    }

    test("a committee with a fraction or an out-of-range number is a TypeError naming it") {
        def member(expiresAfter: Double) = js.Dynamic.literal(
          credentialType = "key",
          credentialHash = Charles.addrKeyHash.toHex,
          expiresAfter = expiresAfter
        )
        def badCommittee(expiresAfter: Double, numerator: Double, denominator: Double) =
            js.Dynamic.literal(
              members = js.Array(member(expiresAfter)),
              threshold = js.Dynamic.literal(numerator = numerator, denominator = denominator)
            )
        val cases = Seq(
          badCommittee(10_000.0, 0.67, 1.0) -> "committee.threshold",
          badCommittee(10_000.0, 3.0, 2.0) -> "committee.threshold",
          badCommittee(10_000.0, 1.0, 0.0) -> "committee.threshold",
          badCommittee(10.5, 1.0, 1.0) -> "committee.members.expiresAfter"
        )
        for (bad, name) <- cases do
            val thrown = intercept[js.JavaScriptException](create("committee" -> bad))
            assert(thrown.exception.isInstanceOf[js.TypeError], thrown)
            val message = thrown.exception.asInstanceOf[js.Error].message
            assert(message.contains(name), message)
    }

    test("spec [ES-9]: a proposal and a committee vote on it are read back") {
        val e = create(
          "committee" -> committee,
          "committeeHotKeys" -> js.Array(hotKey(Charles.addrKeyHash))
        )
        val proposalTx = TxBuilder(testEnv)
            .addSteps(SubmitProposal(proposal(GovAction.InfoAction)))
            .complete(utxo(0), alice)
            .sign(Alice.signer)
            .transaction
        assert(e.submitTx(bytes(proposalTx)).isSuccess)
        val id = GovActionId(proposalTx.id, 0)
        val voter = Voter.ConstitutionalCommitteeHotKey(Bob.addrKeyHash)
        val voteTx = TxBuilder(testEnv)
            .addSteps(SubmitVotingProcedure(voter, Map(id -> VotingProcedure(Vote.Yes, None))))
            .complete(utxo(1), alice)
            .sign(Alice.signer)
            .sign(Bob.signer)
            .transaction
        val result = e.submitTx(bytes(voteTx))
        assert(result.isSuccess, result.error.toOption)

        val proposals = e.getGovState().proposals
        assert(proposals.length == 1)
        val p = proposals(0)
        assert(p.txHash == proposalTx.id.toHex)
        assert(p.index == 0.0)
        assert(p.actionType == "InfoAction")
        assert(p.expiresAfter > p.proposedIn)
        assert(p.votes.length == 1)
        val v = p.votes(0)
        assert(
          (v.voterRole, v.credentialType, v.credentialHash, v.vote) ==
              ("committee", "key", Bob.addrKeyHash.toHex, "Yes")
        )
    }

    test("spec [ES-9]: DRep, script DRep and pool votes on a NoConfidence proposal are read back") {
        // The other `voterRole`, `credentialType` and `actionType` literals of `getGovState`. The
        // script DRep is a native script that needs no signer, so the vote tx carries only the
        // script; a committee voter cannot vote on NoConfidence, spec [GV-11].
        val drepScript = Script.Native(Timelock.AllOf(IndexedSeq.empty))
        val e = create(
          "drepRegistrations" -> js.Array(
            drepRegistration("key", Alice.addrKeyHash.toHex),
            drepRegistration("script", drepScript.scriptHash.toHex)
          ),
          "poolRegistrations" -> js.Array(js.Dynamic.literal(poolId = Dave.addrKeyHash.toHex))
        )
        val proposalTx = TxBuilder(testEnv)
            .addSteps(SubmitProposal(proposal(GovAction.NoConfidence(None))))
            .complete(utxo(0), alice)
            .sign(Alice.signer)
            .transaction
        assert(e.submitTx(bytes(proposalTx)).isSuccess)
        val id = GovActionId(proposalTx.id, 0)
        def votes(vote: Vote) = Map(id -> VotingProcedure(vote, None))
        val voteTx = TxBuilder(testEnv)
            .addSteps(
              SubmitVotingProcedure(Voter.DRepKey(Alice.addrKeyHash), votes(Vote.Yes)),
              SubmitVotingProcedure(
                Voter.DRepScript(drepScript.scriptHash),
                votes(Vote.No),
                NativeScriptWitness.attached(drepScript)
              ),
              SubmitVotingProcedure(Voter.StakingPoolKey(Dave.addrKeyHash), votes(Vote.Abstain))
            )
            .complete(utxo(1), alice)
            .sign(Alice.signer)
            .sign(Dave.signer)
            .transaction
        val result = e.submitTx(bytes(voteTx))
        assert(result.isSuccess, result.error.toOption)

        val proposals = e.getGovState().proposals
        assert(proposals.length == 1)
        assert(proposals(0).actionType == "NoConfidence")
        val got = proposals(0).votes
            .map(v => (v.voterRole, v.credentialType, v.credentialHash, v.vote))
            .toSet
        assert(
          got == Set(
            ("drep", "key", Alice.addrKeyHash.toHex, "Yes"),
            ("drep", "script", drepScript.scriptHash.toHex, "No"),
            ("pool", "key", Dave.addrKeyHash.toHex, "Abstain")
          )
        )
    }

    test("spec [ES-9b]: getPendingRatify marks an InfoAction expired after its last epoch") {
        // spec [RT-10]: the boundary after expiresAfter marks it; the next one removes it
        val e = create()
        val genesis = e.getPendingRatify()
        assert(genesis.enacted.length == 0 && genesis.expired.length == 0 && !genesis.delayed)
        val proposalTx = TxBuilder(testEnv)
            .addSteps(SubmitProposal(proposal(GovAction.InfoAction)))
            .complete(utxo(0), alice)
            .sign(Alice.signer)
            .transaction
        assert(e.submitTx(bytes(proposalTx)).isSuccess)
        val expiresAfter = e.getGovState().proposals(0).expiresAfter.toLong
        e.setSlot(testEnv.slotConfig.firstSlotOfEpoch(expiresAfter + 1).toDouble)
        val ratified = e.getPendingRatify()
        assert(ratified.enacted.length == 0)
        assert(
          ratified.expired.toSeq.map(r => (r.txHash, r.index)) == Seq((proposalTx.id.toHex, 0.0))
        )
        assert(!ratified.delayed)
        assert(e.getGovState().proposals.length == 1)
    }
}
