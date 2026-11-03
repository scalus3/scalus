package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

import scala.collection.immutable.SortedMap

/** DRep expiry and `numDormantEpochs` in the CERTS step, spec [DR-1] to [DR-4a], [DR-1a]. The
  * epochs and the `dRepActivity = 20` follow the example of spec section 10.1.
  */
class CertsMutatorDRepExpiryTest extends AnyFunSuite with Matchers with EitherValues {

    private val slotConfig =
        SlotConfig(zeroTime = 0, zeroSlot = 0, slotLength = 1000, epochLength = 100, zeroEpoch = 0)
    private val baseParams = CardanoInfo.preprod.protocolParams.copy(dRepActivity = 20)
    private val drepDeposit = Coin(baseParams.dRepDeposit)

    private val drepA = Credential.KeyHash(AddrKeyHash.fromHex("a" * 56))
    private val drepB = Credential.KeyHash(AddrKeyHash.fromHex("b" * 56))
    private val unregistered = Credential.KeyHash(AddrKeyHash.fromHex("c" * 56))
    private val scriptDRep = Credential.ScriptHash(ScriptHash.fromHex("9" * 56))
    private val poolVoter = Voter.StakingPoolKey(AddrKeyHash.fromHex("d" * 56))

    private val proposal = ProposalProcedure(
      deposit = Coin(baseParams.govActionDeposit),
      rewardAccount = RewardAccount(
        StakeAddress(
          Network.Testnet,
          StakePayload.Stake(StakeKeyHash.fromHex("e" * 56))
        )
      ),
      govAction = GovAction.InfoAction,
      anchor = Anchor("https://example.com", DataHash.fromHex("0" * 64))
    )

    private def voter(credential: Credential): Voter = credential match
        case Credential.KeyHash(hash)    => Voter.DRepKey(hash)
        case Credential.ScriptHash(hash) => Voter.DRepScript(hash)

    private def run(
        state: State,
        epoch: Long,
        certs: Seq[Certificate] = Seq.empty,
        proposals: Seq[ProposalProcedure] = Seq.empty,
        voters: Seq[Voter] = Seq.empty,
        protocolMajor: Int = 11
    ): State = {
        val params = baseParams.copy(protocolVersion = ProtocolVersion(protocolMajor, 0))
        val context = new Context(
          env = UtxoEnv(
            slot = slotConfig.firstSlotOfEpoch(epoch),
            params = params,
            certState = state.certState,
            network = Network.Testnet,
            treasury = Coin.zero
          ),
          slotConfig = slotConfig
        )
        val govActionId = GovActionId(TransactionHash.fromHex("f" * 64), 0)
        val votes =
            if voters.isEmpty then None
            else
                Some(
                  VotingProcedures(
                    SortedMap.from(
                      voters.map(v =>
                          v -> SortedMap(govActionId -> VotingProcedure(Vote.Yes, None))
                      )
                    )
                  )
                )
        val tx = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            certificates =
                if certs.isEmpty then TaggedOrderedStrictSet.empty
                else TaggedOrderedStrictSet.from(certs),
            votingProcedures = votes,
            proposalProcedures =
                if proposals.isEmpty then TaggedOrderedSet.empty
                else TaggedOrderedSet.from(proposals)
          )
        )
        CertsMutator.transit(context, state, tx).value
    }

    /** A state with `dreps` registered at the given expiries and the given dormant count. */
    private def withDReps(dormant: Long, dreps: (Credential, Long)*): State =
        State(certState =
            CertState(
              vstate = VotingState(
                dreps.map((c, e) => c -> DRepState(e, None, drepDeposit, Set.empty)).toMap,
                dormant
              ),
              pstate = PoolsState.empty,
              dstate = DelegationState(Map.empty)
            )
        )

    private def expiry(state: State, credential: Credential): Long =
        state.certState.vstate.dreps(credential).expiry

    test("spec [DR-1]: register at epoch 100 with no dormant epochs gives 120") {
        val after = run(withDReps(0), 100, Seq(Certificate.RegDRepCert(drepA, drepDeposit, None)))
        expiry(after, drepA) shouldBe 120
    }

    test("spec [DR-1]: register subtracts the dormant epochs") {
        val after = run(withDReps(3), 103, Seq(Certificate.RegDRepCert(drepA, drepDeposit, None)))
        expiry(after, drepA) shouldBe 120
        after.certState.vstate.numDormantEpochs shouldBe 3
    }

    test("spec [DR-1a]: at PV 9 a registration does not subtract the dormant epochs") {
        val after = run(
          withDReps(3),
          103,
          Seq(Certificate.RegDRepCert(drepA, drepDeposit, None)),
          protocolMajor = 9
        )
        expiry(after, drepA) shouldBe 123
    }

    test("spec [DR-1a]: an update subtracts the dormant epochs at every PV") {
        for pv <- Seq(9, 10, 11) do
            val after = run(
              withDReps(3, drepA -> 50),
              103,
              Seq(Certificate.UpdateDRepCert(drepA, None)),
              protocolMajor = pv
            )
            withClue(s"PV $pv: ")(expiry(after, drepA) shouldBe 120)
    }

    test("spec [DR-4]: a vote at epoch 103 with 3 dormant epochs sets 103 + 20 - 3 at every PV") {
        // computeDRepExpiry of C/Rules/Certs.hs:277-298 has no PV gate, unlike [DR-1a]
        for pv <- Seq(9, 10, 11) do
            val after = run(
              withDReps(3, drepA -> 110, drepB -> 110),
              103,
              voters = Seq(voter(drepA)),
              protocolMajor = pv
            )
            withClue(s"PV $pv: ") {
                expiry(after, drepA) shouldBe 120
                expiry(after, drepB) shouldBe 110
                after.certState.vstate.numDormantEpochs shouldBe 3
            }
    }

    test("spec [DR-4]: a vote by a script DRep sets its expiry") {
        val after = run(
          withDReps(3, scriptDRep -> 110, drepA -> 110),
          103,
          voters = Seq(voter(scriptDRep))
        )
        expiry(after, scriptDRep) shouldBe 120
        expiry(after, drepA) shouldBe 110
    }

    test("spec [DR-4]: an unregistered DRep voter and other voters change no DRep") {
        val before = withDReps(3, drepA -> 110)
        val after = run(before, 103, voters = Seq(voter(unregistered), poolVoter))
        after.certState.vstate shouldBe before.certState.vstate
    }

    test("spec [DR-2], [DR-3]: a tx with a proposal at epoch 104 adds 3 and resets the counter") {
        val after = run(withDReps(3, drepA -> 120, drepB -> 100), 104, proposals = Seq(proposal))
        expiry(after, drepA) shouldBe 123
        // 100 + 3 < 104: an expired DRep keeps its expiry (C/Rules/Certs.hs:327-333)
        expiry(after, drepB) shouldBe 100
        after.certState.vstate.numDormantEpochs shouldBe 0
    }

    test("spec [DR-3]: the bump keeps a DRep whose expiry plus the dormant epochs is the epoch") {
        val after = run(withDReps(3, drepA -> 101), 104, proposals = Seq(proposal))
        expiry(after, drepA) shouldBe 104
    }

    test("spec [DR-2]: a tx with no proposal does not bump") {
        val after = run(withDReps(3, drepA -> 120), 104)
        expiry(after, drepA) shouldBe 120
        after.certState.vstate.numDormantEpochs shouldBe 3
    }

    test("spec [DR-4]: with a proposal and a vote, the vote uses the counter after the bump") {
        val after = run(
          withDReps(3, drepA -> 110, drepB -> 120),
          104,
          proposals = Seq(proposal),
          voters = Seq(voter(drepA))
        )
        expiry(after, drepA) shouldBe 124
        expiry(after, drepB) shouldBe 123
    }

    test("spec [DR-4a]: the bumps run before the cert loop") {
        // the registration in the same tx reads the counter after the bump, 0
        val after = run(
          withDReps(3, drepB -> 120),
          104,
          certs = Seq(Certificate.RegDRepCert(drepA, drepDeposit, None)),
          proposals = Seq(proposal)
        )
        expiry(after, drepA) shouldBe 124
        expiry(after, drepB) shouldBe 123
    }

    test("spec [DR-4a]: at PV 9 the registration after the bump is not bumped again") {
        // At PV 11 the subtraction and the bump cancel out, so only PV 9, where the registration
        // ignores the counter ([DR-1a]), tells bump-then-loop (124) from loop-then-bump (127)
        val after = run(
          withDReps(3, drepB -> 120),
          104,
          certs = Seq(Certificate.RegDRepCert(drepA, drepDeposit, None)),
          proposals = Seq(proposal),
          protocolMajor = 9
        )
        expiry(after, drepA) shouldBe 124
        expiry(after, drepB) shouldBe 123
    }
}
