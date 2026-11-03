package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party

/** Ports of the Conway Imp `RatifySpec` (eras/conway/impl/testlib/Test/Cardano/Ledger/Conway/Imp/
  * RatifySpec.hs) to the Emulator, with the numbers and epochs the Imp tests assert, ledger-rules
  * spec section 16, phase 5 acceptance 4 to 8. Imp's `isCommitteeAccepted` and `isDRepAccepted` are
  * [[GovImp.isCommitteeAccepted]] and [[GovImp.isDRepAccepted]].
  */
class EmulatorRatifySpecTest extends AnyFunSuite {
    import GovImp.*

    test("Delaying actions: a delaying action delays all other actions") {
        // RatifySpec.hs:1641-1681, "... even when all of them may be ratified in the same epoch";
        // spec [RT-1], [RT-5c], [RT-5f]: each NewConstitution delays the ParameterChange chain by
        // one boundary, then the three ParameterChange actions are enacted in one run
        val imp = GovImp(dRepStakes = Seq(1_000_000), hotKeys = true)
        import imp.*
        def dRepDeposit(lovelace: Long) = ProtocolParamUpdate(drepDeposit = Some(Coin(lovelace)))
        val pGai0 = parameterChange(None, dRepDeposit(1_000_000))
        val pGai1 = parameterChange(Some(pGai0), dRepDeposit(1_000_001))
        val pGai2 = parameterChange(Some(pGai1), dRepDeposit(1_000_002))
        val (cGai0, _) = constitution(None)
        val (cGai1, _) = constitution(Some(cGai0))
        Seq(cGai0, cGai1, pGai0, pGai1, pGai2).foreach { id =>
            yesDRep(0, id)
            yesCCs(id)
        }
        passNEpochs(2)
        assert(e.govState.roots.constitution == Some(cGai0))
        assert(e.govState.roots.pparamUpdate.isEmpty)
        passEpoch()
        // the ParameterChange actions have priority 4, NewConstitution 2: delayed again
        assert(e.govState.roots.constitution == Some(cGai1))
        assert(!e.govState.proposals.exists(_.action.isInstanceOf[GovAction.NewConstitution]))
        assert(e.govState.roots.pparamUpdate.isEmpty)
        passEpoch()
        assert(e.govState.roots.pparamUpdate == Some(pGai2))
        assert(!e.govState.proposals.exists(_.action.isInstanceOf[GovAction.ParameterChange]))
        assert(e.currentParams.dRepDeposit == 1_000_002L)
    }

    /** The common start of the two "discounted from quorum" tests (RatifySpec.hs:352-418): a
      * committee of `c1` and `c2` with the terms given is elected and both authorize a hot key;
      * then a constitution gets the Yes of `c1` only. Returns the constitution id.
      */
    private def twoMemberCommittee(imp: GovImp, c2Term: Long): GovActionId = {
        import imp.*
        val Seq(c1, c2, hot1, hot2) = fresh.take(4)
        val gaiCC =
            updateCommittee(None, Set.empty, Seq(c1 -> 10L, c2 -> c2Term), UnitInterval.half)
        yesDRep(0, gaiCC)
        yesPool(0, gaiCC)
        passNEpochs(2)
        assert(e.govState.roots.committee == Some(gaiCC))
        authorizeHotKey(c1, hot1)
        authorizeHotKey(c2, hot2)
        val (gaiConstitution, _) = constitution(None)
        yesCC(hot1, gaiConstitution)
        gaiConstitution
    }

    private def minSizeTwo(): GovImp = GovImp(
      adjust = _.copy(committeeMinSize = 2),
      dRepStakes = Seq(1_000_000),
      poolStakes = Seq(42_000_000)
    )

    private def expiry(imp: GovImp, cold: Party): EpochNo =
        imp.e.govState.committee.get.members(keyOf(cold))

    test("Expired and resigned committee members are discounted from quorum: Expired") {
        // RatifySpec.hs:352-383; spec [RT-6], [RT-6a], [RT-7]: once the second member expires, one
        // active member is below committeeMinSize 2
        val imp = minSizeTwo()
        import imp.*
        val gaiConstitution = twoMemberCommittee(imp, c2Term = 2)
        val c2 = fresh(1)
        assert(e.currentEpoch <= expiry(imp, c2))
        assert(isCommitteeAccepted(gaiConstitution))
        passNEpochs(2)
        assert(e.currentEpoch > expiry(imp, c2))
        assert(!isCommitteeAccepted(gaiConstitution))
    }

    test("Expired and resigned committee members are discounted from quorum: Resigned") {
        // RatifySpec.hs:384-418; spec [RT-6a], [RT-7]
        val imp = minSizeTwo()
        import imp.*
        val gaiConstitution = twoMemberCommittee(imp, c2Term = 10)
        val c2 = fresh(1)
        assert(!authorization(c2).exists(_.isInstanceOf[CommitteeAuthorization.Resigned]))
        assert(isCommitteeAccepted(gaiConstitution))
        resign(c2)
        assert(authorization(c2) == Some(CommitteeAuthorization.Resigned(None)))
        assert(!isCommitteeAccepted(gaiConstitution))
    }

    test(
      "Active voting stake: DRep: Proposal deposits contribute to active voting stake: Directly"
    ) {
        // RatifySpec.hs:858-896; spec [RT-3], [RT-3a]: DRep 0 has no UTxO stake; the deposit of a
        // proposal returned to its delegator counts as its stake, 1 of 2 ADA, then 2 of 3 ADA
        val imp = GovImp(
          adjust = _.copy(govActionDeposit = 1_000_000),
          dRepStakes = Seq(0, 1_000_000),
          poolStakes = Seq(42_000_000)
        )
        import imp.*
        val delegator = dRepDelegators(0)
        val Seq(cc, cc2) = fresh.take(2)
        val curEpochNo = e.currentEpoch
        def addMember(member: Party): GovAction = GovAction.UpdateCommittee(
          None,
          Set.empty,
          Map(keyOf(member) -> (curEpochNo + 10)),
          UnitInterval(75, 100)
        )
        val addCCGaid = propose(addMember(cc), returnTo = delegator)
        yesDRep(0, addCCGaid)
        yesPool(0, addCCGaid)
        passNEpochs(2)
        assert(!isDRepAccepted(addCCGaid))
        assert(e.govState.roots.committee.isEmpty)
        propose(addMember(cc2), returnTo = delegator)
        passNEpochs(2)
        assert(e.govState.roots.committee == Some(addCCGaid))
    }

    test("Active voting stake: DRep: Rewards contribute to active voting stake") {
        // RatifySpec.hs:789-813; spec [RT-3]: a reward balance of 857_142_858 brings DRep 0 to
        // 1_857_142_858 of 2_857_142_858, just over 65 %. Imp's pool has no stake beyond the
        // min-UTxO fixup of its delegator's output; here that output holds 1 ADA.
        val imp = GovImp(
          dRepStakes = Seq(1_000_000_000, 1_000_000_000),
          poolStakes = Seq(1_000_000)
        )
        import imp.*
        val addCCGaid =
            updateCommittee(None, Set.empty, Seq(fresh.head -> 10L), UnitInterval(75, 100))
        yesDRep(0, addCCGaid)
        yesPool(0, addCCGaid)
        passNEpochs(2)
        assert(!isDRepAccepted(addCCGaid))
        assert(e.govState.roots.committee.isEmpty)
        e.addRewards(stakeCredential(dRepDelegators(0)), Coin(857_142_858))
        passNEpochs(2)
        assert(e.govState.roots.committee == Some(addCCGaid))
    }

    test(
      "SPO default votes: After bootstrap phase: Staking address delegated to AlwaysNoConfidence"
    ) {
        // RatifySpec.hs:1530-1560; spec [RT-8], [RT-14], [RT-14a]: a pool with no vote counts as
        // No until its reward account delegates to AlwaysNoConfidence, then as Yes on a
        // NoConfidence. Imp's `calculatePoolAcceptedRatio` (0, then 1) is read here as whether the
        // action is enacted.
        val imp = GovImp(dRepStakes = Seq(1_000_000), poolStakes = Seq(1_000_000))
        import imp.*
        val gid = propose(GovAction.NoConfidence(None))
        yesDRep(0, gid)
        passNEpochs(2)
        assert(committeeMembers.nonEmpty)
        // delegateSPORewardAddressToDRep_ (ImpTest.hs:1897-1909)
        val operator = poolOperators(0)
        submit(
          TxBuilder(env)
              .payTo(baseAddress(operator), Value.lovelace(1_000_000))
              .delegateVoteToDRep(stakeAddress(operator), DRep.AlwaysNoConfidence),
          operator
        )
        passNEpochs(2)
        assert(committeeMembers.isEmpty)
    }

    test(
      "CommitteeMinSize affects in-flight proposals: TreasuryWithdrawal fails to ratify due to an increase in CommitteeMinSize"
    ) {
        // RatifySpec.hs:606-632; spec [RT-1], [RT-6], [RT-17]: the ParameterChange has priority over
        // the TreasuryWithdrawals and raises committeeMinSize to 3 in the same run. Imp sets
        // committeeMinSize 2 after its first passEpoch; no proposal exists before it, and Imp's
        // modifyPParams patches the ratify state too (ImpTest.hs:250-256), so it is set at genesis
        // here. Imp draws the amount from 1 to 100 ADA.
        val recipient = fresh.head
        val imp = GovImp(
          adjust = p => noTreasuryExpansion(p).copy(committeeMinSize = 2),
          dRepStakes = Seq(1_000_000),
          hotKeys = true,
          accounts = Seq(recipient)
        )
        import imp.*
        val amount = Coin(50_000_000)
        submit(TxBuilder(env).donateToTreasury(amount).setCurrentTreasuryValue(e.treasury))
        passEpoch()
        val gaiTW = propose(
          GovAction.TreasuryWithdrawals(Map(RewardAccount(stakeAddress(recipient)) -> amount), None)
        )
        yesCCs(gaiTW)
        yesDRep(0, gaiTW)
        assert(isCommitteeAccepted(gaiTW))
        val gaiPC = parameterChange(None, ProtocolParamUpdate(minCommitteeSize = Some(3)))
        yesCCs(gaiPC)
        yesDRep(0, gaiPC)
        val treasury = e.treasury
        passNEpochs(2)
        assert(e.govState.roots.pparamUpdate == Some(gaiPC))
        assert(!isCommitteeAccepted(gaiTW))
        assert(e.govState.proposal(gaiTW).isDefined)
        assert(e.treasury == treasury)
    }
}
