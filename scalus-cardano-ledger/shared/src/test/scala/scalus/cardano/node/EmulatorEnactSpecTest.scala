package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.internal.Ratify
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party
import scalus.testing.kit.Party.*

/** Ports of the Conway Imp `EnactSpec` (eras/conway/impl/testlib/Test/Cardano/Ledger/Conway/Imp/
  * EnactSpec.hs) to the Emulator, with the numbers and epochs the Imp tests assert, ledger-rules
  * spec section 16, phase 5 acceptance 1 to 3. A proposal voted in epoch `n` is ratified at
  * boundary `n + 1` and enacted at boundary `n + 2`, as `passNEpochs 2` does in Imp.
  */
class EmulatorEnactSpecTest extends AnyFunSuite {
    import GovImp.*

    test("Treasury withdrawals: Modify EnactState as expected") {
        // EnactSpec.hs:66-105; spec [EN-6], [RT-5d], [EB-11a]. Imp runs ENACT twice on an enact
        // state with a treasury of 1000; here both proposals are enacted in one RATIFY run, so the
        // withdrawals after the first ENACT (666 alone) are not observable: the run gives 777 and
        // 222, and leaves 1 in the treasury.
        val imp = GovImp(
          adjust = noTreasuryExpansion,
          dRepStakes = Seq(1_000_000),
          hotKeys = true,
          treasury = Coin(1000),
          accounts = Seq(Peggy, Sybil)
        )
        import imp.*
        val first = propose(withdrawals(Peggy -> 666))
        val second = propose(withdrawals(Peggy -> 111, Sybil -> 222))
        Seq(first, second).foreach { id =>
            yesDRep(0, id)
            yesCCs(id)
        }
        passEpoch()
        assert(e.pendingRatify.enacted == Vector(first, second))
        assert(
          e.pendingRatify.enactState.withdrawals ==
              Map(stakeCredential(Peggy) -> Coin(777), stakeCredential(Sybil) -> Coin(222))
        )
        passEpoch()
        assert(balance(Peggy) == Coin(777))
        assert(balance(Sybil) == Coin(222))
        assert(e.treasury == Coin(1))
    }

    test("Treasury withdrawals: Withdrawals exceeding treasury submitted in several proposals") {
        // EnactSpec.hs:139-173; spec [RT-1], [RT-5d], [RT-17]. Imp draws the withdrawals at random
        // so that they sum to more than the treasury; these are fixed: 3, 2.5, 1.5 and 0.6 ADA
        // against 5 ADA. In order of submission, each that fits the rest is paid.
        val accounts = Seq(Peggy, Sybil, Trent, Victor)
        val imp = GovImp(
          adjust = noTreasuryExpansion,
          dRepStakes = Seq(1_000_000),
          hotKeys = true,
          accounts = accounts
        )
        import imp.*
        // donateToTreasury (ImpTest.hs:1765-1775): the donation moves in at the next boundary
        submit(
          TxBuilder(env).donateToTreasury(Coin(5_000_000)).setCurrentTreasuryValue(e.treasury)
        )
        val start = e.treasury
        passEpoch()
        assert(e.treasury - start == Coin(5_000_000))
        val initialTreasury = e.treasury
        val amounts = Seq(3_000_000L, 2_500_000L, 1_500_000L, 600_000L).map(Coin(_))
        assert(amounts.foldLeft(Coin.zero)(_ + _) > initialTreasury)
        accounts.zip(amounts).foreach { (account, amount) =>
            val id = propose(withdrawals(account -> amount.value))
            yesDRep(0, id)
            yesCCs(id)
        }
        passNEpochs(2)
        val expectedTreasury = amounts.foldLeft(initialTreasury) { (left, amount) =>
            if left >= amount then left - amount else left
        }
        assert(expectedTreasury == Coin(500_000))
        assert(e.treasury == expectedTreasury)
        assert(
          accounts.map(balance).foldLeft(Coin.zero)(_ + _) == initialTreasury - expectedTreasury
        )
    }

    test("Competing proposals: higher action priority wins") {
        // EnactSpec.hs:408-442; spec [RT-1], [RT-5c], [EB-12c]: the first NoConfidence is enacted
        // first and delays the rest; the other two lose their parent and go
        val imp = GovImp(dRepStakes = Seq(1_000_000), poolStakes = Seq(1_000_000))
        import imp.*
        val cc = fresh.head
        val gai1 = updateCommittee(None, Set.empty, Seq(cc -> 30L), UnitInterval(1, 3))
        val gai2 = propose(GovAction.NoConfidence(None))
        val gai3 = propose(GovAction.NoConfidence(None))
        Seq(gai1, gai2, gai3).foreach { id =>
            yesDRep(0, id)
            yesPool(0, id)
        }
        passNEpochs(2)
        assert(e.govState.roots.committee == Some(gai2))
        assert(e.govState.proposals.isEmpty)
        assert(e.govState.committee.isEmpty)
    }

    test("Competing proposals: proposals of same priority are enacted in order of submission") {
        // EnactSpec.hs:444-474; spec [RT-1], [RT-17]: a chain of three ParameterChange actions is
        // enacted in one run. Imp draws the fees from 30-330, 330-660 and 660-1000.
        val imp = GovImp(
          adjust = p =>
              p.copy(
                poolVotingThresholds =
                    p.poolVotingThresholds.copy(ppSecurityGroup = UnitInterval.one),
                dRepVotingThresholds =
                    p.dRepVotingThresholds.copy(ppEconomicGroup = UnitInterval.zero)
              ),
          poolStakes = Seq(42_000_000),
          hotKeys = true
        )
        import imp.*
        val (val1, val2, val3) = (100L, 400L, 800L)
        val p0 = parameterChange(None, fee(val1))
        val p1 = parameterChange(Some(p0), fee(val2))
        val p2 = parameterChange(Some(p1), fee(val3))
        Seq(p0, p1, p2).foreach { id =>
            yesPool(0, id)
            yesCCs(id)
        }
        passNEpochs(2)
        assert(e.govState.roots.pparamUpdate == Some(p2))
        assert(e.govState.proposals.isEmpty)
        assert(e.currentParams.txFeePerByte == val3)
    }

    test("Committee enactment: CC re-election") {
        // EnactSpec.hs:555-601; spec [EN-5], [EB-16]: a member that resigned stays resigned when it
        // is elected again, and is free of it only after it is removed, since [EB-16] then drops
        // its committee state
        val imp = GovImp(dRepStakes = Seq(1_000_000), poolStakes = Seq(42_000_000))
        import imp.*
        val Seq(cc, hot1, hot2) = fresh.take(3)
        def yes(id: GovActionId): Unit = {
            yesDRep(0, id)
            yesPool(0, id)
        }
        passNEpochs(2)
        val addCC = updateCommittee(None, Set.empty, Seq(cc -> 10L), UnitInterval.half)
        yes(addCC)
        passNEpochs(2)
        assert(committeeMembers.contains(keyOf(cc)))
        authorizeHotKey(cc, hot1)
        assert(authorization(cc) == Some(CommitteeAuthorization.HotCredential(keyOf(hot1))))
        resign(cc)
        assert(authorization(cc) == Some(CommitteeAuthorization.Resigned(None)))

        val reAdd = updateCommittee(None, Set.empty, Seq(cc -> 20L), UnitInterval.half)
        yes(reAdd)
        passNEpochs(2)
        assert(e.govState.roots.committee == Some(reAdd))
        assert(authorization(cc) == Some(CommitteeAuthorization.Resigned(None)))

        val remove = updateCommittee(Some(Some(reAdd)), Set(cc), Seq.empty, UnitInterval.half)
        yes(remove)
        passNEpochs(2)
        assert(!committeeMembers.contains(keyOf(cc)))

        val secondAdd = updateCommittee(None, Set.empty, Seq(cc -> 20L), UnitInterval.half)
        yes(secondAdd)
        passNEpochs(2)
        assert(committeeMembers.contains(keyOf(cc)))
        authorizeHotKey(cc, hot2)
        assert(authorization(cc) == Some(CommitteeAuthorization.HotCredential(keyOf(hot2))))
    }

    test("NoConfidence") {
        // EnactSpec.hs:295-337; spec [EN-1], [EN-5], [EB-13a]. Imp sets up the pool after the
        // election proposal; here it is seeded at genesis, which no assertion reads.
        val imp = GovImp(
          adjust = p =>
              p.copy(
                dRepVotingThresholds =
                    p.dRepVotingThresholds.copy(committeeNoConfidence = UnitInterval.half),
                poolVotingThresholds =
                    p.poolVotingThresholds.copy(committeeNoConfidence = UnitInterval.half),
                committeeMaxTermLength = 200
              ),
          dRepStakes = Seq(1_000_000),
          poolStakes = Seq(42_000_000)
        )
        import imp.*
        val initialMembers = committeeMembers
        val committeeMap = Map(keyOf(fresh.head) -> (e.currentEpoch + 50))
        // submitCommitteeElection (ImpTest.hs:1335-1355)
        val election = propose(
          GovAction.UpdateCommittee(None, initialMembers, committeeMap, UnitInterval.half)
        )
        yesDRep(0, election)
        yesPool(0, election)
        passNEpochs(4)
        assert(e.govState.committee == Some(Committee(committeeMap, UnitInterval.half)))
        val noConfidence = propose(GovAction.NoConfidence(Some(election)))
        yesPool(0, noConfidence)
        yesDRep(0, noConfidence)
        passNEpochs(4)
        assert(e.govState.committee.isEmpty)
    }

    test("Constitution") {
        // EnactSpec.hs:339-405; spec [EB-20], [EB-13a], [DM-9b]: votes change no ratify result
        // until the next boundary; the result of boundary 1 holds the new constitution, boundary
        // 2 enacts it, and its own RATIFY starts from the enact state of the gov state
        val imp = GovImp(dRepStakes = Seq(1_000_000), hotKeys = true)
        import imp.*
        val initialConstitution = e.govState.constitution
        val (id, constitution) = imp.constitution(None)
        val pendingBeforeVotes = e.pendingRatify
        yesDRep(0, id)
        yesCC(Eve, id)
        yesCC(Faith, id)
        val voted = e.govState.proposal(id).get
        assert(voted.drepVotes == Map(Credential.KeyHash(dRepParties(0).addrKeyHash) -> Vote.Yes))
        assert(voted.committeeVotes == Map(keyOf(Eve) -> Vote.Yes, keyOf(Faith) -> Vote.Yes))
        assert(e.pendingRatify == pendingBeforeVotes)

        passEpoch()
        assert(e.govState.constitution == initialConstitution)
        assert(e.pendingRatify.enacted == Vector(id))
        assert(e.pendingRatify.enactState.constitution == constitution)

        passEpoch()
        assert(e.govState.constitution == constitution)
        assert(e.pendingRatify.enacted.isEmpty)
        assert(e.pendingRatify.enactState == Ratify.enactStateOf(e.govState, e.currentParams))
    }

    test("futurePParams") {
        // EnactSpec.hs:273-293; spec [EN-1], [EB-13a], [EB-19a]: the ratify result of boundary 1
        // holds the next protocol version, boundary 2 makes it current. Imp forks to the next
        // major; at PV 11 that is 12, which [EN-4a] keeps out, so this runs from PV 10. Two of
        // three equal pools meet the threshold 2/3 exactly, spec [RT-18].
        val imp = GovImp(
          adjust = p =>
              p.copy(
                protocolVersion = ProtocolVersion(10, 0),
                poolVotingThresholds =
                    p.poolVotingThresholds.copy(hardForkInitiation = UnitInterval(2, 3)),
                dRepVotingThresholds =
                    p.dRepVotingThresholds.copy(hardForkInitiation = UnitInterval.zero)
              ),
          poolStakes = Seq(22_000_000, 22_000_000, 22_000_000),
          hotKeys = true
        )
        import imp.*
        val nextProtVer = ProtocolVersion(11, 0)
        val id = propose(GovAction.HardForkInitiation(None, nextProtVer))
        yesCCs(id)
        yesPool(1, id)
        yesPool(2, id)
        passEpoch()
        assert(e.pendingRatify.enactState.params.protocolVersion == nextProtVer)
        assert(e.currentParams.protocolVersion == ProtocolVersion(10, 0))
        passEpoch()
        assert(e.currentParams.protocolVersion == nextProtVer)
    }

    private def withdrawals(amounts: (Party, Long)*): GovAction =
        GovAction.TreasuryWithdrawals(
          amounts.map((p, amount) => RewardAccount(stakeAddress(p)) -> Coin(amount)).toMap,
          None
        )

    private def fee(perByte: Long): ProtocolParamUpdate =
        ProtocolParamUpdate(minFeeA = Some(Coin(perByte)))
}
