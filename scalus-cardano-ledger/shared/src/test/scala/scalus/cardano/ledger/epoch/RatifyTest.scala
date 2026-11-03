package scalus.cardano.ledger.epoch

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.GovFixtures.*
import scalus.cardano.ledger.epoch.internal.{PParamGroup, PoolDistribution, Ratify, RatifyEnv}
import scalus.cardano.ledger.rules.State
import scalus.uplc.builtin.ByteString

/** RATIFY, ledger-rules spec section 15 (conway Rules/Ratify.hs:118-379, Governance/Internal.hs
  * 319-552, Governance/DRepPulser.hs:208-249).
  */
class RatifyTest extends AnyFunSuite {

    // The pparam groups, spec [EN-2a] field table, D90 (conway PParams.hs:643-708)

    private val thresholds = CardanoInfo.mainnet.protocolParams

    test("each field of a ParameterChange is in the group of the [EN-2a] table") {
        import PParamGroup.*
        val none = ProtocolParamUpdate()
        val c = Coin(1)
        // key, the update, the DRep group, whether it is in the security group
        val rows = Seq(
          (0, none.copy(minFeeA = Some(c)), Economic, true),
          (1, none.copy(minFeeB = Some(c)), Economic, true),
          (2, none.copy(maxBlockBodySize = Some(1)), PParamGroup.Network, true),
          (3, none.copy(maxTxSize = Some(1)), PParamGroup.Network, true),
          (4, none.copy(maxBlockHeaderSize = Some(1)), PParamGroup.Network, true),
          (5, none.copy(keyDeposit = Some(c)), Economic, false),
          (6, none.copy(poolDeposit = Some(c)), Economic, false),
          (7, none.copy(maxEpoch = Some(1)), Technical, false),
          (8, none.copy(nOpt = Some(1)), Technical, false),
          (9, none.copy(poolPledgeInfluence = Some(NonNegativeInterval(1, 2))), Technical, false),
          (10, none.copy(expansionRate = Some(UnitInterval(1, 2))), Economic, false),
          (11, none.copy(treasuryGrowthRate = Some(UnitInterval(1, 2))), Economic, false),
          (16, none.copy(minPoolCost = Some(c)), Economic, false),
          (17, none.copy(adaPerUtxoByte = Some(c)), Economic, true),
          (18, none.copy(costModels = Some(CostModels(Map.empty))), Technical, false),
          (19, none.copy(executionCosts = Some(thresholds.executionUnitPrices)), Economic, false),
          (20, none.copy(maxTxExUnits = Some(ExUnits(1, 1))), PParamGroup.Network, false),
          (21, none.copy(maxBlockExUnits = Some(ExUnits(1, 1))), PParamGroup.Network, true),
          (22, none.copy(maxValueSize = Some(1)), PParamGroup.Network, true),
          (23, none.copy(collateralPercentage = Some(1)), Technical, false),
          (24, none.copy(maxCollateralInputs = Some(1)), PParamGroup.Network, false),
          (25, none.copy(poolVotingThresholds = Some(thresholds.poolVotingThresholds)), Gov, false),
          (26, none.copy(drepVotingThresholds = Some(thresholds.dRepVotingThresholds)), Gov, false),
          (27, none.copy(minCommitteeSize = Some(1)), Gov, false),
          (28, none.copy(committeeTermLimit = Some(1)), Gov, false),
          (29, none.copy(governanceActionValidityPeriod = Some(1)), Gov, false),
          (30, none.copy(governanceActionDeposit = Some(c)), Gov, true),
          (31, none.copy(drepDeposit = Some(c)), Gov, false),
          (32, none.copy(drepInactivityPeriod = Some(1)), Gov, false),
          (
            33,
            none.copy(minFeeRefScriptCoinsPerByte = Some(NonNegativeInterval(1, 1))),
            Economic,
            true
          )
        )
        for (key, update, group, security) <- rows do
            withClue(s"key $key: ") {
                assert(Ratify.modifiedGroups(update) == Set(group))
                assert(Ratify.changesSecurityGroup(update) == security)
            }
    }

    test("an update of fields of several groups is in each of them") {
        val update = ProtocolParamUpdate(maxTxSize = Some(1), drepDeposit = Some(Coin(1)))
        assert(Ratify.modifiedGroups(update) == Set(PParamGroup.Network, PParamGroup.Gov))
        assert(Ratify.changesSecurityGroup(update))
        assert(Ratify.modifiedGroups(ProtocolParamUpdate()) == Set.empty)
    }

    // Fixtures

    private def hash28(i: Int): String = f"$i%02x" * 28

    private def poolId(i: Int): PoolKeyHash = PoolKeyHash.fromHex(hash28(i))

    private def dRep(i: Int): DRep = DRep.KeyHash(AddrKeyHash.fromHex(hash28(i)))

    private def dRepState(expiry: EpochNo): DRepState =
        DRepState(expiry, None, Coin(500), Set.empty)

    private val anchor = Anchor("https://example.com", DataHash.fromHex("0" * 64))

    /** A proposal `i` of `action` with `deposit` returned to `returnTo`. */
    private def proposal(
        i: Int,
        action: GovAction,
        deposit: Long = 100,
        returnTo: Int = 1,
        expiresAfter: EpochNo = 20
    ): GovActionState =
        GovActionState(
          actionId(i),
          Map.empty,
          Map.empty,
          Map.empty,
          ProposalProcedure(Coin(deposit), rewardAccount(returnTo), action, anchor),
          proposedIn = expiresAfter - 6,
          expiresAfter = expiresAfter
        )

    private def account(
        balance: Long,
        pool: Option[PoolKeyHash] = None,
        drep: Option[DRep] = None
    ): ConwayAccountState = ConwayAccountState(Coin(balance), Coin(2), pool, drep)

    private val paymentKey = Credential.KeyHash(AddrKeyHash.fromHex("ee" * 28))

    /** A ledger with `accounts`, the registered `dReps`, `proposals` and one base-address UTxO of
      * each `(credential, lovelace)` of `utxos`.
      */
    private def ledger(
        accounts: Map[Credential, ConwayAccountState],
        dReps: Map[Credential, DRepState] = Map.empty,
        proposals: Vector[GovActionState] = Vector.empty,
        utxos: Seq[(Credential, Long)] = Seq.empty
    ): State = {
        val hash = TransactionHash.fromByteString(ByteString.fromHex("cc" * 32))
        val outputs: Utxos = utxos.zipWithIndex.map { case ((stake, lovelace), i) =>
            TransactionInput(hash, i) ->
                TransactionOutput(
                  Address(Network.Mainnet, paymentKey, stake),
                  Value.lovelace(lovelace)
                )
        }.toMap
        State(
          utxos = outputs,
          certState = CertState(
            vstate = VotingState(dReps, 0L, Map.empty),
            dstate = DelegationState(accounts)
          ),
          govState = GovState.empty.copy(proposals = proposals)
        )
    }

    private def keyCredential(d: DRep): Credential = d match
        case DRep.KeyHash(hash) => Credential.KeyHash(hash)
        case other              => fail(s"not a key DRep: $other")

    // The DRep distribution, spec [RT-3], [RT-3a] (computeDRepDistr, conway
    // Governance/DRepPulser.hs:208-249)

    test("a DRep's stake is the UTxO stake, the balance and the proposal deposits it delegates") {
        // acceptance 6: "Proposal deposits contribute to active voting stake.Directly" and
        // "Rewards contribute to active voting stake"; proposalsDeposits sums per return account
        val state = ledger(
          accounts = Map(
            credential(1) -> account(10, drep = Some(dRep(7))),
            credential(2) -> account(3, drep = Some(dRep(7)))
          ),
          dReps = Map(keyCredential(dRep(7)) -> dRepState(30)),
          proposals = Vector(
            proposal(1, GovAction.InfoAction, deposit = 100, returnTo = 1),
            proposal(2, GovAction.InfoAction, deposit = 40, returnTo = 1)
          ),
          utxos = Seq(credential(1) -> 1_000L, credential(1) -> 5L, credential(2) -> 20L)
        )
        assert(
          Ratify.dRepDistribution(state) == Map(dRep(7) -> Coin(1_000 + 5 + 10 + 140 + 20 + 3))
        )
    }

    test("a credential DRep counts only when registered, the predefined DReps always") {
        val state = ledger(
          accounts = Map(
            credential(1) -> account(10, drep = Some(dRep(7))),
            credential(2) -> account(3, drep = Some(DRep.AlwaysAbstain)),
            credential(3) -> account(4, drep = Some(DRep.AlwaysNoConfidence)),
            credential(4) -> account(5)
          ),
          utxos = Seq(credential(4) -> 1_000L)
        )
        assert(
          Ratify.dRepDistribution(state) ==
              Map(DRep.AlwaysAbstain -> Coin(3), DRep.AlwaysNoConfidence -> Coin(4))
        )
    }

    test("a DRep delegation with no stake adds an entry of 0") {
        // Map.insertWith (<>) runs for every delegated account, DRepPulser.hs:240-242
        val state = ledger(
          accounts = Map(credential(1) -> account(0, drep = Some(dRep(7)))),
          dReps = Map(keyCredential(dRep(7)) -> dRepState(30))
        )
        assert(Ratify.dRepDistribution(state) == Map(dRep(7) -> Coin.zero))
    }

    // The SPO distribution, spec [RT-4], [RT-4a] (calculatePoolDistr', cardano-ledger-core
    // State/SnapShots.hs:449-465; addToPoolDistr, DRepPulser.hs:228-236)

    private def poolSnapshot(stake: Long, delegators: Int): PoolSnapshot =
        PoolSnapshot(
          Coin(stake),
          Set.empty,
          Coin.zero,
          VrfKeyHash.fromHex("11" * 32),
          Coin.zero,
          Coin(340),
          UnitInterval.zero,
          delegators,
          credential(99)
        )

    test("the SPO distribution is the mark pools with delegators, and its total is the mark's") {
        val mark = StakeSnapshot(
          Map(credential(1) -> Coin(30), credential(2) -> Coin(12)),
          Map(credential(1) -> poolId(1), credential(2) -> poolId(1)),
          Map(poolId(1) -> poolSnapshot(42, 2), poolId(2) -> poolSnapshot(0, 0))
        )
        assert(
          Ratify.poolDistribution(ledger(Map.empty), mark) ==
              PoolDistribution(Map(poolId(1) -> Coin(42)), Coin(42))
        )
        assert(
          Ratify.poolDistribution(ledger(Map.empty), StakeSnapshot.empty) ==
              PoolDistribution(Map.empty, Coin(1))
        )
    }

    test("a delegator's proposal deposits add to its pool and to the total") {
        // also with no DRep delegation (addToPoolDistr runs apart from addToDRepDistr); a pool
        // that is not in the distribution and an account with no pool get nothing
        val mark = StakeSnapshot(
          Map(credential(1) -> Coin(30)),
          Map(credential(1) -> poolId(1)),
          Map(poolId(1) -> poolSnapshot(30, 1), poolId(2) -> poolSnapshot(0, 0))
        )
        val state = ledger(
          accounts = Map(
            credential(1) -> account(0, pool = Some(poolId(1))),
            credential(2) -> account(0, pool = Some(poolId(2))),
            credential(3) -> account(0)
          ),
          proposals = Vector(
            proposal(1, GovAction.InfoAction, deposit = 100, returnTo = 1),
            proposal(2, GovAction.InfoAction, deposit = 7, returnTo = 2),
            proposal(3, GovAction.InfoAction, deposit = 9, returnTo = 3)
          )
        )
        assert(
          Ratify.poolDistribution(state, mark) ==
              PoolDistribution(Map(poolId(1) -> Coin(130)), Coin(130))
        )
    }

    test("a delegator whose pool retired at the boundary adds no deposit") {
        // spec [RT-4a]: the accounts are those after POOLREAP, which cleared the delegation, and
        // the pool keeps its mark stake
        val mark = StakeSnapshot(
          Map(credential(1) -> Coin(30)),
          Map(credential(1) -> poolId(1)),
          Map(poolId(1) -> poolSnapshot(30, 1))
        )
        val state = ledger(
          accounts = Map(credential(1) -> account(0)),
          proposals = Vector(proposal(1, GovAction.InfoAction, deposit = 100, returnTo = 1))
        )
        assert(
          Ratify.poolDistribution(state, mark) ==
              PoolDistribution(Map(poolId(1) -> Coin(30)), Coin(30))
        )
    }

    // RATIFY, spec [RT-1], [RT-5a] to [RT-18], [EN-4a] (ratifyTransition, conway
    // Rules/Ratify.hs:318-360). The base run accepts every action but an InfoAction, since every
    // threshold is 0 (spec [RT-11]); each test raises the thresholds it is about.

    private val epochNo: EpochNo = 10

    private def cold(i: Int): Credential = Credential.KeyHash(AddrKeyHash.fromHex(hash28(30 + i)))
    private def hot(i: Int): Credential = Credential.KeyHash(AddrKeyHash.fromHex(hash28(40 + i)))

    private val z = UnitInterval.zero

    private val openParams: ProtocolParams = CardanoInfo.mainnet.protocolParams.copy(
      protocolVersion = ProtocolVersion(11, 0),
      poolVotingThresholds = zeroPools,
      dRepVotingThresholds = zeroDReps,
      committeeMinSize = 0,
      committeeMaxTermLength = 100
    )

    private val constitution =
        Constitution(Anchor("https://example.com/c", DataHash.fromHex("3" * 64)), None)

    /** Three members until epoch 30, with threshold 0. */
    private val baseState = EnactState(
      committee = Some(Committee(Map(cold(1) -> 30L, cold(2) -> 30L, cold(3) -> 30L), z)),
      constitution = constitution,
      params = openParams,
      withdrawals = Map.empty,
      roots = GovRoots.empty
    )

    /** Pools 1 and 2 with 60 and 40, DReps 1 and 2 with 60 and 40, every member authorized. */
    private val baseEnv = RatifyEnv(
      currentEpoch = epochNo,
      poolDistribution =
          PoolDistribution(Map(poolId(1) -> Coin(60), poolId(2) -> Coin(40)), Coin(100)),
      dRepDistribution = Map(dRep(1) -> Coin(60), dRep(2) -> Coin(40)),
      dReps = Map(keyCredential(dRep(1)) -> dRepState(30), keyCredential(dRep(2)) -> dRepState(30)),
      committeeState =
          (1 to 3).map(i => cold(i) -> CommitteeAuthorization.HotCredential(hot(i))).toMap,
      accounts = Map.empty,
      stakePools = Map.empty
    )

    private val treasury = Coin(1_000)

    private def ratifyIn(state: EnactState = baseState, env: RatifyEnv = baseEnv)(
        proposals: GovActionState*
    ): RatifyState = Ratify.ratify(env, state, treasury, proposals.toVector)

    private def run(proposals: GovActionState*): RatifyState = ratifyIn()(proposals*)

    /** Whether the run enacts the single `proposal`. */
    private def enacts(state: EnactState = baseState, env: RatifyEnv = baseEnv)(
        proposal: GovActionState
    ): Boolean = ratifyIn(state, env)(proposal).enacted == Vector(proposal.id)

    private def withParams(f: ProtocolParams => ProtocolParams): EnactState =
        baseState.copy(params = f(baseState.params))

    private def ids(is: Int*): Vector[GovActionId] = is.map(actionId).toVector

    private def newConstitution(parent: Option[GovActionId] = None): GovAction =
        GovAction.NewConstitution(parent, constitution)
    private def paramChange(
        update: ProtocolParamUpdate,
        parent: Option[GovActionId] = None
    ): GovAction = GovAction.ParameterChange(parent, update, None)
    private def withdrawal(amount: Long, to: Int = 5): GovAction =
        GovAction.TreasuryWithdrawals(Map(rewardAccount(to) -> Coin(amount)), None)
    private val updateCommittee: GovAction =
        GovAction.UpdateCommittee(None, Set.empty, Map(cold(4) -> 20L), UnitInterval.half)
    private val hardFork: GovAction = GovAction.HardForkInitiation(None, ProtocolVersion(11, 1))
    private val noConfidence: GovAction = GovAction.NoConfidence(None)
    private val feeChange = ProtocolParamUpdate(minFeeA = Some(Coin(45)))
    private val nOptChange = ProtocolParamUpdate(nOpt = Some(600))

    private def committeeVotes(p: GovActionState, votes: (Int, Vote)*): GovActionState =
        p.copy(committeeVotes = votes.map((i, v) => hot(i) -> v).toMap)
    private def dRepVotes(p: GovActionState, votes: (Int, Vote)*): GovActionState =
        p.copy(drepVotes = votes.map((i, v) => keyCredential(dRep(i)) -> v).toMap)
    private def poolVotes(p: GovActionState, votes: (Int, Vote)*): GovActionState =
        p.copy(spoVotes = votes.map((i, v) => poolId(i) -> v).toMap)

    test("a run with every threshold at 0 enacts every action but an InfoAction") {
        // spec [RT-11]; an InfoAction has no threshold (NoVotingThreshold, Internal.hs:539)
        val result = run(
          proposal(1, paramChange(feeChange)),
          proposal(2, GovAction.InfoAction),
          proposal(3, withdrawal(10))
        )
        assert(result.enacted == ids(1, 3))
        assert(!result.delayed)
        assert(result.expired.isEmpty)
        assert(result.enactState.params == feeChange.applyTo(openParams))
        assert(result.enactState.withdrawals == Map(credential(5) -> Coin(10)))
    }

    test("RATIFY takes the actions by priority, then in the order they were made") {
        // spec [RT-1] (reorderActions, a stable sortOn actionPriority, Internal.hs:542-552);
        // acceptance 2: "proposals of same priority are enacted in order of submission"
        val result = run(
          proposal(1, withdrawal(1)),
          proposal(2, paramChange(nOptChange)),
          proposal(3, withdrawal(2)),
          proposal(4, paramChange(feeChange, parent = Some(actionId(2))))
        )
        assert(result.enacted == ids(2, 4, 1, 3))
    }

    test("a delaying action stops the run from enacting the actions after it") {
        // spec [RT-5c], [RT-5f]; acceptance 4: "A delaying action delays all other actions even
        // when all of them may be ratified in the same epoch"; NoConfidence has priority 0
        val result = run(
          proposal(1, paramChange(feeChange)),
          proposal(2, newConstitution()),
          proposal(3, noConfidence)
        )
        assert(result.enacted == ids(3))
        assert(result.delayed)
        assert(result.enactState.committee.isEmpty)
        assert(result.enactState.params == openParams)
    }

    test("an action whose parent is not the root of its purpose is not enacted") {
        // spec [RT-5a] (prevActionAsExpected, Ratify.hs:364-367)
        val rooted = baseState.copy(roots = GovRoots.empty.copy(constitution = Some(actionId(9))))
        assert(!enacts(rooted)(proposal(1, newConstitution())))
        assert(enacts(rooted)(proposal(1, newConstitution(Some(actionId(9))))))
    }

    test("a parent and its child ParameterChange enact in one run") {
        // spec [RT-17]: the child reads the root that the parent set; a ParameterChange delays
        // nothing
        val result = run(
          proposal(1, paramChange(feeChange)),
          proposal(2, paramChange(nOptChange, parent = Some(actionId(1))))
        )
        assert(result.enacted == ids(1, 2))
        assert(result.enactState.roots.pparamUpdate == Some(actionId(2)))
        assert(result.enactState.params == nOptChange.applyTo(feeChange.applyTo(openParams)))
    }

    test("an UpdateCommittee with a term beyond the max term length is not enacted") {
        // spec [RT-5b] (validCommitteeTerm, Ratify.hs:369-381): epoch 10 + 100
        def update(term: EpochNo) = proposal(
          1,
          GovAction.UpdateCommittee(None, Set.empty, Map(cold(4) -> term), UnitInterval.half)
        )
        assert(enacts()(update(110)))
        assert(!enacts()(update(111)))
        // [RT-17]: the max term length of the running pparams
        assert(!enacts(withParams(_.copy(committeeMaxTermLength = 99)))(update(110)))
    }

    test("withdrawals beyond the treasury of the run are not enacted") {
        // spec [RT-5d], [RT-17]; acceptance 1: "Withdrawals exceeding treasury submitted in
        // several proposals within the same epoch": the second reads what the first left
        val result = run(
          proposal(1, withdrawal(600)),
          proposal(2, withdrawal(600)),
          proposal(3, withdrawal(400, to = 6))
        )
        assert(result.enacted == ids(1, 3))
        assert(
          result.enactState.withdrawals ==
              Map(credential(5) -> Coin(600), credential(6) -> Coin(400))
        )
        assert(!enacts()(proposal(1, withdrawal(1_001))))
    }

    test("an action that is not enacted expires when its expiresAfter is before the epoch") {
        // spec [RT-10] (Ratify.hs:353-359); an enacted action never expires
        val result = run(
          proposal(1, GovAction.InfoAction, expiresAfter = epochNo - 1),
          proposal(2, GovAction.InfoAction, expiresAfter = epochNo),
          proposal(3, paramChange(feeChange), expiresAfter = epochNo - 1)
        )
        assert(result.enacted == ids(3))
        assert(result.expired == Set(actionId(1)))
    }

    test("a hard fork to PV 12 is not enacted, delays nothing and expires") {
        // spec [EN-4a], D91
        val result = run(
          proposal(1, GovAction.HardForkInitiation(None, ProtocolVersion(12, 0)), expiresAfter = 9),
          proposal(2, paramChange(feeChange))
        )
        assert(result.enacted == ids(2))
        assert(!result.delayed)
        assert(result.expired == Set(actionId(1)))
        assert(enacts()(proposal(1, hardFork)))
    }

    test("a committeeMinSize raised in the run blocks a later TreasuryWithdrawals") {
        // spec [RT-17]; acceptance 8: "TreasuryWithdrawal fails to ratify due to an increase in
        // CommitteeMinSize" (votingCommitteeThreshold reads the running pparams,
        // Internal.hs:441-450)
        val raise = ProtocolParamUpdate(minCommitteeSize = Some(4))
        assert(run(proposal(1, paramChange(raise)), proposal(2, withdrawal(1))).enacted == ids(1))
        assert(enacts()(proposal(2, withdrawal(1))))
    }

    // The committee, spec [RT-6], [RT-6a], [RT-7], [RT-7a] (committeeAccepted, Ratify.hs:118-163;
    // votingCommitteeThresholdInternal, Internal.hs:452-495)

    private val twoThirds =
        baseState.copy(committee = baseState.committee.map(_.copy(threshold = UnitInterval(2, 3))))

    test("the committee ratio counts Yes over the active members that do not abstain") {
        // spec [RT-7], [RT-7a]: a member with no vote is No; threshold 2/3
        val p = proposal(1, newConstitution())
        val yes = enacts(twoThirds)
        assert(yes(committeeVotes(p, 1 -> Vote.Yes, 2 -> Vote.Yes)))
        assert(!yes(committeeVotes(p, 1 -> Vote.Yes)))
        assert(yes(committeeVotes(p, 1 -> Vote.Yes, 2 -> Vote.Yes, 3 -> Vote.No)))
        assert(yes(committeeVotes(p, 1 -> Vote.Yes, 2 -> Vote.Abstain, 3 -> Vote.Abstain)))
        assert(!yes(committeeVotes(p, 1 -> Vote.Yes, 2 -> Vote.No, 3 -> Vote.No)))
    }

    test("an expired, resigned or unauthorized member does not count in the committee ratio") {
        // spec [RT-7]: member 1 expired (10 > 9), member 2 resigned, member 3 has no hot key;
        // member 4 is active at its last epoch
        val committee = Committee(
          Map(cold(1) -> 9L, cold(2) -> 30L, cold(3) -> 30L, cold(4) -> 10L),
          UnitInterval(2, 3)
        )
        val env = baseEnv.copy(committeeState =
            Map(
              cold(1) -> CommitteeAuthorization.HotCredential(hot(1)),
              cold(2) -> CommitteeAuthorization.Resigned(None),
              cold(4) -> CommitteeAuthorization.HotCredential(hot(4))
            )
        )
        val yes = enacts(baseState.copy(committee = Some(committee)), env)
        assert(yes(committeeVotes(proposal(1, newConstitution()), 4 -> Vote.Yes)))
        assert(!yes(committeeVotes(proposal(1, newConstitution()), 1 -> Vote.Yes)))
    }

    test("with no committee or too few active members the committee threshold is never met") {
        // spec [RT-6], [RT-6a]: member 3 resigned, so 2 members are active
        val env = baseEnv.copy(committeeState =
            baseEnv.committeeState.updated(cold(3), CommitteeAuthorization.Resigned(None))
        )
        val p = proposal(1, withdrawal(1))
        assert(!enacts(baseState.copy(committee = None), env)(p))
        assert(!enacts(withParams(_.copy(committeeMinSize = 3)), env)(p))
        assert(enacts(withParams(_.copy(committeeMinSize = 2)), env)(p))
        // [RT-6a]: a member with no committeeState entry is not active either
        val unauthorized = baseEnv.copy(committeeState = baseEnv.committeeState - cold(3))
        assert(!enacts(withParams(_.copy(committeeMinSize = 3)), unauthorized)(p))
        assert(enacts(withParams(_.copy(committeeMinSize = 2)), unauthorized)(p))
    }

    test("NoConfidence and UpdateCommittee need no committee vote; an InfoAction never passes") {
        // the committee rows of the threshold table: auto, auto, never
        val none = twoThirds.copy(committee = None)
        assert(enacts(none)(proposal(1, noConfidence)))
        assert(enacts(none)(proposal(1, updateCommittee)))
        assert(!enacts()(proposal(1, GovAction.InfoAction)))
    }

    test("a committee threshold of 0 accepts with no vote") {
        // spec [RT-11]: r == minBound, Ratify.hs:130; also as 0/5, not only 0/1
        val zeroOfFive =
            baseState.copy(committee =
                baseState.committee.map(_.copy(threshold = UnitInterval(0, 5)))
            )
        assert(enacts()(proposal(1, newConstitution())))
        assert(enacts(zeroOfFive)(proposal(1, newConstitution())))
    }

    // The SPOs, spec [RT-8], [RT-13], [RT-14], [RT-14a] (spoAcceptedRatio, Ratify.hs:195-226;
    // votingStakePoolThresholdInternal, Internal.hs:381-410)

    private def poolThresholds(t: UnitInterval): EnactState =
        withParams(_.copy(poolVotingThresholds = PoolVotingThresholds(t, t, t, t, t)))

    test("the SPO ratio is Yes over the total stake without the abstaining stake") {
        // spec [RT-13]: 60 Yes / (100 - 40 Abstain) = 1 >= 1/2; 40 Yes / 100 < 1/2
        val yes = enacts(poolThresholds(UnitInterval.half))
        val p = proposal(1, hardFork)
        assert(yes(poolVotes(p, 1 -> Vote.Yes, 2 -> Vote.Abstain)))
        assert(!yes(poolVotes(p, 2 -> Vote.Yes, 1 -> Vote.No)))
        assert(yes(poolVotes(p, 1 -> Vote.Yes)))
        // spec [RT-8]: a pool with no vote is No on a HardForkInitiation: 40 / 100
        assert(!yes(poolVotes(p, 2 -> Vote.Yes)))
    }

    test("each SPO threshold row reads its own threshold") {
        // NoConfidence: motionNoConfidence; UpdateCommittee: committeeNormal, or
        // committeeNoConfidence with no committee; HardFork: hardForkInitiation; a security
        // ParameterChange: ppSecurityGroup
        val one = UnitInterval.one
        def only(f: PoolVotingThresholds => PoolVotingThresholds) =
            withParams(_.copy(poolVotingThresholds = f(zeroPools)))
        val noCommittee = baseState.copy(committee = None)
        val rows = Seq(
          (noConfidence, only(_.copy(motionNoConfidence = one)), baseState),
          (updateCommittee, only(_.copy(committeeNormal = one)), baseState),
          (
            updateCommittee,
            only(_.copy(committeeNoConfidence = one)).copy(committee = None),
            noCommittee
          ),
          (hardFork, only(_.copy(hardForkInitiation = one)), baseState),
          (paramChange(feeChange), only(_.copy(ppSecurityGroup = one)), baseState)
        )
        for ((action, blocked, open), i) <- rows.zipWithIndex do
            withClue(s"row $i: ") {
                val p = poolVotes(proposal(1, action), 1 -> Vote.No, 2 -> Vote.No)
                assert(!enacts(blocked)(p))
                assert(enacts(open)(p))
            }
        // auto: the SPOs do not vote on these, so their No votes do not block
        for action <- Seq(paramChange(nOptChange), newConstitution(), withdrawal(1)) do
            withClue(s"$action: ") {
                val p = poolVotes(proposal(1, action), 1 -> Vote.No, 2 -> Vote.No)
                assert(enacts(poolThresholds(one))(p))
            }
    }

    test("a pool with no vote votes by the DRep delegation of its reward account") {
        // spec [RT-8], [RT-14], [RT-14a] (defaultStakePoolVote, Governance.hs:567-576);
        // acceptance 7: "Reward account delegated to AlwaysNoConfidence"
        val registration: Certificate.PoolRegistration = Certificate.PoolRegistration(
          operator = AddrKeyHash.fromByteString(poolId(1)),
          vrfKeyHash = VrfKeyHash.fromHex("11" * 32),
          pledge = Coin.zero,
          cost = Coin(340),
          margin = UnitInterval.zero,
          rewardAccount = rewardAccount(21),
          poolOwners = Set.empty,
          relays = IndexedSeq.empty,
          poolMetadata = None
        )
        def envWith(drep: Option[DRep]): RatifyEnv = baseEnv.copy(
          stakePools = Map(poolId(1) -> registration),
          accounts = Map(credential(21) -> account(0, drep = drep))
        )
        val half = poolThresholds(UnitInterval.half)
        // pool 1 (60) has no vote
        def passes(env: RatifyEnv, action: GovAction, pool2: Vote) =
            enacts(half, env)(poolVotes(proposal(1, action), 2 -> pool2))
        val noConfidenceDRep = envWith(Some(DRep.AlwaysNoConfidence))
        assert(passes(noConfidenceDRep, noConfidence, Vote.No))
        assert(!passes(noConfidenceDRep, updateCommittee, Vote.No))
        // AlwaysAbstain: 0 / (100 - 60) with pool 2 No, 40 / 40 with pool 2 Yes
        assert(!passes(envWith(Some(DRep.AlwaysAbstain)), noConfidence, Vote.No))
        assert(passes(envWith(Some(DRep.AlwaysAbstain)), noConfidence, Vote.Yes))
        // a credential DRep, no delegation, no account, no pool: No, so 40 / 100
        assert(!passes(envWith(Some(dRep(1))), noConfidence, Vote.Yes))
        assert(!passes(envWith(None), noConfidence, Vote.Yes))
        assert(!passes(noConfidenceDRep.copy(accounts = Map.empty), noConfidence, Vote.No))
        assert(!passes(noConfidenceDRep.copy(stakePools = Map.empty), noConfidence, Vote.No))
    }

    test("an SPO ratio with a zero denominator is 0") {
        // spec [RT-12]: %? gives 0 when every pool abstains
        val p = poolVotes(proposal(1, noConfidence), 1 -> Vote.Abstain, 2 -> Vote.Abstain)
        assert(!enacts(poolThresholds(UnitInterval(1, 100)))(p))
    }

    // The DReps, spec [RT-9], [RT-9a], [RT-15] (dRepAcceptedRatio, Ratify.hs:252-281;
    // votingDRepThresholdInternal, Internal.hs:512-540)

    private def dRepThresholds(t: UnitInterval): EnactState =
        withParams(
          _.copy(dRepVotingThresholds = DRepVotingThresholds(t, t, t, t, t, t, t, t, t, t))
        )

    test("the DRep ratio counts a non-voting DRep as No and leaves out the abstaining stake") {
        // spec [RT-15]: 60 / 100 with DRep 2 silent; 60 / 60 with DRep 2 abstaining
        val yes = enacts(dRepThresholds(UnitInterval(2, 3)))
        val p = proposal(1, newConstitution())
        assert(!yes(dRepVotes(p, 1 -> Vote.Yes)))
        assert(yes(dRepVotes(p, 1 -> Vote.Yes, 2 -> Vote.Abstain)))
        assert(!yes(dRepVotes(p, 1 -> Vote.Yes, 2 -> Vote.No)))
    }

    test("the DRep ratio is compared with its threshold exactly") {
        // spec [RT-18]: 3333333333333333 / 10^16 is below 1/3, but as Doubles the two are equal;
        // one lovelace more of Yes stake meets 1/3
        def yes(yesStake: Long) = enacts(
          dRepThresholds(UnitInterval(1, 3)),
          baseEnv.copy(dRepDistribution =
              Map(dRep(1) -> Coin(yesStake), dRep(2) -> Coin(10_000_000_000_000_000L - yesStake))
          )
        )(dRepVotes(proposal(1, newConstitution()), 1 -> Vote.Yes, 2 -> Vote.No))
        assert(!yes(3_333_333_333_333_333L))
        assert(yes(3_333_333_333_333_334L))
    }

    test("the predefined DReps vote as their names say") {
        // spec [RT-15]: AlwaysNoConfidence is Yes on NoConfidence and No otherwise; AlwaysAbstain
        // is left out
        val env = baseEnv.copy(dRepDistribution =
            Map(
              dRep(1) -> Coin(30),
              DRep.AlwaysNoConfidence -> Coin(40),
              DRep.AlwaysAbstain -> Coin(500)
            )
        )
        val yes = enacts(dRepThresholds(UnitInterval(1, 2)), env)
        assert(yes(proposal(1, noConfidence)))
        assert(!yes(proposal(1, newConstitution())))
        assert(!yes(dRepVotes(proposal(1, newConstitution()), 1 -> Vote.Yes)))
    }

    test("an expired or unregistered DRep counts nowhere") {
        // spec [RT-9], [RT-9a]: DRep 2 expired at 9 < 10, so 60 / 60; at 10 it still counts
        val p = dRepVotes(proposal(1, newConstitution()), 1 -> Vote.Yes)
        val dRep2 = keyCredential(dRep(2))
        def yes(dReps: Map[Credential, DRepState]) =
            enacts(dRepThresholds(UnitInterval(2, 3)), baseEnv.copy(dReps = dReps))(p)
        assert(yes(baseEnv.dReps.updated(dRep2, dRepState(9))))
        assert(yes(baseEnv.dReps - dRep2))
        assert(!yes(baseEnv.dReps.updated(dRep2, dRepState(10))))
    }

    test("each DRep threshold row reads its own threshold") {
        val one = UnitInterval.one
        def only(f: DRepVotingThresholds => DRepVotingThresholds) =
            withParams(_.copy(dRepVotingThresholds = f(zeroDReps)))
        val noCommittee = baseState.copy(committee = None)
        val rows = Seq(
          (noConfidence, only(_.copy(motionNoConfidence = one)), baseState),
          (updateCommittee, only(_.copy(committeeNormal = one)), baseState),
          (
            updateCommittee,
            only(_.copy(committeeNoConfidence = one)).copy(committee = None),
            noCommittee
          ),
          (newConstitution(), only(_.copy(updateToConstitution = one)), baseState),
          (hardFork, only(_.copy(hardForkInitiation = one)), baseState),
          (
            paramChange(ProtocolParamUpdate(maxTxSize = Some(1))),
            only(_.copy(ppNetworkGroup = one)),
            baseState
          ),
          (paramChange(feeChange), only(_.copy(ppEconomicGroup = one)), baseState),
          (paramChange(nOptChange), only(_.copy(ppTechnicalGroup = one)), baseState),
          (
            paramChange(ProtocolParamUpdate(drepDeposit = Some(Coin(1)))),
            only(_.copy(ppGovGroup = one)),
            baseState
          ),
          (withdrawal(1), only(_.copy(treasuryWithdrawal = one)), baseState)
        )
        for ((action, blocked, open), i) <- rows.zipWithIndex do
            withClue(s"row $i: ") {
                val p = dRepVotes(proposal(1, action), 1 -> Vote.No, 2 -> Vote.No)
                assert(!enacts(blocked)(p))
                assert(enacts(open)(p))
            }
    }

    test("a ParameterChange of several groups needs the largest of their DRep thresholds") {
        // pparamsUpdateThreshold, Internal.hs:319-335: max(1/2, 3/4) against 60 / 100 Yes
        val state = withParams(
          _.copy(dRepVotingThresholds =
              zeroDReps.copy(ppNetworkGroup = UnitInterval(1, 2), ppGovGroup = UnitInterval(3, 4))
          )
        )
        val both = ProtocolParamUpdate(maxTxSize = Some(1), drepDeposit = Some(Coin(1)))
        val network = ProtocolParamUpdate(maxTxSize = Some(1))
        assert(!enacts(state)(dRepVotes(proposal(1, paramChange(both)), 1 -> Vote.Yes)))
        assert(enacts(state)(dRepVotes(proposal(1, paramChange(network)), 1 -> Vote.Yes)))
    }

    test("a DRep ratio with a zero denominator is 0") {
        // spec [RT-12]: no DRep stake
        val env = baseEnv.copy(dRepDistribution = Map.empty)
        assert(!enacts(dRepThresholds(UnitInterval(1, 100)), env)(proposal(1, newConstitution())))
    }

    // The PV 9 bootstrap, spec [RT-16] (C/Era.hs:175)

    private def atPv9(state: EnactState): EnactState =
        state.copy(params = state.params.copy(protocolVersion = ProtocolVersion(9, 0)))

    test("at PV 9 every DRep threshold but that of an InfoAction is 0") {
        // Internal.hs:525-527
        val strict = dRepThresholds(UnitInterval.one)
        assert(!enacts(strict)(proposal(1, newConstitution())))
        assert(enacts(atPv9(strict))(proposal(1, newConstitution())))
        assert(!enacts(atPv9(baseState))(proposal(1, GovAction.InfoAction)))
    }

    test("at PV 9 the committee min size is not checked") {
        // Internal.hs:475: committeeMinSize 4 with 3 members
        val minSize = withParams(_.copy(committeeMinSize = 4))
        assert(!enacts(minSize)(proposal(1, withdrawal(1))))
        assert(enacts(atPv9(minSize))(proposal(1, withdrawal(1))))
    }

    test("at PV 9 a pool with no vote abstains, except on a HardForkInitiation") {
        // spoAcceptedRatio, Ratify.hs:213-216: pool 2 Yes gives 40 / (100 - 60) at PV 9
        val half = poolThresholds(UnitInterval.half)
        val p = poolVotes(proposal(1, noConfidence), 2 -> Vote.Yes)
        assert(!enacts(half)(p))
        assert(enacts(atPv9(half))(p))
        val fork = GovAction.HardForkInitiation(None, ProtocolVersion(10, 0))
        assert(!enacts(atPv9(half))(poolVotes(proposal(1, fork), 2 -> Vote.Yes)))
    }
}
