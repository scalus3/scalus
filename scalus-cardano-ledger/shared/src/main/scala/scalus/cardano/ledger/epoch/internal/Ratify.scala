package scalus.cardano.ledger.epoch.internal

import scalus.cardano.ledger.{Certificate, Coin, Committee, CommitteeAuthorization, ConwayAccountState, Credential, DRep, DRepState, DRepVotingThresholds, EpochNo, GovAction, GovActionId, GovActionPurpose, GovActionState, GovState, PoolKeyHash, ProtocolParamUpdate, ProtocolParams, UnitInterval, Vote}
import scalus.cardano.ledger.epoch.{EnactState, RatifyState, StakeSnapshot}
import scalus.cardano.ledger.rules.{CertsMutator, State}

/** The group of a pparam whose DRep threshold a ParameterChange of it needs, the ledger's `PPGroup`
  * (conway PParams.hs:643-708), spec [EN-2a].
  */
enum PParamGroup {
    case Network, Economic, Technical, Gov
}

/** The stake each pool votes with, and the total the SPO ratio divides by, the ledger's `PoolDistr`
  * as RATIFY reads it, spec [RT-4a].
  */
case class PoolDistribution(stake: Map[PoolKeyHash, Coin], total: Coin)

/** What RATIFY reads besides the enact state, the ledger's `RatifyEnv` without the instant stake
  * (conway Governance/Internal.hs:562-571), spec [RT-2], [EB-20a]: all of it is read from the state
  * after the boundary.
  *
  * @param currentEpoch
  *   `reCurrentEpoch`, the epoch of the boundary
  * @param poolDistribution
  *   the stake the pools vote with, spec [RT-4a]
  * @param dRepDistribution
  *   the stake the DReps vote with, spec [RT-3a]
  * @param dReps
  *   the registered DReps, with their stored expiry, spec [RT-9a]
  * @param committeeState
  *   the authorization of each committee cold credential
  * @param accounts
  *   the accounts, for the default vote of a pool, spec [RT-14a]
  * @param stakePools
  *   the registered pools, for the default vote of a pool, spec [RT-14a]
  */
case class RatifyEnv(
    currentEpoch: EpochNo,
    poolDistribution: PoolDistribution,
    dRepDistribution: Map[DRep, Coin],
    dReps: Map[Credential, DRepState],
    committeeState: Map[Credential, CommitteeAuthorization],
    accounts: Map[Credential, ConwayAccountState],
    stakePools: Map[PoolKeyHash, Certificate.PoolRegistration]
)

/** RATIFY and ENACT, ledger-rules spec section 15. */
object Ratify {

    /** RATIFY, `ratifyTransition` (conway Rules/Ratify.hs:318-360), spec [RT-1] to [RT-18]: the
      * proposals, in the order they were made and sorted stably by priority (`reorderActions`,
      * conway Governance/Internal.hs:542-552), each enacted on the enact state when all the checks
      * hold, in the ledger's order (:337-341): its parent is the root of its purpose [RT-5a], its
      * committee terms are within the max term length [RT-5b], no delaying action was enacted
      * before it in the run [RT-5c], its withdrawals fit the treasury of the run [RT-5d], it is
      * within the Conway rules [EN-4a], and the committee, the SPOs and the DReps accept it
      * [RT-5e]. Every check reads the enact state as the actions enacted before it left it, spec
      * [RT-17]. An action that is not enacted expires when `expiresAfter < currentEpoch`, spec
      * [RT-10].
      *
      * The ledger recurses and inserts the expired ids after the rest of the run; the expiry of an
      * action reads nothing the run changes, so one fold gives the same sets. `delayed` is set to
      * whether the enacted action delays (`rsDelayedL .~`, :348); once it is true nothing more is
      * enacted, so `||` gives the same value.
      *
      * @param treasury
      *   the treasury after the boundary, `ensTreasury` of the run (conway Governance.hs:511); the
      *   run ends it at 0 (:360), so the result does not keep it (D86)
      */
    def ratify(
        env: RatifyEnv,
        enactState: EnactState,
        treasury: Coin,
        proposals: Vector[GovActionState]
    ): RatifyState = {
        val start = (RatifyState(enactState, Vector.empty, Set.empty, false), treasury)
        val (result, _) =
            proposals.sortBy(p => priority(p.action)).foldLeft(start) { case ((state, left), p) =>
                val action = p.action
                val es = state.enactState
                if prevActionAsExpected(action, es)
                    && validCommitteeTerm(action, es.params, env.currentEpoch)
                    && !state.delayed
                    && withdrawalCanWithdraw(action, left)
                    && isConwayAction(action)
                    && committeeAccepted(env, es, p)
                    && spoAccepted(env, es, p)
                    && dRepAccepted(env, es, p)
                then
                    val (enacted, newLeft) = enact(es, left, p.id, action)
                    val next = RatifyState(
                      enacted,
                      state.enacted :+ p.id,
                      state.expired,
                      state.delayed || isDelaying(action)
                    )
                    (next, newLeft)
                else if p.expiresAfter < env.currentEpoch then
                    (state.copy(expired = state.expired + p.id), left)
                else (state, left)
            }
        result
    }

    /** `actionPriority` (conway Governance/Internal.hs:542-549), spec [RT-1]. */
    private def priority(action: GovAction): Int = action match
        case _: GovAction.NoConfidence        => 0
        case _: GovAction.UpdateCommittee     => 1
        case _: GovAction.NewConstitution     => 2
        case _: GovAction.HardForkInitiation  => 3
        case _: GovAction.ParameterChange     => 4
        case _: GovAction.TreasuryWithdrawals => 5
        case GovAction.InfoAction             => 6

    /** `delayingAction` (conway Rules/Ratify.hs:283-290), spec [RT-5f]. */
    private def isDelaying(action: GovAction): Boolean = action match
        case _: GovAction.NoConfidence | _: GovAction.UpdateCommittee |
            _: GovAction.NewConstitution | _: GovAction.HardForkInitiation =>
            true
        case _ => false

    /** Spec [RT-5a] (`prevActionAsExpected`, conway Rules/Ratify.hs:364-367): the parent is the
      * root of the purpose; an action without a purpose passes.
      */
    private def prevActionAsExpected(action: GovAction, state: EnactState): Boolean =
        GovActionPurpose.of(action).forall { purpose =>
            GovActionPurpose.parent(action) == state.roots.root(purpose)
        }

    /** Spec [RT-5b] (`validCommitteeTerm`, conway Rules/Ratify.hs:369-381). */
    private def validCommitteeTerm(
        action: GovAction,
        params: ProtocolParams,
        currentEpoch: EpochNo
    ): Boolean = action match
        case GovAction.UpdateCommittee(_, _, added, _) =>
            added.values.forall(_ <= currentEpoch + params.committeeMaxTermLength)
        case _ => true

    /** Spec [RT-5d] (`withdrawalCanWithdraw`, conway Rules/Ratify.hs:292-295). */
    private def withdrawalCanWithdraw(action: GovAction, treasury: Coin): Boolean = action match
        case GovAction.TreasuryWithdrawals(withdrawals, _) =>
            withdrawals.values.foldLeft(Coin.zero)(_ + _) <= treasury
        case _ => true

    /** `hardforkConwayBootstrapPhase` (conway Era.hs:175), spec [RT-16]. */
    private def isBootstrap(params: ProtocolParams): Boolean = params.protocolVersion.major == 9

    /** Whether `ratio` meets `threshold`: `None` is the ledger's `SNothing`, which never passes; a
      * threshold of 0 passes with no ratio, spec [RT-11]; else the exact comparison of [RT-18].
      */
    private def meets(threshold: Option[UnitInterval], ratio: => Rational): Boolean =
        threshold.exists(t => t.numerator == 0 || Rational.of(t) <= ratio)

    /** `NoVotingAllowed`: the body counts as accepting (`toRatifyVotingThreshold`, conway
      * Governance/Internal.hs:346-350).
      */
    private val auto: Option[UnitInterval] = Some(UnitInterval.zero)

    /** Spec [RT-6], [RT-6a], [RT-7], [RT-7a] (`committeeAccepted`, conway Rules/Ratify.hs:118-163;
      * `votingCommitteeThresholdInternal`, Governance/Internal.hs:452-495).
      */
    private def committeeAccepted(env: RatifyEnv, state: EnactState, p: GovActionState): Boolean = {
        val members = state.committee.fold(Map.empty[Credential, EpochNo])(_.members)
        def hotKey(cold: Credential, expiry: EpochNo): Option[Credential] =
            if env.currentEpoch > expiry then None
            else
                env.committeeState.get(cold).collect {
                    case CommitteeAuthorization.HotCredential(hot) => hot
                }
        // the committee threshold, else NoVotingThreshold: no committee, or too few active
        // members out of the bootstrap
        def threshold: Option[UnitInterval] = state.committee.flatMap { committee =>
            val active = members.count((cold, expiry) => hotKey(cold, expiry).isDefined)
            Option.when(isBootstrap(state.params) || active >= state.params.committeeMinSize)(
              committee.threshold
            )
        }
        val required = p.action match
            case _: GovAction.NoConfidence | _: GovAction.UpdateCommittee => auto
            case GovAction.InfoAction                                     => None
            case _                                                        => threshold
        meets(
          required, {
              val votes =
                  members.toSeq.flatMap((cold, expiry) => hotKey(cold, expiry)).flatMap { hot =>
                      p.committeeVotes.get(hot) match
                          case None | Some(Vote.No) => Some(false)
                          case Some(Vote.Yes)       => Some(true)
                          case Some(Vote.Abstain)   => None
                  }
              Rational.ratio(votes.count(identity), votes.size)
          }
        )
    }

    /** Spec [RT-8], [RT-13], [RT-14], [RT-14a], [RT-16] (`spoAccepted`, `spoAcceptedRatio`, conway
      * Rules/Ratify.hs:165-226; `votingStakePoolThresholdInternal`, Governance/Internal.hs:381-410;
      * `defaultStakePoolVote`, Governance.hs:567-576): Yes over the total stake without the
      * abstaining stake. A pool with no vote is No on a HardForkInitiation, Abstain at PV 9, and
      * otherwise votes by the DRep delegation of its reward account.
      */
    private def spoAccepted(env: RatifyEnv, state: EnactState, p: GovActionState): Boolean = {
        val t = state.params.poolVotingThresholds
        val required = p.action match
            case _: GovAction.NoConfidence => Some(t.motionNoConfidence)
            case _: GovAction.UpdateCommittee =>
                Some(
                  if state.committee.isDefined then t.committeeNormal else t.committeeNoConfidence
                )
            case _: GovAction.NewConstitution    => auto
            case _: GovAction.HardForkInitiation => Some(t.hardForkInitiation)
            case GovAction.ParameterChange(_, update, _) =>
                if changesSecurityGroup(update) then Some(t.ppSecurityGroup) else auto
            case _: GovAction.TreasuryWithdrawals => auto
            case GovAction.InfoAction             => None
        def defaultVote(pool: PoolKeyHash): Vote = {
            val dRep = for
                params <- env.stakePools.get(pool)
                account <- env.accounts.get(params.rewardAccount.address.credential)
                dRep <- account.dRepDelegation
            yield dRep
            dRep match
                case Some(DRep.AlwaysNoConfidence) =>
                    p.action match
                        case _: GovAction.NoConfidence => Vote.Yes
                        case _                         => Vote.No
                case Some(DRep.AlwaysAbstain) => Vote.Abstain
                case _                        => Vote.No
        }
        meets(
          required, {
              val (yes, abstain) = env.poolDistribution.stake.foldLeft((0L, 0L)) {
                  case ((yes, abstain), (pool, stake)) =>
                      val vote = p.spoVotes.get(pool) match
                          case Some(vote) => vote
                          case None =>
                              p.action match
                                  case _: GovAction.HardForkInitiation => Vote.No
                                  case _ if isBootstrap(state.params)  => Vote.Abstain
                                  case _                               => defaultVote(pool)
                      vote match
                          case Vote.Yes     => (yes + stake.value, abstain)
                          case Vote.Abstain => (yes, abstain + stake.value)
                          case Vote.No      => (yes, abstain)
              }
              Rational.ratio(yes, env.poolDistribution.total.value - abstain)
          }
        )
    }

    /** Spec [RT-9], [RT-9a], [RT-15], [RT-16] (`dRepAccepted`, `dRepAcceptedRatio`, conway
      * Rules/Ratify.hs:228-281; `votingDRepThresholdInternal`, Governance/Internal.hs:512-540): Yes
      * over the stake of the active DReps that do not abstain. An unregistered or expired
      * credential DRep counts nowhere; AlwaysNoConfidence is Yes on NoConfidence and No otherwise;
      * AlwaysAbstain counts nowhere. At PV 9 every threshold but that of an InfoAction is 0.
      */
    private def dRepAccepted(env: RatifyEnv, state: EnactState, p: GovActionState): Boolean = {
        val t = state.params.dRepVotingThresholds
        def threshold(of: DRepVotingThresholds => UnitInterval) =
            Some(if isBootstrap(state.params) then UnitInterval.zero else of(t))
        val required = p.action match
            case _: GovAction.NoConfidence => threshold(_.motionNoConfidence)
            case _: GovAction.UpdateCommittee =>
                threshold(t =>
                    if state.committee.isDefined then t.committeeNormal else t.committeeNoConfidence
                )
            case _: GovAction.NewConstitution    => threshold(_.updateToConstitution)
            case _: GovAction.HardForkInitiation => threshold(_.hardForkInitiation)
            case GovAction.ParameterChange(_, update, _) =>
                threshold(t => parameterChangeThreshold(t, update))
            case _: GovAction.TreasuryWithdrawals => threshold(_.treasuryWithdrawal)
            case GovAction.InfoAction             => None
        meets(
          required, {
              val (yes, total) = env.dRepDistribution.foldLeft((0L, 0L)) {
                  case ((yes, total), (dRep, stake)) =>
                      val s = stake.value
                      dRep match
                          case DRep.AlwaysAbstain => (yes, total)
                          case DRep.AlwaysNoConfidence =>
                              p.action match
                                  case _: GovAction.NoConfidence => (yes + s, total + s)
                                  case _                         => (yes, total + s)
                          case _ =>
                              val active = CertsMutator.drepCredential(dRep).filter { cred =>
                                  env.dReps.get(cred).exists(env.currentEpoch <= _.expiry)
                              }
                              active.map(p.drepVotes.get) match
                                  case None                     => (yes, total)
                                  case Some(Some(Vote.Yes))     => (yes + s, total + s)
                                  case Some(Some(Vote.Abstain)) => (yes, total)
                                  case Some(_)                  => (yes, total + s)
              }
              Rational.ratio(yes, total)
          }
        )
    }

    /** The DRep threshold of a ParameterChange, the largest of the thresholds of the groups it
      * changes, 0 for none (`pparamsUpdateThreshold`, conway Governance/Internal.hs:319-335).
      */
    private def parameterChangeThreshold(
        t: DRepVotingThresholds,
        update: ProtocolParamUpdate
    ): UnitInterval =
        modifiedGroups(update)
            .map {
                case PParamGroup.Network   => t.ppNetworkGroup
                case PParamGroup.Economic  => t.ppEconomicGroup
                case PParamGroup.Technical => t.ppTechnicalGroup
                case PParamGroup.Gov       => t.ppGovGroup
            }
            .foldLeft(UnitInterval.zero)((a, b) =>
                if Rational.of(a) <= Rational.of(b) then b else a
            )

    /** The deposits of the proposals of `govState`, summed per return-account credential,
      * `proposalsDeposits` (conway Governance/Proposals.hs:566-579).
      */
    private def proposalDeposits(govState: GovState): Map[Credential, Coin] =
        govState.proposals
            .map(p => p.procedure.rewardAccount.address.credential -> p.procedure.deposit)
            .groupMapReduce(_._1)(_._2)(_ + _)

    /** The stake each DRep votes with, spec [RT-3], [RT-3a] (`computeDRepDistr`, conway
      * Governance/DRepPulser.hs:208-249): for every account with a DRep delegation, its
      * base-address coin in the UTxO, its balance and the deposits of the proposals it is the
      * return account of. The entry is added also when it is 0. A credential DRep counts only when
      * it is registered. Read from the ledger after the boundary, spec [EB-20a].
      */
    def dRepDistribution(ledger: State): Map[DRep, Coin] = {
        val utxoStake = EpochTransition.instantStake(ledger.utxos)
        val deposits = proposalDeposits(ledger.govState)
        val dReps = ledger.certState.vstate.dreps
        ledger.certState.dstate.accounts.foldLeft(Map.empty[DRep, Coin]) {
            case (distr, (cred, account)) =>
                account.dRepDelegation.filter(d =>
                    CertsMutator.drepCredential(d).forall(dReps.contains)
                ) match
                    case None => distr
                    case Some(dRep) =>
                        val stake = utxoStake.getOrElse(cred, Coin.zero) +
                            deposits.getOrElse(cred, Coin.zero) + account.balance
                        distr.updated(dRep, distr.getOrElse(dRep, Coin.zero) + stake)
        }
    }

    /** The stake each pool votes with, spec [RT-4], [RT-4a]: the pools of `mark` with a delegator,
      * with their stake, and the total of `mark` (`calculatePoolDistr'`, cardano-ledger-core
      * State/SnapShots.hs:449-465); then the proposal deposits of each account that delegates to a
      * pool of it, added to the pool and to the total (`addToPoolDistr`, conway
      * Governance/DRepPulser.hs:228-236). The accounts are those of `ledger` after POOLREAP, so a
      * delegator of a pool that retired at the boundary adds nothing.
      */
    def poolDistribution(ledger: State, mark: StakeSnapshot): PoolDistribution = {
        val pools = mark.pools.collect { case (id, p) if p.numDelegators > 0 => id -> p.stake }
        val deposits = proposalDeposits(ledger.govState)
        ledger.certState.dstate.accounts.foldLeft(PoolDistribution(pools, mark.totalActiveStake)) {
            case (distr, (cred, account)) =>
                (for
                    pool <- account.stakePoolDelegation
                    deposit <- deposits.get(cred)
                    stake <- distr.stake.get(pool)
                yield PoolDistribution(
                  distr.stake.updated(pool, stake + deposit),
                  distr.total + deposit
                )).getOrElse(distr)
        }
    }

    /** The groups of the fields `update` holds, `modifiedPPGroups` (conway PParams.hs:643-708),
      * spec [EN-2a]: the DRep threshold of a ParameterChange is the largest of these groups'.
      */
    def modifiedGroups(update: ProtocolParamUpdate): Set[PParamGroup] =
        fieldGroups(update).map(_._1).toSet

    /** Whether `update` holds a field of the security group, so that the SPOs vote on it, spec
      * [GV-11a], [RT-5e] (`isSecurityRelevant`, conway Governance/Internal.hs:396-400), D32, D90.
      */
    def changesSecurityGroup(update: ProtocolParamUpdate): Boolean =
        fieldGroups(update).exists(_._2)

    /** The group of each field `update` holds, and whether the field is in the security group: the
      * field table of spec [EN-2a], the only copy of it (D90).
      */
    private def fieldGroups(u: ProtocolParamUpdate): List[(PParamGroup, Boolean)] = {
        import PParamGroup.*
        List(
          u.minFeeA -> (Economic, true),
          u.minFeeB -> (Economic, true),
          u.maxBlockBodySize -> (Network, true),
          u.maxTxSize -> (Network, true),
          u.maxBlockHeaderSize -> (Network, true),
          u.keyDeposit -> (Economic, false),
          u.poolDeposit -> (Economic, false),
          u.maxEpoch -> (Technical, false),
          u.nOpt -> (Technical, false),
          u.poolPledgeInfluence -> (Technical, false),
          u.expansionRate -> (Economic, false),
          u.treasuryGrowthRate -> (Economic, false),
          u.minPoolCost -> (Economic, false),
          u.adaPerUtxoByte -> (Economic, true),
          u.costModels -> (Technical, false),
          u.executionCosts -> (Economic, false),
          u.maxTxExUnits -> (Network, false),
          u.maxBlockExUnits -> (Network, true),
          u.maxValueSize -> (Network, true),
          u.collateralPercentage -> (Technical, false),
          u.maxCollateralInputs -> (Network, false),
          u.poolVotingThresholds -> (Gov, false),
          u.drepVotingThresholds -> (Gov, false),
          u.minCommitteeSize -> (Gov, false),
          u.committeeTermLimit -> (Gov, false),
          u.governanceActionValidityPeriod -> (Gov, false),
          u.governanceActionDeposit -> (Gov, true),
          u.drepDeposit -> (Gov, false),
          u.drepInactivityPeriod -> (Gov, false),
          u.minFeeRefScriptCoinsPerByte -> (Economic, true)
        ).collect { case (Some(_), group) => group }
    }

    /** The enact state of `govState` with the pparams `params` and no withdrawals, `mkEnactState`
      * (conway Governance.hs:327-337): the start of a RATIFY run, spec [EB-20a], and of the
      * genesis, spec [DM-9c].
      */
    def enactStateOf(govState: GovState, params: ProtocolParams): EnactState =
        EnactState(govState.committee, govState.constitution, params, Map.empty, govState.roots)

    /** The result of a run that enacts and expires nothing, as at the genesis, spec [DM-9c]. */
    def nothingRatified(govState: GovState, params: ProtocolParams): RatifyState =
        RatifyState(enactStateOf(govState, params), Vector.empty, Set.empty, false)

    /** The environment of a run at `currentEpoch` over `ledger`, its SPO distribution from `mark`,
      * spec [EB-20a].
      */
    def envOf(ledger: State, mark: StakeSnapshot, currentEpoch: EpochNo): RatifyEnv = {
        val certState = ledger.certState
        RatifyEnv(
          currentEpoch = currentEpoch,
          poolDistribution = poolDistribution(ledger, mark),
          dRepDistribution = dRepDistribution(ledger),
          dReps = certState.vstate.dreps,
          committeeState = certState.vstate.committeeState,
          accounts = certState.dstate.accounts,
          stakePools = certState.pstate.stakePools
        )
    }

    /** Whether `action` stays within the Conway rules the Emulator runs: false for a
      * HardForkInitiation to major 12 or more, spec [EN-4a], D91 (owner, Q8). RATIFY MUST take such
      * an action as not accepted after the checks of [RT-5a] to [RT-5d], so it delays nothing and
      * expires by [RT-10]. The ledger has no such rule: running Conway rules under a PV 12 label
      * would give wrong results silently.
      */
    def isConwayAction(action: GovAction): Boolean = action match
        case GovAction.HardForkInitiation(_, version) => version.major < 12
        case _                                        => true

    /** ENACT of the action `id` (conway Rules/Enact.hs:83-115), spec [EN-1]: the enact state after
      * `action`, and the treasury of the RATIFY run after it.
      *
      *   - ParameterChange: the update applied to the pparams, spec [EN-2], [EN-2a].
      *   - HardForkInitiation: its protocol version in the pparams.
      *   - TreasuryWithdrawals: each amount added to `withdrawals` under the credential of its
      *     reward account, and the sum taken from `treasury`, spec [EN-6]. RATIFY enacts it only
      *     when the sum is at most the treasury, spec [RT-5d].
      *   - NoConfidence: no committee. UpdateCommittee: [EN-5]. NewConstitution: the constitution.
      *   - InfoAction: nothing.
      *
      * Each action with a purpose becomes the root of its purpose.
      */
    def enact(
        state: EnactState,
        treasury: Coin,
        id: GovActionId,
        action: GovAction
    ): (EnactState, Coin) = {
        val rooted = state.copy(roots =
            GovActionPurpose.of(action).fold(state.roots)(state.roots.updated(_, id))
        )
        action match
            case GovAction.ParameterChange(_, update, _) =>
                (rooted.copy(params = update.applyTo(state.params)), treasury)
            case GovAction.HardForkInitiation(_, version) =>
                (rooted.copy(params = state.params.copy(protocolVersion = version)), treasury)
            case GovAction.TreasuryWithdrawals(withdrawals, _) =>
                val total = withdrawals.values.foldLeft(Coin.zero)(_ + _)
                val summed = withdrawals.foldLeft(state.withdrawals) { case (acc, (account, c)) =>
                    val credential = account.address.credential
                    acc.updated(credential, acc.getOrElse(credential, Coin.zero) + c)
                }
                (rooted.copy(withdrawals = summed), treasury - total)
            case GovAction.NoConfidence(_) => (rooted.copy(committee = None), treasury)
            case GovAction.UpdateCommittee(_, removed, added, threshold) =>
                val committee = updatedCommittee(state.committee, removed, added, threshold)
                (rooted.copy(committee = Some(committee)), treasury)
            case GovAction.NewConstitution(_, constitution) =>
                (rooted.copy(constitution = constitution), treasury)
            case GovAction.InfoAction => (rooted, treasury)
    }

    /** Spec [EN-5] (`updatedCommittee`, conway Rules/Enact.hs:118-134): with no committee, the
      * added members and the threshold, the removed members ignored; else the added members and the
      * old members not removed, an added member's term winning (`Map.union` is left-biased).
      */
    private def updatedCommittee(
        committee: Option[Committee],
        removed: Set[Credential],
        added: Map[Credential, Long],
        threshold: UnitInterval
    ): Committee =
        committee match
            case None          => Committee(added, threshold)
            case Some(current) => Committee((current.members -- removed) ++ added, threshold)
}
