package scalus.cardano.ledger

import scalus.cardano.address.Network
import scalus.uplc.builtin.ByteString

/** The authorization of a constitutional committee member, the ledger's `CommitteeAuthorization`
  * (libs/cardano-ledger-core/src/Cardano/Ledger/State/CertState.hs:302-307), spec [DM-2].
  */
enum CommitteeAuthorization {

    /** The cold credential authorized this hot credential to vote. */
    case HotCredential(hot: Credential)

    /** The member resigned, with an optional anchor that explains why. */
    case Resigned(anchor: Option[Anchor])
}

/** A constitutional committee, the ledger's `Committee`
  * (eras/conway/impl/src/Cardano/Ledger/Conway/Governance/Procedures.hs:544-549).
  *
  * @param members
  *   the cold credential of each member and the last epoch of its term
  * @param threshold
  *   the share of Yes votes the committee needs to ratify an action
  */
case class Committee(members: Map[Credential, EpochNo], threshold: UnitInterval)

/** A proposal in the gov state with its votes, the ledger's `GovActionState`
  * (eras/conway/impl/src/Cardano/Ledger/Conway/Governance/Procedures.hs:213-221), spec [DM-6]. The
  * fields are in the ledger's order.
  *
  * @param committeeVotes
  *   the votes of the committee, keyed by the hot credential
  * @param proposedIn
  *   the epoch of the tx that made the proposal
  * @param expiresAfter
  *   the last epoch in which the proposal can be ratified
  */
case class GovActionState(
    id: GovActionId,
    committeeVotes: Map[Credential, Vote],
    drepVotes: Map[Credential, Vote],
    spoVotes: Map[PoolKeyHash, Vote],
    procedure: ProposalProcedure,
    proposedIn: EpochNo,
    expiresAfter: EpochNo
) {
    def action: GovAction = procedure.govAction
}

/** The last enacted action of each purpose that has a chain of parents, `pRoots` of the ledger's
  * `Proposals` (eras/conway/impl/src/Cardano/Ledger/Conway/Governance/Proposals.hs:240). A new
  * proposal of a purpose names its root, or another proposal of the same purpose, as its parent.
  */
case class GovRoots(
    pparamUpdate: Option[GovActionId],
    hardFork: Option[GovActionId],
    committee: Option[GovActionId],
    constitution: Option[GovActionId]
) {

    /** The root of `purpose`. */
    def root(purpose: GovActionPurpose): Option[GovActionId] = purpose match
        case GovActionPurpose.PParamUpdate => pparamUpdate
        case GovActionPurpose.HardFork     => hardFork
        case GovActionPurpose.Committee    => committee
        case GovActionPurpose.Constitution => constitution
}

object GovRoots {

    /** No action of any purpose is enacted yet. */
    val empty: GovRoots = GovRoots(None, None, None, None)
}

/** The governance state of the tx rules, the Scalus name of the ledger's `ConwayGovState`
  * (eras/conway/impl/src/Cardano/Ledger/Conway/Governance.hs:361-371), spec [DM-5], D27.
  *
  * The ledger's current and previous pparams stay in `Context.env.params` (spec [DM-7]), and the
  * future pparams and the DRep pulser belong to the epoch boundary.
  *
  * @param proposals
  *   the active proposals in the order they were made (D28)
  * @param committee
  *   the committee, or `None` in a state of no confidence
  */
case class GovState(
    proposals: Vector[GovActionState],
    roots: GovRoots,
    committee: Option[Committee],
    constitution: Constitution
) {

    /** The proposal with this id, if it is active. */
    def proposal(id: GovActionId): Option[GovActionState] = proposals.find(_.id == id)

    /** This state with `proposal` appended, or `None` when its parent is neither the root of its
      * purpose nor an active proposal of the same purpose, spec [GV-6]. An action without a purpose
      * (TreasuryWithdrawals, InfoAction) has no parent and is always added.
      *
      * It is `proposalsAddAction` (eras/conway/impl/src/Cardano/Ledger/Conway/Governance/
      * Proposals.hs:297-331); the ledger's graph is the list itself, D28.
      */
    def addProposal(proposal: GovActionState): Option[GovState] = {
        val appended = copy(proposals = proposals :+ proposal)
        GovActionPurpose.of(proposal.action) match
            case None => Some(appended)
            case Some(purpose) =>
                val parent = GovActionPurpose.parent(proposal.action)
                def isPending(id: GovActionId) =
                    this.proposal(id).exists(p => GovActionPurpose.of(p.action).contains(purpose))
                if parent == roots.root(purpose) || parent.exists(isPending) then Some(appended)
                else None
    }

    /** This state with `vote` of `voter` on the proposal `actionId`, which replaces an earlier vote
      * of the same voter, spec [GV-14] (`proposalsAddVote`,
      * eras/conway/impl/src/Cardano/Ledger/Conway/Governance/Proposals.hs:410-430). A committee
      * vote is keyed by the hot credential and a pool vote by the pool id. An unknown `actionId`
      * leaves the state unchanged.
      */
    def withVote(voter: Voter, actionId: GovActionId, vote: Vote): GovState =
        copy(proposals = proposals.map { p =>
            if p.id != actionId then p
            else
                voter match
                    case Voter.ConstitutionalCommitteeHotKey(hash) =>
                        p.copy(committeeVotes =
                            p.committeeVotes.updated(Credential.KeyHash(hash), vote)
                        )
                    case Voter.ConstitutionalCommitteeHotScript(hash) =>
                        p.copy(committeeVotes =
                            p.committeeVotes.updated(Credential.ScriptHash(hash), vote)
                        )
                    case Voter.DRepKey(hash) =>
                        p.copy(drepVotes = p.drepVotes.updated(Credential.KeyHash(hash), vote))
                    case Voter.DRepScript(hash) =>
                        p.copy(drepVotes = p.drepVotes.updated(Credential.ScriptHash(hash), vote))
                    case Voter.StakingPoolKey(hash) =>
                        p.copy(spoVotes =
                            p.spoVotes.updated(PoolKeyHash.fromByteString(hash), vote)
                        )
        })

    /** This state without the votes of `dreps` on any proposal, spec [GV-15]
      * (eras/conway/impl/src/Cardano/Ledger/Conway/Rules/Gov.hs:622-639).
      */
    def withoutDRepVotes(dreps: Set[Credential]): GovState =
        if dreps.isEmpty then this
        else copy(proposals = proposals.map(p => p.copy(drepVotes = p.drepVotes -- dreps)))
}

object GovState {

    // The constitution of a genesis that seeds none: an empty anchor and no guardrails script,
    // spec [ES-14].
    private val emptyConstitution: Constitution =
        Constitution(
          Anchor("", DataHash.fromByteString(ByteString.fromArray(new Array[Byte](32)))),
          None
        )

    /** No proposal, no enacted action, no committee (spec [ES-13]) and a constitution with an empty
      * anchor and no guardrails script (spec [ES-14]).
      */
    val empty: GovState = GovState(Vector.empty, GovRoots.empty, None, emptyConstitution)
}

/** The purpose of a gov action that names a parent, the ledger's `GovActionPurpose`
  * (eras/conway/impl/src/Cardano/Ledger/Conway/Governance/Procedures.hs:782-790). The actions of
  * one purpose form a chain from the purpose's root, spec [GV-6].
  */
enum GovActionPurpose {
    case PParamUpdate, HardFork, Committee, Constitution
}

object GovActionPurpose {

    /** The purpose of `action`: NoConfidence and UpdateCommittee share `Committee`;
      * TreasuryWithdrawals and InfoAction have none.
      */
    def of(action: GovAction): Option[GovActionPurpose] = action match
        case _: GovAction.ParameterChange    => Some(PParamUpdate)
        case _: GovAction.HardForkInitiation => Some(HardFork)
        case _: GovAction.NoConfidence       => Some(Committee)
        case _: GovAction.UpdateCommittee    => Some(Committee)
        case _: GovAction.NewConstitution    => Some(Constitution)
        case _                               => None

    /** The parent `action` names, `None` for an action without a purpose. */
    private[ledger] def parent(action: GovAction): Option[GovActionId] = action match
        case a: GovAction.ParameterChange    => a.prevActionId
        case a: GovAction.HardForkInitiation => a.prevActionId
        case a: GovAction.NoConfidence       => a.prevActionId
        case a: GovAction.UpdateCommittee    => a.prevActionId
        case a: GovAction.NewConstitution    => a.prevActionId
        case _                               => None
}

/** A failure of the Conway GOV rule, one case per constructor of the ledger's
  * `ConwayGovPredFailure` (eras/conway/impl/src/Cardano/Ledger/Conway/Rules/Gov.hs:180-221), spec
  * [GV-19], D31. The case name is the ledger name. `ProposalDepositIncorrect` keeps its own
  * exception, and the two PV 9 bootstrap failures are not emulated.
  */
enum GovFailure {

    /** Votes on actions that are not active. */
    case GovActionsDoNotExist(actionIds: List[GovActionId])

    /** A ParameterChange whose update is not well formed, spec [GV-7]. */
    case MalformedProposal(action: GovAction)

    /** A deposit return account on another network. */
    case ProposalProcedureNetworkIdMismatch(account: RewardAccount, network: Network)

    /** Treasury withdrawals to accounts on another network. */
    case TreasuryWithdrawalsNetworkIdMismatch(accounts: Set[RewardAccount], network: Network)

    /** Votes by voters that may not vote on the action, spec [GV-11]. */
    case DisallowedVoters(votes: List[(Voter, GovActionId)])

    /** An UpdateCommittee that adds and removes the same cold credentials, spec [GV-18]. */
    case ConflictingCommitteeUpdate(credentials: Set[Credential])

    /** An UpdateCommittee that adds members whose term ends no later than the current epoch, spec
      * [GV-18].
      */
    case ExpirationEpochTooSmall(members: Map[Credential, EpochNo])

    /** A proposal whose parent is not the root of its purpose or an active proposal of it, spec
      * [GV-6].
      */
    case InvalidPrevGovActionId(proposal: ProposalProcedure)

    /** Votes on actions that expired before the current epoch. */
    case VotingOnExpiredGovAction(votes: List[(Voter, GovActionId)])

    /** A HardForkInitiation whose version cannot follow `expected`, spec [GV-17]. */
    case ProposalCantFollow(
        prevActionId: Option[GovActionId],
        supplied: ProtocolVersion,
        expected: ProtocolVersion
    )

    /** A guardrails script hash that differs from the constitution's, spec [GV-8]. */
    case InvalidGuardrailsScriptHash(supplied: Option[ScriptHash], expected: Option[ScriptHash])

    /** Voters that are not registered, spec [GV-10a]. */
    case VotersDoNotExist(voters: List[Voter])

    /** A TreasuryWithdrawals whose withdrawals sum to zero. */
    case ZeroTreasuryWithdrawals(action: GovAction)

    /** A deposit return account that is not registered, spec [GV-3b]. */
    case ProposalReturnAccountDoesNotExist(account: RewardAccount)

    /** Treasury withdrawals to accounts that are not registered, spec [GV-3b]. */
    case TreasuryWithdrawalReturnAccountsDoNotExist(accounts: List[RewardAccount])

    /** At PV >= 11, committee votes by hot credentials that no member of the elected committee
      * authorized, spec [GV-12].
      */
    case UnelectedCommitteeVoters(credentials: Set[Credential])
}
