package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.GovFailure.*

/** The Conway GOV rule of cardano-ledger (eras/conway/impl/src/Cardano/Ledger/Conway/Rules/
  * Gov.hs:456-643), spec [GV-1] to [GV-20]: checks the proposals and votes of a tx and stores them
  * in `govState`.
  *
  * It runs after [[CertsMutator]], so it sees the accounts, DReps, pools and committee hot keys
  * after CERTS, spec [GV-2], [GV-2a]. It reads the committee, the guardrails hash and the roots
  * from the gov state before the tx, which no earlier mutator writes, spec [GV-20]. The deposit
  * check of GOV is [[ProposalDepositValidator]], D30.
  *
  * The ledger collects every failure of the rule; this mutator reports the first in the ledger's
  * order, spec [GV-3a], [GV-10b]. The PV 9 bootstrap failures are not emulated (spec section 5.8),
  * but the PV 9 exemptions of the other checks are.
  */
object GovMutator extends STS.Mutator {
    override final type Error = TransactionException

    /** A phase-2-failed tx changes no gov state, spec [GV-16]: LEDGER runs GOV only for
      * `isValid = true` (C/Rules/Ledger.hs:391).
      */
    override def transit(context: Context, state: State, event: Event): Result =
        transit(context, state, event, event.id)

    /** [[transit]] with the tx id given. A real tx cannot name its own id, so only tests use a
      * different `txId`, for votes and parents that name a proposal of the same tx.
      */
    private[rules] def transit(
        context: Context,
        state: State,
        event: Event,
        txId: TransactionHash
    ): Result =
        if !event.isValid then success(state)
        else
            gov(context, state, event, txId) match
                case Left(err)       => failure(TransactionException.GovException(txId, err))
                case Right(govState) => success(state.copy(govState = govState))

    private def gov(
        context: Context,
        state: State,
        event: Event,
        txId: TransactionHash
    ): Either[GovFailure, GovState] = {
        val body = event.body.value
        val params = context.env.params
        val major = params.protocolVersion.major
        val epoch = context.slotConfig.epochOf(context.env.slot)
        val before = state.govState
        val votes: List[(Voter, GovActionId, Vote)] =
            body.votingProcedures.toList.flatMap(_.procedures.toList).flatMap { (voter, byId) =>
                byId.toList.map((id, procedure) => (voter, id, procedure.vote))
            }
        val unregisteredDReps: Set[Credential] = body.certificates.toSeq.collect {
            case Certificate.UnregDRepCert(credential, _) => credential
        }.toSet
        val unelected = unelectedCommitteeVoters(before, state.certState, votes)
        for
            // Gov.hs:489-492: hardforkConwayDisallowUnelectedCommitteeFromVoting, PV >= 11
            _ <- check(major < 11 || unelected.isEmpty, UnelectedCommitteeVoters(unelected))
            withProposals <- body.proposalProcedures.toSeq.zipWithIndex.foldLeft[
              Either[GovFailure, GovState]
            ](Right(before)) { case (acc, (procedure, index)) =>
                acc.flatMap(proposals =>
                    addProposal(context, state, epoch, txId, index, procedure, proposals)
                )
            }
            _ <- checkVotes(state.certState, withProposals, epoch, votes)
        yield votes
            .foldLeft(withProposals)((gov, vote) => gov.withVote(vote._1, vote._2, vote._3))
            .withoutDRepVotes(unregisteredDReps)
    }

    /** `processProposal` of Gov.hs:494-578: the checks of one proposal, in the order of spec
      * [GV-3a], then the parent check of [GV-6], which adds it.
      */
    private def addProposal(
        context: Context,
        state: State,
        epoch: Long,
        txId: TransactionHash,
        index: Int,
        procedure: ProposalProcedure,
        proposals: GovState
    ): Either[GovFailure, GovState] = {
        val params = context.env.params
        val pv = params.protocolVersion
        val bootstrap = pv.major == 9 // hardforkConwayBootstrapPhase
        val network = context.env.network
        val accounts = state.certState.dstate.accounts
        val guardrails = state.govState.constitution.scriptHash
        def registered(account: RewardAccount) = accounts.contains(account.address.credential)
        val action = procedure.govAction
        for
            _ <- proposalCantFollow(pv, state.govState, action).toLeft(())
            _ <- check(isWellFormed(pv.major, action), MalformedProposal(action))
            _ <- check(
              bootstrap || registered(procedure.rewardAccount),
              ProposalReturnAccountDoesNotExist(procedure.rewardAccount)
            )
            _ <- action match
                case GovAction.TreasuryWithdrawals(withdrawals, _) if !bootstrap =>
                    val unregistered = withdrawals.keys.filterNot(registered).toList
                    check(
                      unregistered.isEmpty,
                      TreasuryWithdrawalReturnAccountsDoNotExist(unregistered)
                    )
                case _ => Right(())
            _ <- check(
              procedure.rewardAccount.address.network == network,
              ProposalProcedureNetworkIdMismatch(procedure.rewardAccount, network)
            )
            _ <- action match
                case GovAction.TreasuryWithdrawals(withdrawals, policy) =>
                    val mismatched = withdrawals.keySet.filter(_.address.network != network)
                    for
                        _ <- check(
                          mismatched.isEmpty,
                          TreasuryWithdrawalsNetworkIdMismatch(mismatched, network)
                        )
                        _ <- checkGuardrails(guardrails, policy)
                        _ <- check(
                          bootstrap || withdrawals.values.exists(_.value != 0),
                          ZeroTreasuryWithdrawals(action)
                        )
                    yield ()
                case GovAction.UpdateCommittee(_, removed, added, _) =>
                    val conflicting = added.keySet.intersect(removed)
                    val ending = added.filter((_, term) => term <= epoch)
                    for
                        _ <- check(conflicting.isEmpty, ConflictingCommitteeUpdate(conflicting))
                        _ <- check(ending.isEmpty, ExpirationEpochTooSmall(ending))
                    yield ()
                case GovAction.ParameterChange(_, _, policy) => checkGuardrails(guardrails, policy)
                case _                                       => Right(())
            added <- proposals
                .addProposal(
                  GovActionState(
                    GovActionId(txId, index),
                    Map.empty,
                    Map.empty,
                    Map.empty,
                    procedure,
                    epoch,
                    epoch + params.govActionLifetime
                  )
                )
                .toRight(InvalidPrevGovActionId(procedure))
        yield added
    }

    /** `checkGuardrailsScriptHash` of Gov.hs:431-437, spec [GV-8]. */
    private def checkGuardrails(
        expected: Option[ScriptHash],
        supplied: Option[ScriptHash]
    ): Either[GovFailure, Unit] =
        check(supplied == expected, InvalidGuardrailsScriptHash(supplied, expected))

    /** `preceedingHardFork` of Gov.hs:685-707 with `pvCanFollow`
      * (eras/shelley/impl/src/Cardano/Ledger/Shelley/PParams.hs:298-306), spec [GV-17]. The
      * predecessor is looked up in the proposals before the tx only, spec [GV-17a]; a parent that
      * is not a pending hard fork is left to the parent check of [GV-6].
      */
    private def proposalCantFollow(
        current: ProtocolVersion,
        before: GovState,
        action: GovAction
    ): Option[GovFailure] = action match
        case GovAction.HardForkInitiation(prev, supplied) =>
            val predecessor =
                if prev == before.roots.hardFork || supplied.major > current.major + 1 then
                    Some(current)
                else
                    prev.flatMap(before.proposal).map(_.action).collect {
                        case GovAction.HardForkInitiation(_, version) => version
                    }
            predecessor.collect {
                case expected if !canFollow(expected, supplied) =>
                    ProposalCantFollow(prev, supplied, expected)
            }
        case _ => None

    private def canFollow(previous: ProtocolVersion, next: ProtocolVersion): Boolean =
        (next.major == previous.major + 1 && next.minor == 0) ||
            (next.major == previous.major && next.minor == previous.minor + 1)

    /** `actionWellFormed` of Gov.hs:404-410: only a ParameterChange can be malformed. */
    private def isWellFormed(major: Int, action: GovAction): Boolean = action match
        case GovAction.ParameterChange(_, update, _) => ppuWellFormed(major, update)
        case _                                       => true

    /** `ppuWellFormed` of eras/conway/impl/src/Cardano/Ledger/Conway/PParams.hs:934-957, spec
      * [GV-7a], D32.
      */
    private def ppuWellFormed(major: Int, u: ProtocolParamUpdate): Boolean = {
        def nonZero(field: Option[Int]) = !field.contains(0)
        def nonZeroCoin(field: Option[Coin]) = !field.exists(_.value == 0)
        nonZero(u.maxBlockBodySize) && nonZero(u.maxTxSize) && nonZero(u.maxBlockHeaderSize) &&
        nonZero(u.maxValueSize) && nonZero(u.collateralPercentage) &&
        nonZero(u.committeeTermLimit) && nonZero(u.governanceActionValidityPeriod) &&
        nonZeroCoin(u.poolDeposit) && nonZeroCoin(u.governanceActionDeposit) &&
        nonZeroCoin(u.drepDeposit) &&
        (major == 9 || nonZeroCoin(u.adaPerUtxoByte)) && // the PV 9 bootstrap exemption
        u != ProtocolParamUpdate() &&
        (major < 11 || nonZero(u.nOpt))
    }

    /** A ParameterChange changes a parameter of the security group, the `SecurityGroup` fields of
      * `ConwayPParams` (C/PParams.hs:643-708), spec [GV-11a], D32.
      */
    private def changesSecurityGroup(u: ProtocolParamUpdate): Boolean =
        u.minFeeA.isDefined || u.minFeeB.isDefined || u.maxBlockBodySize.isDefined ||
            u.maxTxSize.isDefined || u.maxBlockHeaderSize.isDefined ||
            u.adaPerUtxoByte.isDefined || u.maxBlockExUnits.isDefined ||
            u.maxValueSize.isDefined || u.governanceActionDeposit.isDefined ||
            u.minFeeRefScriptCoinsPerByte.isDefined

    /** The voter permission table of spec section 15 (`isCommitteeVotingAllowed`,
      * `isDRepVotingAllowed`, `isStakePoolVotingAllowed`, C/Governance/Internal.hs:358-540), spec
      * [GV-11]. A DRep can vote on every action.
      */
    private def mayVote(voter: Voter, action: GovAction): Boolean = voter match
        case _: Voter.ConstitutionalCommitteeHotKey | _: Voter.ConstitutionalCommitteeHotScript =>
            action match
                case _: GovAction.NoConfidence | _: GovAction.UpdateCommittee => false
                case _                                                        => true
        case _: Voter.DRepKey | _: Voter.DRepScript => true
        case _: Voter.StakingPoolKey =>
            action match
                case _: GovAction.NewConstitution | _: GovAction.TreasuryWithdrawals => false
                case GovAction.ParameterChange(_, update, _) => changesSecurityGroup(update)
                case _                                       => true

    /** The vote checks of Gov.hs:580-620 after the proposals, in the order of spec [GV-10b]. The
      * action checks run over the votes of known voters only, spec [GV-10].
      */
    private def checkVotes(
        certState: CertState,
        proposals: GovState,
        epoch: Long,
        votes: List[(Voter, GovActionId, Vote)]
    ): Either[GovFailure, Unit] = {
        val hotKeys = authorizedHotKeys(certState.vstate.committeeState)
        val unknownVoters = votes.map(_._1).distinct.filterNot(isKnown(certState, hotKeys, _))
        val known = votes.filterNot(v => unknownVoters.contains(v._1))
        val unknownActions = known.map(_._2).distinct.filter(proposals.proposal(_).isEmpty)
        val withAction =
            known.flatMap((voter, id, _) => proposals.proposal(id).map(voter -> _))
        val expired = withAction.collect {
            case (voter, p) if p.expiresAfter < epoch => voter -> p.id
        }
        val disallowed = withAction.collect {
            case (voter, p) if !mayVote(voter, p.action) => voter -> p.id
        }
        for
            _ <- check(unknownVoters.isEmpty, VotersDoNotExist(unknownVoters))
            _ <- check(unknownActions.isEmpty, GovActionsDoNotExist(unknownActions))
            _ <- check(expired.isEmpty, VotingOnExpiredGovAction(expired))
            _ <- check(disallowed.isEmpty, DisallowedVoters(disallowed))
        yield ()
    }

    /** spec [GV-10a]: a committee voter is known when any cold credential authorized its hot
      * credential, whether or not the cold credential is a member (Gov.hs:477, 607).
      */
    private def isKnown(certState: CertState, hotKeys: Set[Credential], voter: Voter): Boolean =
        voter match
            case Voter.ConstitutionalCommitteeHotKey(hash) =>
                hotKeys.contains(Credential.KeyHash(hash))
            case Voter.ConstitutionalCommitteeHotScript(hash) =>
                hotKeys.contains(Credential.ScriptHash(hash))
            case Voter.DRepKey(hash) => certState.vstate.dreps.contains(Credential.KeyHash(hash))
            case Voter.DRepScript(hash) =>
                certState.vstate.dreps.contains(Credential.ScriptHash(hash))
            case Voter.StakingPoolKey(hash) =>
                certState.pstate.stakePools.contains(PoolKeyHash.fromByteString(hash))

    /** `authorizedHotCommitteeCredentials`
      * (libs/cardano-ledger-core/src/Cardano/Ledger/State/CertState.hs:338-343).
      */
    private def authorizedHotKeys(
        committeeState: Map[Credential, CommitteeAuthorization]
    ): Set[Credential] =
        committeeState.values.collect { case CommitteeAuthorization.HotCredential(hot) =>
            hot
        }.toSet

    /** `unelectedCommitteeVoters` of Gov.hs:664-677, spec [GV-12], [GV-13]: the committee hot
      * credentials of `votes` that no member of the committee before the tx authorized. The term of
      * a member is not checked.
      */
    private def unelectedCommitteeVoters(
        before: GovState,
        certState: CertState,
        votes: List[(Voter, GovActionId, Vote)]
    ): Set[Credential] = {
        val members = before.committee.map(_.members.keySet).getOrElse(Set.empty)
        val elected =
            authorizedHotKeys(certState.vstate.committeeState.filter((cold, _) => members(cold)))
        votes.flatMap {
            case (Voter.ConstitutionalCommitteeHotKey(hash), _, _) =>
                Some(Credential.KeyHash(hash))
            case (Voter.ConstitutionalCommitteeHotScript(hash), _, _) =>
                Some(Credential.ScriptHash(hash))
            case _ => None
        }.toSet -- elected
    }

    /** `Right(())` when `ok` holds, else `failure`. */
    private def check(ok: Boolean, failure: => GovFailure): Either[GovFailure, Unit] =
        if ok then Right(()) else Left(failure)
}
