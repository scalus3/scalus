package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.TxBalance

import scala.collection.immutable.SortedMap

// It's the Conway CERTS state transition in cardano-ledger: the withdrawals, then one CERT step
// (DELEG, POOL or GOVCERT) per certificate, in the order of the tx
object CertsMutator extends STS.Mutator {
    override final type Error = TransactionException

    /** Subtracts the withdrawals from the accounts, then applies the certificates one by one, in
      * the order of the tx, spec [SC-22]. A phase-2-failed tx applies none, spec [SC-3b], [SC-3e].
      *
      * Also changes `deposited` by the deposits and refunds of the tx, computed against the cert
      * state before CERTS, which only this mutator holds, spec [BK-1], [BK-2], D16. It runs for a
      * tx with no certificate too: proposal deposits count.
      *
      * Before the cert loop it runs the dormant bump and then the voting-DRep expiry bump, spec
      * [DR-2], [DR-4], [DR-4a]. The ledger runs them with the withdrawal drain, before CERTS
      * (eras/conway/impl/src/Cardano/Ledger/Conway/Rules/Ledger.hs:413-421 at PV > 10, and the
      * empty-certificates base case of Rules/Certs.hs:228-245 before).
      */
    override def transit(context: Context, state: State, event: Event): Result = {
        if !event.isValid then success(state)
        else
            CertsValidator.validate(context, state, event) match
                case Left(err) => failure(err)
                case Right(_) =>
                    val withdrawals: SortedMap[RewardAccount, Coin] =
                        event.body.value.withdrawals.getOrElse(Withdrawals.empty).withdrawals
                    val accounts =
                        CertsValidator.applyWithdrawals(
                          state.certState.dstate.accounts,
                          withdrawals
                        )
                    // S/Rules/Utxo.hs:607-608: deposited + deposits - refunds, before CERTS
                    val depositChange =
                        TxBalance.depositChange(event, state.certState, context.env.params)
                    val epoch = context.slotConfig.epochOf(context.env.slot)
                    val vstate = votingDRepExpiries(
                      event,
                      epoch,
                      context.env.params.dRepActivity,
                      dormantDRepExpiries(event, epoch, state.certState.vstate)
                    )
                    val drained = state.copy(
                      certState = state.certState.copy(
                        vstate = vstate,
                        dstate = DelegationState(accounts)
                      ),
                      deposited = Coin((BigInt(state.deposited.value) + depositChange).toLong)
                    )
                    applyCertificates(context, drained, event)
    }

    /** `updateDormantDRepExpiries` (C/Rules/Certs.hs:262-272, 313-333): a tx with a proposal adds
      * `numDormantEpochs` to the expiry of every DRep that is still active with it, then resets the
      * counter, spec [DR-2], [DR-3].
      */
    private def dormantDRepExpiries(event: Event, epoch: Long, vstate: VotingState): VotingState = {
        val dormant = vstate.numDormantEpochs
        if event.body.value.proposalProcedures.toSeq.isEmpty || dormant == 0 then vstate
        else
            vstate.copy(
              dreps = vstate.dreps.view.mapValues { drep =>
                  val bumped = drep.expiry + dormant
                  if bumped < epoch then drep else drep.copy(expiry = bumped)
              }.toMap,
              numDormantEpochs = 0L
            )
    }

    /** `updateVotingDRepExpiries` (C/Rules/Certs.hs:277-298): each registered DRep that votes in
      * the tx gets the expiry of spec [DR-1]; an unregistered one is skipped (`Map.adjust`), spec
      * [DR-4].
      */
    private def votingDRepExpiries(
        event: Event,
        epoch: Long,
        dRepActivity: Long,
        vstate: VotingState
    ): VotingState = {
        val voters = event.body.value.votingProcedures.toSeq.flatMap(_.procedures.keys)
        val expiry = drepExpiry(epoch, dRepActivity, vstate.numDormantEpochs)
        def adjust(dreps: Map[Credential, DRepState], credential: Credential) =
            dreps.updatedWith(credential)(_.map(_.copy(expiry = expiry)))
        val dreps = voters.foldLeft(vstate.dreps) { (dreps, voter) =>
            voter match
                case Voter.DRepKey(hash)    => adjust(dreps, Credential.KeyHash(hash))
                case Voter.DRepScript(hash) => adjust(dreps, Credential.ScriptHash(hash))
                case _                      => dreps
        }
        vstate.copy(dreps = dreps)
    }

    /** `computeDRepExpiry` (C/Rules/GovCert.hs:297-310), spec [DR-1]. */
    private def drepExpiry(epoch: Long, dRepActivity: Long, numDormantEpochs: Long): Long =
        epoch + dRepActivity - numDormantEpochs

    private def applyCertificates(context: Context, state: State, event: Event): Result = {
        val certificates = event.body.value.certificates.toSeq
        if certificates.isEmpty then success(state)
        else
            for
                _ <- StakeCertificatesValidator.validateAfterWithdrawals(context, state, event)
                _ <- StakePoolCertificatesValidator.validate(context, state, event)
                certState <- certificates.foldLeft[Either[Error, CertState]](
                  Right(state.certState)
                ) { (acc, cert) =>
                    acc.flatMap(applyCertificate(context, event.id, state.govState, _, cert))
                }
            yield state.copy(certState = certState)
    }

    /** The CERT step for one certificate. Each kind changes only its part of the state. `govState`
      * is the gov state before the tx: CERTS runs before GOV, spec [GC-2a].
      */
    private def applyCertificate(
        context: Context,
        txId: TransactionHash,
        govState: GovState,
        certState: CertState,
        cert: Certificate
    ): Either[TransactionException, CertState] = {
        val params = context.env.params
        val delegated = deleg(Coin(params.stakeAddressDeposit))(certState, cert)
        for
            pstate <- pool(Coin(params.stakePoolDeposit), params.protocolVersion.major, txId)(
              delegated.pstate,
              cert
            )
            votes <- govCert(context, txId, delegated.vstate.numDormantEpochs)(
              (delegated.vstate.dreps, delegated.dstate.accounts),
              cert
            )
            committeeState <- committeeCert(txId, govState)(delegated.vstate.committeeState, cert)
        yield CertState(
          vstate = delegated.vstate.copy(dreps = votes._1, committeeState = committeeState),
          pstate = pstate,
          dstate = DelegationState(votes._2)
        )
    }

    /** The DELEG step: a stake certificate changes the accounts and the delegates of the DReps.
      * [[StakeCertificatesValidator]] has checked it.
      *
      * `delegates` follows eras/conway/impl/src/Cardano/Ledger/Conway/Rules/Deleg.hs:236-373 and
      * C/State/VState.hs:120-143, spec [BK-3]. The PV9 bug that keeps the old reverse delegation
      * (ledger issue 4772) is not emulated, D20.
      */
    private[rules] def deleg(
        defaultDeposit: Coin
    )(state: CertState, cert: Certificate): CertState = cert match
        case Certificate.RegCert(credential, maybeDeposit) =>
            register(state, credential, maybeDeposit.getOrElse(defaultDeposit))
        case Certificate.StakeRegDelegCert(credential, poolId, deposit) =>
            delegateStake(register(state, credential, deposit), credential, poolId)
        case Certificate.VoteRegDelegCert(credential, drep, deposit) =>
            delegateVote(register(state, credential, deposit), credential, drep)
        case Certificate.StakeVoteRegDelegCert(credential, poolId, drep, deposit) =>
            val registered = register(state, credential, deposit)
            delegateVote(delegateStake(registered, credential, poolId), credential, drep)
        case Certificate.UnregCert(credential, _) =>
            // Deleg.hs:277-282: unDelegReDelegDRep with no new DRep
            val oldDRep = state.dstate.accounts.get(credential).flatMap(_.dRepDelegation)
            state.copy(
              vstate = state.vstate.copy(dreps = moveDelegate(state, credential, oldDRep, None)),
              dstate = DelegationState(state.dstate.accounts - credential)
            )
        case Certificate.StakeDelegation(credential, poolId) =>
            delegateStake(state, credential, poolId)
        case Certificate.VoteDelegCert(credential, drep) =>
            delegateVote(state, credential, drep)
        case Certificate.StakeVoteDelegCert(credential, poolId, drep) =>
            delegateVote(delegateStake(state, credential, poolId), credential, drep)
        case _ => state

    private def register(state: CertState, credential: Credential, deposit: Coin): CertState =
        state.copy(dstate =
            DelegationState(
              state.dstate.accounts
                  .updated(credential, ConwayAccountState(Coin.zero, deposit, None, None))
            )
        )

    private def delegateStake(
        state: CertState,
        credential: Credential,
        pool: PoolKeyHash
    ): CertState =
        updateAccount(state, credential)(_.copy(stakePoolDelegation = Some(pool)))

    /** Deleg.hs:342-373: the account delegates to `drep`, and moves from the `delegates` of its old
      * DRep to those of `drep`.
      */
    private def delegateVote(state: CertState, credential: Credential, drep: DRep): CertState = {
        val oldDRep = state.dstate.accounts.get(credential).flatMap(_.dRepDelegation)
        val dreps = moveDelegate(state, credential, oldDRep, Some(drep))
        updateAccount(state.copy(vstate = state.vstate.copy(dreps = dreps)), credential)(
          _.copy(dRepDelegation = Some(drep))
        )
    }

    /** `unDelegReDelegDRep` of C/State/VState.hs:120-143: removes `credential` from the delegates
      * of `oldDRep` and adds it to those of `newDRep`, unless they are equal. Only a credential
      * DRep has delegates, and an unregistered one is left alone (`Map.adjust`).
      */
    private def moveDelegate(
        state: CertState,
        credential: Credential,
        oldDRep: Option[DRep],
        newDRep: Option[DRep]
    ): Map[Credential, DRepState] = {
        def adjust(
            dreps: Map[Credential, DRepState],
            drep: Option[DRep]
        )(f: Set[Credential] => Set[Credential]) =
            drep.flatMap(drepCredential) match
                case Some(cred) =>
                    dreps.updatedWith(cred)(_.map(d => d.copy(delegates = f(d.delegates))))
                case None => dreps
        val dreps = state.vstate.dreps
        if oldDRep == newDRep then dreps
        else adjust(adjust(dreps, oldDRep)(_ - credential), newDRep)(_ + credential)
    }

    private def drepCredential(drep: DRep): Option[Credential] = drep match
        case DRep.KeyHash(hash)    => Some(Credential.KeyHash(hash))
        case DRep.ScriptHash(hash) => Some(Credential.ScriptHash(hash))
        case _                     => None

    /** The validator has checked that the account is registered. */
    private def updateAccount(state: CertState, credential: Credential)(
        f: ConwayAccountState => ConwayAccountState
    ): CertState =
        state.copy(dstate =
            DelegationState(state.dstate.accounts.updatedWith(credential)(_.map(f)))
        )

    /** The POOL step: a pool certificate changes the pools. [[StakePoolCertificatesValidator]] has
      * checked it, except for the VRF key, which depends on the earlier certificates of the tx,
      * spec [BK-8].
      *
      * At PV >= 11 a registration fails on a VRF key of another pool and keeps `vrfKeyHashes` as
      * eras/shelley/impl/src/Cardano/Ledger/Shelley/Rules/Pool.hs:266-310 does, spec [BK-4] to
      * [BK-6]. POOLREAP removes the VRF keys of retired pools (S/Rules/PoolReap.hs:225-228); the
      * Emulator has no POOLREAP before ledger-rules phase 3.
      */
    private[rules] def pool(poolDeposit: Coin, protocolMajor: Int, txId: TransactionHash)(
        pstate: PoolsState,
        cert: Certificate
    ): Either[TransactionException.VrfKeyHashAlreadyRegisteredException, PoolsState] =
        cert match
            case registration: Certificate.PoolRegistration =>
                val poolId = PoolKeyHash.fromByteString(registration.operator)
                val vrf = registration.vrfKeyHash
                val vrfChecked = protocolMajor >= 11 // hardforkConwayDisallowDuplicatedVRFKeys
                val vrfKeyHashes = pstate.vrfKeyHashes
                def alreadyRegistered =
                    Left(
                      TransactionException.VrfKeyHashAlreadyRegisteredException(txId, poolId, vrf)
                    )
                pstate.stakePools.get(poolId) match
                    case Some(current) =>
                        if vrfChecked && vrf != current.vrfKeyHash && vrfKeyHashes.contains(vrf)
                        then alreadyRegistered
                        else
                            // Pool.hs:288-299: record the new VRF key and drop the one of an
                            // earlier re-registration in this epoch
                            val updatedVrfKeyHashes =
                                if !vrfChecked then vrfKeyHashes
                                else
                                    pstate.futureStakePoolParams.get(poolId) match
                                        case None => vrfKeyHashes.updated(vrf, 1L)
                                        case Some(future) if future.vrfKeyHash != vrf =>
                                            vrfKeyHashes.updated(vrf, 1L) - future.vrfKeyHash
                                        case Some(_) => vrfKeyHashes
                            // Re-registration: the new params take effect at the next epoch
                            // boundary, and a pending retirement is cancelled. The deposit does
                            // not change.
                            Right(
                              pstate.copy(
                                futureStakePoolParams =
                                    pstate.futureStakePoolParams.updated(poolId, registration),
                                retiring = pstate.retiring - poolId,
                                vrfKeyHashes = updatedVrfKeyHashes
                              )
                            )
                    case None =>
                        if vrfChecked && vrfKeyHashes.contains(vrf) then alreadyRegistered
                        else
                            Right(
                              pstate.copy(
                                stakePools = pstate.stakePools.updated(poolId, registration),
                                deposits = pstate.deposits.updated(poolId, poolDeposit),
                                // Pool.hs:272-274: an overwrite with 1, not a count
                                vrfKeyHashes =
                                    if vrfChecked then vrfKeyHashes.updated(vrf, 1L)
                                    else vrfKeyHashes
                              )
                            )
            case Certificate.PoolRetirement(poolId, epochNo) =>
                // Schedule the pool for retirement. The actual removal and deposit refund
                // happen at the epoch boundary (POOLREAP), which the emulator does not process.
                Right(pstate.copy(retiring = pstate.retiring.updated(poolId, epochNo)))
            case _ => Right(pstate)

    /** The GOVCERT step for DRep certificates: checks one against the DReps and changes the DReps
      * and, for a deregistration, the accounts. [[committeeCert]] is the step for committee
      * certificates.
      */
    private[rules] def govCert(context: Context, txId: TransactionHash, numDormantEpochs: Long)(
        state: (Map[Credential, DRepState], Map[Credential, ConwayAccountState]),
        cert: Certificate
    ): Either[
      TransactionException.DRepException,
      (Map[Credential, DRepState], Map[Credential, ConwayAccountState])
    ] = {
        val (dreps, accounts) = state
        val params = context.env.params
        val expectedDeposit = Coin(params.dRepDeposit)
        val epoch = context.slotConfig.epochOf(context.env.slot)
        // spec [DR-1]; [DR-1a]: at PV 9 a registration skips the dormant subtraction
        // (computeDRepExpiryVersioned, C/Rules/GovCert.hs:281-295), an update never does
        val expiry = drepExpiry(epoch, params.dRepActivity, numDormantEpochs)
        val registrationExpiry =
            if params.protocolVersion.major == 9 then epoch + params.dRepActivity else expiry

        def fail(
            alreadyRegistered: Set[Credential] = Set.empty,
            notRegistered: Set[Credential] = Set.empty,
            invalidDeposits: Map[Credential, (Coin, Coin)] = Map.empty,
            invalidRefunds: Map[Credential, (Coin, Coin)] = Map.empty
        ) = Left(
          TransactionException.DRepException(
            txId,
            alreadyRegistered,
            notRegistered,
            invalidDeposits,
            invalidRefunds
          )
        )

        cert match
            case Certificate.RegDRepCert(credential, deposit, anchor) =>
                if dreps.contains(credential) then fail(alreadyRegistered = Set(credential))
                else if deposit != expectedDeposit then
                    fail(invalidDeposits = Map(credential -> (expectedDeposit, deposit)))
                else
                    Right(
                      dreps.updated(
                        credential,
                        DRepState(registrationExpiry, anchor, deposit, Set.empty)
                      ) ->
                          accounts
                    )
            case Certificate.UnregDRepCert(credential, refund) =>
                dreps.get(credential) match
                    case None => fail(notRegistered = Set(credential))
                    case Some(drepState) if drepState.deposit != refund =>
                        fail(invalidRefunds = Map(credential -> (drepState.deposit, refund)))
                    case Some(_) =>
                        Right(dreps - credential -> clearDelegations(accounts, credential))
            case Certificate.UpdateDRepCert(credential, anchor) =>
                dreps.get(credential) match
                    case None => fail(notRegistered = Set(credential))
                    case Some(drepState) =>
                        Right(
                          dreps.updated(
                            credential,
                            drepState.copy(expiry = expiry, anchor = anchor)
                          ) -> accounts
                        )
            case _ => Right(state)
    }

    /** The GOVCERT step for committee certificates, `checkAndOverwriteCommitteeMemberState`
      * (eras/conway/impl/src/Cardano/Ledger/Conway/Rules/GovCert.hs:193-211, 276-279): a cold
      * credential that resigned is rejected first, spec [GC-1], [GC-4]; then one that is neither a
      * member of the committee nor added by a pending `UpdateCommittee`, spec [GC-2]; else its
      * state becomes the new authorization, spec [GC-3].
      *
      * `govState` is the state before the tx, so an `UpdateCommittee` of the same tx does not
      * count, spec [GC-2a] (`committeeProposals`, C/Rules/Ledger.hs:397-400).
      */
    private[rules] def committeeCert(txId: TransactionHash, govState: GovState)(
        committeeState: Map[Credential, CommitteeAuthorization],
        cert: Certificate
    ): Either[TransactionException, Map[Credential, CommitteeAuthorization]] = {
        def overwrite(cold: Credential, authorization: CommitteeAuthorization) = {
            def isCurrentMember = govState.committee.exists(_.members.contains(cold))
            def isPotentialFutureMember = govState.proposals.exists(_.action match
                case GovAction.UpdateCommittee(_, _, added, _) => added.contains(cold)
                case _                                         => false)
            committeeState.get(cold) match
                case Some(CommitteeAuthorization.Resigned(_)) =>
                    Left(TransactionException.CommitteeHasPreviouslyResignedException(txId, cold))
                case _ if !isCurrentMember && !isPotentialFutureMember =>
                    Left(TransactionException.CommitteeIsUnknownException(txId, cold))
                case _ => Right(committeeState.updated(cold, authorization))
        }
        cert match
            case Certificate.AuthCommitteeHotCert(cold, hot) =>
                overwrite(cold, CommitteeAuthorization.HotCredential(hot))
            case Certificate.ResignCommitteeColdCert(cold, anchor) =>
                overwrite(cold, CommitteeAuthorization.Resigned(anchor))
            case _ => Right(committeeState)
    }

    /** `accounts` with no vote delegation to the DRep `credential`, spec [SC-21].
      *
      * Haskell clears the accounts in the DRep's `drepDelegs` (C/Rules/GovCert.hs:237-257). This
      * finds them by their `dRepDelegation` instead, which is the same set when `delegates` is
      * consistent, D21.
      */
    private def clearDelegations(
        accounts: Map[Credential, ConwayAccountState],
        credential: Credential
    ): Map[Credential, ConwayAccountState] = {
        val drep = credential match
            case Credential.KeyHash(hash)    => DRep.KeyHash(hash)
            case Credential.ScriptHash(hash) => DRep.ScriptHash(hash)
        accounts.transform { (_, account) =>
            if account.dRepDelegation.contains(drep) then account.copy(dRepDelegation = None)
            else account
        }
    }
}
