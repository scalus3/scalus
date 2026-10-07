package scalus.cardano.ledger
package rules

import scala.collection.immutable.SortedMap

// It's the Conway CERTS state transition in cardano-ledger: the withdrawals, then one CERT step
// (DELEG, POOL or GOVCERT) per certificate, in the order of the tx
object CertsMutator extends STS.Mutator {
    override final type Error = TransactionException

    /** Subtracts the withdrawals from the accounts, then applies the certificates one by one, in
      * the order of the tx, spec [SC-22]. A phase-2-failed tx applies none, spec [SC-3b], [SC-3e].
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
                    val drained =
                        state.copy(certState =
                            state.certState.copy(dstate = DelegationState(accounts))
                        )
                    applyCertificates(context, drained, event)
    }

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
                    acc.flatMap(applyCertificate(context, event.id, _, cert))
                }
            yield state.copy(certState = certState)
    }

    /** The CERT step for one certificate. Each kind changes only its part of the state. */
    private def applyCertificate(
        context: Context,
        txId: TransactionHash,
        certState: CertState,
        cert: Certificate
    ): Either[TransactionException.DRepException, CertState] = {
        val params = context.env.params
        val dstate = deleg(Coin(params.stakeAddressDeposit))(certState.dstate, cert)
        val pstate = pool(Coin(params.stakePoolDeposit))(certState.pstate, cert)
        govCert(context, txId)((certState.vstate.dreps, dstate.accounts), cert).map {
            (dreps, accounts) =>
                CertState(
                  vstate = certState.vstate.copy(dreps = dreps),
                  pstate = pstate,
                  dstate = DelegationState(accounts)
                )
        }
    }

    /** The DELEG step: a stake certificate changes the accounts. [[StakeCertificatesValidator]] has
      * checked it.
      */
    private[rules] def deleg(
        defaultDeposit: Coin
    )(state: DelegationState, cert: Certificate): DelegationState = cert match
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
            DelegationState(state.accounts - credential)
        case Certificate.StakeDelegation(credential, poolId) =>
            delegateStake(state, credential, poolId)
        case Certificate.VoteDelegCert(credential, drep) =>
            delegateVote(state, credential, drep)
        case Certificate.StakeVoteDelegCert(credential, poolId, drep) =>
            delegateVote(delegateStake(state, credential, poolId), credential, drep)
        case _ => state

    private def register(
        state: DelegationState,
        credential: Credential,
        deposit: Coin
    ): DelegationState =
        DelegationState(
          state.accounts.updated(credential, ConwayAccountState(Coin.zero, deposit, None, None))
        )

    private def delegateStake(
        state: DelegationState,
        credential: Credential,
        pool: PoolKeyHash
    ): DelegationState =
        updateAccount(state, credential)(_.copy(stakePoolDelegation = Some(pool)))

    private def delegateVote(
        state: DelegationState,
        credential: Credential,
        drep: DRep
    ): DelegationState =
        updateAccount(state, credential)(_.copy(dRepDelegation = Some(drep)))

    /** The validator has checked that the account is registered. */
    private def updateAccount(state: DelegationState, credential: Credential)(
        f: ConwayAccountState => ConwayAccountState
    ): DelegationState =
        DelegationState(state.accounts.updatedWith(credential)(_.map(f)))

    /** The POOL step: a pool certificate changes the pools. [[StakePoolCertificatesValidator]] has
      * checked it.
      */
    private[rules] def pool(poolDeposit: Coin)(pstate: PoolsState, cert: Certificate): PoolsState =
        cert match
            case registration: Certificate.PoolRegistration =>
                val poolId = PoolKeyHash.fromByteString(registration.operator)
                if pstate.stakePools.contains(poolId) then
                    // Re-registration: the new params take effect at the next epoch boundary,
                    // and a pending retirement is cancelled. The deposit does not change.
                    pstate.copy(
                      futureStakePoolParams =
                          pstate.futureStakePoolParams.updated(poolId, registration),
                      retiring = pstate.retiring - poolId
                    )
                else
                    pstate.copy(
                      stakePools = pstate.stakePools.updated(poolId, registration),
                      deposits = pstate.deposits.updated(poolId, poolDeposit)
                    )
            case Certificate.PoolRetirement(poolId, epochNo) =>
                // Schedule the pool for retirement. The actual removal and deposit refund
                // happen at the epoch boundary (POOLREAP), which the emulator does not process.
                pstate.copy(retiring = pstate.retiring.updated(poolId, epochNo))
            case _ => pstate

    /** The GOVCERT step for DRep certificates: checks one against the DReps and changes the DReps
      * and, for a deregistration, the accounts. Committee certificates are accepted but not
      * tracked: the emulator does not model committee state yet.
      */
    private[rules] def govCert(context: Context, txId: TransactionHash)(
        state: (Map[Credential, DRepState], Map[Credential, ConwayAccountState]),
        cert: Certificate
    ): Either[
      TransactionException.DRepException,
      (Map[Credential, DRepState], Map[Credential, ConwayAccountState])
    ] = {
        val (dreps, accounts) = state
        val params = context.env.params
        val expectedDeposit = Coin(params.dRepDeposit)
        val expiry = context.slotConfig.epochOf(context.env.slot) + params.dRepActivity

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
                      dreps.updated(credential, DRepState(expiry, anchor, deposit, Set.empty)) ->
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

    /** `accounts` with no vote delegation to the DRep `credential`, spec [SC-21].
      *
      * Haskell clears the accounts in the DRep's `drepDelegs`. No rule here writes
      * `DRepState.delegates`, so this finds them by their `dRepDelegation` instead.
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
