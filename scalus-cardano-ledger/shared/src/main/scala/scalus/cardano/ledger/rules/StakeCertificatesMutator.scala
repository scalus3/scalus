package scalus.cardano.ledger
package rules

// It's the Conway DELEG state transition in cardano-ledger
object StakeCertificatesMutator extends STS.Mutator {
    override final type Error = TransactionException.StakeCertificatesException

    /** A phase-2-failed tx applies no certificates, spec [SC-3e]. */
    override def transit(context: Context, state: State, event: Event): Result = {
        val certificates = event.body.value.certificates.toSeq
        if certificates.isEmpty || !event.isValid then success(state)
        else
            StakeCertificatesValidator.validateAfterWithdrawals(context, state, event) match
                case Left(err) => failure(err)
                case Right(_) =>
                    val defaultDeposit = Coin(context.env.params.stakeAddressDeposit)
                    val updatedDState =
                        applyCertificates(state.certState.dstate, defaultDeposit, certificates)
                    val updatedCertState = state.certState.copy(dstate = updatedDState)
                    success(state.copy(certState = updatedCertState))
    }

    private def applyCertificates(
        dstate: DelegationState,
        defaultDeposit: Coin,
        certificates: Seq[Certificate]
    ): DelegationState =
        certificates.foldLeft(dstate) { (state, cert) =>
            cert match
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
                    deregister(state, credential)
                case Certificate.StakeDelegation(credential, poolId) =>
                    delegateStake(state, credential, poolId)
                case Certificate.VoteDelegCert(credential, drep) =>
                    delegateVote(state, credential, drep)
                case Certificate.StakeVoteDelegCert(credential, poolId, drep) =>
                    delegateVote(delegateStake(state, credential, poolId), credential, drep)
                case _ => state
        }

    private def register(
        state: DelegationState,
        credential: Credential,
        deposit: Coin
    ): DelegationState =
        DelegationState(
          state.accounts.updated(credential, ConwayAccountState(Coin.zero, deposit, None, None))
        )

    private def deregister(state: DelegationState, credential: Credential): DelegationState =
        DelegationState(state.accounts - credential)

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
}
