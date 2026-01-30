package scalus.cardano.ledger
package rules

// It's the Conway DELEG state transition in cardano-ledger
object StakeCertificatesMutator extends STS.Mutator {
    override final type Error = TransactionException.StakeCertificatesException

    override def transit(context: Context, state: State, event: Event): Result = {
        val certificates = event.body.value.certificates.toSeq
        if certificates.isEmpty then success(state)
        else
            StakeCertificatesValidator.validate(context, state, event) match
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
        state.copy(
          deposits = state.deposits.updated(credential, deposit),
          rewards = state.rewards.updated(credential, Coin.zero)
        )

    private def deregister(state: DelegationState, credential: Credential): DelegationState =
        state.copy(
          deposits = state.deposits - credential,
          rewards = state.rewards - credential,
          stakePools = state.stakePools - credential,
          dreps = state.dreps - credential
        )

    private def delegateStake(
        state: DelegationState,
        credential: Credential,
        pool: PoolKeyHash
    ): DelegationState =
        state.copy(stakePools = state.stakePools.updated(credential, pool))

    private def delegateVote(
        state: DelegationState,
        credential: Credential,
        drep: DRep
    ): DelegationState =
        state.copy(dreps = state.dreps.updated(credential, drep))
}
