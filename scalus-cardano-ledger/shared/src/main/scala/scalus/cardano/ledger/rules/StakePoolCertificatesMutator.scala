package scalus.cardano.ledger
package rules

// It's the Shelley/Conway POOL state transition in cardano-ledger
object StakePoolCertificatesMutator extends STS.Mutator {
    override final type Error = TransactionException.StakePoolException

    override def transit(context: Context, state: State, event: Event): Result = {
        val certificates = event.body.value.certificates.toSeq
        if certificates.isEmpty then success(state)
        else
            StakePoolCertificatesValidator.validate(context, state, event) match
                case Left(err) => failure(err)
                case Right(_) =>
                    val poolDeposit = Coin(context.env.params.stakePoolDeposit)
                    val updatedPState = certificates.foldLeft(state.certState.pstate) {
                        applyCertificate(poolDeposit)
                    }
                    val updatedCertState = state.certState.copy(pstate = updatedPState)
                    success(state.copy(certState = updatedCertState))
    }

    private def applyCertificate(
        poolDeposit: Coin
    )(pstate: PoolsState, cert: Certificate): PoolsState = cert match
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
}
