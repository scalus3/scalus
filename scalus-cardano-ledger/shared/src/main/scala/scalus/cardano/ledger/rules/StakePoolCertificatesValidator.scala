package scalus.cardano.ledger
package rules

import scalus.cardano.address.Network

// It's the Shelley/Conway POOL rule predicate checks in cardano-ledger
object StakePoolCertificatesValidator extends STS.Validator {
    override final type Error = TransactionException.StakePoolException

    private case class ValidationState(
        network: Network,
        minPoolCost: Coin,
        currentEpoch: Long,
        maxRetirementEpoch: Long,
        existingPools: Set[PoolKeyHash],
        notRegistered: Set[PoolKeyHash] = Set.empty,
        rewardAccountMismatches: Set[(PoolKeyHash, Network, Network)] = Set.empty,
        costTooLow: Map[PoolKeyHash, (Coin, Coin)] = Map.empty,
        invalidRetirementEpochs: Map[PoolKeyHash, (Long, Long, Long)] = Map.empty
    ) {
        def hasErrors: Boolean =
            notRegistered.nonEmpty ||
                rewardAccountMismatches.nonEmpty ||
                costTooLow.nonEmpty ||
                invalidRetirementEpochs.nonEmpty

        def handlePoolRegistration(
            operator: AddrKeyHash,
            cost: Coin,
            rewardAccount: RewardAccount
        ): ValidationState = {
            val poolId = PoolKeyHash.fromByteString(operator)

            val withNetworkCheck =
                if rewardAccount.address.network != network then
                    copy(rewardAccountMismatches =
                        rewardAccountMismatches + ((poolId, rewardAccount.address.network, network))
                    )
                else this

            if cost < minPoolCost then
                withNetworkCheck.copy(costTooLow =
                    withNetworkCheck.costTooLow.updated(poolId, minPoolCost -> cost)
                )
            else withNetworkCheck
        }

        def handlePoolRetirement(poolId: PoolKeyHash, epochNo: Long): ValidationState =
            if !existingPools.contains(poolId) then copy(notRegistered = notRegistered + poolId)
            else if epochNo <= currentEpoch || epochNo > maxRetirementEpoch then
                copy(invalidRetirementEpochs =
                    invalidRetirementEpochs.updated(
                      poolId,
                      (epochNo, currentEpoch, maxRetirementEpoch)
                    )
                )
            else this

        def processCertificate(cert: Certificate): ValidationState = cert match
            case Certificate.PoolRegistration(
                  operator,
                  _,
                  _,
                  cost,
                  _,
                  rewardAccount,
                  _,
                  _,
                  _
                ) =>
                handlePoolRegistration(operator, cost, rewardAccount)
            case Certificate.PoolRetirement(poolId, epochNo) =>
                handlePoolRetirement(poolId, epochNo)
            case _ => this
    }

    override def validate(context: Context, state: State, event: Event): Result = {
        val certificates = event.body.value.certificates.toSeq
        if certificates.isEmpty then success
        else {
            val protocolParams = context.env.params
            // TODO: UtxoEnv.slot is a slot number, not an epoch number.
            //   We need to know the current epoch, but we only know the current slot,
            //   and we don't have a way to convert between the two of them.
            val currentEpoch: Long = context.env.slot

            val initialState = ValidationState(
              network = context.env.network,
              minPoolCost = Coin(protocolParams.minPoolCost),
              currentEpoch = currentEpoch,
              maxRetirementEpoch = currentEpoch + protocolParams.poolRetireMaxEpoch,
              existingPools = state.certState.pstate.stakePools.keySet
            )

            val finalState = certificates.foldLeft(initialState)(_ processCertificate _)

            if finalState.hasErrors then
                failure(
                  TransactionException.StakePoolException(
                    event.id,
                    finalState.notRegistered,
                    finalState.rewardAccountMismatches,
                    finalState.costTooLow,
                    finalState.invalidRetirementEpochs
                  )
                )
            else success
        }
    }
}
