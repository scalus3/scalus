package scalus.cardano.ledger
package rules

import scalus.cardano.address.Network

import scala.annotation.nowarn

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
            else if epochNo <= currentEpoch then
                copy(invalidRetirementEpochs =
                    invalidRetirementEpochs.updated(
                      poolId,
                      (epochNo, currentEpoch, maxRetirementEpoch)
                    )
                )
            else if epochNo > maxRetirementEpoch then
                copy(invalidRetirementEpochs =
                    invalidRetirementEpochs.updated(
                      poolId,
                      (epochNo, currentEpoch, maxRetirementEpoch)
                    )
                )
            else this

        infix def processCertificate(cert: Certificate): ValidationState = cert match
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

    // @nowarn: Suppress Long→Double implicit conversion warning. This is intentional for
    // cross-platform compatibility: JS SlotConfig uses Double (JavaScript's number type),
    // while JVM uses Long. The conversion is safe because slot values are well within
    // Double's safe integer range (2^53).
    @nowarn("msg=long2double")
    override def validate(context: Context, state: State, event: Event): Result = {
        val certificates = event.body.value.certificates.toSeq
        if certificates.isEmpty then success
        else {
            val protocolParams = context.env.params
            val currentEpoch: Long = context.slotConfig.epochOf(context.env.slot).toLong

            val initialState = ValidationState(
              network = context.env.network,
              minPoolCost = Coin(protocolParams.minPoolCost),
              currentEpoch = currentEpoch,
              maxRetirementEpoch = currentEpoch + protocolParams.poolRetireMaxEpoch,
              existingPools = state.certState.pstate.stakePools.keySet
                  ++ state.certState.pstate.futureStakePoolParams.keySet
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
