package scalus.cardano.ledger
package rules

// It's the Conway DELEG rule predicate checks in cardano-ledger
object StakeCertificatesValidator extends STS.Validator {
    override final type Error = TransactionException.StakeCertificatesException

    private case class ValidationState(
        onChainDeposits: Map[Credential, Coin],
        rewardAccounts: Map[Credential, Coin],
        expectedDeposit: Coin,
        newlyRegistered: Map[Credential, Coin] = Map.empty,
        deregisteredInTx: Set[Credential] = Set.empty,
        alreadyRegistered: Set[Credential] = Set.empty,
        missingRegistrations: Set[Credential] = Set.empty,
        nonZeroRewards: Map[Credential, Coin] = Map.empty,
        invalidDeposits: Map[Credential, (Coin, Coin)] = Map.empty,
        invalidRefunds: Map[Credential, (Coin, Coin)] = Map.empty
    ) {
        def hasErrors: Boolean =
            alreadyRegistered.nonEmpty ||
                missingRegistrations.nonEmpty ||
                nonZeroRewards.nonEmpty ||
                invalidDeposits.nonEmpty ||
                invalidRefunds.nonEmpty

        def isCurrentlyRegistered(credential: Credential): Boolean =
            newlyRegistered.contains(credential) ||
                (onChainDeposits.contains(credential) && !deregisteredInTx.contains(credential))

        def depositFor(credential: Credential): Option[Coin] =
            newlyRegistered.get(credential).orElse {
                if deregisteredInTx.contains(credential) then None
                else onChainDeposits.get(credential)
            }

        def handleRegistration(
            credential: Credential,
            suppliedDeposit: Option[Coin]
        ): ValidationState = {
            val withDepositCheck = suppliedDeposit match
                case Some(provided) if provided != expectedDeposit =>
                    copy(invalidDeposits =
                        invalidDeposits.updated(credential, expectedDeposit -> provided)
                    )
                case _ => this

            if withDepositCheck.isCurrentlyRegistered(credential) then
                withDepositCheck.copy(alreadyRegistered =
                    withDepositCheck.alreadyRegistered + credential
                )
            else
                withDepositCheck.copy(
                  newlyRegistered =
                      withDepositCheck.newlyRegistered.updated(credential, expectedDeposit),
                  deregisteredInTx = withDepositCheck.deregisteredInTx - credential
                )
        }

        def ensureRegistered(credential: Credential): ValidationState =
            if isCurrentlyRegistered(credential) then this
            else copy(missingRegistrations = missingRegistrations + credential)

        def handleDeregistration(
            credential: Credential,
            suppliedRefund: Option[Coin]
        ): ValidationState =
            depositFor(credential) match
                case None =>
                    copy(missingRegistrations = missingRegistrations + credential)
                case Some(expectedRefund) =>
                    val rewards = rewardAccounts.getOrElse(credential, Coin.zero)
                    val withRewardsCheck =
                        if rewards.value > 0 then
                            copy(nonZeroRewards = nonZeroRewards.updated(credential, rewards))
                        else this

                    val withRefundCheck = suppliedRefund match
                        case Some(provided) if provided != expectedRefund =>
                            withRewardsCheck.copy(invalidRefunds =
                                withRewardsCheck.invalidRefunds
                                    .updated(credential, expectedRefund -> provided)
                            )
                        case _ => withRewardsCheck

                    withRefundCheck.copy(
                      newlyRegistered = withRefundCheck.newlyRegistered - credential,
                      deregisteredInTx = withRefundCheck.deregisteredInTx + credential
                    )

        def processCertificate(cert: Certificate): ValidationState = cert match
            case Certificate.RegCert(credential, suppliedDeposit) =>
                handleRegistration(credential, suppliedDeposit)
            case Certificate.StakeRegDelegCert(credential, _, deposit) =>
                handleRegistration(credential, Some(deposit))
            case Certificate.VoteRegDelegCert(credential, _, deposit) =>
                handleRegistration(credential, Some(deposit))
            case Certificate.StakeVoteRegDelegCert(credential, _, _, deposit) =>
                handleRegistration(credential, Some(deposit))
            case Certificate.UnregCert(credential, suppliedRefund) =>
                handleDeregistration(credential, suppliedRefund)
            case Certificate.StakeDelegation(credential, _) =>
                ensureRegistered(credential)
            case Certificate.StakeVoteDelegCert(credential, _, _) =>
                ensureRegistered(credential)
            case Certificate.VoteDelegCert(credential, _) =>
                ensureRegistered(credential)
            case _ => this
    }

    override def validate(context: Context, state: State, event: Event): Result = {
        val certificates = event.body.value.certificates.toSeq
        if certificates.isEmpty then success
        else {
            val initialState = ValidationState(
              onChainDeposits = state.certState.dstate.deposits,
              rewardAccounts = state.certState.dstate.rewards,
              expectedDeposit = Coin(context.env.params.stakeAddressDeposit)
            )

            val finalState = certificates.foldLeft(initialState)(_ processCertificate _)

            if finalState.hasErrors then
                failure(
                  TransactionException.StakeCertificatesException(
                    event.id,
                    finalState.alreadyRegistered,
                    finalState.missingRegistrations,
                    finalState.nonZeroRewards,
                    finalState.invalidDeposits,
                    finalState.invalidRefunds
                  )
                )
            else success
        }
    }
}
