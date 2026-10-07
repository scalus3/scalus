package scalus.cardano.ledger
package rules

// It's the Conway DELEG rule predicate checks in cardano-ledger
object StakeCertificatesValidator extends STS.Validator {
    override final type Error = TransactionException.StakeCertificatesException

    /** @param pools
      *   the registered pools, with those registered by earlier certificates of the tx
      * @param dreps
      *   the registered DReps, after the DRep certificates earlier in the tx
      */
    private case class ValidationState(
        accounts: Map[Credential, ConwayAccountState],
        expectedDeposit: Coin,
        pools: Set[PoolKeyHash],
        dreps: Set[Credential],
        newlyRegistered: Map[Credential, Coin] = Map.empty,
        deregisteredInTx: Set[Credential] = Set.empty,
        alreadyRegistered: Set[Credential] = Set.empty,
        missingRegistrations: Set[Credential] = Set.empty,
        nonZeroRewards: Map[Credential, Coin] = Map.empty,
        invalidDeposits: Map[Credential, (Coin, Coin)] = Map.empty,
        invalidRefunds: Map[Credential, (Coin, Coin)] = Map.empty,
        unregisteredPools: Set[PoolKeyHash] = Set.empty,
        unregisteredDReps: Set[Credential] = Set.empty
    ) {
        def hasErrors: Boolean =
            alreadyRegistered.nonEmpty ||
                missingRegistrations.nonEmpty ||
                nonZeroRewards.nonEmpty ||
                invalidDeposits.nonEmpty ||
                invalidRefunds.nonEmpty ||
                unregisteredPools.nonEmpty ||
                unregisteredDReps.nonEmpty

        def isCurrentlyRegistered(credential: Credential): Boolean =
            newlyRegistered.contains(credential) ||
                (accounts.contains(credential) && !deregisteredInTx.contains(credential))

        def depositFor(credential: Credential): Option[Coin] =
            newlyRegistered.get(credential).orElse {
                if deregisteredInTx.contains(credential) then None
                else accounts.get(credential).map(_.deposit)
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
                    val rewards = accounts.get(credential).fold(Coin.zero)(_.balance)
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

        /** spec [SC-4]: Haskell DelegateeStakePoolNotRegisteredDELEG */
        def ensurePoolRegistered(pool: PoolKeyHash): ValidationState =
            if pools.contains(pool) then this
            else copy(unregisteredPools = unregisteredPools + pool)

        /** spec [SC-5]: Haskell DelegateeDRepNotRegisteredDELEG. Abstain and no confidence need no
          * registration.
          */
        def ensureDRepRegistered(drep: DRep): ValidationState = drep match
            case DRep.KeyHash(hash)    => ensureDRepCredential(Credential.KeyHash(hash))
            case DRep.ScriptHash(hash) => ensureDRepCredential(Credential.ScriptHash(hash))
            case DRep.AlwaysAbstain | DRep.AlwaysNoConfidence => this

        private def ensureDRepCredential(credential: Credential): ValidationState =
            if dreps.contains(credential) then this
            else copy(unregisteredDReps = unregisteredDReps + credential)

        infix def processCertificate(cert: Certificate): ValidationState = cert match
            case Certificate.RegCert(credential, suppliedDeposit) =>
                handleRegistration(credential, suppliedDeposit)
            case Certificate.StakeRegDelegCert(credential, pool, deposit) =>
                handleRegistration(credential, Some(deposit)).ensurePoolRegistered(pool)
            case Certificate.VoteRegDelegCert(credential, drep, deposit) =>
                handleRegistration(credential, Some(deposit)).ensureDRepRegistered(drep)
            case Certificate.StakeVoteRegDelegCert(credential, pool, drep, deposit) =>
                handleRegistration(credential, Some(deposit))
                    .ensurePoolRegistered(pool)
                    .ensureDRepRegistered(drep)
            case Certificate.UnregCert(credential, suppliedRefund) =>
                handleDeregistration(credential, suppliedRefund)
            case Certificate.StakeDelegation(credential, pool) =>
                ensureRegistered(credential).ensurePoolRegistered(pool)
            case Certificate.StakeVoteDelegCert(credential, pool, drep) =>
                ensureRegistered(credential).ensurePoolRegistered(pool).ensureDRepRegistered(drep)
            case Certificate.VoteDelegCert(credential, drep) =>
                ensureRegistered(credential).ensureDRepRegistered(drep)
            // A new pool is registered at once, so a later certificate of the tx can delegate to it.
            case registration: Certificate.PoolRegistration =>
                copy(pools = pools + PoolKeyHash.fromByteString(registration.operator))
            case Certificate.RegDRepCert(credential, _, _) => copy(dreps = dreps + credential)
            case Certificate.UnregDRepCert(credential, _)  => copy(dreps = dreps - credential)
            case _                                         => this
    }

    /** Checks the certificates against the accounts after the withdrawals of the same tx, as the
      * Conway LEDGER rule drains withdrawals before CERTS, spec [SC-3a]. So a tx can withdraw the
      * full balance and deregister the account.
      */
    override def validate(context: Context, state: State, event: Event): Result = {
        val withdrawals = event.body.value.withdrawals.getOrElse(Withdrawals.empty).withdrawals
        val accounts =
            CertsValidator.applyWithdrawals(state.certState.dstate.accounts, withdrawals)
        validateAccounts(context, state, accounts, event)
    }

    /** Checks the certificates against `state`, whose withdrawals [[CertsMutator]] has applied. */
    private[rules] def validateAfterWithdrawals(
        context: Context,
        state: State,
        event: Event
    ): Result =
        validateAccounts(context, state, state.certState.dstate.accounts, event)

    private def validateAccounts(
        context: Context,
        state: State,
        accounts: Map[Credential, ConwayAccountState],
        event: Event
    ): Result = {
        val certificates = event.body.value.certificates.toSeq
        if certificates.isEmpty then success
        else {
            val initialState = ValidationState(
              accounts = accounts,
              expectedDeposit = Coin(context.env.params.stakeAddressDeposit),
              pools = state.certState.pstate.stakePools.keySet,
              dreps = state.certState.vstate.dreps.keySet
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
                    finalState.invalidRefunds,
                    finalState.unregisteredPools,
                    finalState.unregisteredDReps
                  )
                )
            else success
        }
    }
}
