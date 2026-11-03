package scalus.cardano.ledger.epoch.internal

import scalus.cardano.ledger.{CertState, Coin, Credential, EpochNo, GovState, PoolKeyHash, VrfKeyHash}
import scalus.cardano.ledger.rules.State

/** The epoch transition: what the ledger does to its state at one epoch boundary, ledger-rules spec
  * section 12.2.
  *
  * The Emulator runs [[transition]] once for each boundary a slot change crosses, in order, spec
  * [EB-1], [EB-1a]: unlike NEWEPOCH (conway Rules/NewEpoch.hs:165), which runs the boundary only
  * when the new epoch follows the last one.
  */
object EpochTransition {

    /** The ledger state and the epoch state after the epoch transition into epoch `boundary`, spec
      * [EB-4].
      *
      * The steps run in the order of the table of spec section 12.2. A step whose phase has not
      * landed is identity, spec [EB-24]; in this version the steps are POOLREAP ([EB-7] to [EB-9]),
      * the expiry of proposals ([EB-12], [EB-14]), the voting state ([EB-15], [EB-16]), the
      * treasury ([EB-17]) and `deposited` ([EB-18]), as conway Rules/Epoch.hs:276-363 runs them.
      * The last epoch becomes `boundary`, [EB-21] (conway Rules/NewEpoch.hs:186).
      */
    def transition(ledger: State, epoch: EpochState, boundary: EpochNo): (State, EpochState) = {
        val (reaped, reapedEpoch) = poolReap(ledger, epoch, boundary)
        val (expired, unclaimed) = expireProposals(reaped, boundary)
        val voted = updatedVotingState(expired, boundary)
        val (moved, movedEpoch) = movedToTreasury(voted, reapedEpoch, unclaimed)
        (
          moved.copy(deposited = totalObligation(moved.certState, moved.govState)),
          movedEpoch.copy(lastEpoch = boundary)
        )
    }

    /** POOLREAP, spec [EB-7] to [EB-9] (shelley Rules/PoolReap.hs:132-229).
      *
      *   - [EB-7]: the future params of each registered pool replace its params; the deposit stays
      *     (`:151-168`). [EB-7a]: the whole VRF entry of a replaced VRF key goes (`:139-149`).
      *   - [EB-8], [EB-8a]: the pools with `retiring = boundary`, read from the merged params,
      *     refund their deposits, summed per reward account, to the registered accounts and the
      *     rest to the treasury (`:172-195, 218`); [EB-23]: registration is read here. The drop of
      *     `deposited` by both (`:217`) is left to [EB-18], which recomputes it after this step.
      *   - [EB-9], [EB-9a], [EB-9b]: the accounts delegated to a retired pool lose that delegation,
      *     found by a scan of the accounts (D51, `:221-224`); the retired pools leave `stakePools`,
      *     `retiring` and `deposits`, and the count of their VRF keys drops by one, the entry going
      *     at zero (`:225-228, 235-238`).
      */
    private def poolReap(
        ledger: State,
        epoch: EpochState,
        boundary: EpochNo
    ): (State, EpochState) = {
        val pstate = ledger.certState.pstate
        val retired: Set[PoolKeyHash] =
            pstate.retiring.iterator.collect { case (pool, e) if e == boundary => pool }.toSet
        if pstate.futureStakePoolParams.isEmpty && retired.isEmpty then (ledger, epoch)
        else
            // [EB-7a]: the current VRF keys that a re-registration replaces (danglingVRFKeyHashes)
            val dangling: Set[VrfKeyHash] = pstate.futureStakePoolParams.iterator.flatMap {
                (pool, future) =>
                    pstate.stakePools.get(pool).map(_.vrfKeyHash).filter(_ != future.vrfKeyHash)
            }.toSet
            // [EB-7]: a future param of a pool that is not registered is dropped (Map.dropMissing)
            val stakePools = pstate.stakePools.map { (pool, params) =>
                pool -> pstate.futureStakePoolParams.getOrElse(pool, params)
            }
            // [EB-8a]: the retiring pools with their merged params (retiringPools, :176)
            val retiringPools = stakePools.view.filterKeys(retired).toMap
            val refunds: Map[Credential, Coin] = retiringPools.toSeq
                .map((pool, params) =>
                    params.rewardAccount.address.credential ->
                        pstate.deposits.getOrElse(pool, Coin.zero)
                )
                .groupMapReduce(_._1)(_._2)(_ + _)
            val accounts0 = ledger.certState.dstate.accounts
            val (claimed, unclaimed) = refunds.partition((cred, _) => accounts0.contains(cred))
            val unclaimedSum = unclaimed.values.foldLeft(Coin.zero)(_ + _)
            // [EB-8], [EB-9b]: one pass; the ledger keeps the number of accounts (:123-130)
            val accounts = accounts0.map { (cred, account) =>
                val refunded = claimed
                    .get(cred)
                    .fold(account)(c => account.copy(balance = account.balance + c))
                cred -> (
                  if refunded.stakePoolDelegation.exists(retired) then
                      refunded.copy(stakePoolDelegation = None)
                  else refunded
                )
            }
            // [EB-7a] first, then [EB-9a] (:227-228)
            val vrfKeyHashes = retiringPools.values.foldLeft(pstate.vrfKeyHashes -- dangling) {
                (counts, params) =>
                    counts.updatedWith(params.vrfKeyHash)(_.map(_ - 1).filter(_ > 0))
            }
            val newPstate = pstate.copy(
              stakePools = stakePools -- retired,
              futureStakePoolParams = Map.empty,
              retiring = pstate.retiring -- retired,
              deposits = pstate.deposits -- retired,
              vrfKeyHashes = vrfKeyHashes
            )
            val certState = ledger.certState.copy(
              pstate = newPstate,
              dstate = ledger.certState.dstate.copy(accounts = accounts)
            )
            val newEpoch =
                if unclaimedSum == Coin.zero then epoch
                else
                    val account = epoch.accountState
                    epoch.copy(accountState =
                        account.copy(treasury = account.treasury + unclaimedSum)
                    )
            (ledger.copy(certState = certState), newEpoch)
    }

    /** Spec [EB-12] (expiry only), [EB-12a], [EB-12b], [EB-14]: the ledger without the proposals
      * that expire at `boundary` and their descendants, with each removed deposit on the balance of
      * its return account when that account is registered, and the sum of the other deposits.
      *
      * Until phase 5, the expired proposals are those with `expiresAfter < boundary - 1`, D50: the
      * set RATIFY marked at boundary `boundary - 1` (conway Rules/Ratify.hs:353-359) for every
      * proposal that is not enacted. The removal is `proposalsApplyEnactment` with nothing enacted
      * (conway Governance/Proposals.hs:492-507, Rules/Epoch.hs:321-322). The refunds are
      * `returnProposalDeposits` (conway Rules/Epoch.hs:186-200, 334-336); [EB-23]: registration is
      * read from the accounts after POOLREAP.
      */
    private def expireProposals(ledger: State, boundary: EpochNo): (State, Coin) = {
        val expiredIds =
            ledger.govState.proposals.iterator.filter(_.expiresAfter < boundary - 1).map(_.id).toSet
        if expiredIds.isEmpty then (ledger, Coin.zero)
        else
            val (govState, removed) = ledger.govState.removeWithDescendants(expiredIds)
            val (accounts, unclaimed) =
                removed.foldLeft((ledger.certState.dstate.accounts, Coin.zero)) {
                    case ((accounts, unclaimed), p) =>
                        val credential = p.procedure.rewardAccount.address.credential
                        accounts.get(credential) match
                            case Some(account) =>
                                val refunded = account.balance + p.procedure.deposit
                                (
                                  accounts.updated(credential, account.copy(balance = refunded)),
                                  unclaimed
                                )
                            case None => (accounts, unclaimed + p.procedure.deposit)
                }
            val certState =
                ledger.certState.copy(dstate = ledger.certState.dstate.copy(accounts = accounts))
            (ledger.copy(certState = certState, govState = govState), unclaimed)
    }

    /** Spec [EB-15], [EB-15a], [EB-16]: the dormant counter grows by one when no proposal left
      * after [EB-12] is active at `boundary` (`updateNumDormantEpochs`, conway
      * Rules/Epoch.hs:204-208, 348), also when there is no proposal at all; and the committee state
      * keeps only the cold credentials of the members of the committee (`updateCommitteeState`,
      * conway Rules/Epoch.hs:350, 426-430), none when there is no committee. Until phase 5 the
      * committee is the one of the ledger, since nothing is enacted.
      */
    private def updatedVotingState(ledger: State, boundary: EpochNo): State = {
        val vstate = ledger.certState.vstate
        val active = ledger.govState.proposals.exists(_.expiresAfter >= boundary)
        val members = ledger.govState.committee.fold(Set.empty[Credential])(_.members.keySet)
        val newVstate = vstate.copy(
          numDormantEpochs =
              if active then vstate.numDormantEpochs else vstate.numDormantEpochs + 1,
          committeeState = vstate.committeeState.filter((cold, _) => members(cold))
        )
        ledger.copy(certState = ledger.certState.copy(vstate = newVstate))
    }

    /** Spec [EB-17]: the donations of the epoch and the `unclaimed` proposal deposits move into the
      * treasury, and the donations are cleared (conway Rules/Epoch.hs:357, 362). The unclaimed pool
      * deposits are already there: POOLREAP adds them (shelley Rules/PoolReap.hs:218).
      */
    private def movedToTreasury(
        ledger: State,
        epoch: EpochState,
        unclaimed: Coin
    ): (State, EpochState) = {
        val moved = ledger.donation + unclaimed
        if moved == Coin.zero then (ledger, epoch)
        else
            val account = epoch.accountState
            (
              ledger.copy(donation = Coin.zero),
              epoch.copy(accountState = account.copy(treasury = account.treasury + moved))
            )
    }

    /** Spec [EB-18]: the deposits the ledger owes, `totalObligation` (shelley
      * LedgerState/Types.hs:690-691): the deposits of the accounts, pools and DReps of `certState`
      * (`conwayObligationCertState`, conway State/CertState.hs:112-117) and of the proposals of
      * `govState` (`obligationGovState`, conway Governance.hs:416-422). The Emulator genesis starts
      * with it too, D52.
      */
    def totalObligation(certState: CertState, govState: GovState): Coin =
        Coin(
          certState.dstate.accounts.valuesIterator.map(_.deposit.value).sum
              + certState.pstate.deposits.valuesIterator.map(_.value).sum
              + certState.vstate.dreps.valuesIterator.map(_.deposit.value).sum
              + govState.proposals.iterator.map(_.procedure.deposit.value).sum
        )
}
