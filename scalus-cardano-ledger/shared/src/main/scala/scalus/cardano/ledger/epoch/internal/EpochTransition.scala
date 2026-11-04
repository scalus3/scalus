package scalus.cardano.ledger.epoch.internal

import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart}
import scalus.cardano.ledger.{AddrKeyHash, CertState, Coin, ConwayAccountState, Credential, EpochNo, GovAction, GovState, PoolKeyHash, ProtocolParams, ProtocolVersion, Utxos, VrfKeyHash}
import scalus.cardano.ledger.epoch.{PoolPerformance, PoolSnapshot, RatifyState, RewardUpdate, SnapShots, StakeSnapshot}
import scalus.cardano.ledger.rules.State

/** The epoch transition: what the ledger does to its state at one epoch boundary, ledger-rules spec
  * section 12.2.
  *
  * The Emulator runs [[transition]] once for each boundary a slot change crosses, in order, spec
  * [EB-1], [EB-1a]: unlike NEWEPOCH (conway Rules/NewEpoch.hs:165), which runs the boundary only
  * when the new epoch follows the last one.
  */
object EpochTransition {

    /** The ledger state, the epoch state and the current pparams after the epoch transition into
      * epoch `boundary`, spec [EB-4], [EB-25], D88. `env.params` are the current pparams before it.
      *
      * The steps run in the order of the table of spec section 12.2. A step whose phase has not
      * landed is identity, spec [EB-24]. First [EB-5] applies `pendingRewards` (conway
      * Rules/NewEpoch.hs:172-178), so SNAP sees the paid rewards and the reduced fees. Then, as
      * conway Rules/Epoch.hs:276-378 runs them: SNAP ([EB-6]), POOLREAP ([EB-7] to [EB-9]), the
      * pparams of the enact state ([EB-13]), the enactment of the pending ratify result ([EB-10] to
      * [EB-14]), the voting state ([EB-15], [EB-16]), the treasury ([EB-17]), HARDFORK ([EB-19])
      * and RATIFY ([EB-20]), then `deposited` ([EB-18]) on the result: neither HARDFORK nor RATIFY
      * changes a deposit. The blocks made move to the previous epoch and the last epoch becomes
      * `boundary`, [EB-21] (conway Rules/NewEpoch.hs:182-192). Last, [EB-22] replaces
      * `pendingRewards` with the reward update of the new epoch, [RW-21], read from the reserves
      * after [EB-5] and from `prevParams` after [EB-13], [EB-22a].
      */
    def transition(
        ledger: State,
        epoch: EpochBookkeeping,
        boundary: EpochNo,
        env: EpochEnv
    ): (State, EpochBookkeeping, ProtocolParams) = {
        val (paid, paidEpoch) = applyRewards(ledger, epoch)
        val (reaped, reapedEpoch) = poolReap(paid, snap(paid, paidEpoch), boundary)
        // [EB-13a]: read before [EB-12] removes the enacted proposals
        val params = enactedParams(reaped.govState, epoch.pendingRatify, env.params)
        val (enacted, enactedEpoch, unclaimed) = enact(reaped, reapedEpoch)
        val voted = updatedVotingState(enacted, boundary)
        val (moved, movedEpoch) = movedToTreasury(voted, enactedEpoch, unclaimed)
        val forked = hardFork(moved, env.params.protocolVersion, params.protocolVersion)
        val ratified = ratify(forked, movedEpoch, boundary, params)
        val rotated = movedEpoch.copy(
          lastEpoch = boundary,
          blocksMadePrev = movedEpoch.blocksMadeCur,
          blocksMadeCur = Map.empty,
          prevParams = env.params,
          pendingRatify = ratified
        )
        // [EB-22], [RW-21]: RUPD of the new epoch, run at once (D3) on the state after [EB-21].
        // [EB-22a]: env.params are the pparams of before [EB-13], which become prevParams
        val rewards = rewardUpdate(
          rotated.snapshots.go,
          rotated.snapshots.fee,
          rotated.blocksMadePrev,
          rotated.accountState.reserves,
          env
        )
        (
          forked.copy(deposited = totalObligation(forked.certState, forked.govState)),
          rotated.copy(pendingRewards = Some(rewards)),
          params
        )
    }

    /** Spec [EB-13a], D87: the pparams after the boundary. They are the pparams of the enact state
      * of `ratified` when it enacted a ParameterChange or a HardForkInitiation
      * (`predictFuturePParams`, conway Governance.hs:301-325; `nextEpochPParams`,
      * cardano-ledger-core State/Governance.hs:121-133), else `current`, also when `current`
      * changed since the RATIFY run. The ledger's `rsEnacted` holds the actions; Scalus keeps the
      * ids (D86), so the actions are read from `govState` as the boundary finds it, before [EB-12]
      * removes them. An id that is not a proposal is no pparam action (D98).
      */
    private def enactedParams(
        govState: GovState,
        ratified: RatifyState,
        current: ProtocolParams
    ): ProtocolParams = {
        val ids = ratified.enacted.toSet
        val changesParams = govState.proposals.exists { p =>
            ids(p.id) && (p.action match
                case _: GovAction.ParameterChange | _: GovAction.HardForkInitiation => true
                case _                                                              => false)
        }
        if changesParams then ratified.enactState.params else current
    }

    /** Spec [EB-20], [EB-20a], [RT-2]: RATIFY of the proposals of `ledger`, the state after
      * [EB-19], run at once (D3) as the pulser that `setFreshDRepPulsingState` starts (conway
      * Governance.hs:460-516; Rules/Epoch.hs:379) computes it. The run starts from the enact state
      * of the gov state with the pparams after [EB-13] (`mkEnactState`) and the treasury of `epoch`
      * after [EB-17] (`ensTreasuryL`, :511), at `reCurrentEpoch = boundary`; the SPO distribution
      * starts from the mark of [EB-6] (`ssStakeMarkPoolDistr`, Rules/Epoch.hs:300). With no
      * proposal, neither distribution is computed (D95).
      */
    private def ratify(
        ledger: State,
        epoch: EpochBookkeeping,
        boundary: EpochNo,
        params: ProtocolParams
    ): RatifyState = {
        val proposals = ledger.govState.proposals
        if proposals.isEmpty then Ratify.nothingRatified(ledger.govState, params)
        else
            Ratify.ratify(
              Ratify.envOf(ledger, epoch.snapshots.mark, boundary),
              Ratify.enactStateOf(ledger.govState, params),
              epoch.accountState.treasury,
              proposals
            )
    }

    /** HARDFORK, spec [EB-19a], [EN-3] (conway Rules/HardFork.hs:68-125), run when the protocol
      * version changed at this boundary, major or minor (conway Rules/Epoch.hs:375-378). Into major
      * 11 it adds one count per pool to the VRF key of its params, to the counts there are
      * (`populateVRFKeyHashes`, :107-125), D92. The ledger also counts the future params; POOLREAP
      * emptied them before this step, so only the params add counts. Into major 10 it is identity:
      * `updateDRepDelegations` drops dangling DRep delegations, and no state Scalus reaches has
      * one, D93.
      */
    private def hardFork(ledger: State, from: ProtocolVersion, to: ProtocolVersion): State =
        if from == to || to.major != 11 then ledger
        else
            val pstate = ledger.certState.pstate
            val vrfKeys = pstate.stakePools.valuesIterator.map(_.vrfKeyHash)
            val counts = vrfKeys.foldLeft(pstate.vrfKeyHashes) { (counts, vrf) =>
                counts.updated(vrf, counts.getOrElse(vrf, 0L) + 1L)
            }
            ledger.copy(certState =
                ledger.certState.copy(pstate = pstate.copy(vrfKeyHashes = counts))
            )

    /** Spec [EB-5], [RW-10] to [RW-12], [RW-21]: the pending reward update applied, as
      * `applyRUpdFiltered` at PV >= 7 does (shelley LedgerState/IncrementalStake.hs:95-128): the
      * reward of each account registered now goes to its balance, the rewards of the others and
      * `deltaT` to the treasury, `deltaR` to the reserves and `deltaF` to the fees. Identity when
      * nothing is pending, which is the first boundary after the genesis ([ES-8a]). The update is
      * cleared (`nesRu = SNothing`, conway Rules/NewEpoch.hs:190); [EB-22] sets the next one.
      */
    private def applyRewards(ledger: State, epoch: EpochBookkeeping): (State, EpochBookkeeping) =
        epoch.pendingRewards match
            case None => (ledger, epoch)
            case Some(update) =>
                val (accounts, unclaimed) = credit(ledger.certState.dstate.accounts, update.rewards)
                val account = epoch.accountState
                val certState =
                    ledger.certState.copy(dstate =
                        ledger.certState.dstate.copy(accounts = accounts)
                    )
                (
                  ledger.copy(certState = certState, fees = ledger.fees + update.deltaF),
                  epoch.copy(
                    accountState = account.copy(
                      treasury = account.treasury + update.deltaT + unclaimed,
                      reserves = account.reserves + update.deltaR
                    ),
                    pendingRewards = None
                  )
                )

    /** SNAP, spec [EB-6] (shelley Rules/Snap.hs:76-103): `go := set`, `set := mark`, `mark :=` the
      * stake of `ledger`, `fee :=` its fees. Spec [SN-9]: EPOCH runs it on the ledger before
      * POOLREAP (conway Rules/Epoch.hs:293-296), so a pool that retires at this boundary is in the
      * new mark.
      */
    private def snap(ledger: State, epoch: EpochBookkeeping): EpochBookkeeping = {
        val old = epoch.snapshots
        epoch.copy(snapshots = SnapShots(stakeSnapshot(ledger), old.mark, old.set, ledger.fees))
    }

    /** The stake snapshot of `ledger`, `snapShotFromInstantStake` (cardano-ledger-core
      * State/SnapShots.hs:418-430), spec [SN-1] to [SN-8], [SN-10].
      *
      *   - `activeStake`: each registered account that delegates to a pool, with the coin of the
      *     base-address outputs of its credential plus its balance, when that is above 0
      *     (`resolveActiveInstantStakeCredentials`, State/Stake.hs:220-246), [SN-2] to [SN-5]. As
      *     in the ledger, the pool is not checked: DELEG accepts a delegation only to a registered
      *     pool, and POOLREAP clears the delegations to a retired one.
      *   - `pools`: every pool in `stakePools`, with its current params (`resetStakePoolsSnapShot`,
      *     State/SnapShots.hs:409-416), [SN-7]; the delegators of each are found by a scan of the
      *     accounts, [SN-10], since Scalus keeps no delegator set (D22, D51), and its fields are
      *     those of `mkStakePoolSnapShot` (:187-216), [SN-8].
      */
    def stakeSnapshot(ledger: State): StakeSnapshot = {
        val accounts = ledger.certState.dstate.accounts
        val utxoStake = instantStake(ledger.utxos)
        val delegated: Map[Credential, PoolKeyHash] =
            accounts.flatMap((cred, account) => account.stakePoolDelegation.map(cred -> _))
        val delegators: Map[PoolKeyHash, Set[Credential]] =
            delegated.toSeq.groupMap(_._2)(_._1).view.mapValues(_.toSet).toMap
        val active: Map[Credential, Coin] = accounts.flatMap { (cred, account) =>
            val stake = utxoStake.getOrElse(cred, Coin.zero) + account.balance
            Option.when(account.stakePoolDelegation.isDefined && stake > Coin.zero)(cred -> stake)
        }
        def stakeOf(creds: Iterable[Credential]): Coin =
            creds.foldLeft(Coin.zero)((sum, cred) => sum + active.getOrElse(cred, Coin.zero))
        val pools = ledger.certState.pstate.stakePools.map { (pool, params) =>
            val members = delegators.getOrElse(pool, Set.empty)
            val owners: Set[AddrKeyHash] =
                params.poolOwners.filter(owner => members(Credential.KeyHash(owner)))
            pool -> PoolSnapshot(
              stake = stakeOf(members),
              selfDelegatedOwners = owners,
              selfDelegatedOwnersStake = stakeOf(owners.map(Credential.KeyHash(_))),
              vrfKeyHash = params.vrfKeyHash,
              pledge = params.pledge,
              cost = params.cost,
              margin = params.margin,
              numDelegators = members.size,
              rewardAccount = params.rewardAccount.address.credential
            )
        }
        StakeSnapshot(active, delegated.filter((cred, _) => active.contains(cred)), pools)
    }

    /** The coin of the base-address outputs of `utxos`, per stake credential, the ledger's
      * `InstantStake`: pointer, enterprise and Byron addresses add nothing (`StakeRefBase` only,
      * conway State/Stake.hs:101-121), spec [SN-2], [SN-5]. `EmulatorBase.stakeDistribution` reads
      * it too, spec [DM-15] (D80).
      */
    def instantStake(utxos: Utxos): Map[Credential, Coin] =
        utxos.valuesIterator
            .flatMap { output =>
                output.address match
                    case ShelleyAddress(_, _, ShelleyDelegationPart.Key(hash)) =>
                        Some(Credential.KeyHash(AddrKeyHash(hash)) -> output.value.coin)
                    case ShelleyAddress(_, _, ShelleyDelegationPart.Script(hash)) =>
                        Some(Credential.ScriptHash(hash) -> output.value.coin)
                    case _ => None
            }
            .toSeq
            .groupMapReduce(_._1)(_._2)(_ + _)

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
        epoch: EpochBookkeeping,
        boundary: EpochNo
    ): (State, EpochBookkeeping) = {
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

    /** Each amount of `credits` added to the balance of its account when that account is
      * registered, spec [EB-5], [EB-11a], [EB-14a], [EB-23]. Returns the accounts and the sum of
      * the credits whose account is not registered.
      */
    private def credit(
        accounts: Map[Credential, ConwayAccountState],
        credits: Iterable[(Credential, Coin)]
    ): (Map[Credential, ConwayAccountState], Coin) =
        credits.foldLeft((accounts, Coin.zero)) { case ((accounts, unclaimed), (cred, amount)) =>
            accounts.get(cred) match
                case Some(a) =>
                    (accounts.updated(cred, a.copy(balance = a.balance + amount)), unclaimed)
                case None => (accounts, unclaimed + amount)
        }

    /** Spec [EB-10] to [EB-14]: the ratify result that boundary `n - 1` computed, `pendingRatify`
      * ([EB-10a], conway Rules/Epoch.hs:301-304), applied to `ledger`, the state after POOLREAP.
      * Returns the ledger, the epoch state with the treasury after the withdrawals, and the sum of
      * the proposal deposits whose return account is not registered, which [EB-17] moves into the
      * treasury.
      *
      *   - [EB-11a]: each withdrawal of the enact state goes to the balance of a registered
      *     account; only the paid sum leaves the treasury (`applyEnactedWithdrawals`, :216-249).
      *   - [EB-12c]: the expired proposals go with their descendants, then each enacted id in order
      *     becomes the root of its purpose and takes its siblings with it
      *     (`proposalsApplyEnactment`, conway Governance/Proposals.hs:492-556).
      *   - [EB-13a]: the committee and the constitution of the enact state, in every case
      *     (:328-329). Its pparams were taken before, by `enactedParams`.
      *   - [EB-14a], [EB-23]: every removed proposal refunds its deposit to the balance of its
      *     return account when that account is registered after the withdrawals
      *     (`returnProposalDeposits`, :186-200, 333-336).
      */
    private def enact(ledger: State, epoch: EpochBookkeeping): (State, EpochBookkeeping, Coin) = {
        val ratified = epoch.pendingRatify
        val enactState = ratified.enactState
        val (withdrawn, unpaid) = credit(ledger.certState.dstate.accounts, enactState.withdrawals)
        val (unexpired, expired) = ledger.govState.removeWithDescendants(ratified.expired)
        val (govState, removed) = ratified.enacted.foldLeft((unexpired, expired)) {
            case ((govState, removed), id) =>
                val (next, gone) = govState.enact(id)
                (next, removed ++ gone)
        }
        val (accounts, unclaimed) = credit(
          withdrawn,
          removed.map(p => p.procedure.rewardAccount.address.credential -> p.procedure.deposit)
        )
        val certState =
            ledger.certState.copy(dstate = ledger.certState.dstate.copy(accounts = accounts))
        val newGovState = govState.copy(
          committee = enactState.committee,
          constitution = enactState.constitution
        )
        val paidSum = enactState.withdrawals.values.foldLeft(Coin.zero)(_ + _) - unpaid
        val account = epoch.accountState
        (
          ledger.copy(certState = certState, govState = newGovState),
          epoch.copy(accountState = account.copy(treasury = account.treasury - paidSum)),
          unclaimed
        )
    }

    /** Spec [EB-15], [EB-15a], [EB-16]: the dormant counter grows by one when no proposal left
      * after [EB-12] is active at `boundary` (`updateNumDormantEpochs`, conway
      * Rules/Epoch.hs:204-208, 348), also when there is no proposal at all; and the committee state
      * keeps only the cold credentials of the members of the committee (`updateCommitteeState`,
      * conway Rules/Epoch.hs:350, 426-430), none when there is no committee. The committee is the
      * one [EB-13a] set.
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
        epoch: EpochBookkeeping,
        unclaimed: Coin
    ): (State, EpochBookkeeping) = {
        val moved = ledger.donation + unclaimed
        if moved == Coin.zero then (ledger, epoch)
        else
            val account = epoch.accountState
            (
              ledger.copy(donation = Coin.zero),
              epoch.copy(accountState = account.copy(treasury = account.treasury + moved))
            )
    }

    /** The reward update of an epoch, `startStep` and `completeRupd` run in one go (shelley
      * LedgerState/PulsingReward.hs:99-212, 242-280; Rewards.hs:86-157, 291-393), ledger-rules spec
      * [RW-1], [RW-2], [RW-13] to [RW-20], with exact rationals floored where the ledger floors,
      * [RW-19].
      *
      * At PV >= 7 only, [RW-20]: rewards summed per credential and no registration prefilter;
      * [EB-5] checks registration when it pays. Under `Ideal`, `eta = 1` and every pool of `go`
      * with stake earns with `appPerf = 1`, [RW-15], D70. The ledger's crashes become rules:
      * `expectedBlocks = 0` [RW-14], `totalStake <= 0` [RW-16], `nOpt = 0` [RW-17] (D71, D83).
      *
      * @param go
      *   the stake snapshot of two boundaries ago, `ssStakeGo`
      * @param fee
      *   the fee snapshot, `ssFee`
      * @param blocksMadePrev
      *   the blocks each pool made in the epoch before, `nesBprev`
      * @param reserves
      *   the reserves after the boundary
      */
    def rewardUpdate(
        go: StakeSnapshot,
        fee: Coin,
        blocksMadePrev: Map[PoolKeyHash, Long],
        reserves: Coin,
        env: EpochEnv
    ): RewardUpdate = {
        val pp = env.params
        val measured = env.poolPerformance == PoolPerformance.Measured
        // [RW-14]: Conway fixes d = 0 (conway PParams.hs:855), so 1 - d = 1 (:127-129)
        val expectedBlocks = (Rational.of(env.activeSlotsCoeff) * Rational(env.epochLength)).floor
        // [RW-15]: blocksMadePrev is read under Measured only
        lazy val blocksTotal = BigInt(blocksMadePrev.valuesIterator.sum)
        val eta =
            if measured then Rational.one.min(Rational(blocksTotal, expectedBlocks.max(1)))
            else Rational.one
        val rho = Rational.of(pp.monetaryExpansion)
        val tau = Rational.of(pp.treasuryCut)
        // :121-141
        val deltaR1 = (eta * rho * Rational(reserves.value)).floor
        val rPot = BigInt(fee.value) + deltaR1
        val deltaT1 = (tau * Rational(rPot)).floor
        val r = rPot - deltaT1
        // circulation, :144, 304-306
        val totalStake = maxLovelaceSupply - reserves.value
        val totalActiveStake = BigInt(go.totalActiveStake.value)
        val a0 = Rational.of(pp.poolPledgeInfluence)
        // [RW-17]: maxPool takes nOpt = 0 as 1 (K/State/SnapShots.hs:148-151)
        val z0 = Rational(1, BigInt(pp.stakePoolTargetNum.max(1L)))
        // [RW-15]: the pools that earn, with the blocks each made under Measured
        val earning: Map[PoolKeyHash, (PoolSnapshot, Rational)] =
            go.pools.flatMap { (id, pool) =>
                if measured then
                    blocksMadePrev.get(id).map { n =>
                        // mkApparentPerformance, S/Rewards.hs:86-97: 0 when sigmaA = 0
                        val sigmaA = Rational(BigInt(pool.stake.value), totalActiveStake)
                        val appPerf =
                            if pool.stake == Coin.zero then Rational.zero
                            else Rational(BigInt(n), blocksTotal.max(1)) / sigmaA
                        id -> (pool, appPerf)
                    }
                else if pool.stake > Coin.zero then Some(id -> (pool, Rational.one))
                else None
            }
        // mkPoolRewardInfo, S/Rewards.hs:349-393: per pool, its poolR and its leader reward
        val poolRewards: Map[PoolKeyHash, PoolReward] =
            earning.map { case (id, (pool, appPerf)) =>
                val sigma = Rational.ratio(BigInt(pool.stake.value), totalStake)
                val p = Rational.ratio(BigInt(pool.pledge.value), totalStake)
                val s = Rational.ratio(BigInt(pool.selfDelegatedOwnersStake.value), totalStake)
                val maxP =
                    if pool.pledge <= pool.selfDelegatedOwnersStake then
                        maxPool(r, a0, z0, sigma, p)
                    else BigInt(0)
                val poolR = (appPerf * Rational(maxP)).floor
                // calcStakePoolOperatorReward, S/Rewards.hs:110-127. Here and in the member reward,
                // `/ sigma` runs only when poolR > cost >= 0, so maxP > 0 and sigma > 0: SNAP keeps
                // pledge <= selfDelegatedOwnersStake <= stake, so sigma = 0 gives p = 0 and maxP = 0
                val cost = BigInt(pool.cost.value)
                val m = Rational.of(pool.margin)
                val leader =
                    if poolR <= cost then poolR
                    else
                        cost + (Rational(poolR - cost) * (m + (Rational.one - m) * s / sigma)).floor
                id -> PoolReward(pool, poolR, leader, m, sigma)
            }
        // [RW-18]: rewardOnePoolMember, S/Rewards.hs:291-321, for each credential of go
        val members: Iterator[(Credential, BigInt)] = go.activeStake.iterator.flatMap {
            (cred, stake) =>
                go.delegations.get(cred).flatMap(poolRewards.get).flatMap { info =>
                    val pool = info.pool
                    val owner = cred match
                        case Credential.KeyHash(hash) => pool.selfDelegatedOwners.contains(hash)
                        case Credential.ScriptHash(_) => false
                    val cost = BigInt(pool.cost.value)
                    // calcStakePoolMemberReward, S/Rewards.hs:140-157; [RW-8]: no zero entry
                    if owner || info.poolR <= cost then None
                    else
                        val t = Rational.ratio(BigInt(stake.value), totalStake)
                        val reward =
                            (Rational(info.poolR - cost) * (Rational.one - info.margin) * t
                                / info.sigma).floor
                        Option.when(reward != 0)(cred -> reward)
                }
        }
        // [RW-8a]: a leader entry per earning pool, also of 0, at the go reward account (collectLRs,
        // S/LedgerState/PulsingReward.hs:177-182); [RW-7]: summed per credential
        val leaders =
            poolRewards.valuesIterator.map(info => info.pool.rewardAccount -> info.leader)
        val rewards: Map[Credential, BigInt] =
            (members ++ leaders).toSeq.groupMapReduce(_._1)(_._2)(_ + _)
        // completeRupd, :265-280: deltaR = deltaR2 - deltaR1, deltaR2 = R - Σ rewards
        val deltaR2 = r - rewards.valuesIterator.sum
        RewardUpdate(
          deltaT = Coin.exact(deltaT1),
          deltaR = Coin.exact(deltaR2 - deltaR1),
          rewards = rewards.view.mapValues(Coin.exact).toMap,
          deltaF = -fee
        )
    }

    /** The reward of one earning pool, `PoolRewardInfo` (shelley Rewards.hs:242-253), with the
      * pool's margin and `sigma` that the member rewards reuse.
      */
    private final case class PoolReward(
        pool: PoolSnapshot,
        poolR: BigInt,
        leader: BigInt,
        margin: Rational,
        sigma: Rational
    )

    /** `maxPool'` (shelley Rewards.hs:330-344): the most a pool can earn, with `z0 = 1/nOpt`. */
    private def maxPool(
        r: BigInt,
        a0: Rational,
        z0: Rational,
        sigma: Rational,
        p: Rational
    ): BigInt = {
        val sigma1 = sigma.min(z0)
        val p1 = p.min(z0)
        val factor4 = (z0 - sigma1) / z0
        val factor3 = (sigma1 - p1 * factor4) / z0
        val factor2 = sigma1 + p1 * a0 * factor3
        val factor1 = Rational(r) / (Rational.one + a0)
        (factor1 * factor2).floor
    }

    /** The Shelley genesis `maxLovelaceSupply` of mainnet, preprod and preview, spec [RW-13]. */
    private val maxLovelaceSupply: BigInt = BigInt(45_000_000_000_000_000L)

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
