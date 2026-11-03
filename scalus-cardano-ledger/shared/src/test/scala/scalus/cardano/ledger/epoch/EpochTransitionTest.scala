package scalus.cardano.ledger.epoch

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network, Pointer, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.internal.{EpochEnv, EpochState, EpochTransition}
import scalus.cardano.ledger.rules.State
import scalus.uplc.builtin.ByteString

class EpochTransitionTest extends AnyFunSuite {

    /** A snapshot with one credential of stake `i`, so that mark, set and go differ. */
    private def oneStake(i: Int): StakeSnapshot =
        StakeSnapshot(
          Map(credential(90 + i) -> Coin(i)),
          Map(credential(90 + i) -> poolId(90 + i)),
          Map.empty
        )

    private val epoch = EpochState(
      AccountState(Coin(1000), Coin(5000)),
      lastEpoch = 6L,
      snapshots = SnapShots(oneStake(1), oneStake(2), oneStake(3), Coin(77)),
      blocksMadePrev = Map(poolId(95) -> 3L),
      blocksMadeCur = Map(poolId(96) -> 4L),
      pendingRewards = None
    )

    private val env = EpochEnv(
      CardanoInfo.mainnet.protocolParams,
      432_000L,
      PoolPerformance.Ideal,
      UnitInterval(1, 20)
    )

    /** `epoch` after the transition of `ledger` into epoch 7 with no other change: SNAP, spec
      * [EB-6], and the blocks made and the last epoch, spec [EB-21].
      */
    private def ticked(ledger: State): EpochState =
        epoch.copy(
          lastEpoch = 7L,
          snapshots = SnapShots(
            EpochTransition.stakeSnapshot(ledger),
            epoch.snapshots.mark,
            epoch.snapshots.set,
            ledger.fees
          ),
          blocksMadePrev = epoch.blocksMadeCur,
          blocksMadeCur = Map.empty,
          pendingRewards = Some(
            EpochTransition.rewardUpdate(
              epoch.snapshots.set,
              ledger.fees,
              epoch.blocksMadeCur,
              epoch.accountState.reserves,
              env
            )
          )
        )

    /** `ledger` with the dormant counter one higher, as every boundary with no active proposal
      * leaves it, spec [EB-15].
      */
    private def dormant(ledger: State): State = {
        val vstate = ledger.certState.vstate
        ledger.copy(certState =
            ledger.certState.copy(vstate =
                vstate.copy(numDormantEpochs = vstate.numDormantEpochs + 1)
            )
        )
    }

    test("a boundary moves the donations into the treasury and clears them") {
        // spec [EB-17]: conway Rules/Epoch.hs:357, 362
        val ledger = State(fees = Coin(3), donation = Coin(12))
        val (newLedger, newEpoch) = EpochTransition.transition(ledger, epoch, 7L, env)
        assert(newEpoch.accountState == AccountState(Coin(1012), Coin(5000)))
        assert(newLedger == dormant(ledger.copy(donation = Coin.zero)))
    }

    test("a boundary with no donations and no proposals changes only the dormant counter") {
        // spec [EB-15], [EB-24]: the steps of phases 4 and 5 are identity
        val ledger = State(fees = Coin(3))
        assert(
          EpochTransition.transition(ledger, epoch, 7L, env) == (dormant(ledger), ticked(ledger))
        )
    }

    test("the mainnet account state is that of epoch 665") {
        // spec [ES-7], [ES-7a]: Koios /totals at epoch 665
        assert(AccountState.mainnet.treasury == Coin(1_392_896_975_262_771L))
        assert(AccountState.mainnet.reserves == Coin(6_029_414_056_167_760L))
    }

    // POOLREAP, spec [EB-7] to [EB-9] (shelley Rules/PoolReap.hs:132-229)

    private val boundary = 7L
    private val deposit = Coin(500)

    private def poolId(i: Int): PoolKeyHash = PoolKeyHash.fromHex(f"$i%02x" * 28)
    private def vrf(i: Int): VrfKeyHash = VrfKeyHash.fromHex(f"$i%02x" * 32)
    private def rewardAccount(i: Int): RewardAccount =
        RewardAccount(
          StakeAddress(Network.Mainnet, StakePayload.Stake(StakeKeyHash.fromHex(f"$i%02x" * 28)))
        )
    private def credential(i: Int): Credential = rewardAccount(i).address.credential

    private def params(
        pool: Int,
        vrfKey: Int,
        account: Int,
        cost: Long = 340
    ): Certificate.PoolRegistration =
        Certificate.PoolRegistration(
          operator = AddrKeyHash.fromByteString(poolId(pool)),
          vrfKeyHash = vrf(vrfKey),
          pledge = Coin(1),
          cost = Coin(cost),
          margin = UnitInterval.one,
          rewardAccount = rewardAccount(account),
          poolOwners = Set.empty,
          relays = IndexedSeq.empty,
          poolMetadata = None
        )

    private def account(delegation: Option[PoolKeyHash] = None): ConwayAccountState =
        ConwayAccountState(Coin(10), Coin(2), delegation, Some(DRep.AlwaysAbstain))

    /** A ledger with `pools` registered, each with `deposit`, and the VRF counts of their keys. */
    private def ledgerWith(
        pools: Seq[Certificate.PoolRegistration],
        future: Seq[Certificate.PoolRegistration] = Seq.empty,
        retiring: Map[PoolKeyHash, EpochNo] = Map.empty,
        accounts: Map[Credential, ConwayAccountState] = Map.empty,
        vrfKeyHashes: Map[VrfKeyHash, Long] = Map.empty
    ): State = {
        def byId(ps: Seq[Certificate.PoolRegistration]) =
            ps.map(p => PoolKeyHash.fromByteString(p.operator) -> p).toMap
        val stakePools = byId(pools)
        val pstate = PoolsState(
          stakePools,
          byId(future),
          retiring,
          stakePools.view.mapValues(_ => deposit).toMap,
          vrfKeyHashes
        )
        State(
          certState = CertState(pstate = pstate, dstate = DelegationState(accounts)),
          deposited = Coin(deposit.value * pools.size + 2L * accounts.size)
        )
    }

    private def reaped(ledger: State): (State, EpochState) =
        EpochTransition.transition(ledger, epoch, boundary, env)

    test("a boundary activates the future pool params and keeps the deposit") {
        // spec [EB-7]: PoolReap.hs:151-168; [EB-7a]: the VRF key does not change, so its entry
        // stays (:139-149)
        val current = params(1, vrfKey = 1, account = 1)
        val future = params(1, vrfKey = 1, account = 1, cost = 999)
        val before =
            ledgerWith(Seq(current), future = Seq(future), vrfKeyHashes = Map(vrf(1) -> 1L))
        val (ledger, newEpoch) = reaped(before)
        val pstate = ledger.certState.pstate
        assert(pstate.stakePools == Map(poolId(1) -> future))
        assert(pstate.futureStakePoolParams.isEmpty)
        assert(pstate.deposits == Map(poolId(1) -> deposit))
        assert(pstate.vrfKeyHashes == Map(vrf(1) -> 1L))
        assert(ledger.deposited == deposit)
        assert(newEpoch == ticked(before))
    }

    test("an activation with a new VRF key removes the whole entry of the old key") {
        // spec [EB-7a]: danglingVRFKeyHashes, Map.withoutKeys, PoolReap.hs:139-149, 227
        val ledger = ledgerWith(
          Seq(params(1, vrfKey = 1, account = 1)),
          future = Seq(params(1, vrfKey = 2, account = 1)),
          vrfKeyHashes = Map(vrf(1) -> 2L, vrf(2) -> 1L)
        )
        assert(reaped(ledger)._1.certState.pstate.vrfKeyHashes == Map(vrf(2) -> 1L))
    }

    test("a retired pool's deposit goes to its registered reward account") {
        // spec [EB-8], [EB-9]: PoolReap.hs:172-228
        val ledger = ledgerWith(
          Seq(params(1, vrfKey = 1, account = 1)),
          retiring = Map(poolId(1) -> boundary),
          accounts = Map(credential(1) -> account()),
          vrfKeyHashes = Map(vrf(1) -> 1L)
        )
        val (newLedger, newEpoch) = reaped(ledger)
        assert(newLedger.certState.pstate == PoolsState.empty)
        assert(newLedger.certState.dstate.accounts(credential(1)).balance == Coin(510))
        assert(newEpoch == ticked(ledger))
        // spec [EB-18]: the refunded deposit leaves `deposited`
        assert(newLedger.deposited == ledger.deposited - deposit)
    }

    test(
      "a retired pool's deposit goes to the treasury when its reward account is not registered"
    ) {
        // spec [EB-8], [EB-23]: PoolReap.hs:188-193, 217-218; spec section 12.3
        val ledger = ledgerWith(
          Seq(params(1, vrfKey = 1, account = 1)),
          retiring = Map(poolId(1) -> boundary),
          accounts = Map(credential(2) -> account()),
          vrfKeyHashes = Map(vrf(1) -> 1L)
        )
        val (newLedger, newEpoch) = reaped(ledger)
        assert(newEpoch.accountState.treasury == epoch.accountState.treasury + deposit)
        assert(newLedger.certState.dstate.accounts == Map(credential(2) -> account()))
        assert(newLedger.certState.pstate == PoolsState.empty)
        // spec [EB-18]: the unclaimed deposit leaves `deposited`
        assert(newLedger.deposited == ledger.deposited - deposit)
    }

    test(
      "a re-registered pool that retires refunds to its new reward account and frees both VRF keys"
    ) {
        // spec [EB-7a], [EB-8a], [EB-9a]: retiringPools reads the merged params, PoolReap.hs:175-176
        val ledger = ledgerWith(
          Seq(params(1, vrfKey = 1, account = 1)),
          future = Seq(params(1, vrfKey = 2, account = 2)),
          retiring = Map(poolId(1) -> boundary),
          accounts = Map(credential(1) -> account(), credential(2) -> account()),
          vrfKeyHashes = Map(vrf(1) -> 1L, vrf(2) -> 1L)
        )
        val (newLedger, newEpoch) = reaped(ledger)
        val accounts = newLedger.certState.dstate.accounts
        assert(accounts(credential(1)).balance == Coin(10))
        assert(accounts(credential(2)).balance == Coin(510))
        assert(newLedger.certState.pstate == PoolsState.empty)
        assert(newEpoch == ticked(ledger))
    }

    test("a retired pool's VRF key count drops by one, and its entry goes at zero") {
        // spec [EB-9a]: removeVRFKeyHashOccurrence, PoolReap.hs:226, 235-238
        val ledger = ledgerWith(
          Seq(params(1, vrfKey = 1, account = 1), params(2, vrfKey = 1, account = 1)),
          retiring = Map(poolId(1) -> boundary),
          accounts = Map(credential(1) -> account()),
          vrfKeyHashes = Map(vrf(1) -> 2L)
        )
        assert(reaped(ledger)._1.certState.pstate.vrfKeyHashes == Map(vrf(1) -> 1L))
    }

    test("a retired pool loses its delegators, and other delegations stay") {
        // spec [EB-9], [EB-9b]: removeStakePoolDelegations, PoolReap.hs:221-224, 239-240; the
        // delegators of pool 2, which retires later, keep their delegation
        val ledger = ledgerWith(
          Seq(params(1, vrfKey = 1, account = 1), params(2, vrfKey = 2, account = 1)),
          retiring = Map(poolId(1) -> boundary, poolId(2) -> (boundary + 1)),
          accounts = Map(
            credential(3) -> account(Some(poolId(1))),
            credential(4) -> account(Some(poolId(2))),
            credential(5) -> account()
          )
        )
        val accounts = reaped(ledger)._1.certState.dstate.accounts
        assert(accounts.keySet == ledger.certState.dstate.accounts.keySet)
        assert(accounts(credential(3)) == account())
        assert(accounts(credential(4)) == account(Some(poolId(2))))
        assert(accounts(credential(5)) == account())
    }

    test("the refunds of two retired pools with one reward account add up") {
        // spec [EB-8]: Map.fromListWith (<>), PoolReap.hs:181-185
        val ledger = ledgerWith(
          Seq(params(1, vrfKey = 1, account = 1), params(2, vrfKey = 2, account = 1)),
          retiring = Map(poolId(1) -> boundary, poolId(2) -> boundary),
          accounts = Map(credential(1) -> account())
        )
        val (newLedger, _) = reaped(ledger)
        assert(newLedger.certState.dstate.accounts(credential(1)).balance == Coin(1010))
        // spec [EB-18]
        assert(newLedger.deposited == ledger.deposited - deposit - deposit)
    }

    test("a pool that retires at a later epoch stays") {
        // spec [EB-8]: only the pools with retiring = n retire, PoolReap.hs:173
        val ledger = ledgerWith(
          Seq(params(1, vrfKey = 1, account = 1)),
          retiring = Map(poolId(1) -> (boundary + 1)),
          vrfKeyHashes = Map(vrf(1) -> 1L)
        )
        assert(reaped(ledger) == (dormant(ledger), ticked(ledger)))
    }

    // Proposal expiry and the gov bookkeeping, spec [EB-12] (expiry), [EB-14] to [EB-18]
    // (conway Rules/Epoch.hs:186-208, 321-363, 426-430)

    private val proposalDeposit = Coin(100)

    private def actionId(i: Int): GovActionId =
        GovActionId(TransactionHash.fromByteString(ByteString.fromHex(f"$i%02x" * 32)), 0)

    private def proposal(
        i: Int,
        expiresAfter: EpochNo,
        returnTo: Int = 1,
        parent: Option[Int] = None
    ): GovActionState =
        GovActionState(
          actionId(i),
          Map.empty,
          Map.empty,
          Map.empty,
          ProposalProcedure(
            proposalDeposit,
            rewardAccount(returnTo),
            GovAction.NewConstitution(
              parent.map(actionId),
              Constitution(Anchor("https://example.com", DataHash.fromHex("2" * 64)), None)
            ),
            Anchor("https://example.com", DataHash.fromHex("0" * 64))
          ),
          proposedIn = expiresAfter - 1,
          expiresAfter = expiresAfter
        )

    /** A ledger with `proposals` and `accounts`, whose `deposited` is their obligation. */
    private def govLedger(
        proposals: Vector[GovActionState],
        accounts: Map[Credential, ConwayAccountState] = Map.empty
    ): State =
        State(
          certState = CertState(dstate = DelegationState(accounts)),
          govState = GovState.empty.copy(proposals = proposals),
          deposited = Coin(proposalDeposit.value * proposals.size + 2L * accounts.size)
        )

    test("a proposal expires at boundary n when its expiresAfter is below n - 1") {
        // spec [EB-12], [EB-12a] (D50); RATIFY marks it at boundary n - 1, Ratify.hs:353-359
        val expired = proposal(1, expiresAfter = boundary - 2)
        val kept = proposal(2, expiresAfter = boundary - 1)
        val (newLedger, _) =
            EpochTransition.transition(govLedger(Vector(expired, kept)), epoch, boundary, env)
        assert(newLedger.govState.proposals == Vector(kept))
    }

    test("an expired proposal's deposit returns to its registered return account") {
        // spec [EB-14], [EB-23]: returnProposalDeposits, Epoch.hs:186-200
        val ledger = govLedger(
          Vector(proposal(1, expiresAfter = boundary - 2)),
          accounts = Map(credential(1) -> account())
        )
        val (newLedger, newEpoch) = EpochTransition.transition(ledger, epoch, boundary, env)
        assert(newLedger.certState.dstate.accounts(credential(1)).balance == Coin(110))
        assert(newEpoch == ticked(ledger))
        assert(newLedger.deposited == Coin(2))
    }

    test("an expired proposal's deposit goes to the treasury when its return account is gone") {
        // spec [EB-14], [EB-17]: the unclaimed deposits, Epoch.hs:196, 357
        val ledger = govLedger(Vector(proposal(1, expiresAfter = boundary - 2)))
            .copy(donation = Coin(5))
        val (newLedger, newEpoch) = EpochTransition.transition(ledger, epoch, boundary, env)
        assert(
          newEpoch.accountState.treasury == epoch.accountState.treasury + proposalDeposit + Coin(5)
        )
        assert(newLedger.govState.proposals.isEmpty)
        assert(newLedger.donation == Coin.zero)
        assert(newLedger.deposited == Coin.zero)
    }

    test("an expired proposal takes its descendants and refunds each of their deposits") {
        // spec [EB-12b], [EB-14]: proposalsRemoveWithDescendants, Proposals.hs:465-484
        val parent = proposal(1, expiresAfter = boundary - 2)
        // still active at the boundary, but removed with its parent
        val child = proposal(2, expiresAfter = boundary + 3, returnTo = 2, parent = Some(1))
        val ledger = govLedger(
          Vector(parent, child),
          accounts = Map(credential(1) -> account(), credential(2) -> account())
        )
        val (newLedger, newEpoch) = EpochTransition.transition(ledger, epoch, boundary, env)
        assert(newLedger.govState.proposals.isEmpty)
        val accounts = newLedger.certState.dstate.accounts
        assert(accounts(credential(1)).balance == Coin(110))
        assert(accounts(credential(2)).balance == Coin(110))
        assert(newEpoch == ticked(ledger))
        assert(newLedger.deposited == Coin(4))
        // spec [EB-15a]: the dormant counter reads the proposals after the removal, so the
        // removed active child does not stop it (updateNumDormantEpochs eNo newProposals,
        // conway Rules/Epoch.hs:348)
        assert(newLedger.certState.vstate.numDormantEpochs == 1L)
    }

    test("the dormant counter grows only when no remaining proposal is active at the boundary") {
        // spec [EB-15], [EB-15a]: updateNumDormantEpochs eNo newProposals, Epoch.hs:204-208, 348
        def counter(proposals: GovActionState*): EpochNo =
            EpochTransition
                .transition(govLedger(proposals.toVector), epoch, boundary, env)
                ._1
                .certState
                .vstate
                .numDormantEpochs
        assert(counter() == 1L)
        // kept, by [EB-12a], but not active any more
        assert(counter(proposal(1, expiresAfter = boundary - 1)) == 1L)
        assert(counter(proposal(1, expiresAfter = boundary)) == 0L)
    }

    test("the committee state keeps only the cold credentials of the current members") {
        // spec [EB-16]: updateCommitteeState, Epoch.hs:426-430; with no committee it empties
        val hot = CommitteeAuthorization.HotCredential(credential(9))
        val committeeState = Map(credential(1) -> hot, credential(2) -> hot)
        val ledger =
            State(certState = CertState(vstate = VotingState(Map.empty, 0L, committeeState)))
        def after(committee: Option[Committee]): Map[Credential, CommitteeAuthorization] =
            EpochTransition
                .transition(
                  ledger.copy(govState = GovState.empty.copy(committee = committee)),
                  epoch,
                  boundary,
                  env
                )
                ._1
                .certState
                .vstate
                .committeeState
        val committee = Committee(Map(credential(1) -> 100L), UnitInterval.one)
        assert(after(Some(committee)) == Map(credential(1) -> hot))
        assert(after(None) == Map.empty)
    }

    test("deposited becomes the total obligation of the cert state and the proposals") {
        // spec [EB-18]: totalObligation certState2 govState1, Epoch.hs:360
        val drep = DRepState(expiry = 100L, anchor = None, deposit = Coin(7), delegates = Set.empty)
        val ledger = ledgerWith(
          Seq(params(1, vrfKey = 1, account = 1)),
          accounts = Map(credential(1) -> account())
        )
        val withGov = ledger.copy(
          certState = ledger.certState
              .copy(vstate = VotingState(Map(credential(3) -> drep), 0L, Map.empty)),
          govState = GovState.empty.copy(proposals = Vector(proposal(1, expiresAfter = boundary))),
          deposited = Coin(1)
        )
        val (newLedger, _) = EpochTransition.transition(withGov, epoch, boundary, env)
        assert(newLedger.deposited == deposit + Coin(2) + Coin(7) + proposalDeposit)
    }

    // SNAP, spec [EB-6], [SN-1] to [SN-10] (shelley Rules/Snap.hs:76-103, K/State/SnapShots.hs,
    // conway State/Stake.hs:101-121)

    private val paymentKey = Credential.KeyHash(AddrKeyHash.fromHex("ee" * 28))

    private def base(stake: Credential): Address = Address(Network.Mainnet, paymentKey, stake)

    private def keyOf(i: Int): AddrKeyHash = credential(i) match
        case Credential.KeyHash(hash) => hash
        case other                    => fail(s"not a key credential: $other")

    /** `ledger` with one UTxO of `lovelace` at each address, in order. */
    private def withUtxos(ledger: State, outputs: (Address, Long)*): State = {
        val hash = TransactionHash.fromByteString(ByteString.fromHex("cc" * 32))
        val utxos: Utxos = outputs.zipWithIndex.map { case ((address, lovelace), i) =>
            TransactionInput(hash, i) -> TransactionOutput(address, Value.lovelace(lovelace))
        }.toMap
        ledger.copy(utxos = utxos)
    }

    private def balance(lovelace: Long, pool: Option[PoolKeyHash]): ConwayAccountState =
        ConwayAccountState(Coin(lovelace), Coin(2), pool, None)

    test("the stake of a credential is its base-address coin plus its reward balance") {
        // spec [SN-2], [SN-5]: only StakeRefBase adds instant stake (conway State/Stake.hs:101-121);
        // the balance joins it in resolveActiveInstantStakeCredentials (K/State/Stake.hs:220-246)
        val pointer = ShelleyAddress(
          Network.Mainnet,
          ShelleyPaymentPart.Key(keyOf(1)),
          ShelleyDelegationPart.Pointer(Pointer(Slot(1), 0, 0))
        )
        val ledger = withUtxos(
          ledgerWith(
            Seq(params(1, vrfKey = 1, account = 1)),
            accounts = Map(credential(1) -> balance(10, Some(poolId(1))))
          ),
          base(credential(1)) -> 100L,
          base(credential(1)) -> 50L,
          Address(Network.Mainnet, credential(1)) -> 1000L,
          pointer -> 500L
        )
        val mark = EpochTransition.stakeSnapshot(ledger)
        assert(mark.activeStake == Map(credential(1) -> Coin(160)))
        assert(mark.delegations == Map(credential(1) -> poolId(1)))
        assert(mark.pools(poolId(1)).stake == Coin(160))
    }

    test("only registered credentials that delegate to a pool have stake") {
        // spec [SN-3]: Map.dropMissing drops an unregistered credential, and an account with no
        // pool delegation gives Nothing (K/State/Stake.hs:228-243)
        val ledger = withUtxos(
          ledgerWith(
            Seq(params(1, vrfKey = 1, account = 1)),
            accounts = Map(credential(3) -> balance(10, None))
          ),
          base(credential(2)) -> 100L,
          base(credential(3)) -> 100L
        )
        val mark = EpochTransition.stakeSnapshot(ledger)
        assert(mark.activeStake.isEmpty)
        assert(mark.delegations.isEmpty)
    }

    test("a delegator with zero stake has no stake but counts as a delegator and an owner") {
        // spec [SN-4]: nonZero of the balance (K/State/Stake.hs:236-239); spec [SN-8]: spsDelegators
        // and the owners among them count whatever their stake (K/State/SnapShots.hs:203-216)
        val owned = params(1, vrfKey = 1, account = 1).copy(poolOwners = Set(keyOf(4), keyOf(5)))
        val ledger = ledgerWith(
          Seq(owned),
          accounts = Map(credential(4) -> balance(0, Some(poolId(1))))
        )
        val mark = EpochTransition.stakeSnapshot(ledger)
        assert(mark.activeStake.isEmpty)
        assert(mark.delegations.isEmpty)
        val pool = mark.pools(poolId(1))
        assert(pool.numDelegators == 1)
        assert(pool.selfDelegatedOwners == Set(keyOf(4)))
        assert(pool.selfDelegatedOwnersStake == Coin.zero)
        assert(pool.stake == Coin.zero)
    }

    test("the total active stake is the sum of the stake, or 1 when there is none") {
        // spec [SN-6], [DM-10b]: sumAllActiveStake, K/State/Stake.hs:157-160
        assert(StakeSnapshot.empty.totalActiveStake == Coin(1))
        val two = StakeSnapshot(
          Map(credential(1) -> Coin(3), credential(2) -> Coin(4)),
          Map(credential(1) -> poolId(1), credential(2) -> poolId(1)),
          Map.empty
        )
        assert(two.totalActiveStake == Coin(7))
    }

    test("the mark snapshot holds every registered pool, with its current params") {
        // spec [SN-7], [SN-8]: resetStakePoolsSnapShot over all of psStakePools and
        // mkStakePoolSnapShot (K/State/SnapShots.hs:187-216, 409-416); pool 2 has no delegator
        // and future params, which SNAP does not read
        val owned = params(1, vrfKey = 1, account = 7).copy(poolOwners = Set(keyOf(1), keyOf(9)))
        val ledger = withUtxos(
          ledgerWith(
            Seq(owned, params(2, vrfKey = 2, account = 2)),
            future = Seq(params(2, vrfKey = 2, account = 2, cost = 999)),
            accounts = Map(
              credential(1) -> balance(10, Some(poolId(1))),
              credential(3) -> balance(5, Some(poolId(1))),
              credential(9) -> balance(1, None)
            )
          ),
          base(credential(1)) -> 100L,
          base(credential(3)) -> 20L
        )
        val mark = EpochTransition.stakeSnapshot(ledger)
        assert(
          mark.pools == Map(
            poolId(1) -> PoolSnapshot(
              stake = Coin(135),
              selfDelegatedOwners = Set(keyOf(1)),
              selfDelegatedOwnersStake = Coin(110),
              vrfKeyHash = vrf(1),
              pledge = Coin(1),
              cost = Coin(340),
              margin = UnitInterval.one,
              numDelegators = 2,
              rewardAccount = credential(7)
            ),
            poolId(2) -> PoolSnapshot(
              stake = Coin.zero,
              selfDelegatedOwners = Set.empty,
              selfDelegatedOwnersStake = Coin.zero,
              vrfKeyHash = vrf(2),
              pledge = Coin(1),
              cost = Coin(340),
              margin = UnitInterval.one,
              numDelegators = 0,
              rewardAccount = credential(2)
            )
          )
        )
    }

    test("a boundary rotates the snapshots and takes the fees") {
        // spec [EB-6]: go := set, set := mark, mark := the stake now, fee := the fees
        // (shelley Rules/Snap.hs:95-103)
        val ledger = withUtxos(
          ledgerWith(
            Seq(params(1, vrfKey = 1, account = 1)),
            accounts = Map(credential(1) -> balance(10, Some(poolId(1))))
          ),
          base(credential(1)) -> 100L
        ).copy(fees = Coin(33))
        val snapshots = reaped(ledger)._2.snapshots
        assert(snapshots.mark == EpochTransition.stakeSnapshot(ledger))
        assert(snapshots.mark.activeStake == Map(credential(1) -> Coin(110)))
        assert(snapshots.set == epoch.snapshots.mark)
        assert(snapshots.go == epoch.snapshots.set)
        assert(snapshots.fee == Coin(33))
    }

    test("a pool that retires at the boundary is in the new mark, and its delegators too") {
        // spec [SN-9]: SNAP runs before POOLREAP (conway Rules/Epoch.hs:293-296)
        val ledger = withUtxos(
          ledgerWith(
            Seq(params(1, vrfKey = 1, account = 1)),
            retiring = Map(poolId(1) -> boundary),
            accounts = Map(credential(1) -> balance(10, Some(poolId(1)))),
            vrfKeyHashes = Map(vrf(1) -> 1L)
          ),
          base(credential(1)) -> 100L
        )
        val (newLedger, newEpoch) = reaped(ledger)
        assert(newLedger.certState.pstate.stakePools.isEmpty)
        val mark = newEpoch.snapshots.mark
        assert(mark.pools.keySet == Set(poolId(1)))
        assert(mark.activeStake == Map(credential(1) -> Coin(110)))
        assert(mark.delegations == Map(credential(1) -> poolId(1)))
    }

    test("a boundary moves the blocks made of the epoch to the previous epoch") {
        // spec [EB-21]: nesBprev = bcur, nesBcur = mempty (conway Rules/NewEpoch.hs:182-192)
        val (_, newEpoch) = reaped(State())
        assert(newEpoch.blocksMadePrev == Map(poolId(96) -> 4L))
        assert(newEpoch.blocksMadeCur.isEmpty)
        assert(newEpoch.lastEpoch == boundary)
    }

    test("a boundary replaces the pending rewards with those of go, the fees and the blocks made") {
        // spec [EB-22], [RW-13], [RW-21]: after SNAP and [EB-21], go is the old set, fee the
        // fees of the ledger and blocksMadePrev the old blocksMadeCur
        val member = credential(97)
        val stake = Coin(10_000_000_000_000L)
        val earning = PoolSnapshot(
          stake,
          Set.empty,
          Coin.zero,
          vrf(96),
          Coin.zero,
          Coin(340),
          UnitInterval(1, 100),
          1,
          credential(98)
        )
        val set =
            StakeSnapshot(
              Map(member -> stake),
              Map(member -> poolId(96)),
              Map(poolId(96) -> earning)
            )
        // [EB-5] runs first, so [EB-22] reads the reserves and the fees after `old` (RUPD starts
        // in the new epoch, on the state NEWEPOCH made)
        val old = RewardUpdate(Coin(1), Coin(-1), Map.empty, Coin(-5))
        val reserves = Coin(35_000_000_000_000_000L)
        val before = epoch.copy(
          accountState = AccountState(Coin(1000), reserves),
          snapshots = epoch.snapshots.copy(set = set),
          pendingRewards = Some(old)
        )
        val measured = env.copy(poolPerformance = PoolPerformance.Measured)
        val (_, newEpoch) = EpochTransition.transition(State(fees = Coin(60)), before, 7L, measured)
        val expected = EpochTransition.rewardUpdate(
          set,
          Coin(55),
          Map(poolId(96) -> 4L),
          reserves - Coin(1),
          measured
        )
        assert(newEpoch.pendingRewards == Some(expected))
        assert(expected.rewards.keySet == Set(member, credential(98)))
        assert(expected.deltaF == Coin(-55))
    }

    // Applying the reward update, spec [EB-5], [RW-10] to [RW-12], [RW-21]
    // (applyRUpdFiltered, shelley LedgerState/IncrementalStake.hs:95-128)

    /** `epoch` with `update` pending. */
    private def pending(update: RewardUpdate): EpochState =
        epoch.copy(pendingRewards = Some(update))

    /** The lovelace in the treasury, the reserves, the fees and the account balances. */
    private def pots(ledger: State, epoch: EpochState): Coin =
        ledger.certState.dstate.accounts.values.foldLeft(
          epoch.accountState.treasury + epoch.accountState.reserves + ledger.fees
        )(_ + _.balance)

    test(
      "a boundary pays the pending rewards to registered accounts and the rest to the treasury"
    ) {
        // spec [RW-10], [RW-11], [RW-12]: deltaT and the unregistered rewards to the treasury,
        // deltaR to the reserves, deltaF to the fees (IncrementalStake.hs:119-127)
        val update = RewardUpdate(
          deltaT = Coin(10),
          deltaR = Coin(-2),
          rewards = Map(credential(1) -> Coin(5), credential(2) -> Coin(7)),
          deltaF = Coin(-20)
        )
        val ledger = ledgerWith(Seq.empty, accounts = Map(credential(1) -> balance(10, None)))
            .copy(fees = Coin(30))
        val (newLedger, newEpoch) = EpochTransition.transition(ledger, pending(update), 7L, env)
        assert(newLedger.certState.dstate.accounts(credential(1)).balance == Coin(15))
        assert(newLedger.certState.dstate.accounts.keySet == Set(credential(1)))
        assert(newEpoch.accountState == AccountState(Coin(1000 + 10 + 7), Coin(5000 - 2)))
        assert(newLedger.fees == Coin(10))
        assert(newEpoch.snapshots.fee == Coin(10))
        assert(pots(newLedger, newEpoch) == pots(ledger, epoch))
    }

    test("a reward is paid before SNAP, so it is in the new mark") {
        // spec [SN-9], [EB-4]: NEWEPOCH applies the update to es0 and passes the result to EPOCH,
        // whose SNAP reads it (conway Rules/NewEpoch.hs:172-179)
        val update = RewardUpdate(Coin.zero, Coin(-5), Map(credential(1) -> Coin(5)), Coin.zero)
        val ledger = withUtxos(
          ledgerWith(
            Seq(params(1, vrfKey = 1, account = 1)),
            accounts = Map(credential(1) -> balance(10, Some(poolId(1))))
          ),
          base(credential(1)) -> 100L
        )
        val (_, newEpoch) = EpochTransition.transition(ledger, pending(update), 7L, env)
        assert(newEpoch.snapshots.mark.activeStake == Map(credential(1) -> Coin(115)))
    }

    test("a reward and a pool deposit refund at the same boundary add up on one balance") {
        // spec [EB-5], [EB-8]. POOLREAP deregisters no account, so the order of the two steps
        // does not show here; only the sum on one balance does
        val update = RewardUpdate(Coin.zero, Coin(-5), Map(credential(1) -> Coin(5)), Coin.zero)
        val ledger = ledgerWith(
          Seq(params(1, vrfKey = 1, account = 1)),
          retiring = Map(poolId(1) -> boundary),
          accounts = Map(credential(1) -> balance(10, None)),
          vrfKeyHashes = Map(vrf(1) -> 1L)
        )
        val (newLedger, _) = EpochTransition.transition(ledger, pending(update), boundary, env)
        assert(newLedger.certState.dstate.accounts(credential(1)).balance == Coin(10 + 5) + deposit)
    }
}
