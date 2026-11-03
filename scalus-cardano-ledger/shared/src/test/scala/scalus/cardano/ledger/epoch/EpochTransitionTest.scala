package scalus.cardano.ledger.epoch

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.internal.{EpochState, EpochTransition}
import scalus.cardano.ledger.rules.State
import scalus.uplc.builtin.ByteString

class EpochTransitionTest extends AnyFunSuite {

    private val epoch = EpochState(AccountState(Coin(1000), Coin(5000)), lastEpoch = 6L)

    /** `epoch` after the transition into epoch 7, spec [EB-21]. */
    private val ticked = epoch.copy(lastEpoch = 7L)

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
        val (newLedger, newEpoch) = EpochTransition.transition(ledger, epoch, 7L)
        assert(newEpoch.accountState == AccountState(Coin(1012), Coin(5000)))
        assert(newLedger == dormant(ledger.copy(donation = Coin.zero)))
    }

    test("a boundary with no donations and no proposals changes only the dormant counter") {
        // spec [EB-15], [EB-24]: the steps of phases 4 and 5 are identity
        val ledger = State(fees = Coin(3))
        assert(EpochTransition.transition(ledger, epoch, 7L) == (dormant(ledger), ticked))
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
        EpochTransition.transition(ledger, epoch, boundary)

    test("a boundary activates the future pool params and keeps the deposit") {
        // spec [EB-7]: PoolReap.hs:151-168; [EB-7a]: the VRF key does not change, so its entry
        // stays (:139-149)
        val current = params(1, vrfKey = 1, account = 1)
        val future = params(1, vrfKey = 1, account = 1, cost = 999)
        val (ledger, newEpoch) = reaped(
          ledgerWith(Seq(current), future = Seq(future), vrfKeyHashes = Map(vrf(1) -> 1L))
        )
        val pstate = ledger.certState.pstate
        assert(pstate.stakePools == Map(poolId(1) -> future))
        assert(pstate.futureStakePoolParams.isEmpty)
        assert(pstate.deposits == Map(poolId(1) -> deposit))
        assert(pstate.vrfKeyHashes == Map(vrf(1) -> 1L))
        assert(ledger.deposited == deposit)
        assert(newEpoch == ticked)
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
        assert(newEpoch == ticked)
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
        assert(newEpoch == ticked)
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
        assert(reaped(ledger) == (dormant(ledger), ticked))
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
            EpochTransition.transition(govLedger(Vector(expired, kept)), epoch, boundary)
        assert(newLedger.govState.proposals == Vector(kept))
    }

    test("an expired proposal's deposit returns to its registered return account") {
        // spec [EB-14], [EB-23]: returnProposalDeposits, Epoch.hs:186-200
        val ledger = govLedger(
          Vector(proposal(1, expiresAfter = boundary - 2)),
          accounts = Map(credential(1) -> account())
        )
        val (newLedger, newEpoch) = EpochTransition.transition(ledger, epoch, boundary)
        assert(newLedger.certState.dstate.accounts(credential(1)).balance == Coin(110))
        assert(newEpoch == ticked)
        assert(newLedger.deposited == Coin(2))
    }

    test("an expired proposal's deposit goes to the treasury when its return account is gone") {
        // spec [EB-14], [EB-17]: the unclaimed deposits, Epoch.hs:196, 357
        val ledger = govLedger(Vector(proposal(1, expiresAfter = boundary - 2)))
            .copy(donation = Coin(5))
        val (newLedger, newEpoch) = EpochTransition.transition(ledger, epoch, boundary)
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
        val (newLedger, newEpoch) = EpochTransition.transition(ledger, epoch, boundary)
        assert(newLedger.govState.proposals.isEmpty)
        val accounts = newLedger.certState.dstate.accounts
        assert(accounts(credential(1)).balance == Coin(110))
        assert(accounts(credential(2)).balance == Coin(110))
        assert(newEpoch == ticked)
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
                .transition(govLedger(proposals.toVector), epoch, boundary)
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
                  boundary
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
        val (newLedger, _) = EpochTransition.transition(withGov, epoch, boundary)
        assert(newLedger.deposited == deposit + Coin(2) + Coin(7) + proposalDeposit)
    }
}
