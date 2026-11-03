package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.AccountState
import scalus.cardano.node.emulator.ChainConfig
import scalus.cardano.txbuilder.{PubKeyWitness, TransactionBuilderStep, TransactionSigner, TxBuilder}
import scalus.testing.kit.Party
import scalus.testing.kit.Party.{Alice, Bob, Charles, Dave}
import scalus.uplc.builtin.ByteString

import scala.util.Random

/** The epoch boundary of the Emulator, ledger-rules spec section 12: the tick runs one epoch
  * transition per boundary a slot change crosses, and the account state is the source of the env
  * treasury.
  */
class EmulatorEpochTest extends AnyFunSuite {

    private given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val slotConfig = testEnv.slotConfig

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private val alice = Alice.address(Network.Mainnet)

    private val initialUtxos: Utxos = (0 until 2).map { i =>
        Input(genesisHash, i) -> Output(alice, Value.ada(1000))
    }.toMap

    private val startEpoch = 500L

    /** An emulator at slot 10 of `startEpoch`, with a treasury of 1000 lovelace. */
    private def emulator(): Emulator = {
        val context = rules.Context.testMainnet(slotConfig.firstSlotOfEpoch(startEpoch) + 10)
        Emulator(
          initialUtxos = initialUtxos,
          initialContext = context.copy(env = context.env.copy(treasury = Coin(1000)))
        )
    }

    private def stateOf(emulator: Emulator): EmulatorState = EmulatorBase.stateOf(emulator)

    private def donate(emulator: Emulator, lovelace: Long): Unit = {
        val tx = TxBuilder(testEnv)
            .donateToTreasury(Coin(lovelace))
            .setCurrentTreasuryValue(emulator.treasury)
            .complete(emulator.utxos, alice)
            .sign(Alice.signer)
            .transaction
        assert(emulator.submitSync(tx) == Right(tx.id))
    }

    test("the env treasury is the treasury of the account state, at genesis and after boundaries") {
        // spec [DM-13]
        val e = emulator()
        assert(stateOf(e).accountState.treasury == Coin(1000))
        donate(e, 5)
        for n <- 1 to 3 do
            e.setSlot(slotConfig.firstSlotOfEpoch(startEpoch + n))
            val state = stateOf(e)
            assert(state.context.env.treasury == state.accountState.treasury, s"boundary $n")
        assert(e.treasury == Coin(1005))
    }

    test("a jump over three boundaries moves the donations once") {
        // spec [EB-1], [EB-1a], [EB-17]: the later boundaries see no donations
        val e = emulator()
        donate(e, 5)
        e.setSlot(slotConfig.firstSlotOfEpoch(startEpoch + 3))
        assert(stateOf(e).accountState.treasury == Coin(1005))
        assert(e.donation == Coin.zero)
    }

    test("the ticked tip is computed once per state") {
        // spec [EB-3a]: every reader of one state sees the same ticked snapshot
        val e = emulator()
        e.setSlot(slotConfig.firstSlotOfEpoch(startEpoch + 2))
        val state = stateOf(e)
        assert(state.tipSnapshot eq state.tipSnapshot)
    }

    test("a jump over 1000 epochs, then reads and a submit") {
        // spec [EB-1], [EB-3a]; Shelley Imp EpochSpec "Crosses epoch boundaries"
        val e = emulator()
        donate(e, 5)
        val started = System.nanoTime()
        e.setSlot(slotConfig.firstSlotOfEpoch(startEpoch + 1000))
        val treasury = e.treasury
        val elapsedUs = (System.nanoTime() - started) / 1_000
        info(s"setSlot over 1000 epochs and the first read: $elapsedUs us")
        assert(treasury == Coin(1005))
        assert(slotConfig.epochOf(stateOf(e).context.env.slot) == startEpoch + 1000)
        donate(e, 7)
        assert(e.donation == Coin(7))
    }

    test("a jump over five boundaries runs each of them") {
        // spec [EB-1], [EB-1a], [EB-15]: with no proposal, each boundary grows the counter
        val e = emulator()
        boundary(e, 5)
        assert(e.certState.vstate.numDormantEpochs == 5L)
    }

    test("a direct edit after a boundary stays when the slot moves back before it") {
        // spec [EB-3b]: the edit lands on the stored tip, before the boundary, and the tick runs
        // the boundary on the edited snapshot
        val e = emulator()
        donate(e, 5)
        boundary(e, 1)
        assert(e.treasury == Coin(1005))
        val input = Input(genesisHash, 7)
        val output = Output(alice, Value.ada(3))
        e.addUtxo(input, output)
        e.setSlot(slotConfig.firstSlotOfEpoch(startEpoch) + 20)
        assert(e.treasury == Coin(1000))
        assert(e.donation == Coin(5))
        assert(e.utxos.get(input).contains(output))
        boundary(e, 1)
        assert(e.treasury == Coin(1005))
        assert(e.donation == Coin.zero)
        assert(e.utxos.get(input).contains(output))
    }

    test("an emulator starts with the mainnet reserves, and no UTxO holds the ADA outside it") {
        // spec [ES-7], [ES-7b]
        val e = emulator()
        assert(e.reserves == AccountState.mainnet.reserves)
        assert(stateOf(e).accountState.reserves == AccountState.mainnet.reserves)
        assert(e.utxos == initialUtxos)
    }

    test("the reserves of the initial state are kept across boundaries") {
        // spec [ES-6], [ES-7]: the reserves are given through EmulatorInitialState
        val context = rules.Context.testMainnet(slotConfig.firstSlotOfEpoch(startEpoch))
        val e = Emulator.withState(
          EmulatorInitialState(initialUtxos, reserves = Coin(77)),
          context,
          ChainConfig.instant
        )
        assert(e.reserves == Coin(77))
        e.setSlot(slotConfig.firstSlotOfEpoch(startEpoch + 2))
        assert(e.reserves == Coin(77))
        assert(e.utxos == initialUtxos)
    }

    test("setSlot over n epochs moves the current epoch by n") {
        // spec [ES-9]; Shelley Imp EpochSpec "Crosses epoch boundaries" (EpochSpec.hs:39)
        for n <- Seq(1L, 1000L) do
            val e = emulator()
            assert(e.currentEpoch == startEpoch)
            e.setSlot(slotConfig.firstSlotOfEpoch(startEpoch + n))
            assert(e.currentEpoch == startEpoch + n, s"n = $n")
    }

    test("the default treasury is the mainnet treasury, and the account state holds it") {
        // spec [ES-7a], [DM-13]; UtxoEnv.default stays at zero (D53)
        assert(rules.Context.testMainnet().env.treasury == AccountState.mainnet.treasury)
        assert(rules.UtxoEnv.default.treasury == Coin.zero)
        val e = Emulator(initialUtxos = initialUtxos)
        assert(e.treasury == AccountState.mainnet.treasury)
        assert(stateOf(e).accountState == AccountState.mainnet)
        assert(e.utxos == initialUtxos)
    }

    // POOLREAP at the boundary, spec [EB-7] to [EB-9]

    /** Alice's pool: she is the operator and the owner; Bob's stake address is the reward account.
      */
    private object alicePool {
        val id: PoolKeyHash = PoolKeyHash.fromByteString(Alice.addrKeyHash)
        val deposit: Coin = Coin(testEnv.protocolParams.stakePoolDeposit)
        val bobStake: StakeAddress = Bob.account.stakeAddress(Network.Mainnet)
        val bob: Credential = bobStake.credential
        val charles: Credential = Charles.account.stakeAddress(Network.Mainnet).credential

        def params(vrf: VrfKeyHash, cost: Coin): Certificate.PoolRegistration =
            Certificate.PoolRegistration(
              operator = Alice.addrKeyHash,
              vrfKeyHash = vrf,
              pledge = Coin.ada(1),
              cost = cost,
              margin = UnitInterval.zero,
              rewardAccount = RewardAccount(bobStake),
              poolOwners = Set(Alice.addrKeyHash),
              relays = IndexedSeq.empty,
              poolMetadata = None
            )

        val vrf: VrfKeyHash = VrfKeyHash.fromHex("a1" * 32)
        val registration: Certificate.PoolRegistration =
            params(vrf, Coin(testEnv.protocolParams.minPoolCost))

        /** An emulator at slot 10 of `startEpoch` with the pool, Bob's registered reward account
          * and Charles delegated to the pool.
          */
        def poolEmulator(): Emulator =
            Emulator.withState(
              EmulatorInitialState(
                utxos = initialUtxos,
                stakeRegistrations = Seq(
                  EmulatorStakeRegistration(bob),
                  EmulatorStakeRegistration(charles, delegatedTo = Some(id))
                ),
                poolRegistrations = Seq(EmulatorPoolRegistration(registration))
              ),
              rules.Context.testMainnet(slotConfig.firstSlotOfEpoch(startEpoch) + 10),
              ChainConfig.instant
            )

        def submit(e: Emulator, builder: TxBuilder, signer: TransactionSigner): Unit = {
            val tx = builder.complete(e.utxos, alice).sign(signer).transaction
            assert(e.submitSync(tx) == Right(tx.id))
        }

        def retire(e: Emulator, epoch: EpochNo): Unit =
            submit(
              e,
              TxBuilder(testEnv).addSteps(
                TransactionBuilderStep.IssueCertificate(
                  Certificate.PoolRetirement(id, epoch),
                  PubKeyWitness
                )
              ),
              Alice.signer
            )

        def deregisterBob(e: Emulator): Unit =
            submit(
              e,
              TxBuilder(testEnv).deregisterStake(bobStake),
              new TransactionSigner(Set(Alice.account.paymentKeyPair, Bob.account.stakeKeyPair))
            )
    }

    private def boundary(e: Emulator, n: Long): Unit =
        e.setSlot(slotConfig.firstSlotOfEpoch(startEpoch + n))

    test(
      "the worked example: a pool whose reward account is unregistered retires to the treasury"
    ) {
        // spec section 12.3, [EB-8], [EB-9], [EB-9b], [EB-23]; on mainnet epochs, not 100 slots
        import alicePool.*
        val e = poolEmulator()
        boundary(e, 2)
        retire(e, startEpoch + 4)
        boundary(e, 3)
        deregisterBob(e)
        val treasury = e.treasury
        val deposited = stateOf(e).ledger.deposited
        assert(e.certState.pstate.retiring == Map(id -> (startEpoch + 4)))

        boundary(e, 4)
        assert(e.treasury - treasury == deposit)
        assert(stateOf(e).accountState.treasury == e.treasury)
        // spec [EB-18]: the pool deposit leaves `deposited`
        assert(deposited - stateOf(e).ledger.deposited == deposit)
        val pstate = e.certState.pstate
        assert(pstate.stakePools.isEmpty)
        assert(pstate.retiring.isEmpty)
        assert(pstate.deposits.isEmpty)
        assert(pstate.vrfKeyHashes.isEmpty)
        assert(e.certState.dstate.accounts(charles).stakePoolDelegation.isEmpty)
        assert(!e.certState.dstate.accounts.contains(bob))
    }

    test("a retired pool refunds its deposit to its registered reward account") {
        // spec [EB-8], [EB-23]: PoolReap.hs:188-193, 220
        import alicePool.*
        val e = poolEmulator()
        retire(e, startEpoch + 2)
        boundary(e, 1)
        assert(e.certState.pstate.stakePools.contains(id))
        val treasury = e.treasury
        boundary(e, 2)
        assert(e.treasury == treasury)
        assert(e.certState.dstate.accounts(bob).balance == deposit)
        assert(e.certState.pstate.stakePools.isEmpty)
        assert(e.certState.dstate.accounts(charles).stakePoolDelegation.isEmpty)
    }

    test("a jump over several boundaries retires a pool at a boundary inside the jump") {
        // spec [EB-1], [EB-1a], [EB-8], section 12.1: the boundary of the retiring epoch runs
        // although the jump ends later
        import alicePool.*
        val e = poolEmulator()
        retire(e, startEpoch + 2)
        boundary(e, 4)
        assert(e.certState.pstate.stakePools.isEmpty)
        assert(e.certState.dstate.accounts(bob).balance == deposit)
        assert(e.certState.vstate.numDormantEpochs == 4L)
    }

    test("a pool re-registration takes effect at the next boundary") {
        // spec [EB-7], [EB-7a]: PoolReap.hs:139-168; acceptance 6 of phase 3
        import alicePool.*
        val e = poolEmulator()
        val newVrf = VrfKeyHash.fromHex("a2" * 32)
        val updated = params(newVrf, registration.cost + Coin.ada(1))
        submit(e, TxBuilder(testEnv).updatePool(updated), Alice.signer)
        val before = e.certState.pstate
        assert(before.stakePools(id) == registration)
        assert(before.futureStakePoolParams == Map(id -> updated))
        assert(before.vrfKeyHashes == Map(vrf -> 1L, newVrf -> 1L))

        boundary(e, 1)
        val after = e.certState.pstate
        assert(after.stakePools == Map(id -> updated))
        assert(after.futureStakePoolParams.isEmpty)
        assert(after.deposits == Map(id -> deposit))
        assert(after.vrfKeyHashes == Map(newVrf -> 1L))
        assert(e.certState.dstate.accounts(charles).stakePoolDelegation.contains(id))
    }

    test("a block made after a move back to an earlier epoch does not run its boundary again") {
        // spec [EB-1], [DM-9]: the snapshot stores the last epoch whose boundary it ran (nesEL),
        // D66. In Instant, a tx submitted after setSlot below the tip lands in a block whose
        // snapshot has an env slot in the earlier epoch and a ledger past the boundary.
        import alicePool.*
        val e = poolEmulator()
        boundary(e, 1)
        donate(e, 5) // the tip block is in startEpoch + 1, after its boundary
        assert(e.certState.vstate.numDormantEpochs == 1L)
        e.setSlot(slotConfig.firstSlotOfEpoch(startEpoch) + 20)
        val updated = params(vrf, registration.cost + Coin.ada(1))
        submit(e, TxBuilder(testEnv).updatePool(updated), Alice.signer)
        e.setSlot(slotConfig.firstSlotOfEpoch(startEpoch + 1) + 5)
        assert(e.certState.vstate.numDormantEpochs == 1L)
        assert(e.certState.pstate.stakePools(id) == registration)
        assert(e.certState.pstate.futureStakePoolParams == Map(id -> updated))
        boundary(e, 2)
        assert(e.certState.vstate.numDormantEpochs == 2L)
        assert(e.certState.pstate.stakePools(id) == updated)
    }

    test("a retirement for a boundary the tip ran, accepted after a move back, is never reaped") {
        // spec D67: after a move back from a block in startEpoch + 1 to startEpoch, a tx is
        // validated at the epoch of the slot, so POOL accepts a retirement at startEpoch + 1
        // (S/Rules/Pool.hs:315, cEpoch < e). The tip already ran boundary startEpoch + 1 (D66),
        // and POOLREAP reaps only retiring = n (S/Rules/PoolReap.hs:173), so the pool stays.
        // If D67 is ever fixed, this test fails and the withSlot scaladoc and D67 change with it.
        import alicePool.*
        val e = poolEmulator()
        boundary(e, 1)
        donate(e, 5) // the tip block is in startEpoch + 1, after its boundary
        e.setSlot(slotConfig.firstSlotOfEpoch(startEpoch) + 20)
        retire(e, startEpoch + 1)
        boundary(e, 2)
        val pstate = e.certState.pstate
        assert(pstate.stakePools(id) == registration)
        assert(pstate.retiring == Map(id -> (startEpoch + 1)))
        assert(e.certState.dstate.accounts(charles).stakePoolDelegation.contains(id))
    }

    // Proposal expiry at the boundary, spec [EB-12] (expiry), [EB-14], [EB-15]; ported from the
    // Conway Imp EpochSpec (eras/conway/impl/testlib/Test/Cardano/Ledger/Conway/Imp/EpochSpec.hs)

    private object govSetup {
        val aliceStake: StakeAddress = Alice.account.stakeAddress(Network.Mainnet)
        val aliceAccount: Credential = aliceStake.credential
        val drep: Credential = Credential.KeyHash(Bob.addrKeyHash)

        val drepDeposit: Coin = Coin(testEnv.protocolParams.dRepDeposit)

        /** An emulator at slot 10 of `startEpoch` with Alice's stake account registered, `dreps`
          * registered as DReps, and the gov action `lifetime` and `deposit` and the `dRepActivity`
          * set.
          */
        def govEmulator(
            lifetime: Long,
            deposit: Coin,
            dRepActivity: Long = testEnv.protocolParams.dRepActivity,
            dreps: Seq[Credential] = Seq(drep)
        ): Emulator = {
            val context = rules.Context.testMainnet(slotConfig.firstSlotOfEpoch(startEpoch) + 10)
            val params = context.env.params.copy(
              govActionLifetime = lifetime,
              govActionDeposit = deposit.value,
              dRepActivity = dRepActivity
            )
            Emulator.withState(
              EmulatorInitialState(
                utxos = initialUtxos,
                stakeRegistrations = Seq(EmulatorStakeRegistration(aliceAccount)),
                drepRegistrations = dreps.map(EmulatorDRepRegistration(_, drepDeposit))
              ),
              context.copy(env = context.env.copy(params = params)),
              ChainConfig.instant
            )
        }

        def submit(e: Emulator, builder: TxBuilder, signers: Party*): Unit = {
            val signer = new TransactionSigner(
              (Alice +: signers).map(_.account.paymentKeyPair).toSet
            )
            val tx = builder.complete(e.utxos, alice).sign(signer).transaction
            assert(e.submitSync(tx) == Right(tx.id))
        }

        /** Submits `action` with the deposit returning to Alice's stake account. */
        def propose(e: Emulator, deposit: Coin, action: GovAction): GovActionId = {
            val proposal = ProposalProcedure(
              deposit,
              RewardAccount(aliceStake),
              action,
              Anchor("https://example.com", DataHash.fromHex("0" * 64))
            )
            val builder =
                TxBuilder(testEnv).addSteps(TransactionBuilderStep.SubmitProposal(proposal))
            val tx = builder.complete(e.utxos, alice).sign(Alice.signer).transaction
            assert(e.submitSync(tx) == Right(tx.id))
            GovActionId(tx.id, 0)
        }

        /** Submits an InfoAction whose deposit returns to Alice's stake account. */
        def propose(e: Emulator, deposit: Coin): GovActionId =
            propose(e, deposit, GovAction.InfoAction)

        /** Imp `mkMinFeeUpdateGovAction`: a ParameterChange of `txFeePerByte` under `parent`. */
        def minFeeUpdate(e: Emulator, parent: Option[GovActionId]): GovAction =
            GovAction.ParameterChange(
              parent,
              ProtocolParamUpdate(minFeeA = Some(Coin(txFeePerByte(e) + 1))),
              None
            )

        def txFeePerByte(e: Emulator): Long = stateOf(e).context.env.params.txFeePerByte

        def isPresent(e: Emulator, id: GovActionId): Boolean = e.govState.proposal(id).isDefined

        def dormant(e: Emulator): Long = e.certState.vstate.numDormantEpochs

        def expiry(e: Emulator, drep: Credential): Long = e.certState.vstate.dreps(drep).expiry

        /** Imp `expectActualDRepExpiry`: `vsActualDRepExpiry` (C/State/VState.hs:154), the stored
          * expiry plus the dormant epochs.
          */
        def actualExpiry(e: Emulator, drep: Credential): Long = expiry(e, drep) + dormant(e)

        /** Imp `isDRepExpired` (Conway ImpTest.hs:1604-1616). */
        def isExpired(e: Emulator, drep: Credential): Boolean =
            actualExpiry(e, drep) < e.currentEpoch
    }

    test("an expired proposal's deposit is refunded to its return account") {
        // phase 3 acceptance 1, Imp "Expired proposal deposit refunded" (EpochSpec.hs:84);
        // spec [EB-12a], [EB-14]
        import govSetup.*
        val deposit = Coin(999)
        val e = govEmulator(lifetime = 1, deposit)
        val initialFee = txFeePerByte(e)
        val id = propose(e, deposit, minFeeUpdate(e, None))
        assert(isPresent(e, id))
        boundary(e, 2)
        assert(isPresent(e, id))
        assert(e.certState.dstate.accounts(aliceAccount).balance == Coin.zero)
        boundary(e, 3)
        assert(!isPresent(e, id))
        // the expired ParameterChange is never enacted
        assert(txFeePerByte(e) == initialFee)
        assert(e.certState.dstate.accounts(aliceAccount).balance == deposit)
    }

    test("a proposal is present after two boundaries and removed at the third") {
        // phase 3 acceptance 2, Imp "Proposals are expired and removed as expected"
        // (EpochSpec.hs:107), on the stored proposals; spec [EB-12a]
        import govSetup.*
        val deposit = Coin.ada(1)
        val e = govEmulator(lifetime = 1, deposit)
        val id = propose(e, deposit)
        val deposited = stateOf(e).ledger.deposited
        boundary(e, 1)
        boundary(e, 2)
        assert(e.govState.proposals.map(_.id) == Vector(id))
        boundary(e, 3)
        assert(e.govState.proposals.isEmpty)
        // spec [EB-18]: the deposit leaves `deposited` with the proposal
        assert(deposited - stateOf(e).ledger.deposited == deposit)
    }

    test("an expired proposal's deposit goes to the treasury when the return account is gone") {
        // phase 3 acceptance 3, Imp "deposit is moved to treasury when the reward address is not
        // registered" (EpochSpec.hs:415, 451-486); spec [EB-14], [EB-17], [EB-23]
        import govSetup.*
        val deposit = Coin(100)
        val e = govEmulator(lifetime = 8, deposit)
        val id = propose(e, deposit)
        for n <- 1 to 5 do boundary(e, n)
        val tx = TxBuilder(testEnv)
            .deregisterStake(aliceStake)
            .complete(e.utxos, alice)
            .sign(
              new TransactionSigner(Set(Alice.account.paymentKeyPair, Alice.account.stakeKeyPair))
            )
            .transaction
        assert(e.submitSync(tx) == Right(tx.id))
        assert(!e.certState.dstate.accounts.contains(aliceAccount))
        for n <- 6 to 9 do boundary(e, n)
        assert(isPresent(e, id))
        val treasury = e.treasury
        boundary(e, 10)
        assert(!isPresent(e, id))
        assert(e.treasury - treasury == deposit)
    }

    test("the dormant counter grows while no proposal is active and bumps the DRep expiry") {
        // phase 3 acceptance 4, Imp "expiry is updated based on the number of dormant epochs"
        // (EpochSpec.hs:150); spec [EB-15], [EB-15a], [DR-3]
        import govSetup.*
        val deposit = Coin.ada(1)
        // Imp's default dRepActivity is 100 (`offDRepActivity 100`)
        val e = govEmulator(lifetime = 2, deposit, dRepActivity = 100)
        val expiry = startEpoch + 100
        def dormant = govSetup.dormant(e)
        def drepExpiry = govSetup.expiry(e, drep)
        assert(dormant == 0L)
        val id = propose(e, deposit)
        for (n, counter) <- Seq(1 -> 0L, 2 -> 0L, 3 -> 1L, 4 -> 2L, 5 -> 3L) do
            boundary(e, n)
            assert(dormant == counter, s"boundary $n")
            assert(drepExpiry == expiry, s"boundary $n")
            // the proposal expires at boundary 3 and is removed at 4, spec [EB-12a]
            assert(isPresent(e, id) == (n < 4), s"boundary $n")
        propose(e, deposit)
        assert(dormant == 0L)
        assert(drepExpiry == expiry + 3)
        boundary(e, 6)
        assert(dormant == 0L)
        assert(drepExpiry == expiry + 3)
    }

    test("proposals survive multiple epochs without any activity") {
        // Imp "Proposals survive multiple epochs without any activity" (EpochSpec.hs:62-83);
        // spec [EB-12a]: a tree of ParameterChange proposals with lifetime 4 stays 5 boundaries
        // and goes, with all its descendants, at the 6th
        import govSetup.*
        val deposit = Coin.ada(1)
        val e = govEmulator(lifetime = 4, deposit)
        def child(parent: Option[GovActionId]) = propose(e, deposit, minFeeUpdate(e, parent))
        val root = child(None)
        val left = child(Some(root))
        child(Some(left))
        child(Some(left))
        child(Some(root))
        val forest = e.govState.proposals
        assert(forest.size == 5)
        for n <- 1 to 5 do boundary(e, n)
        assert(e.govState.proposals == forest)
        boundary(e, 6)
        assert(e.govState.proposals.isEmpty)
    }

    test(
      "a DRep past its stored expiry stays active through the dormant epochs and gets the bump"
    ) {
        // Imp "expiry is not updated for inactive DReps" (EpochSpec.hs:189-237); spec [EB-15],
        // [DR-1], [DR-2], [DR-3]: the dormant epochs keep the DRep active, so the bump applies
        import govSetup.*
        val drepActivity = 2L
        val e = govEmulator(lifetime = 2, Coin.ada(1), dRepActivity = drepActivity)
        def off(offset: Long) = startEpoch + drepActivity + offset
        assert(dormant(e) == 0L)
        propose(e, Coin.ada(1))
        for n <- 1 to 2 do
            boundary(e, n)
            assert(dormant(e) == 0L, s"boundary $n")
            assert(expiry(e, drep) == off(0), s"boundary $n")
        boundary(e, 3)
        assert(dormant(e) == 1L)
        assert(expiry(e, drep) == off(0))
        assert(actualExpiry(e, drep) == off(1))
        assert(!isExpired(e, drep))
        boundary(e, 4)
        assert(dormant(e) == 2L)
        assert(expiry(e, drep) == off(0))
        assert(actualExpiry(e, drep) == off(2))
        boundary(e, 5)
        assert(dormant(e) == 3L)
        assert(expiry(e, drep) == off(0))
        assert(actualExpiry(e, drep) == off(3))
        propose(e, Coin.ada(1))
        assert(dormant(e) == 0L)
        assert(expiry(e, drep) == off(3))
        boundary(e, 6)
        assert(dormant(e) == 0L)
        assert(expiry(e, drep) == off(3))
    }

    test("the DRep expiry updates are correct for a mixture of cases") {
        // Imp "expiry updates are correct for a mixture of cases" (EpochSpec.hs:240-336); spec
        // [EB-15], [DR-1] to [DR-4]
        import govSetup.*
        val drepActivity = 4L
        val drep1 = Credential.KeyHash(Bob.addrKeyHash) // gets an update certificate
        val drep2 = Credential.KeyHash(Charles.addrKeyHash) // turns inactive, then votes
        val drep3 = Credential.KeyHash(Dave.addrKeyHash) // unregisters
        val e = govEmulator(
          lifetime = 2,
          Coin.ada(1),
          dRepActivity = drepActivity,
          dreps = Seq(drep1, drep2, drep3)
        )
        def off(offset: Long) = startEpoch + drepActivity + offset
        def expect(n: Int, counter: Long, expiries: (Credential, Long)*): Unit = {
            assert(dormant(e) == counter, s"epoch +$n")
            for (d, offset) <- expiries do assert(expiry(e, d) == off(offset), s"epoch +$n $d")
        }
        def expectActual(n: Int, expiries: (Credential, Long)*): Unit =
            for (d, offset) <- expiries do
                assert(actualExpiry(e, d) == off(offset), s"epoch +$n $d actual")

        expect(0, 0L)
        val first = propose(e, Coin.ada(1))
        for n <- 1 to 2 do
            boundary(e, n)
            expect(n, 0L, drep1 -> 0, drep2 -> 0, drep3 -> 0)

        boundary(e, 3) // the proposal is no longer active, but still stored
        assert(isPresent(e, first))
        expect(3, 1L, drep1 -> 0, drep2 -> 0, drep3 -> 0)

        boundary(e, 4)
        expect(4, 2L, drep1 -> 0, drep2 -> 0, drep3 -> 0)

        // DRep expiry becomes current epoch (4) + drep activity (4) - dormant epochs (2)
        submit(e, TxBuilder(testEnv).updateDRep(drep1, None), Bob)
        expect(4, 2L, drep1 -> 2)
        submit(e, TxBuilder(testEnv).unregisterDRep(drep3, drepDeposit), Dave)

        boundary(e, 5)
        expect(5, 3L, drep1 -> 2, drep2 -> 0)
        expectActual(5, drep1 -> 5, drep2 -> 3)
        assert(!e.certState.vstate.dreps.contains(drep3))

        propose(e, Coin.ada(1))
        expect(5, 0L, drep1 -> 5, drep2 -> 3)
        for n <- 6 to 7 do
            boundary(e, n)
            expect(n, 0L, drep1 -> 5, drep2 -> 3)

        boundary(e, 8)
        expect(8, 1L, drep1 -> 5, drep2 -> 3)
        expectActual(8, drep1 -> 6, drep2 -> 4)

        val gai = propose(e, Coin.ada(1))
        for n <- 9 to 10 do
            boundary(e, n)
            expect(n, 0L, drep1 -> 6, drep2 -> 4)
            expectActual(n, drep1 -> 6, drep2 -> 4)

        // a vote at the last epoch the proposal is active, spec [DR-1]: 10 + 4 - 0
        submit(
          e,
          TxBuilder(testEnv).addSteps(
            TransactionBuilderStep.SubmitVotingProcedure(
              Voter.DRepKey(Charles.addrKeyHash),
              Map(gai -> VotingProcedure(Vote.Yes, None))
            )
          ),
          Charles
        )

        boundary(e, 11)
        expect(11, 1L, drep1 -> 6, drep2 -> 10)
        expectActual(11, drep1 -> 7, drep2 -> 11)

        boundary(e, 12)
        expect(12, 2L, drep1 -> 6, drep2 -> 10)
        expectActual(12, drep1 -> 8, drep2 -> 12)
    }

    test("crosses epoch boundaries: setSlot over n epochs, for random n") {
        // Shelley Imp "Crosses epoch boundaries" (Shelley/Imp/EpochSpec.hs:39-43), with the
        // `Positive n` of the Imp test drawn from a seeded generator
        val random = new Random(42)
        for _ <- 1 to 10 do
            val n = 1 + random.nextInt(100)
            val e = emulator()
            boundary(e, n)
            assert(e.currentEpoch == startEpoch + n, s"n = $n")
    }
}
