package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

import scala.annotation.nowarn
import scala.collection.immutable.{ListSet, SortedMap}

/** Withdrawals and certificates through the default mutator pipeline, spec 13.1. */
class WithdrawalsTest extends AnyFunSuite with Matchers with EitherValues {

    private val cardanoInfo = CardanoInfo.preprod
    private val params = cardanoInfo.protocolParams
    private val keyDeposit = Coin(params.stakeAddressDeposit)

    private val stakeKeyHash = StakeKeyHash.fromHex("b" * 56)
    private val rewardAccount =
        RewardAccount(StakeAddress(Network.Testnet, StakePayload.Stake(stakeKeyHash)))
    private val credential = rewardAccount.address.credential

    private val poolOperator = AddrKeyHash.fromHex("f" * 56)
    private val poolId = PoolKeyHash.fromByteString(poolOperator)
    private val pool: Certificate.PoolRegistration = Certificate.PoolRegistration(
      operator = poolOperator,
      vrfKeyHash = VrfKeyHash.fromHex("a" * 64),
      pledge = Coin.ada(100),
      cost = Coin(params.minPoolCost),
      margin = UnitInterval.one,
      rewardAccount = rewardAccount,
      poolOwners = Set(poolOperator),
      relays = IndexedSeq.empty,
      poolMetadata = None
    )
    private val drepCredential = Credential.KeyHash(AddrKeyHash.fromHex("c" * 56))

    /** The account holds `balance`, a pool and a DRep are registered. */
    private def stateWith(balance: Coin): State = State(certState =
        CertState(
          vstate = VotingState(
            Map(drepCredential -> DRepState(100, None, Coin(params.dRepDeposit), Set.empty))
          ),
          pstate = PoolsState(stakePools = Map(poolId -> pool)),
          dstate = DelegationState(
            Map(credential -> ConwayAccountState(balance, keyDeposit, None, None))
          )
        )
    )

    private def tx(withdrawal: Option[Coin], certs: Certificate*): Transaction =
        Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty[TransactionInput],
            outputs = IndexedSeq.empty[Sized[TransactionOutput]],
            fee = Coin.zero,
            certificates =
                if certs.isEmpty then TaggedOrderedStrictSet.empty
                else TaggedOrderedStrictSet.from(certs),
            withdrawals = withdrawal.map(amount => Withdrawals(SortedMap(rewardAccount -> amount)))
          )
        )

    private def context(state: State): Context = Context(
      env = UtxoEnv(0, params, state.certState, Network.Testnet, Coin.zero),
      slotConfig = cardanoInfo.slotConfig
    )

    /** The certificate and withdrawal validators, then every default mutator. */
    private def step(state: State, transaction: Transaction) =
        STS.Mutator.transit[TransactionException](
          Seq(CertsValidator, StakeCertificatesValidator, StakePoolCertificatesValidator),
          DefaultMutators.all,
          context(state),
          state,
          transaction
        )

    private def account(state: State): Option[ConwayAccountState] =
        state.certState.dstate.accounts.get(credential)

    test("withdrawing zero twice keeps the account registered with balance 0") {
        // spec [SC-3c] row 1, [SC-3]
        val first = step(stateWith(Coin.zero), tx(Some(Coin.zero))).value
        val second = step(first, tx(Some(Coin.zero))).value
        account(second) shouldBe Some(ConwayAccountState(Coin.zero, keyDeposit, None, None))
    }

    test("withdrawing the full balance and deregistering in one tx removes the account") {
        // spec [SC-3c] row 2, [SC-3a]: withdrawals drain before the certificates are checked
        val result = step(
          stateWith(Coin.ada(7)),
          tx(Some(Coin.ada(7)), Certificate.UnregCert(credential, Some(keyDeposit)))
        ).value
        account(result) shouldBe None
    }

    test("delegating after the full balance was withdrawn keeps the drained balance") {
        // spec [SC-3c] row 3, [SC-2]
        val drained = step(stateWith(Coin.ada(7)), tx(Some(Coin.ada(7)))).value
        val delegated =
            step(drained, tx(None, Certificate.StakeDelegation(credential, poolId))).value
        account(delegated) shouldBe Some(
          ConwayAccountState(Coin.zero, keyDeposit, Some(poolId), None)
        )
    }

    test("withdrawing the same 7 ADA twice is rejected the second time") {
        // spec [SC-3c] row 4, [SC-1]
        val first = step(stateWith(Coin.ada(7)), tx(Some(Coin.ada(7)))).value
        val error = step(first, tx(Some(Coin.ada(7)))).left.value
        error shouldBe a[TransactionException.WithdrawalsNotInRewardsException]
    }

    test("the default mutators apply withdrawals, then certificates, then scripts and UTxO") {
        // spec [SC-3a]: ledger order, by an explicit list rather than by name
        DefaultMutators.all.toList shouldBe List(CertsMutator, PlutusScriptsTransactionMutator)
    }

    // spec [SC-3c] row 5. The whole pipeline needs a failing script; the "Not validating CERT
    // script" conformance vectors cover that. Here each withdrawal and certificate mutator must
    // leave the state alone on its own.
    private def phase2Failed(transaction: Transaction): Transaction =
        transaction.copy(isValid = false)

    test("a phase-2-failed tx does not apply its withdrawals") {
        // spec [SC-3b]
        val state = stateWith(Coin.ada(7))
        val result =
            CertsMutator.transit(context(state), state, phase2Failed(tx(Some(Coin.ada(7))))).value
        result shouldBe state
    }

    test("a phase-2-failed tx does not apply its stake certificates") {
        // spec [SC-3e]
        val state = stateWith(Coin.zero)
        val unreg = phase2Failed(tx(None, Certificate.UnregCert(credential, Some(keyDeposit))))
        CertsMutator.transit(context(state), state, unreg).value shouldBe state
    }

    test("a phase-2-failed tx does not apply its pool certificates") {
        // spec [SC-3e]
        val state = stateWith(Coin.zero)
        val nextEpoch = cardanoInfo.slotConfig.epochOf(0) + 1
        val retire = phase2Failed(tx(None, Certificate.PoolRetirement(poolId, nextEpoch)))
        CertsMutator.transit(context(state), state, retire).value shouldBe state
    }

    test("a phase-2-failed tx does not apply its DRep certificates") {
        // spec [SC-3e]
        val state = stateWith(Coin.zero)
        val unreg = phase2Failed(
          tx(None, Certificate.UnregDRepCert(drepCredential, Coin(params.dRepDeposit)))
        )
        CertsMutator.transit(context(state), state, unreg).value shouldBe state
    }

    test("a phase-2-failed tx skips the withdrawal check") {
        // spec [SC-3f]: Conway LEDGER checks withdrawals only when isValid is true
        val state = stateWith(Coin.ada(7))
        val overdrawn = phase2Failed(tx(Some(Coin.ada(8))))
        CertsValidator.validate(context(state), state, overdrawn) shouldBe Right(())
    }

    test("a phase-2-failed tx skips the stake certificate checks") {
        // spec [SC-3f]: CERTS runs only when isValid is true
        val state = stateWith(Coin.zero)
        val unregistered = Credential.KeyHash(AddrKeyHash.fromHex("d" * 56))
        val delegation =
            phase2Failed(tx(None, Certificate.StakeDelegation(unregistered, poolId)))
        StakeCertificatesValidator.validate(context(state), state, delegation) shouldBe Right(())
    }

    test("a phase-2-failed tx skips the pool certificate checks") {
        // spec [SC-3f]: CERTS runs only when isValid is true
        val state = stateWith(Coin.zero)
        val unknownPool = PoolKeyHash.fromHex("e" * 56)
        val nextEpoch = cardanoInfo.slotConfig.epochOf(0) + 1
        val retire = phase2Failed(tx(None, Certificate.PoolRetirement(unknownPool, nextEpoch)))
        StakePoolCertificatesValidator.validate(context(state), state, retire) shouldBe Right(())
    }

    test("applyWithdrawals subtracts the amount and keeps the account") {
        // spec [SC-2], [SC-3]: a partial amount, so subtraction differs from draining to zero
        val accounts = Map(credential -> ConwayAccountState(Coin.ada(10), keyDeposit, None, None))
        CertsValidator.applyWithdrawals(accounts, SortedMap(rewardAccount -> Coin.ada(3))) shouldBe
            Map(credential -> ConwayAccountState(Coin.ada(7), keyDeposit, None, None))
    }

    // spec [SC-22]: one ordered pass over the certificates, as Haskell CERTS folds them

    private val drepDeposit = Coin(params.dRepDeposit)
    private val drep = DRep.KeyHash(AddrKeyHash.fromHex("c" * 56))
    private val nextEpoch = cardanoInfo.slotConfig.epochOf(0) + 1

    private val newPoolOperator = AddrKeyHash.fromHex("e" * 56)
    private val newPoolId = PoolKeyHash.fromByteString(newPoolOperator)
    private val newPool = pool.copy(operator = newPoolOperator, poolOwners = Set(newPoolOperator))

    private def certs(certificates: Certificate*): Transaction = tx(None, certificates*)

    test("a vote delegation after its DRep re-registers in the same tx is kept") {
        val result = step(
          stateWith(Coin.zero),
          certs(
            Certificate.UnregDRepCert(drepCredential, drepDeposit),
            Certificate.RegDRepCert(drepCredential, drepDeposit, None),
            Certificate.VoteDelegCert(credential, drep)
          )
        ).value
        account(result).flatMap(_.dRepDelegation) shouldBe Some(drep)
    }

    test("a vote delegation before its DRep unregisters and re-registers is cleared") {
        val result = step(
          stateWith(Coin.zero),
          certs(
            Certificate.VoteDelegCert(credential, drep),
            Certificate.UnregDRepCert(drepCredential, drepDeposit),
            Certificate.RegDRepCert(drepCredential, drepDeposit, None)
          )
        ).value
        account(result).flatMap(_.dRepDelegation) shouldBe None
        result.certState.vstate.dreps.keySet shouldBe Set(drepCredential)
    }

    test("a vote delegation to a DRep that unregisters later in the same tx is cleared") {
        // spec [SC-21]
        val result = step(
          stateWith(Coin.zero),
          certs(
            Certificate.VoteDelegCert(credential, drep),
            Certificate.UnregDRepCert(drepCredential, drepDeposit)
          )
        ).value
        account(result).flatMap(_.dRepDelegation) shouldBe None
    }

    test("a DRep script deregistration clears the vote delegations to that script") {
        // spec [SC-21], the Credential.ScriptHash branch
        val scriptHash = ScriptHash.fromHex("d" * 56)
        val scriptDRep = Credential.ScriptHash(scriptHash)
        val base = stateWith(Coin.zero)
        val state = base.copy(certState =
            base.certState.copy(
              vstate = VotingState(Map(scriptDRep -> DRepState(100, None, drepDeposit, Set.empty))),
              dstate = DelegationState(
                Map(
                  credential -> ConwayAccountState(
                    Coin.zero,
                    keyDeposit,
                    None,
                    Some(DRep.ScriptHash(scriptHash))
                  )
                )
              )
            )
        )
        // without the script mutator: the tx carries no script for the DRep's witness
        val result = STS.Mutator
            .transit[TransactionException](
              DefaultMutators.all.filterNot(_ == PlutusScriptsTransactionMutator),
              context(state),
              state,
              certs(Certificate.UnregDRepCert(scriptDRep, drepDeposit))
            )
            .value
        account(result).flatMap(_.dRepDelegation) shouldBe None
    }

    test("a pool registered earlier in the tx can be retired, at a later epoch") {
        // Haskell POOL checks the retirement against the pools after the earlier certificates
        val result = step(
          stateWith(Coin.zero),
          certs(newPool, Certificate.PoolRetirement(newPoolId, nextEpoch))
        ).value
        result.certState.pstate.stakePools.keySet should contain(newPoolId)
        result.certState.pstate.retiring.get(newPoolId) shouldBe Some(nextEpoch)
    }

    test("a pool retirement before its registration in the tx is rejected") {
        val error = step(
          stateWith(Coin.zero),
          certs(Certificate.PoolRetirement(newPoolId, nextEpoch), newPool)
        ).left.value
        error shouldBe a[TransactionException.StakePoolException]
    }

    test("a delegation after a pool retirement in the same tx keeps the pool until the epoch") {
        // Haskell POOL only schedules the retirement; POOLREAP removes the pool at the boundary
        val result = step(
          stateWith(Coin.zero),
          certs(
            Certificate.PoolRetirement(poolId, nextEpoch),
            Certificate.StakeDelegation(credential, poolId)
          )
        ).value
        account(result).flatMap(_.stakePoolDelegation) shouldBe Some(poolId)
        result.certState.pstate.retiring.get(poolId) shouldBe Some(nextEpoch)
    }

    test("a delegation to a pool registered later in the same tx is rejected") {
        val error = step(
          stateWith(Coin.zero),
          certs(Certificate.StakeDelegation(credential, newPoolId), newPool)
        ).left.value
        error match
            case e: TransactionException.StakeCertificatesException =>
                e.delegateeStakePoolsNotRegistered shouldBe Set(newPoolId)
            case other => fail(s"expected StakeCertificatesException, got $other")
    }

    // 1.3 mutator sets may still list the deprecated per-kind mutators next to CertsMutator

    private val newCredential = Credential.KeyHash(AddrKeyHash.fromHex("9" * 56))

    private def withMutators(
        mutators: Iterable[STS.Mutator],
        state: State,
        transaction: Transaction
    ) =
        STS.Mutator.transit[TransactionException](
          Seq(CertsValidator, StakeCertificatesValidator, StakePoolCertificatesValidator),
          mutators,
          context(state),
          state,
          transaction
        )

    @nowarn("cat=deprecation")
    private val withDeprecatedStakeMutator: Iterable[STS.Mutator] =
        ListSet(CertsMutator, StakeCertificatesMutator, PlutusScriptsTransactionMutator)

    test("a deprecated stake mutator next to CertsMutator does not register twice") {
        val state = stateWith(Coin.zero)
        val registration = certs(Certificate.RegCert(newCredential, Some(keyDeposit)))
        withMutators(withDeprecatedStakeMutator, state, registration).value shouldBe
            step(state, registration).value
    }

    @nowarn("cat=deprecation")
    private val onlyDeprecatedStakeMutator: Iterable[STS.Mutator] = Seq(StakeCertificatesMutator)

    test("a deprecated stake mutator without CertsMutator still registers") {
        val state = stateWith(Coin.zero)
        val registration = certs(Certificate.RegCert(newCredential, Some(keyDeposit)))
        val result = withMutators(onlyDeprecatedStakeMutator, state, registration).value
        result.certState.dstate.accounts.get(newCredential) shouldBe
            Some(ConwayAccountState(Coin.zero, keyDeposit, None, None))
    }
}
