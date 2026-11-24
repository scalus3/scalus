package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

import scala.collection.immutable.SortedMap

/** Withdrawals and certificates through the default mutator pipeline, spec 13.1. */
class WithdrawalsTest extends AnyFunSuite, Matchers, EitherValues, CertsTestKit {

    private val stakeKeyHash = StakeKeyHash.fromHex("b" * 56)
    private val rewardAccount =
        RewardAccount(StakeAddress(Network.Testnet, StakePayload.Stake(stakeKeyHash)))
    private val credential = rewardAccount.address.credential

    private val poolOperator = AddrKeyHash.fromHex("f" * 56)
    private val poolId = PoolKeyHash.fromByteString(poolOperator)
    private val pool: Certificate.PoolRegistration = poolRegistration(
      poolOperator,
      rewardAccount,
      pledge = Coin.ada(100),
      owners = Set(poolOperator)
    )
    private val drepCredential = Credential.KeyHash(AddrKeyHash.fromHex("c" * 56))

    /** The account holds `balance`, a pool and a DRep are registered. */
    private def stateWith(balance: Coin): State = State(certState =
        certState(
          accounts = Map(credential -> accountState(balance)),
          pools = Map(poolId -> pool),
          dreps = Map(drepCredential -> DRepState(100, None, Coin(params.dRepDeposit), Set.empty))
        )
    )

    /** A transaction that withdraws `amount` from `rewardAccount`. */
    private def withdrawTx(amount: Coin, certs: Certificate*): Transaction =
        certTx(Map(rewardAccount -> amount), certs*)

    /** The certificate and withdrawal validators, then every default mutator. */
    private def step(state: State, transaction: Transaction) =
        STS.Mutator.transit[TransactionException](
          Seq(CertsValidator, StakeCertificatesValidator, StakePoolCertificatesValidator),
          DefaultMutators.all,
          context(state.certState),
          state,
          transaction
        )

    private def account(state: State): Option[ConwayAccountState] =
        state.certState.dstate.accounts.get(credential)

    test("withdrawing zero twice keeps the account registered with balance 0") {
        // spec [SC-3c] row 1, [SC-3]
        val first = step(stateWith(Coin.zero), withdrawTx(Coin.zero)).value
        val second = step(first, withdrawTx(Coin.zero)).value
        account(second) shouldBe Some(ConwayAccountState(Coin.zero, keyDeposit, None, None))
    }

    test("withdrawing the full balance and deregistering in one tx removes the account") {
        // spec [SC-3c] row 2, [SC-3a]: withdrawals drain before the certificates are checked
        val result = step(
          stateWith(Coin.ada(7)),
          withdrawTx(Coin.ada(7), Certificate.UnregCert(credential, Some(keyDeposit)))
        ).value
        account(result) shouldBe None
    }

    test("delegating after the full balance was withdrawn keeps the drained balance") {
        // spec [SC-3c] row 3, [SC-2]
        val drained = step(stateWith(Coin.ada(7)), withdrawTx(Coin.ada(7))).value
        val delegated =
            step(drained, certTx(Certificate.StakeDelegation(credential, poolId))).value
        account(delegated) shouldBe Some(
          ConwayAccountState(Coin.zero, keyDeposit, Some(poolId), None)
        )
    }

    test("withdrawing the same 7 ADA twice is rejected the second time") {
        // spec [SC-3c] row 4, [SC-1]
        val first = step(stateWith(Coin.ada(7)), withdrawTx(Coin.ada(7))).value
        val error = step(first, withdrawTx(Coin.ada(7))).left.value
        error shouldBe a[TransactionException.WithdrawalsNotInRewardsException]
    }

    test(
      "the default mutators apply withdrawals and certificates, then GOV, then scripts and UTxO"
    ) {
        // spec [SC-3a]: ledger order, by an explicit list rather than by name; GOV after CERTS,
        // ledger-rules spec [GV-2a], D30
        DefaultMutators.all.toList shouldBe
            List(CertsMutator, GovMutator, PlutusScriptsTransactionMutator)
    }

    // spec [SC-3c] row 5. The whole pipeline needs a failing script; the "Not validating CERT
    // script" conformance vectors cover that. Here each withdrawal and certificate mutator must
    // leave the state alone on its own.
    private def phase2Failed(transaction: Transaction): Transaction =
        transaction.copy(isValid = false)

    test("a phase-2-failed tx does not apply its withdrawals") {
        // spec [SC-3b]
        val state = stateWith(Coin.ada(7))
        val result = CertsMutator
            .transit(context(state.certState), state, phase2Failed(withdrawTx(Coin.ada(7))))
            .value
        result shouldBe state
    }

    test("a phase-2-failed tx does not apply its stake certificates") {
        // spec [SC-3e]
        val state = stateWith(Coin.zero)
        val unreg = phase2Failed(certTx(Certificate.UnregCert(credential, Some(keyDeposit))))
        CertsMutator.transit(context(state.certState), state, unreg).value shouldBe state
    }

    test("a phase-2-failed tx does not apply its pool certificates") {
        // spec [SC-3e]
        val state = stateWith(Coin.zero)
        val nextEpoch = slotConfig.epochOf(0) + 1
        val retire = phase2Failed(certTx(Certificate.PoolRetirement(poolId, nextEpoch)))
        CertsMutator.transit(context(state.certState), state, retire).value shouldBe state
    }

    test("a phase-2-failed tx does not apply its DRep certificates") {
        // spec [SC-3e]
        val state = stateWith(Coin.zero)
        val unreg = phase2Failed(
          certTx(Certificate.UnregDRepCert(drepCredential, Coin(params.dRepDeposit)))
        )
        CertsMutator.transit(context(state.certState), state, unreg).value shouldBe state
    }

    test("a phase-2-failed tx skips the withdrawal check") {
        // spec [SC-3f]: Conway LEDGER checks withdrawals only when isValid is true
        val state = stateWith(Coin.ada(7))
        val overdrawn = phase2Failed(withdrawTx(Coin.ada(8)))
        CertsValidator.validate(context(state.certState), state, overdrawn) shouldBe Right(())
    }

    test("a phase-2-failed tx skips the stake certificate checks") {
        // spec [SC-3f]: CERTS runs only when isValid is true
        val state = stateWith(Coin.zero)
        val unregistered = Credential.KeyHash(AddrKeyHash.fromHex("d" * 56))
        val delegation =
            phase2Failed(certTx(Certificate.StakeDelegation(unregistered, poolId)))
        StakeCertificatesValidator.validate(context(state.certState), state, delegation) shouldBe
            Right(())
    }

    test("a phase-2-failed tx skips the pool certificate checks") {
        // spec [SC-3f]: CERTS runs only when isValid is true
        val state = stateWith(Coin.zero)
        val unknownPool = PoolKeyHash.fromHex("e" * 56)
        val nextEpoch = slotConfig.epochOf(0) + 1
        val retire = phase2Failed(certTx(Certificate.PoolRetirement(unknownPool, nextEpoch)))
        StakePoolCertificatesValidator.validate(context(state.certState), state, retire) shouldBe
            Right(())
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
    private val nextEpoch = slotConfig.epochOf(0) + 1

    private val newPoolOperator = AddrKeyHash.fromHex("e" * 56)
    private val newPoolId = PoolKeyHash.fromByteString(newPoolOperator)
    private val newPool = pool.copy(operator = newPoolOperator, poolOwners = Set(newPoolOperator))

    test("a vote delegation after its DRep re-registers in the same tx is kept") {
        val result = step(
          stateWith(Coin.zero),
          certTx(
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
          certTx(
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
          certTx(
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
              vstate = VotingState.empty
                  .copy(dreps = Map(scriptDRep -> DRepState(100, None, drepDeposit, Set.empty))),
              dstate = DelegationState(
                Map(credential -> accountState(Coin.zero, drep = Some(DRep.ScriptHash(scriptHash))))
              )
            )
        )
        // without the script mutator: the tx carries no script for the DRep's witness
        val result = STS.Mutator
            .transit[TransactionException](
              DefaultMutators.all.filterNot(_ == PlutusScriptsTransactionMutator),
              context(state.certState),
              state,
              certTx(Certificate.UnregDRepCert(scriptDRep, drepDeposit))
            )
            .value
        account(result).flatMap(_.dRepDelegation) shouldBe None
    }

    test("a pool registered earlier in the tx can be retired, at a later epoch") {
        // Haskell POOL checks the retirement against the pools after the earlier certificates
        val result = step(
          stateWith(Coin.zero),
          certTx(newPool, Certificate.PoolRetirement(newPoolId, nextEpoch))
        ).value
        result.certState.pstate.stakePools.keySet should contain(newPoolId)
        result.certState.pstate.retiring.get(newPoolId) shouldBe Some(nextEpoch)
    }

    test("a pool retirement before its registration in the tx is rejected") {
        val error = step(
          stateWith(Coin.zero),
          certTx(Certificate.PoolRetirement(newPoolId, nextEpoch), newPool)
        ).left.value
        error shouldBe a[TransactionException.StakePoolException]
    }

    test("a delegation after a pool retirement in the same tx keeps the pool until the epoch") {
        // Haskell POOL only schedules the retirement; POOLREAP removes the pool at the boundary
        val result = step(
          stateWith(Coin.zero),
          certTx(
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
          certTx(Certificate.StakeDelegation(credential, newPoolId), newPool)
        ).left.value
        error match
            case e: TransactionException.StakeCertificatesException =>
                e.delegateeStakePoolsNotRegistered shouldBe Set(newPoolId)
            case other => fail(s"expected StakeCertificatesException, got $other")
    }
}
