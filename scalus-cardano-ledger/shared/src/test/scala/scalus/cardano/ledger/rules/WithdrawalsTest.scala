package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

import scala.collection.immutable.SortedMap

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
      env = UtxoEnv(0, params, state.certState, Network.Testnet),
      slotConfig = cardanoInfo.slotConfig
    )

    /** The certificate and withdrawal validators, then every default mutator. */
    private def step(state: State, transaction: Transaction) =
        STS.Mutator.transit[TransactionException](
          Seq(CertsValidator, StakeCertificatesValidator),
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
        DefaultMutators.all.toList shouldBe List(
          CertsMutator,
          StakeCertificatesMutator,
          StakePoolCertificatesMutator,
          VotingCertificatesMutator,
          PlutusScriptsTransactionMutator
        )
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
        StakeCertificatesMutator.transit(context(state), state, unreg).value shouldBe state
    }

    test("a phase-2-failed tx does not apply its pool certificates") {
        // spec [SC-3e]
        val state = stateWith(Coin.zero)
        val nextEpoch = cardanoInfo.slotConfig.epochOf(0) + 1
        val retire = phase2Failed(tx(None, Certificate.PoolRetirement(poolId, nextEpoch)))
        StakePoolCertificatesMutator.transit(context(state), state, retire).value shouldBe state
    }

    test("a phase-2-failed tx does not apply its DRep certificates") {
        // spec [SC-3e]
        val state = stateWith(Coin.zero)
        val unreg = phase2Failed(
          tx(None, Certificate.UnregDRepCert(drepCredential, Coin(params.dRepDeposit)))
        )
        VotingCertificatesMutator.transit(context(state), state, unreg).value shouldBe state
    }

    test("applyWithdrawals subtracts the amount and keeps the account") {
        // spec [SC-2], [SC-3]: a partial amount, so subtraction differs from draining to zero
        val accounts = Map(credential -> ConwayAccountState(Coin.ada(10), keyDeposit, None, None))
        CertsValidator.applyWithdrawals(accounts, SortedMap(rewardAccount -> Coin.ada(3))) shouldBe
            Map(credential -> ConwayAccountState(Coin.ada(7), keyDeposit, None, None))
    }
}
