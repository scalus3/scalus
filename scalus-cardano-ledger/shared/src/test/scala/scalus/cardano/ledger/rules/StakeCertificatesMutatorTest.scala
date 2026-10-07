package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

class StakeCertificatesMutatorTest extends AnyFunSuite with Matchers with EitherValues {

    private val cardanoInfo = CardanoInfo.preprod
    private val protocolParams = cardanoInfo.protocolParams
    private val keyDeposit = Coin(protocolParams.stakeAddressDeposit)
    private val credential =
        Credential.KeyHash(AddrKeyHash.fromHex("b" * 56))
    private val poolId = PoolKeyHash.fromHex("2" * 56)
    private val drep = DRep.KeyHash(AddrKeyHash.fromHex("3" * 56))

    private val poolRegistration: Certificate.PoolRegistration = Certificate.PoolRegistration(
      operator = AddrKeyHash.fromHex("2" * 56),
      vrfKeyHash = VrfKeyHash.fromHex("a" * 64),
      pledge = Coin.ada(100),
      cost = Coin(protocolParams.minPoolCost),
      margin = UnitInterval.one,
      rewardAccount = RewardAccount(
        StakeAddress(Network.Testnet, StakePayload.Stake(StakeKeyHash.fromHex("b" * 56)))
      ),
      poolOwners = Set.empty,
      relays = IndexedSeq.empty,
      poolMetadata = None
    )

    /** The delegation targets: the pool and the DRep are registered, spec [SC-4], [SC-5]. */
    private val targets = CertState.empty.copy(
      vstate = VotingState(
        Map(
          Credential.KeyHash(AddrKeyHash.fromHex("3" * 56)) ->
              DRepState(100, None, Coin(protocolParams.dRepDeposit), Set.empty)
        )
      ),
      pstate = PoolsState(stakePools = Map(poolId -> poolRegistration))
    )

    private val emptyInputs = TaggedSortedSet.empty[TransactionInput]
    private val emptyOutputs = IndexedSeq.empty[Sized[TransactionOutput]]

    private def mkContext(certState: CertState): Context =
        new Context(
          env = UtxoEnv(
            slot = 0,
            params = protocolParams,
            certState = certState,
            network = Network.Testnet
          ),
          slotConfig = cardanoInfo.slotConfig
        )

    private def toTx(certs: Seq[Certificate]): Transaction =
        Transaction(
          TransactionBody(
            inputs = emptyInputs,
            outputs = emptyOutputs,
            fee = Coin.zero,
            certificates = TaggedOrderedStrictSet.from(certs)
          )
        )

    private def runMutator(
        certs: Seq[Certificate],
        certState: CertState = CertState.empty
    ) =
        StakeCertificatesMutator.transit(
          mkContext(certState),
          State(certState = certState),
          toTx(certs)
        )

    test("register certificate adds an account with the deposit and a zero balance") {
        val result =
            runMutator(Seq(Certificate.RegCert(credential, None))).value

        result.certState.dstate.accounts(credential) shouldBe
            ConwayAccountState(Coin.zero, keyDeposit, None, None)
    }

    test("combined registration and delegation sets both delegations") {
        val result = runMutator(
          Seq(Certificate.StakeVoteRegDelegCert(credential, poolId, drep, keyDeposit)),
          targets
        ).value

        result.certState.dstate.accounts(credential) shouldBe
            ConwayAccountState(Coin.zero, keyDeposit, Some(poolId), Some(drep))
    }

    test("delegation updates without changing the deposit or the balance") {
        val initialState = targets.copy(
          dstate = DelegationState(
            Map(credential -> ConwayAccountState(Coin(5), keyDeposit, None, None))
          )
        )
        val result =
            runMutator(Seq(Certificate.StakeDelegation(credential, poolId)), initialState).value

        result.certState.dstate.accounts(credential) shouldBe
            ConwayAccountState(Coin(5), keyDeposit, Some(poolId), None)
    }

    test("deregistration removes the account") {
        val initialState = CertState.empty.copy(
          dstate = DelegationState(
            Map(credential -> ConwayAccountState(Coin.zero, keyDeposit, Some(poolId), Some(drep)))
          )
        )

        val result =
            runMutator(Seq(Certificate.UnregCert(credential, Some(keyDeposit))), initialState).value

        result.certState.dstate.accounts.contains(credential) shouldBe false
    }

    test("invalid certificate reuses validator failures") {
        val wrongDeposit = keyDeposit + Coin.ada(1)
        val error =
            runMutator(
              Seq(Certificate.StakeRegDelegCert(credential, poolId, wrongDeposit))
            ).left.value

        error.invalidDeposits should contain(credential -> (keyDeposit -> wrongDeposit))
    }
}
