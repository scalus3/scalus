package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

class StakeCertificatesValidatorTest
    extends AnyFunSuite
    with Matchers
    with EitherValues
    with CertsTestKit {

    private val credential =
        Credential.KeyHash(AddrKeyHash.fromHex("a" * 56))
    private val poolId = PoolKeyHash.fromHex("1" * 56)

    private val poolOperator = AddrKeyHash.fromHex("1" * 56)
    private val pool: Certificate.PoolRegistration = poolRegistration(
      poolOperator,
      RewardAccount(
        StakeAddress(Network.Testnet, StakePayload.Stake(StakeKeyHash.fromHex("b" * 56)))
      ),
      pledge = Coin.ada(100),
      owners = Set(poolOperator)
    )
    private val drepDeposit = Coin(params.dRepDeposit)
    private val drepCredential = Credential.KeyHash(AddrKeyHash.fromHex("c" * 56))
    private val drep = DRep.KeyHash(AddrKeyHash.fromHex("c" * 56))
    private val scriptDRepHash = ScriptHash.fromHex("d" * 56)
    private val scriptDRep = DRep.ScriptHash(scriptDRepHash)

    /** Only the pool is registered. */
    private val poolOnly = certState(pools = Map(poolId -> pool))

    /** The account, the pool and both DReps are registered. */
    private val registered = certState(
      accounts = Map(credential -> accountState(Coin.zero)),
      pools = Map(poolId -> pool),
      dreps = Map(
        drepCredential -> DRepState(100, None, drepDeposit, Set.empty),
        Credential.ScriptHash(scriptDRepHash) -> DRepState(100, None, drepDeposit, Set.empty)
      )
    )
    private val accountOnly =
        registered.copy(vstate = VotingState.empty, pstate = PoolsState.empty)
    private val poolAndAccount = registered.copy(vstate = VotingState.empty)
    private val newCredential = Credential.KeyHash(AddrKeyHash.fromHex("e" * 56))

    /** Every certificate that delegates stake, from a registered account or with registration. */
    private def stakeDelegations(target: PoolKeyHash): Seq[Certificate] = Seq(
      Certificate.StakeDelegation(credential, target),
      Certificate.StakeVoteDelegCert(credential, target, DRep.AlwaysAbstain),
      Certificate.StakeRegDelegCert(newCredential, target, keyDeposit),
      Certificate.StakeVoteRegDelegCert(newCredential, target, DRep.AlwaysAbstain, keyDeposit)
    )

    /** Every certificate that delegates votes, from a registered account or with registration. */
    private def voteDelegations(target: DRep): Seq[Certificate] = Seq(
      Certificate.VoteDelegCert(credential, target),
      Certificate.StakeVoteDelegCert(credential, poolId, target),
      Certificate.VoteRegDelegCert(newCredential, target, keyDeposit),
      Certificate.StakeVoteRegDelegCert(newCredential, poolId, target, keyDeposit)
    )

    private def runValidator(
        certs: Seq[Certificate],
        state: CertState = CertState.empty
    ) = StakeCertificatesValidator.validate(
      context(state),
      State(certState = state),
      certTx(certs*)
    )

    test("registering a new credential succeeds") {
        val result = runValidator(Seq(Certificate.RegCert(credential, None)))
        result.isRight shouldBe true
    }

    test("registering an already registered credential fails") {
        val existingState = certState(accounts = Map(credential -> accountState(Coin.zero)))

        val error =
            runValidator(Seq(Certificate.RegCert(credential, None)), existingState).left.value

        error.alreadyRegistered should contain(credential)
    }

    test("deregistration requires zero rewards") {
        val state = certState(accounts = Map(credential -> accountState(Coin.ada(5))))

        val error =
            runValidator(Seq(Certificate.UnregCert(credential, Some(keyDeposit))), state).left.value

        error.nonZeroRewardAccounts should contain(credential -> Coin.ada(5))
    }

    test("deregistration with incorrect refund is rejected") {
        val state = certState(accounts = Map(credential -> accountState(Coin.zero)))

        val error =
            runValidator(
              Seq(Certificate.UnregCert(credential, Some(Coin.ada(1)))),
              state
            ).left.value

        error.invalidRefunds should contain(credential -> (keyDeposit -> Coin.ada(1)))
    }

    test("delegation from unregistered credential fails") {
        val error =
            runValidator(Seq(Certificate.StakeDelegation(credential, poolId)), poolOnly).left.value
        error.missingRegistrations should contain(credential)
    }

    test("stake registration with incorrect deposit amount is rejected") {
        val wrongDeposit = keyDeposit + Coin.ada(1)
        val error = runValidator(
          Seq(Certificate.StakeRegDelegCert(credential, poolId, wrongDeposit))
        ).left.value

        error.invalidDeposits should contain(credential -> (keyDeposit -> wrongDeposit))
    }

    test("delegation to an unregistered pool is rejected, for every delegating certificate") {
        // spec [SC-4]: Haskell DelegateeStakePoolNotRegisteredDELEG
        for cert <- stakeDelegations(poolId) do
            withClue(cert) {
                val error = runValidator(Seq(cert), accountOnly).left.value
                error.delegateeStakePoolsNotRegistered shouldBe Set(poolId)
            }
    }

    test("delegation to a registered pool is accepted, for every delegating certificate") {
        // spec [SC-4]
        for cert <- stakeDelegations(poolId) do
            withClue(cert)(runValidator(Seq(cert), registered).isRight shouldBe true)
    }

    test("vote delegation to an unregistered DRep is rejected, for every delegating certificate") {
        // spec [SC-5]: Haskell DelegateeDRepNotRegisteredDELEG
        for
            (target, targetCredential) <- Seq(
              drep -> drepCredential,
              scriptDRep -> Credential.ScriptHash(scriptDRepHash)
            )
            cert <- voteDelegations(target)
        do
            withClue(cert) {
                val error = runValidator(Seq(cert), poolAndAccount).left.value
                error.delegateeDRepsNotRegistered shouldBe Set(targetCredential)
            }
    }

    test("vote delegation to a registered DRep is accepted, for every delegating certificate") {
        // spec [SC-5]
        for cert <- voteDelegations(drep) ++ voteDelegations(scriptDRep) do
            withClue(cert)(runValidator(Seq(cert), registered).isRight shouldBe true)
    }

    test("vote delegation to abstain or no confidence needs no registered DRep") {
        // spec [SC-5]: only a credential DRep must be registered
        for
            target <- Seq(DRep.AlwaysAbstain, DRep.AlwaysNoConfidence)
            cert <- voteDelegations(target)
        do withClue(cert)(runValidator(Seq(cert), poolAndAccount).isRight shouldBe true)
    }

    test("a pool registered earlier in the same tx can be delegated to") {
        // spec [SC-4]: Haskell CERTS applies POOL before the next DELEG sees the pools
        val certs = Seq(pool, Certificate.StakeDelegation(credential, poolId))
        runValidator(certs, accountOnly).isRight shouldBe true
    }

    test("a pool registered later in the same tx cannot be delegated to") {
        // spec [SC-4]: certificates apply in order
        val certs = Seq(Certificate.StakeDelegation(credential, poolId), pool)
        runValidator(certs, accountOnly).left.value.delegateeStakePoolsNotRegistered shouldBe
            Set(poolId)
    }

    test("a DRep registered earlier in the same tx can be delegated to") {
        // spec [SC-5]
        val certs = Seq(
          Certificate.RegDRepCert(drepCredential, drepDeposit, None),
          Certificate.VoteDelegCert(credential, drep)
        )
        runValidator(certs, accountOnly).isRight shouldBe true
    }

    test("a DRep deregistered earlier in the same tx cannot be delegated to") {
        // spec [SC-5]
        val certs = Seq(
          Certificate.UnregDRepCert(drepCredential, drepDeposit),
          Certificate.VoteDelegCert(credential, drep)
        )
        runValidator(certs, registered).left.value.delegateeDRepsNotRegistered shouldBe
            Set(drepCredential)
    }
}
