package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.Network
import scalus.cardano.ledger.*

class StakeCertificatesMutatorTest extends AnyFunSuite with Matchers with EitherValues {

    private val cardanoInfo = CardanoInfo.preprod
    private val protocolParams = cardanoInfo.protocolParams
    private val keyDeposit = Coin(protocolParams.stakeAddressDeposit)
    private val credential =
        Credential.KeyHash(AddrKeyHash.fromHex("b" * 56))
    private val poolId = PoolKeyHash.fromHex("2" * 56)
    private val drep = DRep.KeyHash(AddrKeyHash.fromHex("3" * 56))

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

    test("register certificate updates deposits and zeroes rewards") {
        val result =
            runMutator(Seq(Certificate.RegCert(credential, None))).value

        val updatedDState = result.certState.dstate
        updatedDState.deposits(credential) shouldBe keyDeposit
        updatedDState.rewards(credential) shouldBe Coin.zero
    }

    test("combined registration and delegation updates maps") {
        val result = runMutator(
          Seq(Certificate.StakeVoteRegDelegCert(credential, poolId, drep, keyDeposit))
        ).value

        val dstate = result.certState.dstate
        dstate.deposits(credential) shouldBe keyDeposit
        dstate.stakePools(credential) shouldBe poolId
        dstate.dreps(credential) shouldBe drep
    }

    test("delegation updates without changing deposits") {
        val initialState = CertState.empty.copy(
          dstate = CertState.empty.dstate.copy(
            deposits = Map(credential -> keyDeposit)
          )
        )
        val result =
            runMutator(Seq(Certificate.StakeDelegation(credential, poolId)), initialState).value

        result.certState.dstate.stakePools(credential) shouldBe poolId
        result.certState.dstate.deposits(credential) shouldBe keyDeposit
    }

    test("deregistration removes deposits and delegations") {
        val initialState = CertState.empty.copy(
          dstate = CertState.empty.dstate.copy(
            deposits = Map(credential -> keyDeposit),
            stakePools = Map(credential -> poolId),
            dreps = Map(credential -> drep),
            rewards = Map(credential -> Coin.zero)
          )
        )

        val result =
            runMutator(Seq(Certificate.UnregCert(credential, Some(keyDeposit))), initialState).value

        val dstate = result.certState.dstate
        dstate.deposits.contains(credential) shouldBe false
        dstate.stakePools.contains(credential) shouldBe false
        dstate.dreps.contains(credential) shouldBe false
        dstate.rewards.contains(credential) shouldBe false
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
