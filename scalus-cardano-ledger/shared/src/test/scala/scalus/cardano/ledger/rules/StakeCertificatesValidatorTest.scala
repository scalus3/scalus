package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.ledger.AddrKeyHash
import scalus.cardano.address.Network
import scalus.cardano.ledger.*

class StakeCertificatesValidatorTest extends AnyFunSuite with Matchers with EitherValues {

    private val cardanoInfo = CardanoInfo.preprod
    private val protocolParams = cardanoInfo.protocolParams
    private val keyDeposit = Coin(protocolParams.stakeAddressDeposit)

    private val credential =
        Credential.KeyHash(AddrKeyHash.fromHex("a" * 56))
    private val poolId = PoolKeyHash.fromHex("1" * 56)

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

    private def runValidator(
        certs: Seq[Certificate],
        certState: CertState = CertState.empty
    ) = {
        val txBody = TransactionBody(
          inputs = emptyInputs,
          outputs = emptyOutputs,
          fee = Coin.zero,
          certificates = TaggedOrderedStrictSet.from(certs)
        )
        val tx = Transaction(txBody)
        StakeCertificatesValidator.validate(
          mkContext(certState),
          State(certState = certState),
          tx
        )
    }

    test("registering a new credential succeeds") {
        val result = runValidator(Seq(Certificate.RegCert(credential, None)))
        result.isRight shouldBe true
    }

    test("registering an already registered credential fails") {
        val existingState = CertState.empty.copy(
          dstate = CertState.empty.dstate.copy(
            deposits = Map(credential -> keyDeposit)
          )
        )

        val error =
            runValidator(Seq(Certificate.RegCert(credential, None)), existingState).left.value

        error.alreadyRegistered should contain(credential)
    }

    test("deregistration requires zero rewards") {
        val state = CertState.empty.copy(
          dstate = CertState.empty.dstate.copy(
            deposits = Map(credential -> keyDeposit),
            rewards = Map(credential -> Coin.ada(5))
          )
        )

        val error =
            runValidator(Seq(Certificate.UnregCert(credential, Some(keyDeposit))), state).left.value

        error.nonZeroRewardAccounts should contain(credential -> Coin.ada(5))
    }

    test("deregistration with incorrect refund is rejected") {
        val state = CertState.empty.copy(
          dstate = CertState.empty.dstate.copy(
            deposits = Map(credential -> keyDeposit)
          )
        )

        val error =
            runValidator(
              Seq(Certificate.UnregCert(credential, Some(Coin.ada(1)))),
              state
            ).left.value

        error.invalidRefunds should contain(credential -> (keyDeposit -> Coin.ada(1)))
    }

    test("delegation from unregistered credential fails") {
        val error = runValidator(Seq(Certificate.StakeDelegation(credential, poolId))).left.value
        error.missingRegistrations should contain(credential)
    }

    test("stake registration with incorrect deposit amount is rejected") {
        val wrongDeposit = keyDeposit + Coin.ada(1)
        val error = runValidator(
          Seq(Certificate.StakeRegDelegCert(credential, poolId, wrongDeposit))
        ).left.value

        error.invalidDeposits should contain(credential -> (keyDeposit -> wrongDeposit))
    }
}
