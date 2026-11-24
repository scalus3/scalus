package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

/** spec [SC-16]: an account is registered if and only if `accounts` contains its credential. */
class AccountRegistrationTest extends AnyFunSuite, EitherValues, CertsTestKit {

    private val stakeKeyHash = StakeKeyHash.fromHex("b" * 56)
    private val rewardAccount =
        RewardAccount(StakeAddress(Network.Testnet, StakePayload.Stake(stakeKeyHash)))
    private val credential = rewardAccount.address.credential

    /** Registered, with a zero balance and a zero deposit. */
    private val zeroAccount = certState(accounts =
        Map(credential -> ConwayAccountState(Coin.zero, Coin.zero, None, None))
    )

    private def certs(state: CertState, cert: Certificate) =
        StakeCertificatesValidator.validate(context(state), State(certState = state), certTx(cert))

    private def withdraw(state: CertState, amount: Coin) =
        CertsValidator.validate(
          context(state),
          State(certState = state),
          certTx(Map(rewardAccount -> amount))
        )

    test("an account with zero balance and zero deposit is registered for certificates") {
        val error = certs(zeroAccount, Certificate.RegCert(credential, Some(keyDeposit))).left.value
        assert(error.alreadyRegistered == Set(credential))
    }

    test("an account with zero balance and zero deposit is registered for withdrawals") {
        assert(withdraw(zeroAccount, Coin.zero).isRight)
    }

    test("a credential not in accounts is not registered for certificates") {
        val error = certs(CertState.empty, Certificate.UnregCert(credential, None)).left.value
        assert(error.missingRegistrations == Set(credential))
    }

    test("a credential not in accounts is not registered for withdrawals") {
        assert(withdraw(CertState.empty, Coin.zero).isLeft)
    }
}
