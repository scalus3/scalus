package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

class CertsValidatorTest extends AnyFunSuite with Matchers with EitherValues with CertsTestKit {

    private val stakeKeyHash = StakeKeyHash.fromHex("a" * 56)
    private val rewardAccount =
        RewardAccount(StakeAddress(Network.Testnet, StakePayload.Stake(stakeKeyHash)))
    private val credential = rewardAccount.address.credential

    private def runValidator(
        withdrawals: Map[RewardAccount, Coin],
        certs: Seq[Certificate] = Seq.empty,
        state: CertState = CertState.empty
    ) = CertsValidator.validate(
      context(state),
      State(certState = state),
      certTx(withdrawals, certs*)
    )

    test("withdrawal succeeds when reward account is registered and drained") {
        val state = certState(accounts = Map(credential -> accountState(Coin.ada(3))))

        val result = runValidator(Map(rewardAccount -> Coin.ada(3)), state = state)
        result.isRight shouldBe true
    }

    test("withdrawal fails when reward account is not registered") {
        val error = runValidator(Map(rewardAccount -> Coin.ada(3))).left.value
        error shouldBe a[TransactionException.WithdrawalsNotInRewardsException]
    }

    test("withdrawal fails when it does not drain the reward account") {
        val state = certState(accounts = Map(credential -> accountState(Coin.ada(5))))

        val error = runValidator(Map(rewardAccount -> Coin.ada(3)), state = state).left.value
        error shouldBe a[TransactionException.WithdrawalsNotInRewardsException]
    }

    test("withdrawal ignores certificates") {
        val state = certState(accounts = Map(credential -> accountState(Coin.ada(2))))

        val result = runValidator(
          withdrawals = Map(rewardAccount -> Coin.ada(2)),
          certs = Seq(Certificate.UnregCert(credential, Some(keyDeposit))),
          state = state
        )
        result.isRight shouldBe true
    }
}
