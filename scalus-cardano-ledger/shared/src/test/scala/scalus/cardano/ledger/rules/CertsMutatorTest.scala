package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

class CertsMutatorTest extends AnyFunSuite with Matchers with EitherValues with CertsTestKit {

    private val stakeKeyHash = StakeKeyHash.fromHex("b" * 56)
    private val rewardAccount =
        RewardAccount(StakeAddress(Network.Testnet, StakePayload.Stake(stakeKeyHash)))
    private val credential = rewardAccount.address.credential

    private def runMutator(
        withdrawals: Map[RewardAccount, Coin],
        certs: Seq[Certificate],
        certState: CertState
    ) = CertsMutator.transit(
      context(certState),
      State(certState = certState),
      certTx(withdrawals, certs*)
    )

    test("a full withdrawal keeps the account registered with balance 0") {
        // spec [SC-2], [SC-3]: draining the balance must not deregister the account
        val state = certState(accounts = Map(credential -> accountState(Coin.ada(4))))

        val result = runMutator(
          withdrawals = Map(rewardAccount -> Coin.ada(4)),
          certs = Seq.empty,
          certState = state
        ).value

        result.certState.dstate.accounts.get(credential) shouldBe
            Some(ConwayAccountState(Coin.zero, keyDeposit, None, None))
    }
}
