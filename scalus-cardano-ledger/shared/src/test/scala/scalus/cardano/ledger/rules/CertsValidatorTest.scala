package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

import scala.collection.immutable.SortedMap

class CertsValidatorTest extends AnyFunSuite with Matchers with EitherValues {

    private val cardanoInfo = CardanoInfo.preprod
    private val protocolParams = cardanoInfo.protocolParams
    private val keyDeposit = Coin(protocolParams.stakeAddressDeposit)

    private val stakeKeyHash = StakeKeyHash.fromHex("a" * 56)
    private val rewardAccount =
        RewardAccount(StakeAddress(Network.Testnet, StakePayload.Stake(stakeKeyHash)))
    private val credential = rewardAccount.address.credential

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
        withdrawals: SortedMap[RewardAccount, Coin],
        certs: Seq[Certificate] = Seq.empty,
        certState: CertState = CertState.empty
    ) = {
        val txBody = TransactionBody(
          inputs = emptyInputs,
          outputs = emptyOutputs,
          fee = Coin.zero,
          certificates =
              if certs.isEmpty then TaggedOrderedStrictSet.empty
              else TaggedOrderedStrictSet.from(certs),
          withdrawals = Some(Withdrawals(withdrawals))
        )
        CertsValidator.validate(
          mkContext(certState),
          State(certState = certState),
          Transaction(txBody)
        )
    }

    test("withdrawal succeeds when reward account is registered and drained") {
        val rewards = Map(credential -> Coin.ada(3))
        val certState = CertState.empty.copy(
          dstate = CertState.empty.dstate.copy(rewards = rewards)
        )

        val result = runValidator(SortedMap(rewardAccount -> Coin.ada(3)), certState = certState)
        result.isRight shouldBe true
    }

    test("withdrawal fails when reward account is not registered") {
        val error = runValidator(SortedMap(rewardAccount -> Coin.ada(3))).left.value
        error shouldBe a[TransactionException.WithdrawalsNotInRewardsException]
    }

    test("withdrawal fails when it does not drain the reward account") {
        val rewards = Map(credential -> Coin.ada(5))
        val certState = CertState.empty.copy(
          dstate = CertState.empty.dstate.copy(rewards = rewards)
        )

        val error =
            runValidator(SortedMap(rewardAccount -> Coin.ada(3)), certState = certState).left.value
        error shouldBe a[TransactionException.WithdrawalsNotInRewardsException]
    }

    test("withdrawal ignores certificates") {
        val rewards = Map(credential -> Coin.ada(2))
        val deposits = Map(credential -> keyDeposit)
        val certState = CertState.empty.copy(
          dstate = CertState.empty.dstate.copy(rewards = rewards, deposits = deposits)
        )

        val result = runValidator(
          withdrawals = SortedMap(rewardAccount -> Coin.ada(2)),
          certs = Seq(Certificate.UnregCert(credential, Some(keyDeposit))),
          certState = certState
        )
        result.isRight shouldBe true
    }
}
