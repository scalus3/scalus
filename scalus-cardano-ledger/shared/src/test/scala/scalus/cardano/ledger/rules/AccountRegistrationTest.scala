package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

import scala.collection.immutable.SortedMap

/** spec [SC-16]: an account is registered if and only if `accounts` contains its credential. */
class AccountRegistrationTest extends AnyFunSuite with EitherValues {

    private val cardanoInfo = CardanoInfo.preprod
    private val keyDeposit = Coin(cardanoInfo.protocolParams.stakeAddressDeposit)
    private val stakeKeyHash = StakeKeyHash.fromHex("b" * 56)
    private val rewardAccount =
        RewardAccount(StakeAddress(Network.Testnet, StakePayload.Stake(stakeKeyHash)))
    private val credential = rewardAccount.address.credential

    /** Registered, with a zero balance and a zero deposit. */
    private val zeroAccount = CertState.empty.copy(dstate =
        DelegationState(Map(credential -> ConwayAccountState(Coin.zero, Coin.zero, None, None)))
    )

    private def tx(certs: Seq[Certificate], withdrawals: SortedMap[RewardAccount, Coin]) =
        Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty[TransactionInput],
            outputs = IndexedSeq.empty[Sized[TransactionOutput]],
            fee = Coin.zero,
            certificates =
                if certs.isEmpty then TaggedOrderedStrictSet.empty
                else TaggedOrderedStrictSet.from(certs),
            withdrawals = if withdrawals.isEmpty then None else Some(Withdrawals(withdrawals))
          )
        )

    private def context(certState: CertState) = Context(
      env = UtxoEnv(0, cardanoInfo.protocolParams, certState, Network.Testnet),
      slotConfig = cardanoInfo.slotConfig
    )

    private def certs(certState: CertState, cert: Certificate) =
        StakeCertificatesValidator.validate(
          context(certState),
          State(certState = certState),
          tx(Seq(cert), SortedMap.empty)
        )

    private def withdraw(certState: CertState, amount: Coin) =
        CertsValidator.validate(
          context(certState),
          State(certState = certState),
          tx(Nil, SortedMap(rewardAccount -> amount))
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
