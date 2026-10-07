package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

import scala.collection.immutable.SortedMap

class CertsMutatorTest extends AnyFunSuite with Matchers with EitherValues {

    private val cardanoInfo = CardanoInfo.preprod
    private val protocolParams = cardanoInfo.protocolParams
    private val keyDeposit = Coin(protocolParams.stakeAddressDeposit)

    private val stakeKeyHash = StakeKeyHash.fromHex("b" * 56)
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

    private def runMutator(
        withdrawals: SortedMap[RewardAccount, Coin],
        certs: Seq[Certificate],
        certState: CertState
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
        CertsMutator.transit(
          mkContext(certState),
          State(certState = certState),
          Transaction(txBody)
        )
    }

    // Old maps: the drained credential left `rewards` and stayed in `deposits`, a state the
    // accounts map cannot hold. It now drops the whole account; spec 13.1 [SC-3] replaces this.
    test("withdrawals drain rewards") {
        val certState = CertState.empty.copy(
          dstate = DelegationState(
            Map(credential -> ConwayAccountState(Coin.ada(4), keyDeposit, None, None))
          )
        )

        val result = runMutator(
          withdrawals = SortedMap(rewardAccount -> Coin.ada(4)),
          certs = Seq(Certificate.UnregCert(credential, Some(keyDeposit))),
          certState = certState
        ).value

        result.certState.dstate.accounts.contains(credential) shouldBe false
    }
}
