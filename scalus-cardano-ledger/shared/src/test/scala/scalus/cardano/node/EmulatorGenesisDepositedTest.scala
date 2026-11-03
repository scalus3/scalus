package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.emulator.ChainConfig

class EmulatorGenesisDepositedTest extends AnyFunSuite with Matchers {

    test("spec [BK-1]: genesis deposited is the sum of the seeded deposits") {
        val context = Context.testMainnet()
        val params = context.env.params
        val pool: Certificate.PoolRegistration = Certificate.PoolRegistration(
          operator = AddrKeyHash.fromHex("1" * 56),
          vrfKeyHash = VrfKeyHash.fromHex("a1" * 32),
          pledge = Coin.ada(1),
          cost = Coin.ada(170),
          margin = UnitInterval.one,
          rewardAccount = RewardAccount(
            StakeAddress(Network.Mainnet, StakePayload.Stake(StakeKeyHash.fromHex("b" * 56)))
          ),
          poolOwners = Set.empty,
          relays = IndexedSeq.empty,
          poolMetadata = None
        )
        val drepDeposit = Coin.ada(7)
        val initState = EmulatorInitialState(
          stakeRegistrations = Seq(
            EmulatorStakeRegistration.of(Credential.KeyHash(AddrKeyHash.fromHex("2" * 56)))
          ),
          poolRegistrations = Seq(EmulatorPoolRegistration(pool)),
          drepRegistrations = Seq(
            EmulatorDRepRegistration
                .of(Credential.KeyHash(AddrKeyHash.fromHex("3" * 56)), drepDeposit)
          )
        )
        val (certState, _) = EmulatorBase.buildInitialState(initState, context)
        val state =
            EmulatorState.initial(Utxos.empty, certState, context, Map.empty, ChainConfig.instant)
        state.ledger.deposited shouldBe
            Coin(params.stakeAddressDeposit) + Coin(params.stakePoolDeposit) + drepDeposit
    }
}
