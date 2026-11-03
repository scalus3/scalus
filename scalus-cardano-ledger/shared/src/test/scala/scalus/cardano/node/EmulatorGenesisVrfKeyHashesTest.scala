package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context

class EmulatorGenesisVrfKeyHashesTest extends AnyFunSuite with Matchers {

    private def pool(operatorHex: String, vrf: VrfKeyHash): EmulatorPoolRegistration =
        EmulatorPoolRegistration(
          Certificate.PoolRegistration(
            operator = AddrKeyHash.fromHex(operatorHex * 56),
            vrfKeyHash = vrf,
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
        )

    test("spec [BK-7]: genesis counts the seeded pools per VRF key") {
        // as the hard-fork fill does, conway Rules/HardFork.hs:107-125
        val vrf1 = VrfKeyHash.fromHex("a1" * 32)
        val vrf2 = VrfKeyHash.fromHex("a2" * 32)
        val initState = EmulatorInitialState(
          poolRegistrations = Seq(pool("1", vrf1), pool("2", vrf1), pool("3", vrf2))
        )
        val (certState, _) = EmulatorBase.buildInitialState(initState, Context.testMainnet())
        certState.pstate.vrfKeyHashes shouldBe Map(vrf1 -> 2L, vrf2 -> 1L)
    }
}
