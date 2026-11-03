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

    test("spec [BK-7]: a PV 11 genesis counts the seeded pools per VRF key") {
        // as the hard-fork fill does, conway Rules/HardFork.hs:107-125
        val vrf1 = VrfKeyHash.fromHex("a1" * 32)
        val vrf2 = VrfKeyHash.fromHex("a2" * 32)
        val initState = EmulatorInitialState(
          poolRegistrations = Seq(pool("1", vrf1), pool("2", vrf1), pool("3", vrf2))
        )
        val (certState, _) = EmulatorBase.buildInitialState(initState, Context.testMainnet())
        certState.pstate.vrfKeyHashes shouldBe Map(vrf1 -> 2L, vrf2 -> 1L)
    }

    test("spec [BK-7a]: a genesis below PV 11 counts no VRF key") {
        // the ledger fills the map only on the hard fork into PV 11, so at PV 10 it is empty and
        // [EB-19a] gives the ledger's counts at that fork
        val initState = EmulatorInitialState(
          poolRegistrations = Seq(pool("1", VrfKeyHash.fromHex("a1" * 32)))
        )
        val context = Context.testMainnet()
        val params = context.env.params.copy(protocolVersion = ProtocolVersion(10, 0))
        val pv10 = context.copy(env = context.env.copy(params = params))
        val (certState, _) = EmulatorBase.buildInitialState(initState, pv10)
        certState.pstate.vrfKeyHashes shouldBe empty
    }
}
