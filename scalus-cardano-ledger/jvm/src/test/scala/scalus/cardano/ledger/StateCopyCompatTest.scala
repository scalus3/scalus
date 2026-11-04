package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.node.EmulatorInitialState

import java.lang.reflect.Modifier

/** `PoolsState` and `VotingState` keep the 1.4 `copy` beside the full one, spec D120;
  * `EmulatorInitialState` keeps its 1.4 entry points, D121.
  */
class StateCopyCompatTest extends AnyFunSuite {

    private val vrf = VrfKeyHash.fromHex("a1" * 32)
    private val pool = PoolKeyHash.fromHex("b1" * 28)
    private val otherPool = PoolKeyHash.fromHex("b2" * 28)
    private val registration: Certificate.PoolRegistration = Certificate.PoolRegistration(
      AddrKeyHash.fromHex("b1" * 28),
      vrf,
      Coin(1),
      Coin(2),
      UnitInterval(1, 10),
      RewardAccount(
        StakeAddress(Network.Mainnet, StakePayload.Stake(StakeKeyHash.fromHex("c1" * 28)))
      ),
      Set.empty,
      IndexedSeq.empty,
      None
    )
    // every field non-empty, so a 1.4 copy that resets the VRF map fails the tests below
    private val pools = PoolsState(
      Map(pool -> registration),
      Map(pool -> registration),
      Map(pool -> 3L),
      Map(pool -> Coin(500)),
      Map(vrf -> 1L)
    )

    test("PoolsState has the 1.4 four-map copy in its bytecode") {
        val map = classOf[Map[?, ?]]
        val copy = classOf[PoolsState].getMethod("copy", map, map, map, map)
        val back = copy.invoke(pools, Map.empty, Map.empty, Map(otherPool -> 7L), Map.empty)
        assert(
          back == PoolsState(Map.empty, Map.empty, Map(otherPool -> 7L), Map.empty, Map(vrf -> 1L))
        )
    }

    test("the 1.4 PoolsState copy keeps the VRF map") {
        val back = pools.copy(Map.empty, Map.empty, Map(otherPool -> 7L), Map.empty)
        assert(back.vrfKeyHashes == Map(vrf -> 1L))
        assert(back.retiring == Map(otherPool -> 7L))
    }

    private val drep = Credential.KeyHash(AddrKeyHash.fromHex("d1" * 28))
    private val otherDRep = Credential.KeyHash(AddrKeyHash.fromHex("d2" * 28))
    private val cold = Credential.KeyHash(AddrKeyHash.fromHex("e1" * 28))
    private val hot = Credential.KeyHash(AddrKeyHash.fromHex("e2" * 28))
    private def drepState(expiry: Long) = DRepState(expiry, None, Coin(500), Set.empty)
    // every field non-empty, so a 1.4 copy that resets the dormant counter or the committee
    // state fails the tests below
    private val voting = VotingState(
      Map(drep -> drepState(10L)),
      4L,
      Map(cold -> CommitteeAuthorization.HotCredential(hot))
    )
    private val otherDReps = Map(otherDRep -> drepState(20L))

    test("VotingState has the 1.4 one-map copy in its bytecode") {
        val copy = classOf[VotingState].getMethod("copy", classOf[Map[?, ?]])
        assert(
          copy.invoke(voting, otherDReps) ==
              VotingState(otherDReps, 4L, Map(cold -> CommitteeAuthorization.HotCredential(hot)))
        )
    }

    test("the 1.4 VotingState copy keeps the dormant counter and the committee state") {
        val back = voting.copy(dreps = otherDReps)
        assert(back.dreps == otherDReps)
        assert(back.numDormantEpochs == 4L)
        assert(back.committeeState == Map(cold -> CommitteeAuthorization.HotCredential(hot)))
    }

    test("EmulatorInitialState keeps the public 1.4 copy and its defaults in its bytecode") {
        val cls = classOf[EmulatorInitialState]
        val map = classOf[Map[?, ?]]
        val seq = classOf[Seq[?]]
        assert(Modifier.isPublic(cls.getMethod("copy", map, seq, seq, seq, map).getModifiers))
        assert(Modifier.isPublic(cls.getConstructor(map, seq, seq, seq, map).getModifiers))
        for i <- 1 to 5 do
            assert(Modifier.isPublic(cls.getMethod(s"copy$$default$$$i").getModifiers))
    }
}
