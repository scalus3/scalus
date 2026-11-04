package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

/** `PoolsState` and `VotingState` keep the 1.4 `copy` beside the full one, spec D120. */
class StateCopyCompatTest extends AnyFunSuite {

    private val vrf = VrfKeyHash.fromHex("a1" * 32)
    private val pools = PoolsState.empty.copy(vrfKeyHashes = Map(vrf -> 1L))
    private val pool = PoolKeyHash.fromHex("b1" * 28)

    test("PoolsState has the 1.4 four-map copy in its bytecode") {
        val map = classOf[Map[?, ?]]
        val copy = classOf[PoolsState].getMethod("copy", map, map, map, map)
        val back = copy.invoke(pools, Map.empty, Map.empty, Map(pool -> 3L), Map.empty)
        assert(back == pools.copy(retiring = Map(pool -> 3L)))
    }

    test("the 1.4 PoolsState copy keeps the VRF map") {
        val back = pools.copy(Map.empty, Map.empty, Map(pool -> 3L), Map.empty)
        assert(back.vrfKeyHashes == Map(vrf -> 1L))
        assert(back.retiring == Map(pool -> 3L))
    }

    private val voting = VotingState.empty.copy(numDormantEpochs = 4L)

    test("VotingState has the 1.4 one-map copy in its bytecode") {
        val copy = classOf[VotingState].getMethod("copy", classOf[Map[?, ?]])
        assert(copy.invoke(voting, Map.empty) == voting)
    }

    test("the 1.4 VotingState copy keeps the dormant counter and the committee state") {
        val back = voting.copy(dreps = Map.empty)
        assert(back == voting)
        assert(voting.copy(numDormantEpochs = 0L) == VotingState.empty)
    }
}
