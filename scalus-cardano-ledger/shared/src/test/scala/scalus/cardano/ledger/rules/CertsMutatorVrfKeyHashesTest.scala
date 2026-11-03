package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

/** The VRF key map of the POOL step, spec [BK-4] to [BK-6], [BK-8]. */
class CertsMutatorVrfKeyHashesTest extends AnyFunSuite with Matchers with EitherValues {

    private val cardanoInfo = CardanoInfo.preprod

    private val vrf1 = VrfKeyHash.fromHex("a1" * 32)
    private val vrf2 = VrfKeyHash.fromHex("a2" * 32)
    private val vrf3 = VrfKeyHash.fromHex("a3" * 32)
    private val operatorA = AddrKeyHash.fromHex("1" * 56)
    private val operatorB = AddrKeyHash.fromHex("2" * 56)
    private val poolA = PoolKeyHash.fromByteString(operatorA)

    private def registration(
        operator: AddrKeyHash,
        vrf: VrfKeyHash
    ): Certificate.PoolRegistration =
        Certificate.PoolRegistration(
          operator = operator,
          vrfKeyHash = vrf,
          pledge = Coin.ada(1),
          cost = Coin(cardanoInfo.protocolParams.minPoolCost),
          margin = UnitInterval.one,
          rewardAccount = RewardAccount(
            StakeAddress(Network.Testnet, StakePayload.Stake(StakeKeyHash.fromHex("b" * 56)))
          ),
          poolOwners = Set.empty,
          relays = IndexedSeq.empty,
          poolMetadata = None
        )

    private def run(
        major: Int,
        certState: CertState,
        certs: Certificate*
    ): Either[TransactionException, CertState] = {
        val params =
            cardanoInfo.protocolParams.copy(protocolVersion = ProtocolVersion(major, 0))
        val context = new Context(
          env = UtxoEnv(
            slot = 0,
            params = params,
            certState = certState,
            network = Network.Testnet,
            treasury = Coin.zero
          ),
          slotConfig = cardanoInfo.slotConfig
        )
        val tx = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            certificates = TaggedOrderedStrictSet.from(certs)
          )
        )
        CertsMutator.transit(context, State(certState = certState), tx).map(_.certState)
    }

    private def registered(certs: Certificate*): CertState =
        run(11, CertState.empty, certs*).value

    private def vrfError(
        result: Either[TransactionException, CertState]
    ): TransactionException.VrfKeyHashAlreadyRegisteredException =
        result.left.value match
            case e: TransactionException.VrfKeyHashAlreadyRegisteredException => e
            case other => fail(s"not a VrfKeyHashAlreadyRegisteredException: $other")

    test("spec [BK-4]: a new pool records its VRF key at PV 11") {
        registered(registration(operatorA, vrf1)).pstate.vrfKeyHashes shouldBe Map(vrf1 -> 1L)
    }

    test("spec [BK-4]: POOL does not touch the VRF map at PV 10") {
        // a seeded map stays as it is: neither written nor dropped (Pool.hs:273-274, 300)
        val seeded = Map(vrf2 -> 1L)
        val before = CertState.empty.copy(pstate = PoolsState.empty.copy(vrfKeyHashes = seeded))
        val state = run(10, before, registration(operatorA, vrf1)).value
        state.pstate.vrfKeyHashes shouldBe seeded
        run(10, state, registration(operatorA, vrf2)).value.pstate.vrfKeyHashes shouldBe seeded
    }

    test("spec [BK-5]: a new pool with a registered VRF key fails at PV 11") {
        val state = registered(registration(operatorA, vrf1))
        val error = vrfError(run(11, state, registration(operatorB, vrf1)))
        error.poolId shouldBe PoolKeyHash.fromByteString(operatorB)
        error.vrfKeyHash shouldBe vrf1
    }

    test("spec [BK-5]: a new pool with a registered VRF key passes at PV 10 and keeps the map") {
        val state = registered(registration(operatorA, vrf1))
        run(10, state, registration(operatorB, vrf1)).value.pstate.vrfKeyHashes shouldBe
            Map(vrf1 -> 1L)
    }

    test("spec [BK-8]: two new pools with one VRF key in one tx fail on the second") {
        val error = vrfError(
          run(11, CertState.empty, registration(operatorA, vrf1), registration(operatorB, vrf1))
        )
        error.poolId shouldBe PoolKeyHash.fromByteString(operatorB)
    }

    test("spec [BK-6]: a re-registration with the VRF key of another pool fails") {
        val state = registered(registration(operatorA, vrf1), registration(operatorB, vrf2))
        vrfError(run(11, state, registration(operatorA, vrf2))).vrfKeyHash shouldBe vrf2
    }

    test("spec [BK-6]: a re-registration with its own VRF key passes") {
        val state = registered(registration(operatorA, vrf1))
        val after = run(11, state, registration(operatorA, vrf1)).value
        after.pstate.vrfKeyHashes shouldBe Map(vrf1 -> 1L)
        after.pstate.futureStakePoolParams.keySet shouldBe Set(poolA)
    }

    test("spec [BK-4]: a re-registration with no future params overwrites the count with 1") {
        // Pool.hs:293 is `Map.insert`, not a count: a count of 2 from the genesis fill drops to 1
        val state = registered(registration(operatorA, vrf1))
        val seeded = state.copy(pstate = state.pstate.copy(vrfKeyHashes = Map(vrf1 -> 2L)))
        run(11, seeded, registration(operatorA, vrf1)).value.pstate.vrfKeyHashes shouldBe
            Map(vrf1 -> 1L)
    }

    test("spec [BK-4]: a re-registration with a fresh VRF key replaces the future VRF key") {
        val state = registered(registration(operatorA, vrf1))
        val second = run(11, state, registration(operatorA, vrf2)).value
        second.pstate.vrfKeyHashes shouldBe Map(vrf1 -> 1L, vrf2 -> 1L)
        val third = run(11, second, registration(operatorA, vrf3)).value
        // the VRF key of the current params stays until POOLREAP, the future one goes
        third.pstate.vrfKeyHashes shouldBe Map(vrf1 -> 1L, vrf3 -> 1L)
    }

    test("spec [BK-4]: going back to the current VRF key deletes the future VRF key") {
        // Pool.hs:295-298: the future VRF key differs from the new one, so the new one is
        // inserted and the future one is deleted
        val state = registered(registration(operatorA, vrf1))
        val second = run(11, state, registration(operatorA, vrf2)).value
        val back = run(11, second, registration(operatorA, vrf1)).value
        back.pstate.vrfKeyHashes shouldBe Map(vrf1 -> 1L)
    }

    test("spec [BK-4]: a fresh VRF key deletes a future VRF key equal to the current one") {
        // Pool.hs:295-298 deletes the future VRF key even when the current params use it
        val state = registered(registration(operatorA, vrf1))
        val same = run(11, state, registration(operatorA, vrf1)).value
        run(11, same, registration(operatorA, vrf2)).value.pstate.vrfKeyHashes shouldBe
            Map(vrf2 -> 1L)
    }

    test("spec [BK-6]: a second re-registration with the same fresh VRF key fails") {
        // Pool.hs:284-286 compares with the VRF key of the current params, not the future ones,
        // and the first re-registration put the fresh key in the map
        val state = registered(registration(operatorA, vrf1))
        val second = run(11, state, registration(operatorA, vrf2)).value
        vrfError(run(11, second, registration(operatorA, vrf2))).poolId shouldBe poolA
    }

    test("spec [BK-4]: a re-registration with the current and future VRF key changes nothing") {
        // Pool.hs:299 (`otherwise -> id`); the count 2 shows that the map is not rewritten
        val state = registered(registration(operatorA, vrf1))
        val same = run(11, state, registration(operatorA, vrf1)).value
        val seeded = same.copy(pstate = same.pstate.copy(vrfKeyHashes = Map(vrf1 -> 2L)))
        run(11, seeded, registration(operatorA, vrf1)).value.pstate.vrfKeyHashes shouldBe
            Map(vrf1 -> 2L)
    }
}
