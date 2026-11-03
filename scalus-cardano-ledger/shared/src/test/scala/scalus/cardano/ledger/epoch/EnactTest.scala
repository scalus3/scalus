package scalus.cardano.ledger.epoch

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.GovFixtures.*
import scalus.cardano.ledger.epoch.internal.Ratify

/** ENACT, `Ratify.enact`, one test per gov action, ledger-rules spec [EN-1], [EN-5], [EN-6] (conway
  * Rules/Enact.hs:83-134).
  */
class EnactTest extends AnyFunSuite {

    private def constitution(i: Int): Constitution =
        Constitution(Anchor(s"https://example.com/$i", DataHash.fromHex(f"$i%02x" * 32)), None)

    private val committee =
        Committee(Map(credential(1) -> 10L, credential(2) -> 20L), UnitInterval(1, 2))

    private val state = EnactState(
      committee = Some(committee),
      constitution = constitution(0),
      params = CardanoInfo.mainnet.protocolParams,
      withdrawals = Map(credential(5) -> Coin(7)),
      roots =
          GovRoots(Some(actionId(90)), Some(actionId(91)), Some(actionId(92)), Some(actionId(93)))
    )

    private val treasury = Coin(1_000)
    private val id = actionId(1)

    private def enact(action: GovAction, from: EnactState = state): (EnactState, Coin) =
        Ratify.enact(from, treasury, id, action)

    test("a ParameterChange applies its update and becomes the pparam root") {
        val update = ProtocolParamUpdate(minFeeA = Some(Coin(45)))
        val (after, left) = enact(GovAction.ParameterChange(state.roots.pparamUpdate, update, None))
        assert(
          after == state.copy(
            params = update.applyTo(state.params),
            roots = state.roots.copy(pparamUpdate = Some(id))
          )
        )
        assert(left == treasury)
    }

    test("a HardForkInitiation sets the protocol version and becomes the hard fork root") {
        val version = ProtocolVersion(11, 1)
        val (after, left) = enact(GovAction.HardForkInitiation(state.roots.hardFork, version))
        assert(
          after == state.copy(
            params = state.params.copy(protocolVersion = version),
            roots = state.roots.copy(hardFork = Some(id))
          )
        )
        assert(left == treasury)
    }

    test(
      "a TreasuryWithdrawals adds each amount per credential and takes the sum from the treasury"
    ) {
        // spec [EN-6]: Map.unionWith (<>) with the earlier withdrawals; no root
        val withdrawals = Map(rewardAccount(5) -> Coin(3), rewardAccount(6) -> Coin(40))
        val (after, left) = enact(GovAction.TreasuryWithdrawals(withdrawals, None))
        assert(
          after == state.copy(withdrawals =
              Map(credential(5) -> Coin(10), credential(6) -> Coin(40))
          )
        )
        assert(left == Coin(957))
    }

    test("a NoConfidence removes the committee and becomes the committee root") {
        val (after, left) = enact(GovAction.NoConfidence(state.roots.committee))
        assert(
          after == state.copy(committee = None, roots = state.roots.copy(committee = Some(id)))
        )
        assert(left == treasury)
    }

    test("an UpdateCommittee with no committee makes one of the added members") {
        // spec [EN-5]: the removed members are ignored (updatedCommittee, Enact.hs:124-125)
        val action = GovAction.UpdateCommittee(
          None,
          Set(credential(1)),
          Map(credential(1) -> 30L, credential(3) -> 40L),
          UnitInterval(2, 3)
        )
        val (after, _) = enact(action, state.copy(committee = None))
        assert(
          after.committee == Some(
            Committee(Map(credential(1) -> 30L, credential(3) -> 40L), UnitInterval(2, 3))
          )
        )
        assert(after.roots == state.roots.copy(committee = Some(id)))
    }

    test("an UpdateCommittee removes members, adds members and sets the threshold") {
        // spec [EN-5]: Map.union added (current withoutKeys removed), Enact.hs:126-134: an added
        // term wins over the old one, and a member both removed and added stays with its new term
        val action = GovAction.UpdateCommittee(
          state.roots.committee,
          Set(credential(1), credential(2)),
          Map(credential(2) -> 25L, credential(3) -> 40L),
          UnitInterval(3, 4)
        )
        val (after, left) = enact(action)
        assert(
          after == state.copy(
            committee = Some(
              Committee(Map(credential(2) -> 25L, credential(3) -> 40L), UnitInterval(3, 4))
            ),
            roots = state.roots.copy(committee = Some(id))
          )
        )
        assert(left == treasury)
    }

    test("an added member's term replaces the term of a member that stays") {
        val action = GovAction.UpdateCommittee(
          None,
          Set.empty,
          Map(credential(1) -> 99L),
          UnitInterval(1, 2)
        )
        val (after, _) = enact(action)
        assert(
          after.committee == Some(
            committee.copy(members = Map(credential(1) -> 99L, credential(2) -> 20L))
          )
        )
    }

    test("a NewConstitution sets the constitution and becomes the constitution root") {
        val (after, left) =
            enact(GovAction.NewConstitution(state.roots.constitution, constitution(4)))
        assert(
          after == state.copy(
            constitution = constitution(4),
            roots = state.roots.copy(constitution = Some(id))
          )
        )
        assert(left == treasury)
    }

    test("an InfoAction changes nothing") {
        assert(enact(GovAction.InfoAction) == (state, treasury))
    }

    test("a hard fork to PV 12 or more is out of the Conway rules") {
        // spec [EN-4a], D91: RATIFY takes it as not accepted
        def supported(major: Int) =
            Ratify.isConwayAction(GovAction.HardForkInitiation(None, ProtocolVersion(major, 0)))
        assert(supported(11))
        assert(!supported(12))
        assert(!supported(13))
        assert(Ratify.isConwayAction(GovAction.InfoAction))
    }
}
