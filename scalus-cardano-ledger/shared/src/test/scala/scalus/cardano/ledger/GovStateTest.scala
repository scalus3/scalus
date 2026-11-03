package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.uplc.builtin.ByteString

class GovStateTest extends AnyFunSuite {

    private def actionId(i: Int): GovActionId =
        GovActionId(TransactionHash.fromByteString(ByteString.fromHex(f"$i%02x" * 32)), 0)

    private val returnAccount = RewardAccount(
      StakeAddress(Network.Mainnet, StakePayload.Stake(StakeKeyHash.fromHex("01" * 28)))
    )

    private val constitution =
        Constitution(Anchor("https://example.com", DataHash.fromHex("2" * 64)), None)

    private def proposal(i: Int, action: GovAction): GovActionState =
        GovActionState(
          actionId(i),
          Map.empty,
          Map.empty,
          Map.empty,
          ProposalProcedure(
            Coin(100),
            returnAccount,
            action,
            Anchor("https://example.com", DataHash.fromHex("0" * 64))
          ),
          proposedIn = 1L,
          expiresAfter = 2L
        )

    private def constitutionChild(i: Int, parent: Option[Int]): GovActionState =
        proposal(i, GovAction.NewConstitution(parent.map(actionId), constitution))

    test("removeWithDescendants removes the ids and their descendants, transitively") {
        // spec [EB-12b]: proposalsRemoveWithDescendants, C/Governance/Proposals.hs:465-484
        val root = constitutionChild(1, None)
        val child = constitutionChild(2, Some(1))
        val grandchild = constitutionChild(3, Some(2))
        val sibling = constitutionChild(4, None)
        val info = proposal(5, GovAction.InfoAction)
        val gov = GovState.empty.copy(proposals = Vector(root, info, child, sibling, grandchild))
        val (kept, removed) = gov.removeWithDescendants(Set(actionId(1)))
        assert(kept == gov.copy(proposals = Vector(info, sibling)))
        assert(removed == Vector(root, child, grandchild))
    }

    test("removeWithDescendants of a child keeps its parent, and of no id is identity") {
        // spec [EB-12b]: only descendants go, not ancestors
        val root = constitutionChild(1, None)
        val child = constitutionChild(2, Some(1))
        val gov = GovState.empty.copy(proposals = Vector(root, child))
        assert(
          gov.removeWithDescendants(Set(actionId(2))) == (
            gov.copy(proposals = Vector(root)),
            Vector(child)
          )
        )
        assert(gov.removeWithDescendants(Set.empty) == (gov, Vector.empty))
    }

    // Enactment, spec [EB-12c] (proposalsApplyEnactment, C/Governance/Proposals.hs:492-556)

    private val noParamChange = GovAction.ParameterChange(None, ProtocolParamUpdate(), None)

    test(
      "enact makes the action the root, removes its siblings with their children, keeps its own"
    ) {
        // enactFromRoot: the siblings are the children of the root of the purpose (:530-533)
        val enacted = constitutionChild(1, None)
        val sibling = constitutionChild(2, None)
        val siblingChild = constitutionChild(3, Some(2))
        val child = constitutionChild(4, Some(1))
        // another purpose with no parent is no sibling
        val other = proposal(5, noParamChange)
        val gov =
            GovState.empty.copy(proposals = Vector(enacted, sibling, other, siblingChild, child))
        val (after, removed) = gov.enact(actionId(1))
        assert(after.proposals == Vector(other, child))
        assert(after.roots == GovRoots.empty.copy(constitution = Some(actionId(1))))
        assert(removed.toSet == Set(enacted, sibling, siblingChild))
    }

    test("enact reads the siblings against the root that the enactment before it set") {
        // spec [RT-17], [EB-12c]: a parent and its child enacted in one run, in order (:503-507)
        val parent = constitutionChild(1, None)
        val child = constitutionChild(2, Some(1))
        val childSibling = constitutionChild(3, Some(1))
        val gov = GovState.empty.copy(proposals = Vector(parent, child, childSibling))
        val (first, _) = gov.enact(actionId(1))
        val (second, removed) = first.enact(actionId(2))
        assert(second.proposals.isEmpty)
        assert(second.roots.constitution == Some(actionId(2)))
        assert(removed.toSet == Set(child, childSibling))
    }

    test("enact of a NoConfidence removes the UpdateCommittee that shares its root") {
        // GovActionPurpose: NoConfidence and UpdateCommittee have the committee purpose
        val noConfidence = proposal(1, GovAction.NoConfidence(None))
        val update =
            proposal(2, GovAction.UpdateCommittee(None, Set.empty, Map.empty, UnitInterval.one))
        val gov = GovState.empty.copy(proposals = Vector(noConfidence, update))
        val (after, removed) = gov.enact(actionId(1))
        assert(after.proposals.isEmpty)
        assert(after.roots == GovRoots.empty.copy(committee = Some(actionId(1))))
        assert(removed.toSet == Set(noConfidence, update))
    }

    test("enact of a TreasuryWithdrawals only removes it") {
        // enactWithoutRoot (:510-519)
        val withdrawal =
            proposal(1, GovAction.TreasuryWithdrawals(Map(returnAccount -> Coin(5)), None))
        val other = proposal(2, GovAction.TreasuryWithdrawals(Map(returnAccount -> Coin(6)), None))
        val gov = GovState.empty.copy(proposals = Vector(withdrawal, other))
        assert(gov.enact(actionId(1)) == (gov.copy(proposals = Vector(other)), Vector(withdrawal)))
    }

    test("enact of an id that is not a proposal is identity") {
        // D98
        val gov = GovState.empty.copy(proposals = Vector(constitutionChild(1, None)))
        assert(gov.enact(actionId(9)) == (gov, Vector.empty))
    }
}
