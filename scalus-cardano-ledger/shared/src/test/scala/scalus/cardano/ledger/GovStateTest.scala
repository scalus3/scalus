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
}
