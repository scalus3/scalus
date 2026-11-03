package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.testing.kit.Party.*

/** The lifecycle of a proposal across epoch boundaries: the Conway Imp `EpochSpec` test "DRep:
  * proposal is accepted after two epochs" (eras/conway/impl/testlib/Test/Cardano/Ledger/Conway/Imp/
  * EpochSpec.hs:354-391), and the lifecycle example of ledger-rules spec section 15, phase 5
  * acceptance 9 and 10.
  */
class EmulatorGovLifecycleTest extends AnyFunSuite {
    import GovImp.*

    test("DRep: proposal is accepted after two epochs") {
        // EpochSpec.hs:354-391; spec [EB-20], [EB-13a], [RT-2]: the votes of epoch 1 are counted
        // at boundary 2, which ratifies it; boundary 3 enacts it
        val imp = GovImp(
          adjust = p =>
              p.copy(dRepVotingThresholds =
                  p.dRepVotingThresholds.copy(ppEconomicGroup = UnitInterval.one)
              ),
          dRepStakes = Seq(1_000_000),
          poolStakes = Seq(42_000_000),
          hotKeys = true
        )
        import imp.*
        val initialParamValue = e.currentParams.txFeePerByte
        val proposedValue = initialParamValue + 300
        val gid = parameterChange(None, ProtocolParamUpdate(minFeeA = Some(Coin(proposedValue))))
        passEpoch()
        assert(!isDRepAccepted(gid))
        assert(e.pendingRatify.enacted.isEmpty)
        yesDRep(0, gid)
        yesPool(0, gid)
        yesCCs(gid)
        assert(isDRepAccepted(gid))
        passEpoch()
        assert(isDRepAccepted(gid))
        assert(e.pendingRatify.enacted == Vector(gid))
        assert(e.currentParams.txFeePerByte == initialParamValue)
        passEpoch()
        assert(e.currentParams.txFeePerByte == proposedValue)
    }

    /** The section 15 example: `govActionLifetime = 6`; the spec's epoch `10 + k` is
      * `startEpoch + k` here. The action is a ParameterChange of `dRepDeposit`, which needs only
      * the committee and the DReps.
      */
    private def lifecycle(): (GovImp, GovActionId) = {
        val imp = GovImp(
          adjust = _.copy(govActionLifetime = 6),
          dRepStakes = Seq(1_000_000),
          hotKeys = true
        )
        val id = imp.parameterChange(None, newDRepDeposit)
        assert(imp.e.govState.proposal(id).get.expiresAfter == imp.epoch(6))
        (imp, id)
    }

    private val newDRepDeposit = ProtocolParamUpdate(drepDeposit = Some(Coin(80_000_000)))

    private def yes(imp: GovImp, id: GovActionId): Unit = {
        imp.yesDRep(0, id)
        imp.yesCCs(id)
    }

    private def moveTo(imp: GovImp, k: Long): Unit =
        imp.e.setSlot(imp.env.slotConfig.firstSlotOfEpoch(imp.epoch(k)))

    test(
      "lifecycle: votes of epoch 10 are ratified at boundary 11, enacted at 12 with the refund"
    ) {
        // spec section 15 example, [EB-20], [EB-12c], [EB-13a], [EB-14a]
        val (imp, id) = lifecycle()
        import imp.*
        yes(imp, id)
        val before = balance(Alice)
        moveTo(imp, 1)
        assert(e.pendingRatify.enacted == Vector(id))
        assert(e.govState.proposal(id).isDefined)
        assert(e.currentParams.dRepDeposit == params.dRepDeposit)
        moveTo(imp, 2)
        assert(e.govState.roots.pparamUpdate == Some(id))
        assert(e.govState.proposal(id).isEmpty)
        assert(e.currentParams.dRepDeposit == 80_000_000L)
        assert(balance(Alice) == before + Coin(params.govActionDeposit))
    }

    test("lifecycle: a vote cast in epoch 11 counts only from boundary 12") {
        // spec section 15 example, [EB-20], [RT-2]: the ratify result of boundary 11 is fixed
        val (imp, id) = lifecycle()
        import imp.*
        moveTo(imp, 1)
        assert(e.pendingRatify.enacted.isEmpty)
        yes(imp, id)
        assert(e.pendingRatify.enacted.isEmpty)
        moveTo(imp, 2)
        assert(e.pendingRatify.enacted == Vector(id))
        assert(e.currentParams.dRepDeposit == params.dRepDeposit)
        moveTo(imp, 3)
        assert(e.currentParams.dRepDeposit == 80_000_000L)
    }

    test("lifecycle: the last ratify attempt is at boundary 17, removal and refund at 18") {
        // spec section 15 example, [RT-10], [EB-12c], [EB-14a]: with no vote the action expires
        // at boundary 17, and boundary 18 removes it and refunds its deposit
        val (imp, id) = lifecycle()
        import imp.*
        val before = balance(Alice)
        moveTo(imp, 6)
        assert(!e.pendingRatify.expired.contains(id))
        moveTo(imp, 7)
        assert(e.pendingRatify.expired == Set(id))
        assert(e.govState.proposal(id).isDefined)
        moveTo(imp, 8)
        assert(e.govState.proposals.isEmpty)
        assert(balance(Alice) == before + Coin(params.govActionDeposit))
        assert(e.currentParams.dRepDeposit == params.dRepDeposit)
    }

    test("lifecycle: a vote cast in epoch 16 is ratified at boundary 17") {
        // spec section 15 example, [RT-10]: an action expires only when it is not enacted
        val (imp, id) = lifecycle()
        import imp.*
        moveTo(imp, 6)
        yes(imp, id)
        moveTo(imp, 7)
        assert(e.pendingRatify.enacted == Vector(id))
        assert(e.pendingRatify.expired.isEmpty)
        moveTo(imp, 8)
        assert(e.govState.roots.pparamUpdate == Some(id))
        assert(e.currentParams.dRepDeposit == 80_000_000L)
    }
}
