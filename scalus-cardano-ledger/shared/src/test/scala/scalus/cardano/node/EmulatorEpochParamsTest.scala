package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.GovFixtures.{actionId, rewardAccount}
import scalus.cardano.ledger.epoch.{EnactState, RatifyState}
import scalus.cardano.ledger.rules.{Context, State}
import scalus.cardano.node.emulator.{ChainConfig, LedgerSnapshot}

/** The pparams and the ratify result of the epoch state at genesis and across boundaries,
  * ledger-rules spec [DM-9c], [EB-13a], [EB-25].
  */
class EmulatorEpochParamsTest extends AnyFunSuite {

    private val id = actionId(1)
    private val other = actionId(2)
    private val account = rewardAccount(0x0b)
    private val constitution =
        Constitution(Anchor("https://example.com/c", DataHash.fromHex("2" * 64)), None)
    private val committee =
        Committee(Map(account.address.credential -> 500L), UnitInterval(2, 3))
    private val context = Context.testMainnet()
    private val params = context.env.params

    private def genesis(ledger: State): LedgerSnapshot =
        EmulatorState.initial(ledger, context, Map.empty, ChainConfig.instant).tipSnapshot

    test("the genesis ratify result holds the enact state of the genesis and enacts nothing") {
        // spec [DM-9c]: DRComplete def ratifyState with mkEnactState govState, Translation.hs:69-83
        val roots = GovRoots(Some(id), None, Some(other), None)
        val govState = GovState(Vector.empty, roots, Some(committee), constitution)
        val epoch = genesis(State(govState = govState)).epoch
        assert(epoch.prevParams == params)
        assert(
          epoch.pendingRatify == RatifyState(
            EnactState(Some(committee), constitution, params, Map.empty, roots),
            Vector.empty,
            Set.empty,
            false
          )
        )
    }

    /** A genesis whose ratify result enacted a ParameterChange of `txFeePerByte` to 45. */
    private def enactedFee: (LedgerSnapshot, ProtocolParams) = {
        val update = ProtocolParamUpdate(minFeeA = Some(Coin(45)))
        val proposal = GovActionState(
          id,
          Map.empty,
          Map.empty,
          Map.empty,
          ProposalProcedure(
            Coin(100),
            account,
            GovAction.ParameterChange(None, update, None),
            Anchor("https://example.com", DataHash.fromHex("0" * 64))
          ),
          proposedIn = 0L,
          expiresAfter = 1000L
        )
        val snapshot = genesis(State(govState = GovState.empty.copy(proposals = Vector(proposal))))
        val ratified = snapshot.epoch.pendingRatify
        val changed = update.applyTo(params)
        val epoch = snapshot.epoch.copy(pendingRatify =
            ratified.copy(
              enactState = ratified.enactState.copy(params = changed),
              enacted = Vector(id)
            )
        )
        (snapshot.copy(epoch = epoch), changed)
    }

    private def firstSlotAfter(snapshot: LedgerSnapshot, boundaries: Int): SlotNo =
        context.slotConfig.firstSlotOfEpoch(snapshot.epoch.lastEpoch + boundaries)

    test("a boundary that enacts a ParameterChange writes the new pparams to the context") {
        // spec [EB-25], [ES-15]
        val (snapshot, changed) = enactedFee
        val after = snapshot.atSlot(firstSlotAfter(snapshot, 1), ChainConfig.instant)
        assert(after.context.env.params == changed)
        assert(after.epoch.prevParams == params)
    }

    test("a move that crosses no boundary keeps the pparams") {
        // spec [EB-2]
        val (snapshot, _) = enactedFee
        val after = snapshot.atSlot(firstSlotAfter(snapshot, 1) - 1, ChainConfig.instant)
        assert(after.context.env.params == params)
    }

    test("a jump over two boundaries gives what two moves give") {
        // spec [EB-25]: boundary 2 takes the pparams boundary 1 left, so its prevParams are new
        val (snapshot, changed) = enactedFee
        val config = ChainConfig.instant
        val jump = snapshot.atSlot(firstSlotAfter(snapshot, 2), config)
        val twoMoves =
            snapshot
                .atSlot(firstSlotAfter(snapshot, 1), config)
                .atSlot(firstSlotAfter(snapshot, 2), config)
        assert(jump == twoMoves)
        assert(jump.epoch.prevParams == changed)
        assert(jump.context.env.params == changed)
    }
}
