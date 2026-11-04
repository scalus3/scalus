package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context

class EmulatorGenesisDRepExpiryTest extends AnyFunSuite with Matchers {

    test("spec [DR-5], [DR-6]: a seeded DRep expires dRepActivity epochs after genesis") {
        val context = Context.testMainnet()
        val genesisEpoch = context.slotConfig.epochOf(context.env.slot)
        val drep = Credential.KeyHash(AddrKeyHash.fromHex("3" * 56))
        val initState = EmulatorInitialState.empty
            .withStakeRegistrations(Seq.empty)
            .withPoolRegistrations(Seq.empty)
            .withDRepRegistrations(Seq(EmulatorDRepRegistration.of(drep, Coin.ada(500))))
        val (certState, _) = EmulatorBase.buildInitialState(initState, context)
        context.env.params.dRepActivity should be > 0L
        certState.vstate.dreps(drep).expiry shouldBe genesisEpoch + context.env.params.dRepActivity
        certState.vstate.numDormantEpochs shouldBe 0L
    }
}
