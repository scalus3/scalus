package scalus.cardano.ledger.rules

import scalus.cardano.ledger.Utxos
import scalus.cardano.node.Emulator


class JsLedgerRulesTest extends LedgerRulesTestBase {

    override def makeEmulator(initialUtxos: Utxos, initialContext: Context, validators: Seq[STS.Validator], mutators: Seq[STS.Mutator]) =
        Emulator(initialUtxos, initialContext, validators, mutators)
}
