package scalus.cardano.txbuilder

import scalus.cardano.address.Network
import scalus.cardano.ledger.*

case class Environment(
    protocolParams: ProtocolParams,
    slotConfig: SlotConfig,
    evaluator: PlutusScriptEvaluator,
    network: Network,
    era: Era = Era.Conway,
)

object Environment {
    def apply(cardanoInfo: CardanoInfo, evaluator: PlutusScriptEvaluator): Environment =
        Environment(
          protocolParams = cardanoInfo.protocolParams,
          slotConfig = cardanoInfo.slotConfig,
          evaluator = evaluator,
          network = cardanoInfo.network,
          era = Era.Conway
        )
}
