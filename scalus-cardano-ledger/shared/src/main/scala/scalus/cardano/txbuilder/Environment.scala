package scalus.cardano.txbuilder

import scalus.cardano.address.Network
import scalus.cardano.ledger.*

case class Environment(
    protocolParams: ProtocolParams,
    slotConfig: SlotConfig,
    network: Network,
    era: Era = Era.Conway,
)

object Environment {
    def apply(cardanoInfo: CardanoInfo): Environment =
        Environment(
          protocolParams = cardanoInfo.protocolParams,
          slotConfig = cardanoInfo.slotConfig,
          network = cardanoInfo.network,
          era = Era.Conway
        )
}
