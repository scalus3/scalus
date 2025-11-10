package scalus.cardano.ledger

import scalus.cardano.address.Network
import scalus.utils.Macros

import scala.annotation.threadUnsafe

/** Contains information about the Cardano network, including protocol parameters and slot
  * configuration
  */
case class CardanoInfo(protocolParams: ProtocolParams, network: Network, slotConfig: SlotConfig) {
    def majorProtocolVersion: MajorProtocolVersion = protocolParams.protocolVersion.toMajor
    def era: Era = Era.Conway
}

object CardanoInfo {

    /** Cardano info for current Cardano Mainnet
      *
      * We use protocol params from epoch 544, major protocol version 10 (Plomin hard fork)
      */
    @threadUnsafe lazy val mainnet: CardanoInfo =
        CardanoInfo(
          ProtocolParams.fromBlockfrostJson(
            inlineResource("blockfrost-params-epoch-544.json")
          ),
          Network.Mainnet,
          SlotConfig.Mainnet
        )

    private inline def inlineResource(name: String): String =
        ${ Macros.inlineResource('name) }
}
