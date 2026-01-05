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
          inlineProtocolParams("blockfrost-params-epoch-544.json"),
          Network.Mainnet,
          SlotConfig.mainnet
        )

    /** Cardano info for Preprod testnet */
    @threadUnsafe lazy val preprod: CardanoInfo =
        CardanoInfo(
          inlineProtocolParams("blockfrost-params-preprod-258.json"),
          Network.Testnet,
          SlotConfig.preprod
        )

    /** Cardano info for Preview testnet */
    @threadUnsafe lazy val preview: CardanoInfo =
        CardanoInfo(
          inlineProtocolParams("blockfrost-params-preview-1145.json"),
          Network.Testnet,
          SlotConfig.preview
        )

    private inline def inlineProtocolParams(name: String): ProtocolParams =
        ${ Macros.inlineProtocolParams('name) }
}
