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
      * We use protocol params from epoch 616, major protocol version 10 (Plomin hard fork)
      */
    @threadUnsafe lazy val mainnet: CardanoInfo =
        CardanoInfo(
          inlineProtocolParams("blockfrost-params-epoch-616.json"),
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

    /** Verify that actual CardanoInfo matches expected. Returns Right(actual) if they match, Left
      * with differences otherwise. Checks network, slotConfig, and all protocol parameter fields.
      */
    def verify(
        expected: CardanoInfo,
        actual: CardanoInfo
    ): Either[Seq[ProtocolParams.ParamDiff], CardanoInfo] = {
        val topLevelDiffs = Seq.newBuilder[ProtocolParams.ParamDiff]
        if expected.network != actual.network then
            topLevelDiffs += ProtocolParams.ParamDiff(
              "network",
              expected.network.toString,
              actual.network.toString
            )
        if expected.slotConfig != actual.slotConfig then
            topLevelDiffs += ProtocolParams.ParamDiff(
              "slotConfig",
              expected.slotConfig.toString,
              actual.slotConfig.toString
            )
        val allDiffs =
            topLevelDiffs
                .result() ++ ProtocolParams.diff(expected.protocolParams, actual.protocolParams)
        if allDiffs.isEmpty then Right(actual)
        else Left(allDiffs)
    }

    private inline def inlineProtocolParams(name: String): ProtocolParams =
        ${ Macros.inlineProtocolParams('name) }
}
