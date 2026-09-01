package scalus.cardano.ledger

import scalus.cardano.address.Network
import scalus.utils.Macros

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
      * We use protocol params from epoch 645, major protocol version 11 (van Rossem hard fork,
      * enacted 2026-07-18 at the epoch 643/644 boundary) with the van Rossem cost models enacted on
      * 2026-06-18 (PlutusV1/V2 extended to 332 entries, PlutusV3 to 350)
      */
    lazy val mainnet: CardanoInfo =
        CardanoInfo(
          inlineProtocolParams("blockfrost-params-epoch-645.json"),
          Network.Mainnet,
          SlotConfig.mainnet
        )

    /** Cardano info for Preprod testnet, epoch 310, major protocol version 11 (van Rossem hard
      * fork), including the parameter update enacted at epoch 305 that raised the Plutus memory
      * limits (tx 17,500,000, block 77,500,000) and lowered minPoolCost to 75 ada
      */
    lazy val preprod: CardanoInfo =
        CardanoInfo(
          inlineProtocolParams("blockfrost-params-preprod-310.json"),
          Network.Testnet,
          SlotConfig.preprod
        )

    /** Cardano info for Preview testnet, epoch 1370, major protocol version 11 (van Rossem hard
      * fork)
      */
    lazy val preview: CardanoInfo =
        CardanoInfo(
          inlineProtocolParams("blockfrost-params-preview-1370.json"),
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
