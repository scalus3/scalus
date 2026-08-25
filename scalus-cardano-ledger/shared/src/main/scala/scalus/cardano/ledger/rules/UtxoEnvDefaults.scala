package scalus.cardano.ledger.rules

import scalus.cardano.address.Network
import scalus.cardano.ledger.{CardanoInfo, CertState, SlotNo}

/** Provides default UtxoEnv instances using pre-inlined protocol parameters from CardanoInfo.
  *
  * Uses CardanoInfo.mainnet.protocolParams which are inlined at compile time via the
  * inlineProtocolParams macro, avoiding the need to embed JSON strings or load resource files.
  */
private[rules] object UtxoEnvDefaults {

    lazy val default: UtxoEnv =
        UtxoEnv(0, CardanoInfo.mainnet.protocolParams, CertState.empty, Network.Testnet)

    def testMainnet(slot: SlotNo = 0): UtxoEnv =
        UtxoEnv(slot, CardanoInfo.mainnet.protocolParams, CertState.empty, Network.Mainnet)
}
