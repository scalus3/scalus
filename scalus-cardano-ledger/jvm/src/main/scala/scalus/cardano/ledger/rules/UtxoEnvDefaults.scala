package scalus.cardano.ledger.rules

import scalus.cardano.address.Network
import scalus.cardano.ledger.{CertState, ProtocolParams, SlotNo}

import scala.annotation.threadUnsafe

/** JVM implementation - loads protocol parameters from resource file. */
private[rules] object UtxoEnvDefaults {

    @threadUnsafe lazy val default: UtxoEnv = {
        val params: ProtocolParams = ProtocolParams.fromBlockfrostJson(
          this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        )
        UtxoEnv(0, params, CertState.empty, Network.Testnet)
    }

    def testMainnet(slot: SlotNo = 0): UtxoEnv = {
        val params: ProtocolParams = ProtocolParams.fromBlockfrostJson(
          this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        )
        UtxoEnv(slot, params, CertState.empty, Network.Mainnet)
    }
}
