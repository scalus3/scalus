package scalus.cardano.wallet

import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.account as bloxbean
import com.bloxbean.cardano.client.crypto.bip32.HdKeyPair
import com.bloxbean.cardano.client.crypto.cip1852.{DerivationPath, Segment}
import com.bloxbean.cardano.client.crypto.config.CryptoConfiguration
import scalus.cardano.address.Network

class BloxbeanKeyPair(override val underlying: HdKeyPair) extends KeyPair {
    type Underlying = HdKeyPair
    override def publicKeyBytes: Array[Byte] = underlying.getPublicKey.getKeyData

    override def privateKeyBytes: Array[Byte] = underlying.getPrivateKey.getKeyData

    override def sign(message: Array[Byte]): Array[Byte] = {
        val signingProvider = CryptoConfiguration.INSTANCE.getSigningProvider
        signingProvider.signExtended(message, privateKeyBytes)
    }
}

class BloxbeanAccount(val account: bloxbean.Account) extends Account {
    override def paymentKeyPair: KeyPair = BloxbeanKeyPair(account.hdKeyPair())

    override def changeKeyPair: KeyPair = paymentKeyPair

    override def stakeKeyPair: KeyPair = BloxbeanKeyPair(account.stakeHdKeyPair())

    override def drepKeyPair: KeyPair = BloxbeanKeyPair(account.drepHdKeyPair())
}

object BloxbeanAccount {
    def apply(network: Network, mnemonic: String, derivationPath: String): BloxbeanAccount = {
        val cclNetwork = network match {
            case Network.Testnet =>
                Networks.testnet()
            case Network.Mainnet =>
                Networks.mainnet()
            case Network.Other(v) =>
                throw new IllegalStateException(s"Unknown network: $v")
        }
        val cclAccount = bloxbean.Account.createFromMnemonic(
          cclNetwork,
          mnemonic,
          makeDerivationPath(derivationPath)
        )
        new BloxbeanAccount(cclAccount)
    }

    private def makeDerivationPath(dp: String) = {
        val derivationPieces = dp.split("/").drop(1).map(_.stripSuffix("'")).map(_.toInt)
        DerivationPath
            .builder()
            .purpose(new Segment(derivationPieces(0), true))
            .coinType(new Segment(derivationPieces(1), true))
            .account(new Segment(derivationPieces(2), true))
            .role(new Segment(derivationPieces(3), false))
            .index(new Segment(derivationPieces(4), false))
            .build()
    }
}
