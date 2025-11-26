package scalus.cardano.wallet

import com.bloxbean.cardano.client.account as bloxbean
import com.bloxbean.cardano.client.crypto.bip32.HdKeyPair
import com.bloxbean.cardano.client.crypto.config.CryptoConfiguration

class BloxbeanKeyPair(override val underlying: HdKeyPair) extends KeyPair {
    type Underlying = HdKeyPair
    override def publicKeyBytes: Array[Byte] = underlying.getPublicKey.getBytes

    override def privateKeyBytes: Array[Byte] = underlying.getPrivateKey.getBytes

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
