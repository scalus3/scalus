package scalus.testing.integration

import scalus.cardano.address.Network
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.BloxbeanAccount
import sttp.client3.*

class HtlcIntegrationTestJvm extends HtlcIntegrationTestBase(using HttpClientFutureBackend()) {
    override protected def makeTransactionSigner(
        derivation: String,
        mnemonic: String
    ): TransactionSigner = {
        val account = BloxbeanAccount(Network.Testnet, mnemonic, derivation)
        new TransactionSigner(Set(account.paymentKeyPair))
    }

    override protected def getEnv(key: String): Option[String] = sys.env.get(key)
}
