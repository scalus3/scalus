package scalus.examples.htlc

import scalus.cardano.address.Network
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.LucidEvolutionAccount
import sttp.client4.*

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("process")
object Process extends js.Object {
    def env: js.Dictionary[String] = js.native
}

class HtlcIntegrationTestJs extends HtlcIntegrationTestBase(using DefaultFutureBackend()) {
    override protected def makeTransactionSigner(
        derivation: String,
        mnemonic: String
    ): TransactionSigner = {
        val account = LucidEvolutionAccount(Network.Testnet, mnemonic, derivation)
        new TransactionSigner(Set(account.paymentKeyPair))
    }

    override protected def getEnv(key: String): Option[String] = {
        if js.typeOf(Process) != "undefined" && js.typeOf(Process.env) != "undefined" then {
            Process.env.get(key)
        } else {
            None
        }
    }
}
