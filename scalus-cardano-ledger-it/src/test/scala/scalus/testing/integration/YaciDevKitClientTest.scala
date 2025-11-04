package scalus.testing.integration

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.testing.integration.YaciDevKitClient
import scalus.uplc.eval.ExBudget

class YaciDevKitClientTest extends AnyFunSuite {

    val mnemonic =
        "test test test test test test test test test test test test test test test test test test test test test test test sauce"

    val address1 =
        "addr_test1qpqy3lufef8c3en9nrnzp2svwy5vy9zangvp46dy4qw23clgfxhn3pqv243d6wptud7fuaj5tjqer7wc7m036gx0emsqaqa8te"

    val utxo0Index = 0

    val derivationPath0 = "m/1852'/1815'/0'/0/0"
    val wallet0 = new Wallet {
        private val in = TransactionInput(
          TransactionHash.fromHex(
            "6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508"
          ),
          0
        )
        private val out = TransactionOutput(owner, Value.ada(10_000))
        override def selectInputs(
            required: Value
        ): Option[Seq[(TransactionUnspentOutput, Witness)]] = Some(
          Seq(
            (TransactionUnspentOutput(in, out), PubKeyWitness)
          )
        )

        override def utxo: Utxos = Map(in -> out)

        override def collateralInputs: Seq[(TransactionUnspentOutput, Witness)] = Seq.empty

        override def owner: Address = Address.fromBech32(
          "addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex"
        )
    }

    ignore("submit tx") {
        val client = YaciDevKitClient()

        val protocolParams = client.fetchLatestParams()
        val cardanoInfo = CardanoInfo(
          protocolParams = protocolParams,
          network = Network.Testnet,
          slotConfig = SlotConfig.Preview
        )

        val env = Environment(
          protocolParams = protocolParams,
          slotConfig = cardanoInfo.slotConfig,
          evaluator = PlutusScriptEvaluator(
            slotConfig = cardanoInfo.slotConfig,
            initialBudget = ExBudget.enormous,
            protocolMajorVersion = cardanoInfo.majorProtocolVersion,
            costModels = protocolParams.costModels
          ),
          network = cardanoInfo.network
        )

        val context = BuilderContext(env, wallet0)

        val sendAmount = Value.ada(100L)
        val recipientAddr = Address.fromBech32(address1)

        val txResult = PaymentBuilder(context)
            .payTo(recipientAddr, sendAmount)
            .build()

        txResult match {
            case Right(tx) =>
                val signedTx = makeSignerFrom(derivationPath0, mnemonic).signTx(tx)
                client.submit(signedTx) match {
                    case Right(_)    => succeed
                    case Left(error) => fail(s"failed to submit transaction: ${error}")
                }
            case Left(_) => fail("failed to build tx")
        }
    }
}
