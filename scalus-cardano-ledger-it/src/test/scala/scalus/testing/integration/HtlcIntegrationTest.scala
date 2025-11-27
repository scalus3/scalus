package scalus.testing.integration

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.builtin.ByteString.utf8
import scalus.cardano.address.{Address, Network, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.wallet.BloxbeanAccount
import scalus.examples.htlc.{HtlcContract, HtlcTransactionCreator}
import scalus.ledger.api.v1.PubKeyHash

class HtlcIntegrationTest extends AnyFunSuite {

    enum TestEnv:
        case Local
        case Preprod

    case class TestContext(
        client: BlockfrostClient,
        cardanoInfo: CardanoInfo,
        env: Environment,
        evaluator: PlutusScriptEvaluator
    )

    private def makeTransactionSigner(derivation: String, mnemonic: String): TransactionSigner = {
        val account = BloxbeanAccount(Network.Testnet, mnemonic, derivation)
        val keyPair = account.paymentKeyPair
        new TransactionSigner(Set(keyPair))
    }

    private def getEnvOrSkip(name: String, testEnv: TestEnv): String = {
        val postfix = testEnv match {
            case TestEnv.Local   => "LOCAL"
            case TestEnv.Preprod => "PREPROD"
        }
        val key = s"${name}_$postfix"
        sys.env.getOrElse(
          key, {
              cancel(s"$key environment variable not set. Skipping test.")
          }
        )
    }

    private def getPaymentPkh(address: Address): ByteString = address match {
        case shelley: ShelleyAddress => ByteString.fromArray(shelley.payment.asHash.bytes)
        case _ => throw new IllegalArgumentException(s"Unsupported address type: $address")
    }

    private def createTestContext(testEnv: TestEnv): TestContext = testEnv match {
        case TestEnv.Local =>
            val client = BlockfrostClient.localYaci
            val protocolParams = client.fetchLatestParams()
            // YaciDevKit uses Start Time: 0, Slot Length: 1 sec
            val yaciSlotConfig = SlotConfig(
              zeroTime = 0L,
              zeroSlot = 0L,
              slotLength = 1000L // 1 second
            )
            val cardanoInfo = CardanoInfo(
              protocolParams = protocolParams,
              network = Network.Testnet,
              slotConfig = yaciSlotConfig
            )
            val evaluator = PlutusScriptEvaluator(
              cardanoInfo,
              EvaluatorMode.EvaluateAndComputeCost
            )
            val env = Environment(cardanoInfo)
            TestContext(client, cardanoInfo, env, evaluator)

        case TestEnv.Preprod =>
            val apiKey = sys.env("BLOCKFROST_API_KEY")
            val client = BlockfrostClient(apiKey, BlockfrostClient.PreprodUrl)
            val protocolParams = client.fetchLatestParams()
            val cardanoInfo = CardanoInfo(
              protocolParams = protocolParams,
              network = Network.Testnet,
              slotConfig = SlotConfig.Preprod
            )
            val evaluator = PlutusScriptEvaluator(
              cardanoInfo,
              EvaluatorMode.EvaluateAndComputeCost
            )
            val env = Environment(cardanoInfo)
            TestContext(client, cardanoInfo, env, evaluator)
    }

    private def runLockTest(testEnv: TestEnv): Unit = {
        val mnemonic = getEnvOrSkip("WALLET_MNEMONIC", testEnv)
        val ctx = createTestContext(testEnv)

        val senderAddress = getEnvOrSkip("SENDER_ADDRESS", testEnv)
        val senderAddr = Address.fromBech32(senderAddress)
        val senderPkh = PubKeyHash(getPaymentPkh(senderAddr))

        val senderUtxos = ctx.client.findUtxos(senderAddr).toOption.get
        assert(senderUtxos.nonEmpty, "No UTXOs found for sender")

        val signer = makeTransactionSigner("m/1852'/1815'/0'/0/0", mnemonic)
        val signers = Map(senderAddr -> signer)

        val preimage = utf8"secret_preimage_54321"
        val image = scalus.builtin.Builtins.sha3_256(preimage)
        val timeout = testEnv match {
            case TestEnv.Local =>
                val timeoutSlot =
                    ctx.cardanoInfo.slotConfig.timeToSlot(System.currentTimeMillis()) + 1875
                BigInt(ctx.cardanoInfo.slotConfig.slotToTime(timeoutSlot))
            case TestEnv.Preprod =>
                val timeoutSlot =
                    ctx.cardanoInfo.slotConfig.timeToSlot(System.currentTimeMillis()) + 1875
                BigInt(ctx.cardanoInfo.slotConfig.slotToTime(timeoutSlot))
        }
        val lockAmount = 8_000_000L

        val compiledContract = HtlcContract.defaultCompiledContract

        val utxoToSpend = Utxo(
          senderUtxos.find(_._2.value.coin.value >= lockAmount).get
        )

        val txCreator = HtlcTransactionCreator(
          ctx.env,
          ctx.evaluator,
          signer,
          compiledContract
        )

        val signedLockTx = txCreator.lock(
          Map(utxoToSpend.input -> utxoToSpend.output),
          Value.lovelace(lockAmount),
          senderAddr,
          AddrKeyHash(senderPkh.hash),
          AddrKeyHash(senderPkh.hash),
          image,
          timeout.toLong
        )

        ctx.client.submit(signedLockTx) match {
            case Right(_) =>
                println(s"Lock TX (${testEnv}): ${signedLockTx.id.toHex}")
            case Left(error) =>
                fail(s"Failed to submit lock transaction: $error")
        }
    }

    private def runRevealTest(testEnv: TestEnv): Unit = {
        val mnemonic = getEnvOrSkip("WALLET_MNEMONIC", testEnv)
        val ctx = createTestContext(testEnv)

        val senderAddress = getEnvOrSkip("SENDER_ADDRESS", testEnv)
        val senderAddr = Address.fromBech32(senderAddress)
        val senderPkh = PubKeyHash(getPaymentPkh(senderAddr))

        val signer = makeTransactionSigner("m/1852'/1815'/0'/0/0", mnemonic)
        val signers = Map(senderAddr -> signer)

        // HTLC parameters - must match the locked UTXO
        val preimage = utf8"secret_preimage_54321"
        val compiledContract = HtlcContract.defaultCompiledContract
        val scriptAddress =
            Address(ctx.env.network, Credential.ScriptHash(compiledContract.script.scriptHash))

        // Fetch the locked UTXO from script address
        val scriptUtxos = ctx.client.findUtxos(scriptAddress).toOption.get
        assert(scriptUtxos.nonEmpty, s"No UTXOs found at script address")
        val lockedUtxo = Utxo(scriptUtxos.find(_._2.value.coin.value > 7_000_000L).get)

        // Fetch sender UTXOs for collateral
        val senderUtxos = ctx.client.findUtxos(senderAddr).toOption.get
        assert(senderUtxos.nonEmpty, "No UTXOs found for sender")

        val collateralUtxo = Utxo(
          senderUtxos
              .find { case (_, output) =>
                  output.value.coin.value >= 5_000_000 && output.value.assets.isEmpty
              }
              .getOrElse(fail("No suitable collateral UTXO found"))
        )

        val revealTime = testEnv match {
            case TestEnv.Local =>
                // yaci time works a lil' differently
                ctx.cardanoInfo.slotConfig.slotToTime(100L)
            case TestEnv.Preprod =>
                val revealSlot =
                    ctx.cardanoInfo.slotConfig.timeToSlot(System.currentTimeMillis()) - 100
                ctx.cardanoInfo.slotConfig.slotToTime(revealSlot)
        }

        val txCreator = HtlcTransactionCreator(
          ctx.env,
          ctx.evaluator,
          signer,
          compiledContract
        )

        val signedRevealTx = txCreator.reveal(
          Map.empty,
          Map(collateralUtxo.input -> collateralUtxo.output),
          lockedUtxo,
          senderAddr,
          senderAddr,
          preimage,
          AddrKeyHash(senderPkh.hash),
          revealTime
        )

        ctx.client.submit(signedRevealTx) match {
            case Right(_) =>
                println(s"Reveal TX (${testEnv}): ${signedRevealTx.id.toHex}")
            case Left(error) =>
                fail(s"Failed to submit reveal transaction: $error")
        }
    }

    test("lock HTLC - local") {
        runLockTest(TestEnv.Local)
    }

    test("reveal HTLC - local") {
        runRevealTest(TestEnv.Local)
    }

    test("lock HTLC - preprod") {
        runLockTest(TestEnv.Preprod)
    }

    test("reveal HTLC - preprod") {
        runRevealTest(TestEnv.Preprod)
    }
}
