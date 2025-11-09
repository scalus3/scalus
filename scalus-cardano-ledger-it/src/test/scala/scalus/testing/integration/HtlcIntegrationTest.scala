package scalus.testing.integration

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.cardano.address.{Address, Network, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.examples.htlc.{HtlcContract, Transactions as HtlcTransactions}
import scalus.uplc.eval.ExBudget

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
              slotConfig = cardanoInfo.slotConfig,
              initialBudget = ExBudget.enormous,
              protocolMajorVersion = cardanoInfo.majorProtocolVersion,
              costModels = cardanoInfo.protocolParams.costModels
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
              slotConfig = cardanoInfo.slotConfig,
              initialBudget = ExBudget.enormous,
              protocolMajorVersion = cardanoInfo.majorProtocolVersion,
              costModels = cardanoInfo.protocolParams.costModels
            )
            val env = Environment(cardanoInfo)
            TestContext(client, cardanoInfo, env, evaluator)
    }

    private def buildWalletWithCollateral(address: Address, utxos: Utxos): Wallet = new Wallet {
        override def selectInputs(
            required: Value
        ): Option[Seq[(Utxo, Witness)]] = {
            utxos.toSeq
                .filter { case (_, output) => output.value.coin.value >= required.coin.value }
                .map { case (input, output) =>
                    (Utxo(input, output), PubKeyWitness)
                }
                .headOption
                .map(Seq(_))
        }

        override def utxo: Utxos = utxos

        override def collateralInputs: Seq[(Utxo, Witness)] = {
            utxos.toSeq
                .filter { case (_, output) =>
                    output.value.coin.value >= 5_000_000 && output.value.assets.isEmpty
                }
                .map { case (input, output) =>
                    (Utxo(input, output), PubKeyWitness)
                }
                .take(1)
        }

        override def owner: Address = address
    }

    private def runLockTest(testEnv: TestEnv): Unit = {
        val mnemonic = getEnvOrSkip("WALLET_MNEMONIC", testEnv)
        val ctx = createTestContext(testEnv)

        val senderAddress = getEnvOrSkip("SENDER_ADDRESS", testEnv)
        val senderAddr = Address.fromBech32(senderAddress)
        val senderPkh = getPaymentPkh(senderAddr)

        val senderUtxos = ctx.client.findUtxos(senderAddr).toOption.get
        assert(senderUtxos.nonEmpty, "No UTXOs found for sender")

        val wallet = buildWalletWithCollateral(senderAddr, senderUtxos)
        val context = BuilderContext(ctx.env, wallet, ctx.evaluator)

        val preimage = ByteString.fromString("secret_preimage_54321")
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

        val lockTxResult = new HtlcTransactions(context, compiledContract).lock(
          Value.lovelace(lockAmount),
          senderPkh,
          senderPkh,
          image,
          timeout
        )

        val lockTx =
            lockTxResult.toOption.getOrElse(fail(s"Failed to build lock tx: $lockTxResult"))
        val signer = makeSignerFrom("m/1852'/1815'/0'/0/0", mnemonic)
        val signedLockTx = signer.signTx(lockTx)

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
        val senderPkh = getPaymentPkh(senderAddr)

        // HTLC parameters - must match the locked UTXO
        val preimage = ByteString.fromString("secret_preimage_54321")
        val compiledContract = HtlcContract.defaultCompiledContract
        val scriptAddress =
            Address(ctx.env.network, Credential.ScriptHash(compiledContract.script.scriptHash))

        // Fetch the locked UTXO from script address
        val scriptUtxos = ctx.client.findUtxos(scriptAddress).toOption.get
        assert(scriptUtxos.nonEmpty, s"No UTXOs found at script address")
        val lockedUtxo = Utxo(scriptUtxos.find(_._2.value.coin.value > 7_000_000L).get)

        // Fetch sender UTXOs for fees and collateral
        val senderUtxos = ctx.client.findUtxos(senderAddr).toOption.get
        assert(senderUtxos.nonEmpty, "No UTXOs found for sender")

        val wallet = buildWalletWithCollateral(senderAddr, senderUtxos)
        val context = BuilderContext(ctx.env, wallet, ctx.evaluator)

        val revealTime = testEnv match {
            case TestEnv.Local =>
                // yaci time works a lil' differently
                ctx.cardanoInfo.slotConfig.slotToTime(100L)
            case TestEnv.Preprod =>
                val revealSlot =
                    ctx.cardanoInfo.slotConfig.timeToSlot(System.currentTimeMillis()) - 100
                ctx.cardanoInfo.slotConfig.slotToTime(revealSlot)
        }

        val revealTxResult = new HtlcTransactions(context, compiledContract).reveal(
          lockedUtxo,
          preimage,
          senderAddr,
          senderPkh,
          revealTime
        )

        val revealTx =
            revealTxResult.toOption.getOrElse(fail(s"Failed to build reveal tx: $revealTxResult"))
        val signer = makeSignerFrom("m/1852'/1815'/0'/0/0", mnemonic)
        val signedRevealTx = signer.signTx(revealTx)

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
