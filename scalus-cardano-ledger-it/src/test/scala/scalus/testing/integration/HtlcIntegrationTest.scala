package scalus.testing.integration

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.crypto.cip1852.{DerivationPath, Segment}
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.cardano.address.{Address, Network, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.examples.htlc.{HtlcContract, Transactions2}

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
        val derivationPieces = derivation.split("/").drop(1).map(_.stripSuffix("'")).map(_.toInt)
        val derivationPath = DerivationPath
            .builder()
            .purpose(new Segment(derivationPieces(0), true))
            .coinType(new Segment(derivationPieces(1), true))
            .account(new Segment(derivationPieces(2), true))
            .role(new Segment(derivationPieces(3), false))
            .index(new Segment(derivationPieces(4), false))
            .build()
        val account = Account.createFromMnemonic(Networks.testnet(), mnemonic, derivationPath)
        val publicKeyBytes = account.publicKeyBytes()
        val privateKeyBytes = account.privateKeyBytes()

        val publicKey = ByteString.fromArray(publicKeyBytes)
        val pkh = AddrKeyHash(scalus.builtin.platform.blake2b_224(publicKey))

        new TransactionSigner {
            override val publicKeyHashes: Set[AddrKeyHash] = Set(pkh)

            // override the default signer to just use the hardcoded keypair
            override protected def signEd25519(
                addrKeyHash: AddrKeyHash,
                transactionId: TransactionHash
            ): VKeyWitness = {
                val signature = scalus.testing.integration.signEd25519(
                  privateKeyBytes,
                  publicKeyBytes,
                  transactionId.bytes
                )
                VKeyWitness(publicKey, ByteString.fromArray(signature))
            }
        }
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
        val senderPkh = AddrKeyHash.fromByteString(getPaymentPkh(senderAddr))

        val senderUtxos = ctx.client.findUtxos(senderAddr).toOption.get
        assert(senderUtxos.nonEmpty, "No UTXOs found for sender")

        val signer = makeTransactionSigner("m/1852'/1815'/0'/0/0", mnemonic)
        val signers = Map(senderAddr -> signer)

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
        
        val utxoToSpend = Utxo(
          senderUtxos.find(_._2.value.coin.value >= lockAmount).get
        )

        val lockTxResult = new Transactions2(ctx.env, signers, compiledContract).lock(
          utxoToSpend,
          Value.lovelace(lockAmount),
          senderPkh,
          senderPkh,
          image,
          timeout
        )

        val signedLockTx =
            lockTxResult.toOption.getOrElse(fail(s"Failed to build lock tx: $lockTxResult"))

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
        val senderPkh = AddrKeyHash.fromByteString(getPaymentPkh(senderAddr))

        val signer = makeTransactionSigner("m/1852'/1815'/0'/0/0", mnemonic)
        val signers = Map(senderAddr -> signer)

        // HTLC parameters - must match the locked UTXO
        val preimage = ByteString.fromString("secret_preimage_54321")
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

        val revealTxResult = new Transactions2(ctx.env, signers, compiledContract).reveal(
          lockedUtxo,
          collateralUtxo,
          preimage,
          senderAddr,
          senderPkh,
          revealTime
        )

        val signedRevealTx =
            revealTxResult.toOption.getOrElse(fail(s"Failed to build reveal tx: $revealTxResult"))

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
