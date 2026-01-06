package scalus.examples.htlc

import org.scalatest.Assertion
import org.scalatest.funsuite.AsyncFunSuite
import scalus.builtin.ByteString
import scalus.builtin.ByteString.utf8
import scalus.cardano.address.{Address, Network, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.node.BlockfrostProvider
import scalus.cardano.txbuilder.*
import scalus.ledger.api.v1.PubKeyHash
import scalus.testing.IntegrationTest
import sttp.client4.*

import scala.concurrent.{ExecutionContext, Future}

/** Base class for HTLC integration tests that interact with real Cardano testnets.
  *
  * These tests are tagged with [[IntegrationTest]] and are excluded from regular test runs. They
  * require external infrastructure (YaciDevKit for local tests, Blockfrost API for Preprod tests).
  *
  * ==Running the tests==
  *
  * To run these integration tests, use:
  * {{{
  * # JVM tests only
  * sbtn "scalusExamplesJVM/testOnly -- -n scalus.testing.IntegrationTest"
  *
  * # JS tests only
  * sbtn "scalusExamplesJS/testOnly -- -n scalus.testing.IntegrationTest"
  * }}}
  *
  * ==Required Environment Variables==
  *
  * ===For Local tests (YaciDevKit)===
  *   - `WALLET_MNEMONIC_LOCAL`: 24-word mnemonic phrase for the test wallet
  *   - `SENDER_ADDRESS_LOCAL`: Bech32 address derived from the mnemonic (e.g., `addr_test1qz...`)
  *
  * ===For Preprod tests===
  *   - `WALLET_MNEMONIC_PREPROD`: 24-word mnemonic phrase for the test wallet
  *   - `SENDER_ADDRESS_PREPROD`: Bech32 address derived from the mnemonic
  *   - `BLOCKFROST_API_KEY`: Blockfrost API key for Preprod network
  *
  * ==Prerequisites==
  *
  * ===Local tests===
  * Require YaciDevKit running locally. Start it with:
  * {{{
  * yaci-cli:devnet start
  * }}}
  *
  * ===Preprod tests===
  * Require:
  *   - A Blockfrost account with Preprod API access
  *   - A funded wallet on Preprod testnet (get test ADA from the faucet)
  *
  * @see
  *   [[https://yaci-devkit.cardano.org/ YaciDevKit documentation]]
  * @see
  *   [[https://blockfrost.io/ Blockfrost API]]
  */
abstract class HtlcIntegrationTestBase(using backend: Backend[Future]) extends AsyncFunSuite {

    implicit override def executionContext: ExecutionContext =
        scala.concurrent.ExecutionContext.global

    enum TestEnv:
        case Local
        case Preprod

    case class TestContext(
        client: BlockfrostProvider,
        cardanoInfo: CardanoInfo,
        evaluator: PlutusScriptEvaluator
    )

    protected def makeTransactionSigner(derivation: String, mnemonic: String): TransactionSigner

    protected def getEnv(key: String): Option[String]

    private def getEnvOrSkip(name: String, testEnv: TestEnv): String = {
        val postfix = testEnv match {
            case TestEnv.Local   => "LOCAL"
            case TestEnv.Preprod => "PREPROD"
        }
        val key = s"${name}_$postfix"
        getEnv(key).getOrElse {
            cancel(s"$key environment variable not set. Skipping test.")
        }
    }

    private def getPaymentPkh(address: Address): ByteString = address match {
        case shelley: ShelleyAddress => ByteString.fromArray(shelley.payment.asHash.bytes)
        case _ => throw new IllegalArgumentException(s"Unsupported address type: $address")
    }

    private def createTestContext(testEnv: TestEnv): Future[TestContext] = testEnv match {
        case TestEnv.Local =>
            val client = BlockfrostProvider.localYaci
            client.fetchLatestParams
                .map { protocolParams =>
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
                    TestContext(client, cardanoInfo, evaluator)
                }

        case TestEnv.Preprod =>
            val apiKey = getEnv("BLOCKFROST_API_KEY").getOrElse(
              throw new IllegalStateException("BLOCKFROST_API_KEY environment variable not set")
            )
            val client = BlockfrostProvider(apiKey, BlockfrostProvider.preprodUrl)
            client.fetchLatestParams
                .map { protocolParams =>
                    val cardanoInfo = CardanoInfo(
                      protocolParams = protocolParams,
                      network = Network.Testnet,
                      slotConfig = SlotConfig.preprod
                    )
                    val evaluator = PlutusScriptEvaluator(
                      cardanoInfo,
                      EvaluatorMode.EvaluateAndComputeCost
                    )
                    TestContext(client, cardanoInfo, evaluator)
                }
    }

    protected def runLockTest(testEnv: TestEnv): Future[Assertion] = {
        val mnemonic = getEnvOrSkip("WALLET_MNEMONIC", testEnv).trim
        val senderAddress = getEnvOrSkip("SENDER_ADDRESS", testEnv).trim
        val senderAddr = Address.fromBech32(senderAddress)
        val senderPkh = PubKeyHash(getPaymentPkh(senderAddr))

        for {
            ctx <- createTestContext(testEnv)

            signer = makeTransactionSigner("m/1852'/1815'/0'/0/0", mnemonic)

            preimage = utf8"secret_preimage_54321"
            image = scalus.builtin.Builtins.sha3_256(preimage)
            timeout = testEnv match {
                case TestEnv.Local =>
                    val timeoutSlot =
                        ctx.cardanoInfo.slotConfig.timeToSlot(System.currentTimeMillis()) + 1875
                    BigInt(ctx.cardanoInfo.slotConfig.slotToTime(timeoutSlot))
                case TestEnv.Preprod =>
                    val timeoutSlot =
                        ctx.cardanoInfo.slotConfig.timeToSlot(System.currentTimeMillis()) + 1875
                    BigInt(ctx.cardanoInfo.slotConfig.slotToTime(timeoutSlot))
            }
            lockAmount = 8_000_000L

            compiledContract = HtlcContract

            txCreator = HtlcTransactionCreator(
              ctx.cardanoInfo,
              ctx.evaluator,
              compiledContract
            )

            // Use lockAsync for cross-platform support (works on both JVM and JS)
            signedLockTx <- txCreator.lockAsync(
              Value.lovelace(lockAmount),
              senderAddr,
              AddrKeyHash(senderPkh.hash),
              AddrKeyHash(senderPkh.hash),
              image,
              timeout.toLong,
              ctx.client,
              signer
            )

            submitResult <- ctx.client.submit(signedLockTx)
        } yield {
            submitResult match {
                case Right(_) =>
                    println(s"Lock TX ($testEnv): ${signedLockTx.id.toHex}")
                    succeed
                case Left(error) =>
                    fail(s"Failed to submit lock transaction: $error")
            }
        }
    }

    protected def runRevealTest(testEnv: TestEnv): Future[Assertion] = {
        val mnemonic = getEnvOrSkip("WALLET_MNEMONIC", testEnv).trim
        val senderAddress = getEnvOrSkip("SENDER_ADDRESS", testEnv).trim
        val senderAddr = Address.fromBech32(senderAddress)
        val senderPkh = PubKeyHash(getPaymentPkh(senderAddr))

        for {
            ctx <- createTestContext(testEnv)

            signer = makeTransactionSigner("m/1852'/1815'/0'/0/0", mnemonic)
            signers = Map(senderAddr -> signer)

            // HTLC parameters - must match the locked UTXO
            preimage = utf8"secret_preimage_54321"
            compiledContract = HtlcContract
            scriptAddress = Address(
              ctx.cardanoInfo.network,
              Credential.ScriptHash(compiledContract.script.scriptHash)
            )

            // Fetch the locked UTXO from script address
            scriptUtxosResult <- ctx.client.findUtxos(scriptAddress)
            scriptUtxos = scriptUtxosResult.toOption.get
            _ = assert(scriptUtxos.nonEmpty, s"No UTXOs found at script address")
            lockedUtxo = Utxo(scriptUtxos.find(_._2.value.coin.value > 7_000_000L).get)

            // Fetch sender UTXOs for collateral
            senderUtxosResult <- ctx.client.findUtxos(senderAddr)
            senderUtxos = senderUtxosResult.toOption.get
            _ = assert(senderUtxos.nonEmpty, "No UTXOs found for sender")

            collateralUtxo = Utxo(
              senderUtxos
                  .find { case (_, output) =>
                      output.value.coin.value >= 5_000_000 && output.value.assets.isEmpty
                  }
                  .getOrElse(fail("No suitable collateral UTXO found"))
            )

            revealTime = testEnv match {
                case TestEnv.Local =>
                    // yaci time works a lil' differently
                    ctx.cardanoInfo.slotConfig.slotToTime(100L)
                case TestEnv.Preprod =>
                    val revealSlot =
                        ctx.cardanoInfo.slotConfig.timeToSlot(System.currentTimeMillis()) - 100
                    ctx.cardanoInfo.slotConfig.slotToTime(revealSlot)
            }

            txCreator = HtlcTransactionCreator(
              ctx.cardanoInfo,
              ctx.evaluator,
              compiledContract
            )

            signedRevealTx = txCreator.reveal(
              Map.empty,
              Map(collateralUtxo.input -> collateralUtxo.output),
              lockedUtxo,
              senderAddr,
              senderAddr,
              preimage,
              AddrKeyHash(senderPkh.hash),
              revealTime,
              signer
            )

            submitResult <- ctx.client.submit(signedRevealTx)
        } yield {
            submitResult match {
                case Right(_) =>
                    println(s"Reveal TX (${testEnv}): ${signedRevealTx.id.toHex}")
                    succeed
                case Left(error) =>
                    fail(s"Failed to submit reveal transaction: $error")
            }
        }
    }

    test("lock HTLC - local", IntegrationTest) {
        runLockTest(TestEnv.Local)
    }

    test("reveal HTLC - local", IntegrationTest) {
        runRevealTest(TestEnv.Local)
    }

    test("lock HTLC - preprod", IntegrationTest) {
        runLockTest(TestEnv.Preprod)
    }

    test("reveal HTLC - preprod", IntegrationTest) {
        runRevealTest(TestEnv.Preprod)
    }
}
