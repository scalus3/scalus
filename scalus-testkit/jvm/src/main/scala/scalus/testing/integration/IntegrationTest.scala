package scalus.testing.integration

import com.bloxbean.cardano.yaci.test.YaciCardanoContainer
import org.scalatest.{BeforeAndAfterAll, Suite}
import org.scalatest.exceptions.TestCanceledException
import scalus.cardano.address.Network
import scalus.cardano.ledger.{CardanoInfo, SlotConfig}
import scalus.cardano.node.{BlockfrostProvider, Emulator}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.hd.HdAccount
import scalus.crypto.ed25519.{Ed25519Signer, JvmEd25519Signer}
import scalus.testing.kit.{Party, ScalusTest}
import scalus.testing.yaci.{YaciConfig, YaciContainer}
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global

/** Base trait for multi-environment integration tests.
  *
  * Override `createTestContext()` to provide a custom environment, or use the built-in factory
  * methods.
  *
  * Environment is selected via SCALUS_TEST_ENV:
  *   - "emulator" (default) - Fast in-memory testing
  *   - "yaci" - YaciDevKit Docker container
  *   - "preprod" - Blockfrost Preprod testnet
  *   - "mainnet" - Blockfrost Mainnet
  *   - Custom values can be handled by overriding createTestContext()
  *
  * Example usage:
  * {{{
  * class MyIntegrationTest extends AnyFunSuite with IntegrationTest {
  *   test("my test") {
  *     val ctx = createTestContext()
  *     val utxos = ctx.provider.findUtxos(ctx.alice.address).await().toOption.get
  *     // ...
  *   }
  * }
  * }}}
  */
trait IntegrationTest extends BeforeAndAfterAll with ScalusTest { self: Suite =>

    // Use JVM Ed25519 signer for key derivation
    given Ed25519Signer = JvmEd25519Signer

    /** Read SCALUS_TEST_ENV, default to "emulator" */
    protected lazy val testEnvName: String =
        sys.env.getOrElse("SCALUS_TEST_ENV", "emulator").toLowerCase.trim

    /** Override to customize YaciDevKit configuration */
    protected def yaciConfig: YaciConfig = YaciConfig()

    /** Get required environment variable or cancel test */
    protected def requireEnv(key: String): String =
        sys.env.getOrElse(key, throw new TestCanceledException(s"$key not set, skipping test", 0))

    // Container management for Yaci
    private var _yaciContainer: Option[YaciCardanoContainer] = None

    /** Get the running Yaci container (if using Yaci environment) */
    protected def yaciContainer: Option[YaciCardanoContainer] = _yaciContainer

    override def beforeAll(): Unit = {
        super.beforeAll()
        if testEnvName == "yaci" then _yaciContainer = Some(YaciContainer.acquire(yaciConfig))
    }

    override def afterAll(): Unit = {
        if testEnvName == "yaci" then YaciContainer.release()
        super.afterAll()
    }

    /** Create test context based on SCALUS_TEST_ENV.
      *
      * Override to add custom environments or change defaults.
      */
    protected def createTestContext(): IntegrationTestContext = testEnvName match
        case "emulator" => createEmulatorContext()
        case "yaci"     => createYaciContext()
        case "preprod"  => createBlockfrostContext("Preprod", Network.Testnet, SlotConfig.preprod)
        case "mainnet"  => createBlockfrostContext("Mainnet", Network.Mainnet, SlotConfig.mainnet)
        case custom =>
            throw new IllegalArgumentException(
              s"Unknown test environment: $custom. " +
                  "Override createTestContext() to handle custom environments."
            )

    // ===== Built-in Factory Methods =====

    /** Create an Emulator-based test context with all Party addresses funded.
      *
      * Uses mainnet CardanoInfo which includes mainnet slotConfig. The emulator internally also
      * uses mainnet slotConfig, ensuring consistent slot-to-time conversions.
      */
    protected def createEmulatorContext(): EmulatorTestContext = {
        given CardanoInfo = CardanoInfo.mainnet
        val addresses = Party.values.toIndexedSeq.map(_.address(Network.Mainnet))
        val emulator = Emulator.withAddresses(addresses)
        val parties = Party.values.toIndexedSeq.map(p =>
            TestParty(p, p.address(Network.Mainnet), p.addrKeyHash, p.signer)
        )
        // Start at a valid slot that works with mainnet slotConfig
        // Slot 10 is before mainnet zeroSlot (4492800), so use a reasonable starting slot
        val startingSlot: Long = 100
        EmulatorTestContext(CardanoInfo.mainnet, emulator, parties, startingSlot)
    }

    /** Create a YaciDevKit-based test context. */
    protected def createYaciContext(): YaciTestContext = {
        val container = _yaciContainer.getOrElse(
          throw new IllegalStateException("YaciDevKit container not started")
        )
        val baseUrl = container.getYaciStoreApiUrl.stripSuffix("/")

        // Yaci DevKit uses slot length of 1 second and start time of 0
        val yaciSlotConfig = SlotConfig(
          zeroTime = 0L,
          zeroSlot = 0L,
          slotLength = 1000
        )

        // Create provider (async) - fetches protocol params during construction
        val provider = BlockfrostProvider.localYaci(baseUrl, 5, yaciSlotConfig).await()

        val parties = Party.values.toIndexedSeq.map(p =>
            TestParty(
              p,
              p.account.baseAddress(Network.Testnet),
              p.addrKeyHash,
              p.signer
            )
        )

        YaciTestContext(
          provider.cardanoInfo,
          provider,
          parties
        )
    }

    /** Create a Blockfrost-based test context for Preprod or Mainnet.
      *
      * Requires environment variables:
      *   - BLOCKFROST_API_KEY: Blockfrost API key
      *   - WALLET_MNEMONIC_PREPROD or WALLET_MNEMONIC_MAINNET: HD wallet mnemonic
      */
    protected def createBlockfrostContext(
        name: String,
        network: Network,
        slotConfig: SlotConfig
    ): BlockfrostTestContext = {
        val apiKey = requireEnv("BLOCKFROST_API_KEY")
        val suffix = name.toUpperCase
        val mnemonic = requireEnv(s"WALLET_MNEMONIC_$suffix")

        val provider = network match
            case Network.Mainnet => BlockfrostProvider.mainnet(apiKey).await()
            case Network.Testnet => BlockfrostProvider.preprod(apiKey).await()
            case _ => throw new IllegalArgumentException(s"Unsupported network: $network")

        // Create parties from mnemonic - each party gets a different account index
        val parties = Party.values.toIndexedSeq.take(3).map { p =>
            val account = HdAccount.fromMnemonic(mnemonic, "", p.ordinal)
            val address = account.baseAddress(network)
            TestParty(
              p,
              address,
              account.paymentKeyHash,
              new TransactionSigner(Set(account.paymentKeyPair))
            )
        }

        BlockfrostTestContext(
          provider.cardanoInfo,
          provider,
          parties,
          name
        )
    }
}
