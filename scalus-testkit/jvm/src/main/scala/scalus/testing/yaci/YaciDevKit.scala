package scalus.testing.yaci

import com.bloxbean.cardano.yaci.test.YaciCardanoContainer
import org.scalatest.{BeforeAndAfterAll, Suite}
import scalus.cardano.address.Network
import scalus.cardano.ledger.{Bech32, CardanoInfo, PoolKeyHash, SlotConfig}
import scalus.cardano.node.BlockfrostProvider
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.hd.HdAccount
import scalus.crypto.ed25519.JvmEd25519Signer
import scalus.utils.await
import sttp.client4.DefaultFutureBackend

import scala.concurrent.ExecutionContext.Implicits.global

// Provide sttp backend for BlockfrostProvider
given sttp.client4.Backend[scala.concurrent.Future] = DefaultFutureBackend()
// Use JVM Ed25519 signer for key derivation
given scalus.crypto.ed25519.Ed25519Signer = JvmEd25519Signer

/** Base trait for integration tests using Yaci DevKit with ScalaTest
  *
  * This trait provides lifecycle management for Yaci DevKit containers in ScalaTest-based tests. It
  * handles container startup/shutdown automatically using BeforeAndAfterAll hooks.
  *
  * Usage:
  * {{{
  * class MyIntegrationTest extends AnyFunSuite with YaciDevKit {
  *   test("submit transaction to devnet") {
  *     val ctx = createTestContext()
  *     // Use ctx for testing
  *   }
  * }
  * }}}
  *
  * The trait supports container reuse across test runs for faster iteration. Override `yaciConfig`
  * to customize behavior:
  *
  * {{{
  * override def yaciConfig = YaciConfig(
  *   enableLogs = true,
  *   reuseContainer = true
  * )
  * }}}
  */
trait YaciDevKit extends BeforeAndAfterAll { self: Suite =>

    /** Override this to customize the Yaci DevKit configuration */
    def yaciConfig: YaciConfig = YaciConfig()

    /** Fixed mnemonic for reproducible tests (same as Yaci CLI default) */
    val testMnemonic: String =
        "test test test test test test test test test test test test test test test test test test test test test test test sauce"

    /** Test HD account created from the fixed mnemonic (CIP-1852 compatible) */
    lazy val testHdAccount: HdAccount = HdAccount.fromMnemonic(testMnemonic)

    /** Pre-registered pool ID in Yaci DevKit */
    val preRegisteredPoolId: PoolKeyHash = {
        val decoded = Bech32.decode("pool1wvqhvyrgwch4jq9aa84hc8q4kzvyq2z3xr6mpafkqmx9wce39zy")
        PoolKeyHash.fromArray(decoded.data)
    }

    private var _container: YaciCardanoContainer = _

    /** Get the running container */
    def container: YaciCardanoContainer = _container

    override def beforeAll(): Unit = {
        super.beforeAll()
        _container = YaciContainer.acquire(yaciConfig)
    }

    override def afterAll(): Unit = {
        YaciContainer.release()
        super.afterAll()
    }

    /** Create TestContext from the running YaciCardanoContainer
      *
      * This method sets up all the necessary components for transaction building and submission:
      *   - BlockfrostProvider connected to Yaci Store API
      *   - Protocol parameters from the devnet
      *   - Slot configuration (1 second slots, zero start time)
      *   - HdAccount with CIP-1852 compatible HD wallet support
      *   - Transaction signer with payment key
      *   - Base and stake addresses
      *
      * @return
      *   TestContext ready for use in tests
      */
    def createTestContext(): TestContext = {
        // Empty API key for local Yaci Store (Blockfrost-compatible API)
        // Strip trailing slash to avoid double-slash in URLs
        val baseUrl = _container.getYaciStoreApiUrl.stripSuffix("/")
        val provider = BlockfrostProvider("", baseUrl)

        val protocolParams = provider.fetchLatestParams.await()

        // Yaci DevKit uses slot length of 1 second and start time of 0
        val yaciSlotConfig = SlotConfig(
          zeroTime = 0L,
          zeroSlot = 0L,
          slotLength = 1000
        )

        val cardanoInfo = CardanoInfo(protocolParams, Network.Testnet, yaciSlotConfig)

        // Default signer with only payment key - sufficient for most transactions
        val signer = new TransactionSigner(Set(testHdAccount.paymentKeyPair))

        val address = testHdAccount.baseAddress(Network.Testnet)
        val stakeAddress = testHdAccount.stakeAddress(Network.Testnet)

        TestContext(
          cardanoInfo,
          provider,
          testHdAccount,
          signer,
          address,
          stakeAddress
        )
    }
}
