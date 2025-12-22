package scalus.testing.integration

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.model.Network as BloxbeanNetwork
import com.bloxbean.cardano.yaci.test.YaciCardanoContainer
import org.scalatest.{BeforeAndAfterAll, Suite}
import scalus.cardano.address.{Address, Network, StakeAddress}
import scalus.cardano.ledger.{Bech32, CardanoInfo, PoolKeyHash, ProtocolParams, SlotConfig, Transaction}
import scalus.cardano.node.{BlockfrostProvider, Provider}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.BloxbeanAccount
import scalus.utils.await
import sttp.client4.DefaultFutureBackend

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

// Provide sttp backend for BlockfrostProvider
given sttp.client4.Backend[scala.concurrent.Future] = DefaultFutureBackend()

/** Configuration for Yaci DevKit container */
case class YaciDevKitConfig(
    enableLogs: Boolean = false,
    containerName: String = "scalus-yaci-devkit",
    reuseContainer: Boolean = false // Set to true for faster iteration during development
)

/** Singleton container holder for sharing across test suites */
object YaciDevKitContainer {
    private var _container: YaciCardanoContainer = _
    private var _refCount: Int = 0
    private val lock = new Object()

    def acquire(config: YaciDevKitConfig): YaciCardanoContainer = lock.synchronized {
        if (_container == null) {
            _container = createContainer(config)
            _container.start()
        }
        _refCount += 1
        _container
    }

    def release(): Unit = lock.synchronized {
        _refCount -= 1
        // Don't stop the container - let testcontainers/ryuk handle cleanup
        // This allows reuse across test runs when reuse is enabled
    }

    private def createContainer(config: YaciDevKitConfig): YaciCardanoContainer = {
        val container = new YaciCardanoContainer()
        // Don't set container name when not reusing - allows fresh containers each run
        if config.reuseContainer then {
            container.withCreateContainerCmdModifier(cmd => cmd.withName(config.containerName))
            container.withReuse(true)
        }

        if config.enableLogs then {
            container.withLogConsumer(frame => println(s"[Yaci] ${frame.getUtf8String}"))
        }

        container
    }
}

/** Test context containing all utilities needed for transaction building and submission */
case class TestContext(
    cardanoInfo: CardanoInfo,
    provider: Provider,
    account: BloxbeanAccount,
    signer: TransactionSigner,
    address: Address,
    stakeAddress: StakeAddress
) {
    def submitTx(tx: Transaction): Either[String, String] =
        provider.submit(tx).await(30.seconds).map(_.toHex).left.map(_.toString)

    def waitForBlock(): Unit = Thread.sleep(2000)
}

/** Base trait for integration tests using Yaci DevKit with ScalaTest
  *
  * Usage:
  * {{{
  * class MyIntegrationTest extends AnyFunSuite with YaciDevKitSpec {
  *   test("submit transaction to devnet") {
  *     val ctx = createTestContext()
  *     // Use ctx for testing
  *   }
  * }
  * }}}
  */
trait YaciDevKitSpec extends BeforeAndAfterAll { self: Suite =>

    /** Override this to customize the Yaci DevKit configuration */
    def yaciDevKitConfig: YaciDevKitConfig = YaciDevKitConfig()

    /** Fixed mnemonic for reproducible tests (same as Yaci CLI default) */
    val testMnemonic: String =
        "test test test test test test test test test test test test test test test test test test test test test test test sauce"

    /** Network configuration matching Yaci DevKit */
    val yaciNetwork: BloxbeanNetwork = new BloxbeanNetwork(0, 42)

    /** Test account created from the fixed mnemonic */
    lazy val testAccount: Account = Account.createFromMnemonic(yaciNetwork, testMnemonic)

    /** Standard derivation path for Cardano payment keys */
    private val PaymentDerivationPath = "m/1852'/1815'/0'/0/0"

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
        _container = YaciDevKitContainer.acquire(yaciDevKitConfig)
    }

    override def afterAll(): Unit = {
        YaciDevKitContainer.release()
        super.afterAll()
    }

    /** Create TestContext from the running YaciCardanoContainer */
    def createTestContext(): TestContext = {
        // Empty API key for local Yaci Store (Blockfrost-compatible API)
        // Strip trailing slash to avoid double-slash in URLs
        val baseUrl = _container.getYaciStoreApiUrl.stripSuffix("/")
        val provider = BlockfrostProvider("", baseUrl)

        val protocolParams = provider.fetchLatestParams().await()

        // Yaci DevKit uses slot length of 1 second and start time of 0
        val yaciSlotConfig = SlotConfig(
          zeroTime = 0L,
          zeroSlot = 0L,
          slotLength = 1000
        )

        val cardanoInfo = CardanoInfo(protocolParams, Network.Testnet, yaciSlotConfig)

        // Use BloxbeanAccount for proper HD key signing
        val bloxbeanAccount =
            BloxbeanAccount(Network.Testnet, testMnemonic, PaymentDerivationPath)
        // Default signer with only payment key - sufficient for most transactions
        val signer = new TransactionSigner(Set(bloxbeanAccount.paymentKeyPair))

        val address = Address.fromBech32(testAccount.baseAddress())
        val stakeAddress = Address.fromBech32(testAccount.stakeAddress()).asInstanceOf[StakeAddress]

        TestContext(
          cardanoInfo,
          provider,
          bloxbeanAccount,
          signer,
          address,
          stakeAddress
        )
    }
}
