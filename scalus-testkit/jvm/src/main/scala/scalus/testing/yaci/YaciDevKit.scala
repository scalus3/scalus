package scalus.testing.yaci

import com.bloxbean.cardano.yaci.test.YaciCardanoContainer
import org.scalatest.Suite
import scalus.cardano.ledger.{Bech32, PoolKeyHash}
import scalus.testing.integration.IntegrationTest

/** Base trait for integration tests using Yaci DevKit with ScalaTest
  *
  * This trait extends [[IntegrationTest]] and forces Yaci environment (ignores SCALUS_TEST_ENV).
  * Use `createYaciContext()` to get a [[scalus.testing.integration.YaciTestContext]] with:
  *   - Multi-party access (alice, bob, eve)
  *   - Full HD account access via `account` (Party.Alice with stake, drep, change keys)
  *   - Pre-registered pool ID for staking tests
  *
  * For multi-environment tests, use [[IntegrationTest]] directly instead.
  *
  * Usage:
  * {{{
  * class MyIntegrationTest extends AnyFunSuite with YaciDevKit {
  *   test("submit transaction to devnet") {
  *     val ctx = createYaciContext()
  *     // ctx.account provides full HD wallet access (Party.Alice)
  *     // ctx.alice, ctx.bob, ctx.eve for multi-party scenarios
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
trait YaciDevKit extends IntegrationTest { self: Suite =>

    // Force Yaci environment regardless of SCALUS_TEST_ENV
    override protected lazy val testEnvName: String = "yaci"

    /** Pre-registered pool ID in Yaci DevKit */
    val preRegisteredPoolId: PoolKeyHash = {
        val decoded = Bech32.decode("pool1wvqhvyrgwch4jq9aa84hc8q4kzvyq2z3xr6mpafkqmx9wce39zy")
        PoolKeyHash.fromArray(decoded.data)
    }

    /** Get the running container */
    def container: YaciCardanoContainer = yaciContainer.getOrElse(
      throw new IllegalStateException("YaciDevKit container not started")
    )
}
