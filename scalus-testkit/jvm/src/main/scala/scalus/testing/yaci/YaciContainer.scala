package scalus.testing.yaci

import com.bloxbean.cardano.yaci.test.YaciCardanoContainer
import org.testcontainers.utility.DockerImageName

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import scala.compiletime.uninitialized
import scala.util.Try

/** Singleton container holder for sharing across test suites with reference counting
  *
  * This object manages the lifecycle of a Yaci DevKit container instance, allowing multiple test
  * suites to share the same container. Reference counting ensures the container stays alive as long
  * as any test suite needs it.
  */
object YaciContainer:
    private var _container: YaciCardanoContainer = uninitialized
    private val lock = new Object()

    /** Acquire container (starts if not running, increments ref count)
      *
      * @param config
      *   Configuration for the container
      * @return
      *   Running YaciCardanoContainer instance
      */
    def acquire(config: YaciConfig): YaciCardanoContainer = lock.synchronized {
        if _container == null then
            _container = createContainer(config)
            _container.start()
            awaitStoreSync(_container, config.startupTimeoutSeconds)
        _container
    }

    /** Release container (decrements ref count)
      *
      * Note: Does not stop the container - cleanup is handled by testcontainers/ryuk. This allows
      * container reuse across test runs when reuse is enabled.
      */
    def release(): Unit = lock.synchronized {
        // Don't stop the container - let testcontainers/ryuk handle cleanup
        // This allows reuse across test runs when reuse is enabled
    }

    private def createContainer(config: YaciConfig): YaciCardanoContainer = {
        val image = DockerImageName.parse("bloxbean/yaci-cli").withTag(config.imageTag)
        val container =
            new YaciCardanoContainer(image, 1f, config.startupTimeoutSeconds)
        // Companion mode: Yano bootstraps the chain - applying the full PV11 (van Rossem)
        // cost models via a governance action, and restoring the PlutusV2 cost model - then
        // the Haskell node takes over. The default haskell-only mode's cost-model bootstrap
        // is broken (bloxbean/yaci-devkit#184, #185), leaving the devnet unable to run
        // PV11-compiled scripts.
        container.withEnv("nodeMode", "companion")
        // yaci-cardano-test 0.1.0 hardcodes these legacy Babbage-era env vars in init();
        // they hang the companion-mode bootstrap (the devnet never becomes ready), and per
        // upstream they are no longer required. init() runs at start() and would override
        // withEnv, so strip them from the final container command instead.
        container.withCreateContainerCmdModifier { cmd =>
            val env = Option(cmd.getEnv).getOrElse(Array.empty[String])
            val filtered = env.filterNot { e =>
                e.startsWith("conwayHardForkAtEpoch=") || e.startsWith("shiftStartTimeBehind=")
            }
            cmd.withEnv((filtered ++ config.extraEnv.map((k, v) => s"$k=$v"))*)
        }
        // Don't set container name when not reusing - allows fresh containers each run
        if config.reuseContainer then
            container.withCreateContainerCmdModifier(cmd => cmd.withName(config.containerName))
            container.withReuse(true)

        if config.enableLogs then
            container.withLogConsumer(frame => println(s"[Yaci] ${frame.getUtf8String}"))

        container
    }

    /** Wait until the yaci-store index catches up with the node tip.
      *
      * The store indexes the chain asynchronously: right after startup it can be over a thousand
      * blocks behind the node (the companion-mode Yano bootstrap mints blocks faster than the store
      * indexes them). Queries during catch-up return stale UTxOs, so submissions fail with "All
      * inputs are spent".
      */
    private def awaitStoreSync(container: YaciCardanoContainer, timeoutSeconds: Long): Unit = {
        val http = HttpClient.newHttpClient()
        def fetch(request: HttpRequest): Option[String] =
            try
                val response = http.send(request, HttpResponse.BodyHandlers.ofString())
                if response.statusCode() == 200 then Some(response.body()) else None
            catch case _: Exception => None

        val storeRequest = HttpRequest
            .newBuilder(
              URI.create(s"${container.getYaciStoreApiUrl.stripSuffix("/")}/blocks/latest")
            )
            .GET()
            .build()
        val ogmiosPort = container.getMappedPort(YaciCardanoContainer.OGMIOS_PORT)
        val ogmiosRequest = HttpRequest
            .newBuilder(URI.create(s"http://${container.getHost}:$ogmiosPort"))
            .header("Content-Type", "application/json")
            .POST(
              HttpRequest.BodyPublishers
                  .ofString("""{"jsonrpc":"2.0","method":"queryNetwork/blockHeight"}""")
            )
            .build()

        def storeHeight: Option[Long] =
            fetch(storeRequest).flatMap(b => Try(ujson.read(b)("height").num.toLong).toOption)
        def nodeHeight: Option[Long] =
            fetch(ogmiosRequest).flatMap(b => Try(ujson.read(b)("result").num.toLong).toOption)

        val deadline = System.currentTimeMillis() + timeoutSeconds * 1000
        var synced = false
        while !synced && System.currentTimeMillis() < deadline do
            (storeHeight, nodeHeight) match
                case (Some(store), Some(node)) if store + 1 >= node => synced = true
                case _                                              => Thread.sleep(500)
        if !synced then
            throw new IllegalStateException(
              s"Yaci store did not catch up with the node tip within $timeoutSeconds seconds"
            )
    }
