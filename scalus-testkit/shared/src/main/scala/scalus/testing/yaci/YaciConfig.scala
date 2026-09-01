package scalus.testing.yaci

/** Configuration for Yaci DevKit container
  *
  * @param enableLogs
  *   Enable container logs for debugging
  * @param containerName
  *   Name for the Docker container (used with reuse)
  * @param reuseContainer
  *   Reuse the same container across test runs for faster iteration
  * @param imageTag
  *   `bloxbean/yaci-cli` image tag to run. The default runs a protocol version 11 (van Rossem)
  *   node. Note: when [[reuseContainer]] is enabled, changing the tag while a container created
  *   from the old tag still exists causes a Docker name conflict - remove the old container
  *   (`docker rm -f <containerName>`) first.
  * @param startupTimeoutSeconds
  *   How long to wait for the devnet API to come up. Companion node mode boots the chain via the
  *   Yano bootstrap before the Haskell node takes over, which takes a few minutes.
  * @param extraEnv
  *   Additional environment variables for the container, appended to the final container command.
  *   Appended there rather than through `withEnv` because `YaciCardanoContainer.init()` runs at
  *   `start()` and overwrites the environment set that way. Yaci Store is a Spring Boot
  *   application, so its settings arrive as relaxed-binding environment variables — for example
  *   `STORE_TRANSACTION_SAVE_CBOR=true` for `store.transaction.save-cbor`, which is what makes
  *   `/txs/{hash}/cbor` return anything.
  */
case class YaciConfig(
    enableLogs: Boolean = false,
    containerName: String = "scalus-yaci-devkit",
    reuseContainer: Boolean = false,
    imageTag: String = YaciConfig.DefaultImageTag,
    startupTimeoutSeconds: Long = 300,
    extraEnv: Map[String, String] = Map.empty
)

object YaciConfig {

    /** Default `bloxbean/yaci-cli` image tag.
      *
      * 0.12.0-beta5 is the newest multi-arch image and runs a protocol version 11 (van Rossem)
      * devnet node, matching current mainnet. [[YaciContainer]] starts it in companion node mode,
      * where the Yano bootstrap installs the full PV11 cost models, so PV11-compiled scripts run on
      * the devnet.
      */
    val DefaultImageTag: String = "0.12.0-beta5"
}
