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
  */
case class YaciConfig(
    enableLogs: Boolean = false,
    containerName: String = "scalus-yaci-devkit",
    reuseContainer: Boolean = false,
    imageTag: String = YaciConfig.DefaultImageTag
)

object YaciConfig {

    /** Default `bloxbean/yaci-cli` image tag.
      *
      * 0.12.0-beta5 is the newest multi-arch image and runs a protocol version 11 (van Rossem)
      * devnet node, matching current mainnet. Caveat: its genesis still ships the pre-PV11 PlutusV3
      * cost model (the image's own cost-model governance bootstrap is broken), so PV11-only script
      * constructs fail on-chain with maxBound costs - compile contracts run against the devnet with
      * a PV10 target (`Options.plomin`) until a fixed image is released.
      */
    val DefaultImageTag: String = "0.12.0-beta5"
}
