package scalus.testing.yaci

/** Configuration for Yaci DevKit container
  *
  * @param enableLogs
  *   Enable container logs for debugging
  * @param containerName
  *   Name for the Docker container (used with reuse)
  * @param reuseContainer
  *   Reuse the same container across test runs for faster iteration
  */
case class YaciConfig(
    enableLogs: Boolean = false,
    containerName: String = "scalus-yaci-devkit",
    reuseContainer: Boolean = false
)
