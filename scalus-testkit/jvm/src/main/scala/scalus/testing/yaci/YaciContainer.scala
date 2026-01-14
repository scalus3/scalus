package scalus.testing.yaci

import com.bloxbean.cardano.yaci.test.YaciCardanoContainer

/** Singleton container holder for sharing across test suites with reference counting
  *
  * This object manages the lifecycle of a Yaci DevKit container instance, allowing multiple test
  * suites to share the same container. Reference counting ensures the container stays alive as long
  * as any test suite needs it.
  */
object YaciContainer:
    private var _container: YaciCardanoContainer = _
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
        val container = new YaciCardanoContainer()
        // Don't set container name when not reusing - allows fresh containers each run
        if config.reuseContainer then
            container.withCreateContainerCmdModifier(cmd => cmd.withName(config.containerName))
            container.withReuse(true)

        if config.enableLogs then
            container.withLogConsumer(frame => println(s"[Yaci] ${frame.getUtf8String}"))

        container
    }
