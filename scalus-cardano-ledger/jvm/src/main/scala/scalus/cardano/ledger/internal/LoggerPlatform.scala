package scalus.cardano.ledger.internal

import scribe.Logger as ScribeLogger

/** JVM [[Logger]]: delegates to scribe, so JVM logging configuration keeps working unchanged. */
private[scalus] object LoggerPlatform {
    def create(name: String): Logger = {
        val logger = ScribeLogger(name)
        new Logger {
            def debug(message: => String): Unit = logger.debug(message)
            def info(message: => String): Unit = logger.info(message)
            def warn(message: => String): Unit = logger.warn(message)
            def error(message: => String): Unit = logger.error(message)
            def error(message: => String, cause: Throwable): Unit =
                logger.error(message, cause)
        }
    }
}
