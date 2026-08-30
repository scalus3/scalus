package scalus.cardano.ledger.internal

import scala.scalajs.js.Dynamic.global

/** Scala.js [[Logger]]: writes to the host `console`, with no scribe on the classpath of the
  * published `scalus.js` bundle.
  *
  * The level defaults to `Warn` so that debug messages, which serialize script contexts and datums,
  * are never built during evaluation.
  *
  * [[setLevel]] raises it, but note that this object is `private[scalus]`, so the knob serves the
  * library and its tests and is not reachable from JavaScript. Exposing it would mean another
  * `@JSExportTopLevel` entry point, and every export is a linker root, which is the cost this
  * facade exists to avoid. If JS consumers ever need to see these logs, that is the trade to make
  * deliberately rather than by accident.
  */
private[scalus] object LoggerPlatform {

    /** Log levels, ordered. Messages below [[level]] are dropped without evaluating them. */
    final val Debug = 0
    final val Info = 1
    final val Warn = 2
    final val Error = 3
    final val Off = 4

    private var level: Int = Warn

    /** Sets the global level; see [[Debug]], [[Info]], [[Warn]], [[Error]] and [[Off]]. */
    def setLevel(newLevel: Int): Unit = level = newLevel

    def create(name: String): Logger = new Logger {
        def debug(message: => String): Unit =
            if level <= Debug then global.console.debug(s"$name: $message")
        def info(message: => String): Unit =
            if level <= Info then global.console.info(s"$name: $message")
        def warn(message: => String): Unit =
            if level <= Warn then global.console.warn(s"$name: $message")
        def error(message: => String): Unit =
            if level <= Error then global.console.error(s"$name: $message")
        def error(message: => String, cause: Throwable): Unit =
            if level <= Error then global.console.error(s"$name: $message", cause.toString)
    }
}
