package scalus.cardano.ledger.internal

/** Minimal logging facade with one implementation per platform.
  *
  * It exists for bundle size. `scribe` costs about 400 KB of unoptimised Scala.js output (`ANSI`,
  * the browser writer, the format blocks, `Trace`), and [[scalus.cardano.ledger]] reaches it from
  * `PlutusScriptEvaluator`, which is on the `evalPlutusScripts` path exported to JavaScript. The
  * JVM keeps scribe; JS logs through `console` behind a level check. See
  * `docs/internal/JS_BUNDLE_SIZE.md`.
  *
  * Messages are by-name so that a disabled level costs nothing at runtime.
  */
private[scalus] trait Logger {
    def debug(message: => String): Unit
    def info(message: => String): Unit
    def warn(message: => String): Unit
    def error(message: => String): Unit

    /** Logs `message` together with the throwable that caused it. */
    def error(message: => String, cause: Throwable): Unit
}

private[scalus] object Logger {

    /** A logger labelled with `name`, which platform implementations may use as a category. */
    def apply(name: String): Logger = LoggerPlatform.create(name)
}
