import org.typelevel.paiges.Doc
import scalus.compiler.Options
import scalus.compiler.sir.*
import scalus.utils.Style
import scalus.compiler.sir.lowering.{LoweredValue, SirToUplcV3Lowering, UplcPipeline}
import scalus.uplc.{Constant, DefaultUni, Term}

package object scalus {

    /** Pipe operator */
    extension [A](inline a: A) inline infix def |>[B](inline f: A => B): B = f(a)

    /** Truncate a string to a maximum length per line, showing only first line if multiline
      * @param s
      *   The string to truncate
      * @param maxLength
      *   Maximum length per line (default 60)
      * @return
      *   Truncated string with "..." if truncated
      */
    private def truncateForDisplay(s: String, maxLength: Int = 60): String = {
        val firstLine = s.linesIterator.nextOption().getOrElse("")
        if firstLine.length <= maxLength then firstLine
        else firstLine.take(maxLength) + "..."
    }

    /** Extension methods for working with [[scalus.compiler.sir.SIR]] (Scalus Intermediate
      * Representation) values, as produced by `scalus.compiler.compile { ... }`.
      *
      * These are inspection and testing helpers. For compiling production smart contracts prefer
      * [[scalus.uplc.PlutusV3.compile]] (or `PlutusV1`/`PlutusV2`): it returns a
      * [[scalus.uplc.CompiledPlutus]] with the versioned `program`, serialized `script`, script
      * `address`, and the Scalus tag applied per [[scalus.compiler.Options]].
      */
    extension (sir: SIR)
        /** Pretty-prints the SIR as a plain-text [[Doc]]. */
        def pretty: Doc = PrettyPrinter.pretty(sir, Style.Normal)

        /** Pretty-prints the SIR as a [[Doc]] with XTerm color escape codes. */
        def prettyXTerm: Doc = PrettyPrinter.pretty(sir, Style.XTerm)

        /** Renders [[pretty]] at 80 columns. */
        def show: String = pretty.render(80)

        /** Renders [[prettyXTerm]] at 80 columns (colored, for terminal output). */
        def showHighlighted: String = sir.prettyXTerm.render(80)

        /** First line of the rendered SIR, truncated to 60 characters - for log messages. */
        def showShort: String = truncateForDisplay(pretty.render(100), 60)

        /** Lowers the SIR to a UPLC [[Term]] through the single
          * [[scalus.compiler.sir.lowering.UplcPipeline]] (the same pipeline
          * [[scalus.uplc.CompiledPlutus]] uses): removeTraces? -> mutual-recursion elimination ->
          * static-argument transformation? -> backend lowering -> optimization? -> source-position
          * back-fill.
          *
          * Intended for tests and inspection of the lowered term. For production scripts use
          * [[scalus.uplc.PlutusV3.compile]] instead - it also produces the versioned program,
          * serialized script, and script address.
          *
          * Each parameter defaults to the corresponding field of the given [[Options]] and
          * overrides it when passed explicitly.
          *
          * @param generateErrorTraces
          *   emit `trace` calls with error details on failure paths (bigger, costlier script)
          * @param backend
          *   the lowering backend; the default V3 backend is right for almost all uses
          * @param optimizeUplc
          *   run the SIR- and UPLC-level optimizers (what `Options.release` enables)
          * @param debug
          *   verbose lowering diagnostics
          */
        def toUplc(using
            options: Options = Options()
        )(
            generateErrorTraces: Boolean = options.generateErrorTraces,
            backend: TargetLoweringBackend = options.targetLoweringBackend,
            optimizeUplc: Boolean = options.optimizeUplc,
            debug: Boolean = options.debug
        ): Term = {
            val eff = options.copy(
              generateErrorTraces = generateErrorTraces,
              targetLoweringBackend = backend,
              optimizeUplc = optimizeUplc,
              debug = debug
            )
            UplcPipeline.run(
              sir,
              eff,
              eff.targetLanguage,
              UplcPipeline.defaultOptimizer(eff.targetLanguage, eff)
            )
        }

        /** [[toUplc]] with `optimizeUplc = true`: lowers and runs the full optimizer pipeline.
          *
          * Intended for tests comparing optimized output. For production scripts use
          * [[scalus.uplc.PlutusV3.compile]] with `Options.release` instead.
          */
        def toUplcOptimized(using
            options: Options = Options.default
        )(
            generateErrorTraces: Boolean = options.generateErrorTraces,
            backend: TargetLoweringBackend = options.targetLoweringBackend,
            debug: Boolean = options.debug
        ): Term = {
            toUplc(
              generateErrorTraces = generateErrorTraces,
              backend = backend,
              optimizeUplc = true,
              debug = debug
            )
        }

        /** Exposes the V3 backend's intermediate lowering result.
          *
          * [[LoweredValue]] is compiler-internal IR with no stability guarantees, and this method
          * bypasses the unified pipeline (no trace removal, no SIR-level optimization).
          */
        @deprecated(
          "internal lowering IR; use SirToUplcV3Lowering.fromOptions(sir, options).toLoweredValue() " +
              "for lowering debugging, or PlutusV3.compile for compilation",
          "1.0.0"
        )
        def toLoweredValue(using
            options: Options = Options()
        )(
            generateErrorTraces: Boolean = options.generateErrorTraces,
            debug: Boolean = options.debug
        ): LoweredValue = {
            val eff = options.copy(generateErrorTraces = generateErrorTraces, debug = debug)
            SirToUplcV3Lowering.fromOptions(sir, eff, eff.debug).toLoweredValue()
        }

        /** Lowers with the given [[Options]] only (no parameter overrides). */
        @deprecated("use toUplc instead", "1.0.0")
        def lowerToUplc(using options: Options = Options()): Term = toUplc(using options)()

    /** Pretty-prints a UPLC builtin type tag. */
    extension (du: DefaultUni) def pretty: Doc = PrettyPrinter.pretty(du)

    /** Pretty-prints a UPLC constant. */
    extension (c: Constant) def pretty: Doc = PrettyPrinter.pretty(c)
}
