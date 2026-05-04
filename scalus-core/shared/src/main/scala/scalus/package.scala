import org.typelevel.paiges.Doc
import scalus.compiler.Options
import scalus.compiler.sir.*
import scalus.utils.Style
import scalus.compiler.sir.lowering.simple.{ScottEncodingLowering, SumOfProductsLowering}
import scalus.compiler.sir.lowering.{LoweredValue, SirToUplcV3Lowering}
import scalus.uplc.{Constant, DefaultUni, Term}
import scalus.uplc.transform.V3Optimizer

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

    extension (sir: SIR)
        def pretty: Doc = PrettyPrinter.pretty(sir, Style.Normal)
        def prettyXTerm: Doc = PrettyPrinter.pretty(sir, Style.XTerm)
        def show: String = pretty.render(80)
        def showHighlighted: String = sir.prettyXTerm.render(80)
        def showShort: String = truncateForDisplay(pretty.render(100), 60)

        def toUplc(using
            options: Options = Options()
        )(
            generateErrorTraces: Boolean = options.generateErrorTraces,
            backend: TargetLoweringBackend = options.targetLoweringBackend,
            optimizeUplc: Boolean = options.optimizeUplc,
            debug: Boolean = options.debug
        ): Term = {
            val backend = options.targetLoweringBackend
            val uplc = backend match
                case TargetLoweringBackend.ScottEncodingLowering =>
                    ScottEncodingLowering(
                      sir,
                      generateErrorTraces,
                      targetProtocolVersion = options.targetProtocolVersion
                    ).lower()
                case TargetLoweringBackend.SumOfProductsLowering =>
                    SumOfProductsLowering(
                      sir,
                      generateErrorTraces,
                      targetProtocolVersion = options.targetProtocolVersion
                    ).lower()
                case TargetLoweringBackend.SirToUplcV3Lowering =>
                    SirToUplcV3Lowering.fromOptions(sir, options, debug).lower()
            val retval =
                if optimizeUplc then
                    val optimizer = V3Optimizer()
                    optimizer(uplc)
                else uplc
            retval
        }

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

        def toLoweredValue(using
            options: Options = Options()
        )(
            generateErrorTraces: Boolean = options.generateErrorTraces,
            debug: Boolean = options.debug
        ): LoweredValue = {
            val retval = SirToUplcV3Lowering.fromOptions(sir, options).toLoweredValue()
            retval
        }

        def lowerToUplc(using options: Options = Options()): Term = {
            val backend = options.targetLoweringBackend
            val uplc = backend match
                case TargetLoweringBackend.ScottEncodingLowering =>
                    ScottEncodingLowering(
                      sir,
                      options.generateErrorTraces,
                      targetProtocolVersion = options.targetProtocolVersion
                    ).lower()
                case TargetLoweringBackend.SumOfProductsLowering =>
                    SumOfProductsLowering(
                      sir,
                      options.generateErrorTraces,
                      targetProtocolVersion = options.targetProtocolVersion
                    ).lower()
                case TargetLoweringBackend.SirToUplcV3Lowering =>
                    SirToUplcV3Lowering.fromOptions(sir, options).lower()
            val retval =
                if options.optimizeUplc then
                    val optimizer = V3Optimizer()
                    optimizer(uplc)
                else uplc
            retval
        }

    extension (du: DefaultUni) def pretty: Doc = PrettyPrinter.pretty(du)
    extension (c: Constant) def pretty: Doc = PrettyPrinter.pretty(c)
}
