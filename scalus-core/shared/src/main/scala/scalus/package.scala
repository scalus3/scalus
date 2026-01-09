import org.typelevel.paiges.Doc
import scalus.cardano.ledger.Language
import scalus.compiler.sir.*
import scalus.compiler.sir.PrettyPrinter.Style
import scalus.compiler.sir.lowering.simple.{ScottEncodingLowering, SumOfProductsLowering}
import scalus.compiler.sir.lowering.{LoweredValue, SirToUplcV3Lowering}
import scalus.uplc.{Constant, DeBruijnedProgram, DefaultUni, Program, Term}
import scalus.uplc.eval.*
import scalus.uplc.transform.V3Optimizer

import scala.annotation.nowarn

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
            options: Compiler.Options = Compiler.Options()
        )(
            generateErrorTraces: Boolean = options.generateErrorTraces,
            backend: Compiler.TargetLoweringBackend = options.targetLoweringBackend,
            optimizeUplc: Boolean = options.optimizeUplc,
            debug: Boolean = options.debug
        ): Term = {
            val backend = options.targetLoweringBackend
            val uplc = backend match
                case TargetLoweringBackend.ScottEncodingLowering =>
                    ScottEncodingLowering(sir, generateErrorTraces).lower()
                case TargetLoweringBackend.SumOfProductsLowering =>
                    SumOfProductsLowering(sir, generateErrorTraces).lower()
                case TargetLoweringBackend.SirToUplcV3Lowering =>
                    SirToUplcV3Lowering(
                      sir,
                      generateErrorTraces = generateErrorTraces,
                      debug = debug,
                      targetLanguage = options.targetLanguage
                    ).lower()
            val retval =
                if optimizeUplc then
                    val optimizer = V3Optimizer()
                    optimizer(uplc)
                else uplc
            retval
        }

        def toUplcOptimized(using
            options: Compiler.Options = Compiler.Options.default
        )(
            generateErrorTraces: Boolean = options.generateErrorTraces,
            backend: Compiler.TargetLoweringBackend = options.targetLoweringBackend,
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
            options: Compiler.Options = Compiler.Options()
        )(
            generateErrorTraces: Boolean = options.generateErrorTraces,
            debug: Boolean = options.debug
        ): LoweredValue = {
            val retval = SirToUplcV3Lowering(
              sir,
              generateErrorTraces = options.generateErrorTraces,
              debug = options.debug,
              targetLanguage = options.targetLanguage
            ).toLoweredValue()
            retval
        }

        def lowerToUplc(using options: Compiler.Options = Compiler.Options()): Term = {
            val backend = options.targetLoweringBackend
            val uplc = backend match
                case TargetLoweringBackend.ScottEncodingLowering =>
                    ScottEncodingLowering(sir, options.generateErrorTraces).lower()
                case TargetLoweringBackend.SumOfProductsLowering =>
                    SumOfProductsLowering(sir, options.generateErrorTraces).lower()
                case TargetLoweringBackend.SirToUplcV3Lowering =>
                    SirToUplcV3Lowering(
                      sir,
                      generateErrorTraces = options.generateErrorTraces,
                      debug = options.debug,
                      targetLanguage = options.targetLanguage
                    ).lower()
            val retval =
                if options.optimizeUplc then
                    val optimizer = V3Optimizer()
                    optimizer(uplc)
                else uplc
            retval
        }

    // Extension methods for Program, DeBruijnedProgram, and Term are deprecated.
    // Use the methods directly on the classes instead. See Program.scala and Term.scala.

    extension (p: Program)
        @deprecated("Use Program.pretty method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def pretty: Doc = p.pretty

        @deprecated("Use Program.prettyXTerm method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def prettyXTerm: Doc = p.prettyXTerm

        @deprecated("Use Program.show method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def show: String = p.show

        @deprecated("Use Program.showHighlighted method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def showHighlighted: String = p.showHighlighted

        @deprecated("Use Program.writePlutusFile method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def writePlutusFile(path: String, plutusVersion: Language): Unit =
            p.writePlutusFile(path, plutusVersion)

        @deprecated("Use Program.evaluate method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def evaluate(using vm: PlutusVM): Term = p.evaluate

        @deprecated("Use Program.evaluateDebug method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def evaluateDebug(using vm: PlutusVM): Result = p.evaluateDebug

    extension (p: DeBruijnedProgram)
        @deprecated("Use DeBruijnedProgram.prettyXTerm method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def prettyXTerm: Doc = p.prettyXTerm

        @deprecated("Use DeBruijnedProgram.show method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def show: String = p.show

        @deprecated("Use DeBruijnedProgram.showHighlighted method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def showHighlighted: String = p.showHighlighted

        @deprecated("Use DeBruijnedProgram.writePlutusFile method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def writePlutusFile(path: String, plutusVersion: Language): Unit =
            p.writePlutusFile(path, plutusVersion)

        @deprecated("Use DeBruijnedProgram.evaluate method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def evaluate(using vm: PlutusVM): Term = p.evaluate

        @deprecated("Use DeBruijnedProgram.evaluateDebug method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def evaluateDebug(using vm: PlutusVM): Result = p.evaluateDebug

    extension (du: DefaultUni) def pretty: Doc = PrettyPrinter.pretty(du)
    extension (c: Constant) def pretty: Doc = PrettyPrinter.pretty(c)

    extension (self: Term)
        @deprecated("Use Term.pretty method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def pretty: Doc = self.pretty

        @deprecated("Use Term.prettyXTerm method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def prettyXTerm: Doc = self.prettyXTerm

        @deprecated("Use Term.show method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def show: String = self.show

        @deprecated("Use Term.showHighlighted method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def showHighlighted: String = self.showHighlighted

        @deprecated("Use Term.showShort method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def showShort: String = self.showShort

        @deprecated("Use Term.evaluate method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def evaluate(using vm: PlutusVM): Term = self.evaluate

        @deprecated("Use Term.evaluateDebug method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def evaluateDebug(using vm: PlutusVM): Result = self.evaluateDebug

        @deprecated("Use Term.plutusV1 method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def plutusV1: Program = self.plutusV1

        @deprecated("Use Term.plutusV2 method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def plutusV2: Program = self.plutusV2

        @deprecated("Use Term.plutusV3 method instead", "0.14.2")
        @nowarn("msg=Extension method .* will never be selected")
        def plutusV3: Program = self.plutusV3
}
