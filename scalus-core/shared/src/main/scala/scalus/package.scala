import org.typelevel.paiges.Doc
import scalus.cardano.ledger.Language
import scalus.compiler.sir.*
import scalus.compiler.sir.PrettyPrinter.Style
import scalus.compiler.sir.lowering.simple.{ScottEncodingLowering, SumOfProductsLowering}
import scalus.compiler.sir.lowering.{LoweredValue, SirToUplcV3Lowering}
import scalus.uplc.eval.*
import scalus.uplc.{Program, *}
import scalus.uplc.transform.V3Optimizer
import scalus.utils.Utils

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

    extension (p: Program)
        def pretty: Doc = PrettyPrinter.pretty(p, Style.Normal)
        def prettyXTerm: Doc = PrettyPrinter.pretty(p, Style.XTerm)
        def show: String = p.pretty.render(80)
        def showHighlighted: String = p.prettyXTerm.render(80)
        def writePlutusFile(path: String, plutusVersion: Language): Unit =
            Utils.writePlutusFile(path, p.deBruijnedProgram, plutusVersion)

        /** Evaluates the program using the given VM according to the Plutus specification.
          *
          * @throws RuntimeException
          *   on evaluation error
          */
        def evaluate(using vm: PlutusVM): Term =
            vm.evaluateScript(p.deBruijnedProgram, NoBudgetSpender, NoLogger)

        /** Evaluates the program using the given VM according to the Plutus specification.
          * @return
          *   [[scalus.uplc.eval.Result]] with the evaluation result and the spent budget
          */
        def evaluateDebug(using vm: PlutusVM): Result = vm.evaluateScriptDebug(p.deBruijnedProgram)

    extension (p: DeBruijnedProgram)
        // we have member with the same name
        // def pretty: Doc = PrettyPrinter.pretty(p.toProgram, Style.Normal)
        def prettyXTerm: Doc = PrettyPrinter.pretty(p.toProgram, Style.XTerm)
        def show: String = p.pretty.render(80)
        def showHighlighted: String = p.prettyXTerm.render(80)
        def writePlutusFile(path: String, plutusVersion: Language): Unit =
            Utils.writePlutusFile(path, p, plutusVersion)

        /** Evaluates the program using the given VM according to the Plutus specification.
          *
          * @throws RuntimeException
          *   on evaluation error
          */
        def evaluate(using vm: PlutusVM): Term = vm.evaluateScript(p, NoBudgetSpender, NoLogger)

        /** Evaluates the program using the given VM according to the Plutus specification.
          * @return
          *   [[scalus.uplc.eval.Result]] with the evaluation result and the spent budget
          */
        def evaluateDebug(using vm: PlutusVM): Result = vm.evaluateScriptDebug(p)

    extension (du: DefaultUni) def pretty: Doc = PrettyPrinter.pretty(du)
    extension (c: Constant) def pretty: Doc = PrettyPrinter.pretty(c)

    extension (self: Term)
        def pretty: Doc = PrettyPrinter.pretty(self, Style.Normal)
        def prettyXTerm: Doc = PrettyPrinter.pretty(self, Style.XTerm)
        def show: String = self.pretty.render(80)
        def showHighlighted: String = self.prettyXTerm.render(80)
        def showShort: String = truncateForDisplay(self.pretty.render(100), 60)

        /** Evaluate the term using the given VM.
          * @note
          *   This method just runs the CEK machine on the term. It does not follow Plutus
          *   specification like CIP-117
          *
          * @throws RuntimeException
          *   on evaluation error
          */
        def evaluate(using vm: PlutusVM): Term =
            vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(self))

        /** Evaluate the term using the given VM.
          * @note
          *   This method just runs the CEK machine on the term. It does not follow Plutus *
          *   specification like CIP-117
          *
          * @return
          *   [[scalus.uplc.eval.Result]] with the evaluation result and the spent budget
          */
        def evaluateDebug(using vm: PlutusVM): Result =
            val spenderLogger = TallyingBudgetSpenderLogger(CountingBudgetSpender())
            try
                val result = vm.evaluateDeBruijnedTerm(
                  DeBruijn.deBruijnTerm(self),
                  spenderLogger,
                  spenderLogger
                )
                Result.Success(
                  result,
                  spenderLogger.getSpentBudget,
                  spenderLogger.costs.toMap,
                  spenderLogger.getLogsWithBudget
                )
            catch
                case e: Exception =>
                    Result.Failure(
                      e,
                      spenderLogger.getSpentBudget,
                      spenderLogger.costs.toMap,
                      spenderLogger.getLogsWithBudget
                    )
        def plutusV1: Program = Program.plutusV1(self)
        def plutusV2: Program = Program.plutusV2(self)
        def plutusV3: Program = Program.plutusV3(self)
}
