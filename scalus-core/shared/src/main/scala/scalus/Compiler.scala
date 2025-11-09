package scalus

import scalus.compiler.sir.{SIR, SIRDefaultOptions, SIRType}
import scalus.builtin.Data
import scalus.utils.Macros

import scala.annotation.Annotation

final class Compile extends Annotation
final class ScalusDebug(val debugLevel: Int) extends Annotation
final class Ignore extends Annotation

/** This is a marker trait for the compiler plugin to compile derivations of the instances of the
  * type classes.
  * @see
  *   scalus.prelude.ToData, scalus.prelude.FromData
  */
trait CompileDerivations

object Compiler:

    type TargetLoweringBackend =
        scalus.compiler.sir.TargetLoweringBackend
    val TargetLoweringBackend = scalus.compiler.sir.TargetLoweringBackend

    case class Options(
        targetLoweringBackend: TargetLoweringBackend = SIRDefaultOptions.targetLoweringBackend,
        generateErrorTraces: Boolean = SIRDefaultOptions.generateErrorTraces,
        optimizeUplc: Boolean = SIRDefaultOptions.optimizeUplc,
        debugLevel: Int = SIRDefaultOptions.debugLevel,
        debug: Boolean = false
    )

    object Options {
        val default: Options = Options()

        val debug: Options = Options(
          targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true,
          optimizeUplc = false,
          debug = false
        )

        val release: Options = Options(
          targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = false,
          optimizeUplc = true,
          debug = false
        )
    }

    def defaultOptions: Options = Options.default

    inline def fieldAsData[A](inline expr: A => Any): Data => Data = ${
        Macros.fieldAsDataMacro('expr)
    }

    /** Compiles the given expression to a [[SIR]] at compile time using the Scalus compiler plugin.
      *
      * @param e
      *   The expression to compile.
      * @return
      *   The compiled [[SIR]].
      *
      * @example
      *   {{{
      *   val sir = Compiler.compile(true) // Compiles the expression `true` to a SIR
      *   val uplc = sir.toUplc() // Converts the SIR to UPLC
      *   }}}
      */
    def compile(e: Any): SIR = throwCompilerPluginMissingException()

    /** Compiles the given expression to a [[SIR]] at compile time using the Scalus compiler plugin,
      * with explicit compiler options.
      *
      * @param options
      *   The compiler options to use.
      * @param e
      *   The expression to compile.
      * @return
      *   The compiled [[SIR]].
      */
    def compileWithOptions(options: Options, e: Any): SIR = throwCompilerPluginMissingException()

    /** Compiles the given expression to a [[SIR]] at compile time using the Scalus compiler plugin,
      * producing debug output during the compilation.
      *
      * @param e
      *   The expression to compile.
      * @return
      *   The compiled [[SIR]].
      */
    def compileDebug(e: Any): SIR = throwCompilerPluginMissingException()

    /** Compiles the given expression to a [[SIR]] at compile time using the Scalus compiler plugin,
      * with explicit compiler options and debug output.
      *
      * @param options
      *   The compiler options to use.
      * @param e
      *   The expression to compile.
      * @return
      *   The compiled [[SIR]].
      */
    def compileDebugWithOptions(options: Options, e: Any): SIR =
        throwCompilerPluginMissingException()

    /** Compiles the given expression to a [[SIRType]] at compile time using the Scalus compiler
      * plugin.
      *
      * @tparam T
      *   The type to compile.
      * @return
      *   The compiled [[SIRType]].
      *
      * @example
      *   {{{
      *   // Compiles the type `BigInt` to a SIRType.Integer
      *   val sirType = Compiler.compileType[BigInt]
      *   }}}
      */
    def compileType[T]: SIRType = throwCompilerPluginMissingException()

    /** Generates a `Compiler.compile(code)` call at compile time.
      *
      * When you want to use [[Compiler.compile]] in an inline method you can't do it directly,
      * because the Scalus compiler plugin finds the call to `Compiler.compile` and replaces it with
      * the compiled code. Which is not what you want in this case!
      *
      * For example this will not work:
      * {{{
      *   inline def myTest(inline code: Any): SIR = {
      *       Compiler.compile(code).toUplc().evaluate
      *   }
      * }}}
      *
      * Instead, you should use this method:
      * {{{
      *  inline def myTest(inline code: Any): SIR = {
      *     Compiler.compileInline(code).toUplc().evaluate
      *  }
      * }}}
      */
    inline def compileInline(inline code: Any): SIR = ${
        Macros.generateCompileCall('code)
    }

    inline def compileInlineWithOptions(inline options: Options, inline code: Any): SIR = ${
        Macros.generateCompileCall('options, 'code)
    }

    private def throwCompilerPluginMissingException(): Nothing =
        throw new RuntimeException(
          "This method call is handled by the Scalus compiler plugin. " +
              "If you see this message at runtime, the compiler plugin is not enabled." +
              "Try adding the compiler plugin to your build.sbt: " +
              "compilerPlugin(\"scalus\" %% \"scalus-plugin\" % scalusPluginVersion)"
        )
