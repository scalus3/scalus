package scalus

import scalus.uplc.builtin.Data
import scalus.compiler.sir.{SIR, SIRType}

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

@deprecated("Use scalus.compiler package instead", "0.14.2")
object Compiler:

    @deprecated("Use scalus.compiler.TargetLoweringBackend instead", "0.14.2")
    type TargetLoweringBackend = scalus.compiler.sir.TargetLoweringBackend

    @deprecated("Use scalus.compiler.TargetLoweringBackend instead", "0.14.2")
    val TargetLoweringBackend = scalus.compiler.sir.TargetLoweringBackend

    @deprecated("Use scalus.compiler.Options instead", "0.14.2")
    type Options = scalus.compiler.Options

    @deprecated("Use scalus.compiler.Options instead", "0.14.2")
    val Options = scalus.compiler.Options

    @deprecated("Use scalus.compiler.defaultOptions instead", "0.14.2")
    def defaultOptions: scalus.compiler.Options = scalus.compiler.defaultOptions

    @deprecated("Use scalus.compiler.fieldAsData instead", "0.14.2")
    inline def fieldAsData[A](inline expr: A => Any): Data => Data =
        scalus.compiler.fieldAsData(expr)

    // Note: These methods are intercepted by the compiler plugin at call sites.
    // They should NOT forward to scalus.compiler.* because the plugin would then
    // try to compile the parameter 'e' which is just a variable, not actual code.
    // The plugin replaces calls to these methods with compiled SIR at compile time.

    @deprecated("Use scalus.compiler.compile instead", "0.14.2")
    def compile(e: Any): SIR = throwCompilerPluginMissingException()

    @deprecated("Use scalus.compiler.compileWithOptions instead", "0.14.2")
    def compileWithOptions(options: scalus.compiler.Options, e: Any): SIR =
        throwCompilerPluginMissingException()

    @deprecated("Use scalus.compiler.compileDebug instead", "0.14.2")
    def compileDebug(e: Any): SIR = throwCompilerPluginMissingException()

    @deprecated("Use scalus.compiler.compileDebugWithOptions instead", "0.14.2")
    def compileDebugWithOptions(options: scalus.compiler.Options, e: Any): SIR =
        throwCompilerPluginMissingException()

    @deprecated("Use scalus.compiler.compileType instead", "0.14.2")
    def compileType[T]: SIRType = throwCompilerPluginMissingException()

    private def throwCompilerPluginMissingException(): Nothing =
        throw new RuntimeException(
          "This method call is handled by the Scalus compiler plugin. " +
              "If you see this message at runtime, the compiler plugin is not enabled. " +
              "Try adding the compiler plugin to your build.sbt: " +
              "compilerPlugin(\"scalus\" %% \"scalus-plugin\" % scalusPluginVersion)"
        )

    @deprecated("Use scalus.compiler.compileInline instead", "0.14.2")
    inline def compileInline(inline code: Any): SIR = scalus.compiler.compileInline(code)

    @deprecated("Use scalus.compiler.compileInlineWithOptions instead", "0.14.2")
    inline def compileInlineWithOptions(
        inline options: scalus.compiler.Options,
        inline code: Any
    ): SIR =
        scalus.compiler.compileInlineWithOptions(options, code)
