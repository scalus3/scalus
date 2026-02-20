package scalus.compiler.sir

enum TargetLoweringBackend:
    case ScottEncodingLowering
    case SumOfProductsLowering
    case SirToUplcV3Lowering

object TargetLoweringBackend:
    @deprecated("Use ScottEncodingLowering instead", "0.13.0")
    val SimpleSirToUplcLowering: TargetLoweringBackend = ScottEncodingLowering
    @deprecated("Use SumOfProductsLowering instead", "0.13.0")
    val SirToUplc110Lowering: TargetLoweringBackend = SumOfProductsLowering

/** Default compiler options for SIR processing. Here to have a single place for default options,
  * which is shared between the compiler plugin and the core library.
  */
object SIRDefaultOptions {

    val targetLoweringBackend: TargetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
    val generateErrorTraces: Boolean = true
    val removeTraces: Boolean = false
    val optimizeUplc: Boolean = false

    // debugging options
    val writeSirToFile: Boolean = false
    val debugLevel: Int = 0

}
