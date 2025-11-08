package scalus.sir

enum TargetLoweringBackend:
    case ScottEncodingLowering
    case SumOfProductsLowering
    @deprecated("Use ScottEncodingLowering instead", "0.13.0") case SimpleSirToUplcLowering
    @deprecated("Use SumOfProductsLowering instead", "0.13.0") case SirToUplc110Lowering
    case SirToUplcV3Lowering

/** Default compiler options for SIR processing. Here to have a single place for default options,
  * which is shared between the compiler plugin and the core library.
  */
object SIRDefaultOptions {

    val targetLoweringBackend: TargetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
    val generateErrorTraces: Boolean = true
    val optimizeUplc: Boolean = false

    // debugging options
    val writeSirToFile: Boolean = false
    val debugLevel: Int = 0

}
