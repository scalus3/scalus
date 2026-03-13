package scalus.compiler.intrinsics

import scalus.Compile
import scalus.compiler.sir.lowering.LoweredValueRepresentation

/** Intrinsic helper functions for type and representation casts.
  *
  * Marker methods that are never evaluated at runtime. The plugin compiles calls as `ExternalVar`
  * references; the lowering intercepts them by name and handles representation changes.
  *
  * @see
  *   [[scalus.uplc.builtin.internal.UniversalDataConversion]] for a similar pattern.
  */
@Compile
object IntrinsicHelpers {

    /** Zero-cost type cast with explicit representation.
      *
      * The `R` singleton type parameter specifies the target `LoweredValueRepresentation`. The
      * plugin intercepts calls and encodes the representation name in a SIR annotation; the
      * lowering reads it and sets the representation directly.
      *
      * Example: `typeProxyRepr[BuiltinList[Data], SumDataList.type](self)` — view `self` as
      * `BuiltinList[Data]` with `SumDataList` representation.
      */
    def typeProxyRepr[V, R <: LoweredValueRepresentation](x: Any): V =
        throw new RuntimeException("typeProxyRepr: handled by lowering")

    /** Type cast that marks the return value as having Data (packed) representation.
      *
      * Use for builtin return values that produce Data — e.g., `headList` returns a Data element
      * from a `BuiltinList[Data]`. The lowering sets representation to `PackedData`, causing
      * appropriate unpacking conversions (e.g., `unIData`, `unBData`) to be generated.
      */
    def typeProxyRetData[T](x: Any): T =
        throw new RuntimeException("typeProxyRetData: handled by lowering")
}
