package scalus.compiler.intrinsics

import scalus.Compile

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

    /** Zero-cost type cast. Keeps the expression's current representation unchanged.
      *
      * Use for arguments where the UPLC representation is already correct — e.g., viewing a
      * `List[A]` as `BuiltinList[Data]` when the list already has SumDataList representation.
      */
    def typeProxy[T](x: Any): T =
        throw new RuntimeException("typeProxy: handled by lowering")

    /** Type cast that marks the return value as having Data (packed) representation.
      *
      * Use for builtin return values that produce Data — e.g., `headList` returns a Data element
      * from a `BuiltinList[Data]`. The lowering sets representation to `PackedData`, causing
      * appropriate unpacking conversions (e.g., `unIData`, `unBData`) to be generated.
      */
    def typeProxyRetData[T](x: Any): T =
        throw new RuntimeException("typeProxyRetData: handled by lowering")
}
