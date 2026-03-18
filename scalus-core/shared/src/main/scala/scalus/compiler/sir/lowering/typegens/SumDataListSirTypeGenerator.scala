package scalus.compiler.sir.lowering
package typegens

/** Internal representation - Plutus List, element type should be data-compatible.
  *
  * Thin alias for `SumBuiltinListSirTypeGenerator.DataList`.
  */
object SumDataListSirTypeGenerator
    extends SumBuiltinListSirTypeGenerator(PrimitiveRepresentation.PackedData)
