package scalus.compiler.sir.lowering
package typegens

/** List(BuiltinPair(Data, Data)) representation.
  *
  * Thin alias for `SumBuiltinListSirTypeGenerator` with PairData elements.
  */
object SumPairDataListSirTypeGenerator
    extends SumBuiltinListSirTypeGenerator(ProductCaseClassRepresentation.PairData)
