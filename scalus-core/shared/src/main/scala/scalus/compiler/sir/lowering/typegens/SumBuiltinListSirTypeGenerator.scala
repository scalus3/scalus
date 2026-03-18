package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*

/** Parameterized list type generator. Unifies the former `SumDataListSirTypeGenerator` and
  * `SumPairDataListSirTypeGenerator` by dispatching on `elementRepr`.
  */
class SumBuiltinListSirTypeGenerator(val elementRepr: LoweredValueRepresentation)
    extends SumListCommonSirTypeGenerator {

    // Cached to avoid repeated case class allocation in hot path
    private val listRepr: LoweredValueRepresentation =
        SumCaseClassRepresentation.SumBuiltinList(elementRepr)

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation = listRepr

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        elementRepr match
            case ProductCaseClassRepresentation.PairData =>
                SumCaseClassRepresentation.SumDataAssocMap
            case _ => SumCaseClassRepresentation.PackedSumDataList

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        defaultDataRepresentation(tp)

    override def defaultListRepresentation(using LoweringContext): LoweredValueRepresentation =
        listRepr

    override def defaultElementRepresentation(tp: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation =
        elementRepr match
            case ProductCaseClassRepresentation.PairData =>
                val constrDecl = SIRType
                    .retrieveConstrDecl(tp)
                    .getOrElse(
                      throw LoweringException(
                        s"SumPair should have a pair or tuple type representation, we have ${tp.show}",
                        pos
                      )
                    )
                if constrDecl.name == "scalus.uplc.builtin.BuiltinPair" || constrDecl.name == "scala.Tuple2"
                then ProductCaseClassRepresentation.PairData
                else
                    throw LoweringException(
                      s"SumPair should have a pair or tuple type representation, we have ${tp.show}",
                      pos
                    )
            case _ => lctx.typeGenerator(tp).defaultDataRepresentation(tp)

    override def isDataSupported(tp: SIRType)(using lctx: LoweringContext): Boolean = true

    override def genNil(resType: SIRType, pos: SIRPosition)(using LoweringContext): LoweredValue =
        elementRepr match
            case ProductCaseClassRepresentation.PairData => lvPairDataNil(pos, resType)
            case _                                       => lvDataNil(pos, SIRType.List.Nil)

}
