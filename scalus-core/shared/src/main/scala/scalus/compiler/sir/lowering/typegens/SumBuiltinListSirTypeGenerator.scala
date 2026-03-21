package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*

/** Parameterized list type generator for BuiltinList[X] where X is not BuiltinPair.
  * For pair lists, use SumPairBuiltinListSirTypeGenerator instead.
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
        SumCaseClassRepresentation.PackedSumDataList

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        defaultDataRepresentation(tp)

    override def defaultListRepresentation(tp: SIRType, pos: SIRPosition)(using LoweringContext): LoweredValueRepresentation =
        listRepr

    override def defaultElementRepresentation(tp: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation =
        elementRepr match
            case PrimitiveRepresentation.Constant => PrimitiveRepresentation.Constant
            case _ => lctx.typeGenerator(tp).defaultDataRepresentation(tp)

    override def canBeConvertedToData(tp: SIRType)(using lctx: LoweringContext): Boolean = {
        val elemType = retrieveElementType(tp, SIRPosition.empty)
        lctx.typeGenerator(elemType).canBeConvertedToData(elemType)
    }

    override def genNil(resType: SIRType, pos: SIRPosition)(using LoweringContext): LoweredValue =
        elementRepr match
            case PrimitiveRepresentation.Constant =>
                val elemType = retrieveElementType(resType, pos)
                if elemType == SIRType.FreeUnificator then lvDataNil(pos, resType, listRepr)
                else lvTypedNil(pos, elemType, resType, listRepr)
            case _ => lvDataNil(pos, SIRType.List.Nil, listRepr)

}
