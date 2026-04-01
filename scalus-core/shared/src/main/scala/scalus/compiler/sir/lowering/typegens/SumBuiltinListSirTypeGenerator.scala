package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*
import scalus.uplc.Constant

/** Parameterized list type generator for BuiltinList[X] where X is not BuiltinPair. For pair lists,
  * use SumPairBuiltinListSirTypeGenerator instead.
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
        lctx: LoweringContext
    ): LoweredValueRepresentation =
        defaultDataRepresentation(tp)

    override def defaultListRepresentation(tp: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValueRepresentation =
        listRepr

    override def defaultElementRepresentation(tp: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation = elementRepr

    override def canBeConvertedToData(tp: SIRType)(using lctx: LoweringContext): Boolean = {
        val elemType = retrieveElementType(tp, SIRPosition.empty)
        lctx.typeGenerator(elemType).canBeConvertedToData(elemType)
    }

    override def genNil(resType: SIRType, pos: SIRPosition)(using LoweringContext): LoweredValue =
        val elemType = retrieveElementType(resType, pos)
        if elemType == SIRType.FreeUnificator || elemType == SIRType.TypeNothing then
            // Use SIRType.List.Nil so isNilType recognizes it for fixNilInConstr
            lvDataNil(pos, SIRType.List.Nil, listRepr)
        else
            ConstantLoweredValue(
              SIR.Const(
                Constant.List(elementRepr.defaultUni(elemType), Nil),
                resType,
                AnnotationsDecl(pos)
              ),
              listRepr
            )

}
