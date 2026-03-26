package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*

/** Generator for BuiltinList[BuiltinPair[Data,Data]] — pair lists. Uses SumPairBuiltinList
  * representation, serialized via mapData/unMapData.
  */
object SumPairBuiltinListSirTypeGenerator extends SumListCommonSirTypeGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation = {
        val elemType = retrieveElementType(tp, SIRPosition.empty)
        SumCaseClassRepresentation.SumPairBuiltinList.fromElementType(elemType)
    }

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.SumDataAssocMap

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.SumDataAssocMap

    override def defaultListRepresentation(tp: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation = {
        val elemType = retrieveElementType(tp, pos)
        SumCaseClassRepresentation.SumPairBuiltinList.fromElementType(elemType)
    }

    override def defaultElementRepresentation(tp: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation = {
        val constrDecl = SIRType
            .retrieveConstrDecl(tp)
            .getOrElse(
              throw LoweringException(
                s"SumPair should have a pair or tuple type representation, we have ${tp.show}",
                pos
              )
            )
        if constrDecl.name == "scalus.uplc.builtin.BuiltinPair" || constrDecl.name == "scala.Tuple2"
        then
            val (fstType, sndType) =
                ProductCaseClassRepresentation.ProdBuiltinPair.extractPairComponentTypes(tp)
            val fstRepr = lctx.typeGenerator(fstType).defaultDataRepresentation(fstType)
            val sndRepr = lctx.typeGenerator(sndType).defaultDataRepresentation(sndType)
            ProductCaseClassRepresentation.ProdBuiltinPair(fstRepr, sndRepr)
        else
            throw LoweringException(
              s"SumPair should have a pair or tuple type representation, we have ${tp.show}",
              pos
            )
    }

    override def genNil(resType: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValue = {
        val listRepr = defaultListRepresentation(resType, pos)
        val elemType = retrieveElementType(resType, pos)
        val (keyType, valueType) =
            SumCaseClassRepresentation.SumPairBuiltinList.extractKeyValueTypes(elemType)
        val pairRepr = listRepr match
            case SumCaseClassRepresentation.SumPairBuiltinList(keyRepr, valueRepr) =>
                ProductCaseClassRepresentation.ProdBuiltinPair(keyRepr, valueRepr)
            case other =>
                throw LoweringException(
                  s"SumPairBuiltinListSirTypeGenerator.genNil: expected SumPairBuiltinList but got $other",
                  pos
                )
        val elemUni = pairRepr.defaultUni(elemType)
        ConstantLoweredValue(
          SIR.Const(
            scalus.uplc.Constant.List(elemUni, Nil),
            resType,
            AnnotationsDecl(pos)
          ),
          listRepr
        )
    }

}
