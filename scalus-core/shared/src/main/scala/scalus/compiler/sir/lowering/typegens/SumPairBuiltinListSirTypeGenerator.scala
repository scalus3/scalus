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
        then ProductCaseClassRepresentation.PairData
        else
            throw LoweringException(
              s"SumPair should have a pair or tuple type representation, we have ${tp.show}",
              pos
            )
    }

    override def genNil(resType: SIRType, pos: SIRPosition)(using LoweringContext): LoweredValue =
        lvPairDataNil(pos, resType, defaultListRepresentation(resType, pos))

}
