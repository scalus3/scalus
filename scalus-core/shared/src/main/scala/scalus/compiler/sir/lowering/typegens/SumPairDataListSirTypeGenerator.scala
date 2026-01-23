package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*

/** List(BuiltinPair(List,List))
  */
object SumPairDataListSirTypeGenerator extends SumListCommonSirTypeGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation = {
        SumCaseClassRepresentation.SumDataPairList
    }

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation = {
        SumCaseClassRepresentation.SumDataAssocMap
    }

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation = {
        SumCaseClassRepresentation.SumDataAssocMap
    }

    override def defaultListRepresentation(using LoweringContext): LoweredValueRepresentation = {
        SumCaseClassRepresentation.SumDataPairList
    }

    override def defaultElementRepresentation(tp: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation = {
        val constrDecl = SIRType
            .retrieveConstrDecl(tp)
            .getOrElse(
              throw LoweringException(
                s"SumPair shoul have a pari or tuple type representation, we have  ${tp.show}",
                pos
              )
            )
        if constrDecl.name == "scalus.uplc.builtin.BuiltinPair" || constrDecl.name == "scala.Tuple2"
        then ProductCaseClassRepresentation.PairData
        else
            throw LoweringException(
              s"SumPair shoul have a pair or tuple type representation, we have  ${tp.show}",
              pos
            )
    }

    override def genNil(resType: SIRType, pos: SIRPosition)(using LoweringContext): LoweredValue = {
        lvBuiltinApply0(
          SIRBuiltins.mkNilPairData,
          resType,
          SumCaseClassRepresentation.SumDataPairList,
          pos
        )

    }

}
