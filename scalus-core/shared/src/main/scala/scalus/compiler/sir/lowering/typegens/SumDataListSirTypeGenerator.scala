package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.sir.*

/** Internal representation - Plutus List, element type should be data-compatibe List[E] when E is
  * data-compatible type is mapped to this type.
  */
object SumDataListSirTypeGenerator extends SumListCommonSirTypeGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation = {
        // TODO: pass position
        SumCaseClassRepresentation.SumDataList
    }

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.PackedSumDataList

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.PackedSumDataList

    override def defaultListRepresentation(using LoweringContext): LoweredValueRepresentation =
        SumCaseClassRepresentation.SumDataList

    override def defaultElementRepresentation(tp: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation = {
        lctx.typeGenerator(tp).defaultDataRepresentation(tp)
    }

    override def genNil(resType: SIRType, pos: SIRPosition)(using LoweringContext): LoweredValue = {
        lvBuiltinApply0(
          SIRBuiltins.mkNilData,
          SIRType.List.Nil,
          SumCaseClassRepresentation.SumDataList,
          pos
        )
    }

    // def uplcToData(input: Term): Term = {
    //    DefaultFun.ListData.tpf $ input
    // }

    // def dataToUplc(input: Term): Term =
    //    DefaultFun.UnListData.tpf $ input

}
