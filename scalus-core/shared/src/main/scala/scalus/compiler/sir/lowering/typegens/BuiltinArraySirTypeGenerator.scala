package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*

/** Type generator for BuiltinArray[Data].
  *
  * BuiltinArray is a native UPLC type that holds Data elements. When used as a field in a case
  * class, it needs to be converted to/from Data representation (Data.List).
  *
  * Default representation: PrimitiveRepresentation.Constant (native UPLC array) Data
  * representation: SumCaseClassRepresentation.PackedSumDataList (Data.List)
  */
object BuiltinArraySirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        PrimitiveRepresentation.Constant

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.PackedSumDataList

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.PackedSumDataList

    override def isDataSupported(tp: SIRType)(using lctx: LoweringContext): Boolean = true

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, outputRepresentation) match
            // Constant -> Constant (identity)
            case (PrimitiveRepresentation.Constant, PrimitiveRepresentation.Constant) =>
                input

            // Constant -> PackedSumDataList (array to Data.List)
            // BuiltinArray[Data] -> Data (as Data.List)
            // Convert by iterating: arr[0], arr[1], ..., arr[n-1] -> mkCons(arr[0], mkCons(arr[1], ...))
            case (
                  PrimitiveRepresentation.Constant,
                  SumCaseClassRepresentation.PackedSumDataList
                ) =>
                // Use runtime helper to convert array to list
                val asList = lvApply(
                  ScalusRuntime.arrayToList,
                  input,
                  pos,
                  Some(SIRType.BuiltinList(SIRType.Data.tp)),
                  Some(SumCaseClassRepresentation.SumDataList)
                )
                lvBuiltinApply(
                  SIRBuiltins.listData,
                  asList,
                  SIRType.Data.tp,
                  SumCaseClassRepresentation.PackedSumDataList,
                  pos
                )

            // PackedSumDataList -> Constant (Data.List to array via unListData + listToArray)
            case (
                  SumCaseClassRepresentation.PackedSumDataList,
                  PrimitiveRepresentation.Constant
                ) =>
                // Unwrap Data.List to list, then convert to array
                val asList = lvBuiltinApply(
                  SIRBuiltins.unListData,
                  input,
                  SIRType.BuiltinList(SIRType.Data.tp),
                  SumCaseClassRepresentation.SumDataList,
                  pos
                )
                lvBuiltinApply(
                  SIRBuiltins.listToArray,
                  asList,
                  input.sirType,
                  PrimitiveRepresentation.Constant,
                  pos
                )

            // PackedSumDataList -> PackedSumDataList (identity)
            case (
                  SumCaseClassRepresentation.PackedSumDataList,
                  SumCaseClassRepresentation.PackedSumDataList
                ) =>
                input

            // TypeVar handling
            case (_, tv @ TypeVarRepresentation(isBuiltin)) =>
                if isBuiltin then input
                else {
                    val inputAsData =
                        input.toRepresentation(SumCaseClassRepresentation.PackedSumDataList, pos)
                    new RepresentationProxyLoweredValue(inputAsData, tv, pos)
                }

            case (TypeVarRepresentation(isBuiltin), _) =>
                if isBuiltin then RepresentationProxyLoweredValue(input, outputRepresentation, pos)
                else if input.representation == outputRepresentation then input
                else
                    val r0 = RepresentationProxyLoweredValue(
                      input,
                      SumCaseClassRepresentation.PackedSumDataList,
                      pos
                    )
                    r0.toRepresentation(outputRepresentation, pos)

            case _ =>
                throw LoweringException(
                  s"Unexpected representation conversion for BuiltinArray from ${input.representation} to ${outputRepresentation}",
                  pos
                )
    }

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        // BuiltinArray doesn't have subtypes, so upcast is just type alignment
        TypeRepresentationProxyLoweredValue(input, targetType, input.representation, pos)
    }

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue = {
        throw LoweringException(
          s"BuiltinArray cannot be constructed via Constr pattern - use BuiltinArray(...) or listToArray",
          constr.anns.pos
        )
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"BuiltinArray does not support field selection - use indexArray or lengthOfArray builtins",
          sel.anns.pos
        )
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using LoweringContext): LoweredValue = {
        throw LoweringException(
          s"BuiltinArray does not support pattern matching",
          matchData.anns.pos
        )
    }
}
