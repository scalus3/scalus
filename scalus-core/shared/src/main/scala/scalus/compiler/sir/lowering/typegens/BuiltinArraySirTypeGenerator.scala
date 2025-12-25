package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*

/** Type generator for BuiltinArray[Data].
  *
  * BuiltinArray is a native UPLC type that holds Data elements. When used as a field in a case
  * class, it needs to be converted to/from Data representation (Data.List).
  *
  * Default representation: ProductCaseClassRepresentation.ArrayData (native UPLC array) Data
  * representation: ProductCaseClassRepresentation.PackedArrayAsList (Data.List)
  */
object BuiltinArraySirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        ProductCaseClassRepresentation.ArrayData

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        ProductCaseClassRepresentation.PackedArrayAsList

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        ProductCaseClassRepresentation.PackedArrayAsList

    override def isDataSupported(tp: SIRType)(using lctx: LoweringContext): Boolean = true

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, outputRepresentation) match
            // ArrayData -> ArrayData (identity)
            case (
                  ProductCaseClassRepresentation.ArrayData,
                  ProductCaseClassRepresentation.ArrayData
                ) =>
                input

            // Constant <-> ArrayData (interoperability - both represent native UPLC arrays)
            case (PrimitiveRepresentation.Constant, ProductCaseClassRepresentation.ArrayData) =>
                TypeRepresentationProxyLoweredValue(
                  input,
                  input.sirType,
                  ProductCaseClassRepresentation.ArrayData,
                  pos
                )

            case (ProductCaseClassRepresentation.ArrayData, PrimitiveRepresentation.Constant) =>
                TypeRepresentationProxyLoweredValue(
                  input,
                  input.sirType,
                  PrimitiveRepresentation.Constant,
                  pos
                )

            case (PrimitiveRepresentation.Constant, PrimitiveRepresentation.Constant) =>
                input

            // ArrayData/Constant -> PackedArrayAsList (array to Data.List)
            // BuiltinArray[Data] -> Data (as Data.List)
            // Convert by iterating: arr[0], arr[1], ..., arr[n-1] -> mkCons(arr[0], mkCons(arr[1], ...))
            case (
                  ProductCaseClassRepresentation.ArrayData | PrimitiveRepresentation.Constant,
                  ProductCaseClassRepresentation.PackedArrayAsList
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
                  ProductCaseClassRepresentation.PackedArrayAsList,
                  pos
                )

            // PackedArrayAsList -> ArrayData/Constant (Data.List to array via unListData + listToArray)
            case (
                  ProductCaseClassRepresentation.PackedArrayAsList,
                  ProductCaseClassRepresentation.ArrayData | PrimitiveRepresentation.Constant
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
                  outputRepresentation,
                  pos
                )

            // PackedArrayAsList -> PackedArrayAsList (identity)
            case (
                  ProductCaseClassRepresentation.PackedArrayAsList,
                  ProductCaseClassRepresentation.PackedArrayAsList
                ) =>
                input

            // TypeVar handling
            case (_, tv @ TypeVarRepresentation(isBuiltin)) =>
                if isBuiltin then
                    val r0 = input.toRepresentation(ProductCaseClassRepresentation.ArrayData, pos)
                    RepresentationProxyLoweredValue(r0, tv, pos)
                else
                    val r1 =
                        input.toRepresentation(
                          ProductCaseClassRepresentation.PackedArrayAsList,
                          pos
                        )
                    new RepresentationProxyLoweredValue(r1, tv, pos)

            case (TypeVarRepresentation(isBuiltin), _) =>
                if isBuiltin then
                    val r0 =
                        RepresentationProxyLoweredValue(
                          input,
                          ProductCaseClassRepresentation.ArrayData,
                          pos
                        )
                    r0.toRepresentation(outputRepresentation, pos)
                else
                    val r0 = RepresentationProxyLoweredValue(
                      input,
                      ProductCaseClassRepresentation.PackedArrayAsList,
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
