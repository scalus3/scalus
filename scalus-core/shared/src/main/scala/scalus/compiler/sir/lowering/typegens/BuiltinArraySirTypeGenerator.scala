package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*

/** Type generator for BuiltinArray.
  *
  * BuiltinArray is a native UPLC type that holds elements. When used as a field in a case class, it
  * needs to be converted to/from Data representation (Data.List).
  *
  * Default representation: ProdBuiltinArray(elementRepr) (native UPLC array) Data representation:
  * PackedArrayAsList (Data.List)
  */
object BuiltinArraySirTypeGenerator extends SirTypeUplcGenerator {

    private def extractElemType(tp: SIRType): SIRType =
        ProductCaseClassRepresentation.ProdBuiltinArray
            .extractElementType(tp)
            .getOrElse(SIRType.Data.tp)

    private def arrayRepr(tp: SIRType)(using
        lctx: LoweringContext
    ): ProductCaseClassRepresentation.ProdBuiltinArray = {
        val elemType = extractElemType(tp)
        val elemRepr = lctx.typeGenerator(elemType).defaultRepresentation(elemType)
        ProductCaseClassRepresentation.ProdBuiltinArray(elemRepr)
    }

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        arrayRepr(tp)

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        ProductCaseClassRepresentation.PackedArrayAsList

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation =
        val elemType = extractElemType(tp)
        if lctx.typeGenerator(elemType).canBeConvertedToData(elemType) then
            ProductCaseClassRepresentation.PackedArrayAsList
        else arrayRepr(tp)

    override def canBeConvertedToData(tp: SIRType)(using lctx: LoweringContext): Boolean = {
        val elemType = extractElemType(tp)
        lctx.typeGenerator(elemType).canBeConvertedToData(elemType)
    }

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, outputRepresentation) match
            // ProdBuiltinArray -> ProdBuiltinArray
            case (
                  ProductCaseClassRepresentation.ProdBuiltinArray(inElemRepr),
                  outArr @ ProductCaseClassRepresentation.ProdBuiltinArray(outElemRepr)
                ) =>
                val elemType = extractElemType(input.sirType)
                if inElemRepr.isCompatibleOn(elemType, outElemRepr, pos) then input
                else
                    input
                        .toRepresentation(ProductCaseClassRepresentation.PackedArrayAsList, pos)
                        .toRepresentation(outArr, pos)

            // ProdBuiltinArray -> PackedArrayAsList (array to Data.List)
            case (
                  _: ProductCaseClassRepresentation.ProdBuiltinArray,
                  ProductCaseClassRepresentation.PackedArrayAsList
                ) =>
                // Use runtime helper to convert array to list
                val asList = lvApply(
                  ScalusRuntime.arrayToList,
                  input,
                  pos,
                  Some(SIRType.BuiltinList(SIRType.Data.tp)),
                  Some(
                    SumCaseClassRepresentation.SumBuiltinList(SumCaseClassRepresentation.DataData)
                  )
                )
                lvBuiltinApply(
                  SIRBuiltins.listData,
                  asList,
                  SIRType.Data.tp,
                  ProductCaseClassRepresentation.PackedArrayAsList,
                  pos
                )

            // PackedArrayAsList -> ProdBuiltinArray (Data.List to array via unListData + listToArray)
            case (
                  ProductCaseClassRepresentation.PackedArrayAsList,
                  _: ProductCaseClassRepresentation.ProdBuiltinArray
                ) =>
                // Unwrap Data.List to list, then convert to array
                val asList = lvBuiltinApply(
                  SIRBuiltins.unListData,
                  input,
                  SIRType.BuiltinList(SIRType.Data.tp),
                  SumCaseClassRepresentation.SumBuiltinList(SumCaseClassRepresentation.DataData),
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

            // TypeVar handling — three distinct kinds, each with its own conversion path
            case (_, tv: TypeVarRepresentation) =>
                import SIRType.TypeVarKind.*
                tv.kind match
                    case Transparent =>
                        // Wildcard target — convert to array form first, then relabel
                        val r0 = input.toRepresentation(arrayRepr(input.sirType), pos)
                        RepresentationProxyLoweredValue(r0, tv, pos)
                    case Unwrapped =>
                        // Concrete-default form — convert to defaultRepresentation, then relabel
                        val targetRepr = defaultRepresentation(input.sirType)
                        val r0 = input.toRepresentation(targetRepr, pos)
                        RepresentationProxyLoweredValue(r0, tv, pos)
                    case Fixed =>
                        // Data-wrapped form — convert to defaultTypeVarReperesentation, then relabel
                        val typeVarRepr = defaultTypeVarReperesentation(input.sirType)
                        val r1 = input.toRepresentation(typeVarRepr, pos)
                        new RepresentationProxyLoweredValue(r1, tv, pos)

            case (tv: TypeVarRepresentation, _) =>
                import SIRType.TypeVarKind.*
                tv.kind match
                    case Transparent =>
                        val r0 =
                            RepresentationProxyLoweredValue(input, arrayRepr(input.sirType), pos)
                        r0.toRepresentation(outputRepresentation, pos)
                    case Unwrapped =>
                        val sourceConcrete = defaultRepresentation(input.sirType)
                        val r0 = RepresentationProxyLoweredValue(input, sourceConcrete, pos)
                        r0.toRepresentation(outputRepresentation, pos)
                    case Fixed =>
                        val typeVarRepr = defaultTypeVarReperesentation(input.sirType)
                        val r0 = RepresentationProxyLoweredValue(input, typeVarRepr, pos)
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

    override def genConstrLowered(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using LoweringContext): LoweredValue = {
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
