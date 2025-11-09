package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.*

object UnitSirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        PrimitiveRepresentation.Constant

    override def defaultDataRepresentation(
        tp: SIRType
    )(using LoweringContext): LoweredValueRepresentation =
        throw IllegalStateException(
          s"UnitSirTypeGenerator can't be used for type ${tp.show}",
        )

    override def defaultTypeVarReperesentation(
        tp: SIRType
    )(using lctx: LoweringContext): LoweredValueRepresentation =
        PrimitiveRepresentation.Constant

    override def isDataSupported(tp: SIRType)(using LoweringContext): Boolean = false

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, outputRepresentation) match
            case (PrimitiveRepresentation.Constant, PrimitiveRepresentation.Constant) =>
                input
            case (TypeVarRepresentation(isBuiltin), PrimitiveRepresentation.Constant) =>
                input
            case (PrimitiveRepresentation.Constant, TypeVarRepresentation(isBuiltin)) =>
                input
            case (ErrorRepresentation, _) =>
                // error can be converted to any representatio
                input
            case _ =>
                throw LoweringException(
                  s"Unit type can't be converted from ${input.representation} to $outputRepresentation",
                  pos
                )

    }

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue =
        throw LoweringException(
          s"Unit type can't be upcasted to $targetType",
          pos
        )

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue =
        throw LoweringException(
          s"Unit type can't be used in constr, but got $constr",
          constr.anns.pos
        )

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue =
        throw LoweringException(
          s"Unit type can't be used in select, but got $sel",
          sel.anns.pos
        )

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        LoweringContext
    ): LoweredValue =
        throw LoweringException(
          s"Unit type can't be used in match, but got $matchData",
          matchData.anns.pos
        )

}
