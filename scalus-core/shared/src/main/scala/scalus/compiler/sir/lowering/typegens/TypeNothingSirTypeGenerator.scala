package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.*

/** throw error on any attempt.
  */
object TypeNothingSirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        ErrorRepresentation

    override def defaultDataRepresentation(
        tp: SIRType
    )(using LoweringContext): LoweredValueRepresentation =
        throw IllegalStateException(
          "Can't ask defaultDataRepresentation access Nothing type generator"
        )

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation =
        ErrorRepresentation

    override def isDataSupported(tp: SIRType)(using LoweringContext): Boolean = false

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue =
        input

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        input
    }

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue = {
        throw LoweringException(
          s"TypeNothingSirTypeGenerator does not support constructors, got ${constr.name}",
          constr.anns.pos
        )
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"TypeNothingSirTypeGenerator does not support select",
          sel.anns.pos
        )
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"TypeNothingSirTypeGenerator does not support match",
          matchData.anns.pos
        )
    }

}
