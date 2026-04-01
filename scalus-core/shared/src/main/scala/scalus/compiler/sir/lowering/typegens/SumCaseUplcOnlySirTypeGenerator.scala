package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.*

object SumCaseUplcOnlySirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.UplcConstr

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.UplcConstr

    // TODO: set position in LoweringContext
    override def defaultTypeVarReperesentation(
        tp: SIRType, kind: SIRType.TypeVarKind
    )(using LoweringContext): LoweredValueRepresentation =
        kind match {
            case SIRType.TypeVarKind.DefaultRepresentation => defaultRepresentation(tp)
            case SIRType.TypeVarKind.Transparent =>
                throw LoweringException(s"Transparent TypeVar: ${tp.show}", SIRPosition.empty)
            case SIRType.TypeVarKind.CanBeListAffected =>
                throw LoweringException(
                  "Type variables with lambdas are not supported in sum cases yet",
                  SIRPosition.empty
                )
        }

    override def canBeConvertedToData(tp: SIRType)(using lctx: LoweringContext): Boolean = false

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        SumCaseSirTypeGenerator.toRepresentation(input, representation, pos)
    }

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = ???

    override def genConstr(constr: SIR.Constr)(using lctx: LoweringContext): LoweredValue = {
        throw LoweringException(
          s"SumCaseUplcOnlySirTypeGenerator does not support constructors, got ${constr.name}",
          constr.anns.pos
        )
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"SumCaseUplcOnlySirTypeGenerator does not support select",
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
        ???
    }

}
