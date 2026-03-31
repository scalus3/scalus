package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.*

object SumCaseUplcOnlySirTypeGenerator extends SirTypeUplcGenerator {

    /** Build SumUplcConstr with proper variant info from the type's DataDecl. */
    private def buildSumUplcConstr(tp: SIRType)(using
        lctx: LoweringContext
    ): SumCaseClassRepresentation.SumUplcConstr = {
        val (constructors, typeArgs) = tp match
            case SIRType.SumCaseClass(decl, tArgs) => (decl.constructors, tArgs)
            case SIRType.CaseClass(cd, tArgs, Some(parent)) =>
                parent match
                    case SIRType.SumCaseClass(decl, pArgs) => (decl.constructors, pArgs)
                    case _                                 => (scala.List(cd), tArgs)
            case SIRType.CaseClass(cd, tArgs, None) => (scala.List(cd), tArgs)
            case SIRType.TypeLambda(_, body)        => return buildSumUplcConstr(body)
            case SIRType.TypeProxy(ref)             => return buildSumUplcConstr(ref)
            case _ => return SumCaseClassRepresentation.SumUplcConstr(Map.empty)

        val variants = constructors.zipWithIndex.map { (constrDecl, idx) =>
            val fieldReprs = constrDecl.params.map { param =>
                val paramType = lctx.resolveTypeVarIfNeeded(param.tp)
                lctx.typeGenerator(paramType).defaultRepresentation(paramType)
            }
            idx -> ProductCaseClassRepresentation.ProdUplcConstr(idx, fieldReprs)
        }.toMap
        SumCaseClassRepresentation.SumUplcConstr(variants)
    }

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        buildSumUplcConstr(tp)

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        buildSumUplcConstr(tp)

    // TODO: set position in LoweringContext
    override def defaultTypeVarReperesentation(
        tp: SIRType
    )(using LoweringContext): LoweredValueRepresentation =
        throw LoweringException(
          "Type variables with lambdas are not supported in sum cases yet",
          SIRPosition.empty
        )

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
        ProductCaseSirTypeGenerator.genConstrUplcConstr(constr)
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
        SumCaseSirTypeGenerator.genMatchUplcConstr(matchData, loweredScrutinee, optTargetType)
    }

}
