package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.*

/** Type generator for product case classes that cannot be converted to Data
  * (contain functions or BLS elements). Uses UplcConstr representation.
  */
object ProductCaseUplcOnlySirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation = {
        val constrIndex = ProductCaseSirTypeGenerator.retrieveConstrIndex(tp, SIRPosition.empty)
        val constrDecl = ProductCaseSirTypeGenerator.retrieveConstrDecl(tp, SIRPosition.empty)
        val fieldReprs = constrDecl.params.map { param =>
            val paramType = lctx.resolveTypeVarIfNeeded(param.tp)
            lctx.typeGenerator(paramType).defaultRepresentation(paramType)
        }
        ProductCaseClassRepresentation.ProdUplcConstr(constrIndex, fieldReprs)
    }

    override def defaultDataRepresentation(
        tp: SIRType
    )(using LoweringContext): LoweredValueRepresentation =
        throw IllegalStateException(
          "Can't ask defaultDataRepresentation for product case class with non-Data fields"
        )

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation = defaultRepresentation(tp)

    override def canBeConvertedToData(tp: SIRType)(using LoweringContext): Boolean = false

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        if input.representation == outputRepresentation then input
        else
            ProductCaseSirTypeGenerator.toRepresentation(input, outputRepresentation, pos)
    }

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        TypeRepresentationProxyLoweredValue(input, targetType, input.representation, pos)
    }

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue =
        ProductCaseSirTypeGenerator.genConstrUplcConstr(constr)

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"ProductCaseUplcOnlySirTypeGenerator does not support select yet",
          sel.anns.pos
        )
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using LoweringContext): LoweredValue = {
        SumCaseSirTypeGenerator.genMatchUplcConstr(matchData, loweredScrutinee, optTargetType)
    }

}
