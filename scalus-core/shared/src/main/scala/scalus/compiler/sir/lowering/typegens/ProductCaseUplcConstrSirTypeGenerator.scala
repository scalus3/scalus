package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.*

/** Type generator for product case classes annotated with @UplcRepr(UplcConstr).
  *
  * Like ProductCaseUplcOnlySirTypeGenerator but for Data-compatible types:
  *   - canBeConvertedToData = true
  *   - defaultDataRepresentation returns DataConstr
  *   - defaultTypeVarRepresentation returns DataConstr
  */
object ProductCaseUplcConstrSirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation = {
        val constrIndex = ProductCaseSirTypeGenerator.retrieveConstrIndex(tp, SIRPosition.empty)
        val constrDecl = ProductCaseSirTypeGenerator.retrieveConstrDecl(tp, SIRPosition.empty)
        val fieldReprs = constrDecl.params.map { param =>
            val paramType = lctx.resolveTypeVarIfNeeded(param.tp)
            // Check for field-level @UplcRepr annotation override
            SirTypeUplcGenerator
                .resolveFieldRepr(param, paramType)
                .getOrElse(
                  lctx.typeGenerator(paramType).defaultRepresentation(paramType)
                )
        }
        ProductCaseClassRepresentation.ProdUplcConstr(constrIndex, fieldReprs)
    }

    override def defaultDataRepresentation(
        tp: SIRType
    )(using LoweringContext): LoweredValueRepresentation =
        ProductCaseClassRepresentation.ProdDataConstr

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation =
        ProductCaseClassRepresentation.ProdDataConstr

    override def canBeConvertedToData(tp: SIRType)(using LoweringContext): Boolean = true

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        if input.representation.isCompatibleOn(input.sirType, outputRepresentation, pos) then
            if input.representation == outputRepresentation then input
            else RepresentationProxyLoweredValue(input, outputRepresentation, pos)
        else
            val resolved = input.representation match
                case tvr: TypeVarRepresentation =>
                    import SIRType.TypeVarKind.*
                    tvr.kind match
                        case Transparent =>
                            // Pure relabel: Transparent bytes are unknown/passthrough.
                            val pucRepr = defaultRepresentation(input.sirType)
                            RepresentationProxyLoweredValue(input, pucRepr, pos)
                        case Unwrapped =>
                            // Bytes ARE in concrete-default form (Unwrapped's invariant).
                            // Use input.toRepresentation so the matrix decides proxy vs
                            // actual conversion if needed downstream.
                            input.toRepresentation(defaultRepresentation(input.sirType), pos)
                        case Fixed =>
                            // Bytes are Data-wrapped — go via defaultTypeVarReperesentation,
                            // which is the Data form for this product type.
                            input.toRepresentation(
                              defaultTypeVarReperesentation(input.sirType),
                              pos
                            )
                case _ => input
            ProductCaseSirTypeGenerator.toRepresentation(resolved, outputRepresentation, pos)
    }

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValue = {
        // Structural upcast from a ProdUplcConstr variant to its parent sum: at the UPLC
        // level the bytes are already `Constr(tag, fields)`, which is exactly the shape of a
        // `SumUplcConstr`. Build the target `SumUplcConstr` repr and relabel — no Data round
        // trip is needed, and none would be safe in isolation (abstract TypeVar fields cannot
        // be Data-encoded without concrete type info).
        val targetSumRepr =
            typegens.SumUplcConstrSirTypeGenerator.buildSumUplcConstr(targetType)
        TypeRepresentationProxyLoweredValue(input, targetType, targetSumRepr, pos)
    }

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue =
        ProductCaseSirTypeGenerator.genConstrUplcConstr(constr)

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue =
        loweredScrutinee.representation match
            case _: ProductCaseClassRepresentation.ProdUplcConstr |
                _: SumCaseClassRepresentation.SumUplcConstr =>
                ProductCaseUplcOnlySirTypeGenerator.genSelect(sel, loweredScrutinee)
            case tvr: TypeVarRepresentation if tvr.isBuiltin =>
                // Transparent TypeVar — value is native Constr at runtime.
                // Resolve to ProdUplcConstr for the concrete type, then use UplcConstr select.
                val pucRepr = defaultRepresentation(loweredScrutinee.sirType)
                val resolved =
                    RepresentationProxyLoweredValue(loweredScrutinee, pucRepr, sel.anns.pos)
                ProductCaseUplcOnlySirTypeGenerator.genSelect(sel, resolved)
            case _ =>
                // Data-based repr (ProdDataConstr, TypeVar(Fixed), etc.) — use Data extraction
                ProductCaseSirTypeGenerator.genSelect(sel, loweredScrutinee)

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using LoweringContext): LoweredValue =
        loweredScrutinee.representation match
            case _: ProductCaseClassRepresentation.ProdUplcConstr |
                _: SumCaseClassRepresentation.SumUplcConstr =>
                SumUplcConstrSirTypeGenerator.genMatchUplcConstr(
                  matchData,
                  loweredScrutinee,
                  optTargetType
                )
            case _ =>
                // Data-based repr — delegate to SumCaseSirTypeGenerator for DataConstr matching
                SumCaseSirTypeGenerator.genMatch(matchData, loweredScrutinee, optTargetType)

}
