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
        val targetGen = lctx.typeGenerator(targetType)
        val targetRepr = targetGen.defaultRepresentation(targetType)
        targetRepr match
            case _: SumCaseClassRepresentation.SumBuiltinList =>
                // Upcasting to list type — convert to Data first
                val asDataConstr =
                    input.toRepresentation(ProductCaseClassRepresentation.ProdDataConstr, pos)
                TypeRepresentationProxyLoweredValue(asDataConstr, targetType, targetRepr, pos)
            case _: SumCaseClassRepresentation.SumUplcConstr =>
                // Upcasting to sum UplcConstr — keep native Constr
                TypeRepresentationProxyLoweredValue(input, targetType, targetRepr, pos)
            case _ =>
                // Default: convert to ProdDataConstr for Data-compatible contexts
                val asDataConstr =
                    input.toRepresentation(ProductCaseClassRepresentation.ProdDataConstr, pos)
                TypeRepresentationProxyLoweredValue(
                  asDataConstr,
                  targetType,
                  SumCaseClassRepresentation.DataConstr,
                  pos
                )
    }

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue =
        ProductCaseSirTypeGenerator.genConstrUplcConstr(constr)

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue =
        // Debug: print info when near the error location (line 428, accessing 'depth')
        val pos = sel.anns.pos
        if pos.startLine == 428 || (sel.field == "depth" && pos.startLine >= 425 && pos.startLine <= 430)
        then
            println(
              s"[DEBUG genSelect at ${pos.startLine}:${pos.startColumn}-${pos.endColumn}] field=${sel.field}"
            )
            println(s"  scrutinee type: ${loweredScrutinee.sirType}")
            println(s"  scrutinee repr: ${loweredScrutinee.representation}")
            println(s"  defaultRepr for type: ${defaultRepresentation(loweredScrutinee.sirType)}")

        val result = loweredScrutinee.representation match
            case _: ProductCaseClassRepresentation.ProdUplcConstr |
                _: SumCaseClassRepresentation.SumUplcConstr =>
                if pos.startLine == 428 || (sel.field == "depth" && pos.startLine >= 425 && pos.startLine <= 430)
                then println(s"  -> Taking ProdUplcConstr branch")
                ProductCaseUplcOnlySirTypeGenerator.genSelect(sel, loweredScrutinee)
            case tvr: TypeVarRepresentation if tvr.isBuiltin =>
                if pos.startLine == 428 || (sel.field == "depth" && pos.startLine >= 425 && pos.startLine <= 430)
                then println(s"  -> Taking TypeVar branch")
                // Transparent TypeVar — value is native Constr at runtime.
                // Resolve to ProdUplcConstr for the concrete type, then use UplcConstr select.
                val pucRepr = defaultRepresentation(loweredScrutinee.sirType)
                val resolved =
                    RepresentationProxyLoweredValue(loweredScrutinee, pucRepr, sel.anns.pos)
                ProductCaseUplcOnlySirTypeGenerator.genSelect(sel, resolved)
            case _ =>
                if pos.startLine == 428 || (sel.field == "depth" && pos.startLine >= 425 && pos.startLine <= 430)
                then println(s"  -> Taking fallback Data branch")
                // Data-based repr (ProdDataConstr, TypeVar(Fixed), etc.) — use Data extraction
                ProductCaseSirTypeGenerator.genSelect(sel, loweredScrutinee)
        result

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
