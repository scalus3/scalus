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
                  SirTypeUplcGenerator.defaultRepresentation(paramType)
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
    )(using lctx: LoweringContext): LoweredValue =
        ProdDispatch.dispatcherBypass("ProductCaseUplcConstrSirTypeGenerator")

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValue = {
        // Structural upcast from a ProdUplcConstr variant to its parent sum: at the UPLC
        // level the bytes are already `Constr(tag, fields)`, which is exactly the shape of a
        // `SumUplcConstr` — but only when `input` is actually in UC form. If it has been
        // converted to Data (e.g. via `toDefaultTypeVarRepr` or a passthrough wrap), a pure
        // relabel would lie about the byte layout: downstream `genSelect`/`genMatch` on the
        // upcasted value would emit native-UC selectors against Data.Constr bytes, surfacing
        // as `MultiplyInteger Apply LamAbs` runtime crashes (4-arg case branch on a 2-field
        // CEK Data.Constr scrutinee). Mirror `SumCaseUplcConstrSirTypeGenerator.upcastOne`:
        // only relabel when input is already UC-shaped; otherwise convert to UC first.
        //
        // We deliberately use the un-overlaid `buildSumUplcConstr(targetType)` rather than
        // `SumDispatch.chooseUpcastOutputRepr`. The overlay would leak per-variant defaults
        // (which carry `TypeVar(Fixed)` field reprs from the DataDecl) into the
        // `SumUplcConstrEmitter.emitConvert` `dataListVar → variant fields`
        // path, surfacing as `Fixed → Unwrapped` aborts.
        input.representation match
            case _: ProductCaseClassRepresentation.ProdUplcConstr |
                _: SumCaseClassRepresentation.SumUplcConstr =>
                val targetSumRepr =
                    typegens.SumUplcConstrEmitter.buildSumUplcConstr(targetType)
                TypeRepresentationProxyLoweredValue(input, targetType, targetSumRepr, pos)
            case _ =>
                val ucRepr = defaultRepresentation(input.sirType)
                val converted = input.toRepresentation(ucRepr, pos)
                val targetSumRepr =
                    typegens.SumUplcConstrEmitter.buildSumUplcConstr(targetType)
                TypeRepresentationProxyLoweredValue(converted, targetType, targetSumRepr, pos)
    }

    override def genConstrLowered(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using LoweringContext): LoweredValue =
        ProdUplcConstrEmitter.genConstr(constr, loweredArgs)

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
        ProdDispatch.genMatch(matchData, loweredScrutinee, optTargetType)

    /** Outbound conversions from a `@UplcRepr(UplcConstr)` product (Phase 5). Resolves TypeVar
      * inputs to the type's concrete repr, then delegates to
      * `ProductCaseSirTypeGenerator.emitConvert` for the actual conversion.
      */
    def emitConvert(
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
                            val pucRepr = defaultRepresentation(input.sirType)
                            RepresentationProxyLoweredValue(input, pucRepr, pos)
                        case Unwrapped =>
                            input.toRepresentation(defaultRepresentation(input.sirType), pos)
                        case Fixed =>
                            input.toRepresentation(
                              defaultTypeVarReperesentation(input.sirType),
                              pos
                            )
                case _ => input
            ProductCaseSirTypeGenerator.emitConvert(resolved, outputRepresentation, pos)
    }

}
