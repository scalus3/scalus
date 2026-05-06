package scalus.compiler.sir.lowering

import scalus.compiler.sir.*

/** Dispatch layer for product-typed operations.
  *
  * Phase 0: pure forwarder that delegates to `lctx.typeGenerator(...)`. Later phases
  * centralize the (sourceRepr, targetRepr) toRepresentation case table, the
  * upcast-preserve decisions, and field-select handling here — see
  * `docs/local/claude/compiler/sum-prod-dispatch-design.md`.
  */
object ProdDispatch {

    def toRepresentation(
        input: LoweredValue,
        target: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue =
        lctx.typeGenerator(input.sirType).toRepresentation(input, target, pos)

    def genConstr(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue =
        lctx.typeGenerator(lctx.resolveTypeVarIfNeeded(constr.tp))
            .genConstrLowered(constr, loweredArgs, optTargetType)

    def genSelect(
        sel: SIR.Select,
        loweredScrutinee: LoweredValue
    )(using lctx: LoweringContext): LoweredValue =
        lctx.typeGenerator(loweredScrutinee.sirType).genSelect(sel, loweredScrutinee)

    def upcastOne(
        input: LoweredValue,
        targetType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue =
        lctx.typeGenerator(input.sirType).upcastOne(input, targetType, pos)

}
