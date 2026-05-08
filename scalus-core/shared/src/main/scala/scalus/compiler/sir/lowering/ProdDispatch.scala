package scalus.compiler.sir.lowering

import scalus.compiler.sir.*
import scalus.compiler.sir.lowering.typegens.*

/** Dispatch layer for product-typed operations. Single entry point for Prod-side
  * `toRepresentation`; per-typegen overrides throw `dispatcherBypass` to surface any caller that
  * bypassed this object. See `docs/local/claude/compiler/sum-prod-dispatch-design.md`.
  */
object ProdDispatch {

    import ProductCaseClassRepresentation.*

    /** Throw used by Prod typegens' `toRepresentation` overrides to assert that dispatch goes
      * through this object.
      */
    def dispatcherBypass(genName: String): Nothing =
        throw new IllegalStateException(
          s"$genName.toRepresentation called directly — dispatch via ProdDispatch.toRepresentation"
        )

    def toRepresentation(
        input: LoweredValue,
        target: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val gen = typegens.SirTypeUplcGenerator(input.sirType)
        gen match
            case ProductCaseSirTypeGenerator =>
                ProductCaseSirTypeGenerator.emitConvert(input, target, pos)
            case ProductCaseUplcConstrSirTypeGenerator =>
                ProductCaseUplcConstrSirTypeGenerator.emitConvert(input, target, pos)
            case ProductCaseUplcOnlySirTypeGenerator =>
                // Original ProductCaseUplcOnly body: identity → input, otherwise delegate to ProdCase.
                if input.representation == target then input
                else ProductCaseSirTypeGenerator.emitConvert(input, target, pos)
            case oneElement: OneElementWrapperEmitter =>
                oneElement.emitConvert(input, target, pos)
            case _ =>
                gen.toRepresentation(input, target, pos)
    }

    def genConstr(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        val gen = SumDispatch.chooseConstrOutputRepr(constr, loweredArgs, optTargetType)
        gen.genConstrLowered(constr, loweredArgs, optTargetType)
    }

    def genSelect(
        sel: SIR.Select,
        loweredScrutinee: LoweredValue
    )(using lctx: LoweringContext): LoweredValue =
        typegens.SirTypeUplcGenerator.genSelect(sel, loweredScrutinee)

    /** Representation-aware dispatch for product-typed `genMatch`. Mirror of
      * `SumDispatch.genMatch`:
      *
      *   - `(Prod|Sum)UplcConstr` → tag-ordered Case via `genMatchUplcConstr`.
      *   - `ProdDataList` / `ProdDataConstr` / `PackedDataList` / `PairIntDataList` →
      *     `ProdDataListEmitter.genMatch` (Data-shape extraction via `unConstrData` +
      *     `headList`/`tailList`, or just field projection).
      *   - `ProdBuiltinPair(_, _)` → `ProdBuiltinPairEmitter.genMatch` (`Case` on Pair for V4+,
      *     `fstPair`/`sndPair` for V1-V3).
      *   - `OneElementWrapper(_)` → fall through to the per-type `OneElementWrapperEmitter`
      *     instance — its `genMatch` captures argType-specific binding extraction.
      *   - `TypeVarRepresentation(_)` → relabel to the type's `defaultTypeVarRepresentation` and
      *     recurse.
      *   - everything else → fall back to the type-keyed typegen's `genMatch`.
      *
      * Pre-Phase-4c-step-2 this dispatch was inlined in
      * `ProductCaseUplcConstrSirTypeGenerator.genMatch` and `ProductCaseSirTypeGenerator.genMatch`;
      * consolidating it here mirrors Phase 4a on the Sum side.
      */
    def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        loweredScrutinee.representation match
            case _: ProdUplcConstr | _: SumCaseClassRepresentation.SumUplcConstr =>
                typegens.SumUplcConstrEmitter
                    .genMatchUplcConstr(matchData, loweredScrutinee, optTargetType)
            case ProdDataList | ProdDataConstr | PackedDataList | PairIntDataList =>
                typegens.ProdDataListEmitter
                    .genMatch(matchData, loweredScrutinee, optTargetType)
            case _: ProdBuiltinPair =>
                typegens.ProdBuiltinPairEmitter
                    .genMatch(matchData, loweredScrutinee, optTargetType)
            case _: OneElementWrapper =>
                typegens.SirTypeUplcGenerator
                    .genMatch(matchData, loweredScrutinee, optTargetType)
            case TypeVarRepresentation(_) =>
                val properRepresentation =
                    typegens.SirTypeUplcGenerator
                        .defaultTypeVarReperesentation(loweredScrutinee.sirType)
                val scrutineeWithProperRepr = TypeRepresentationProxyLoweredValue(
                  loweredScrutinee,
                  loweredScrutinee.sirType,
                  properRepresentation,
                  matchData.anns.pos
                )
                genMatch(matchData, scrutineeWithProperRepr, optTargetType)
            case _ =>
                typegens.SirTypeUplcGenerator
                    .genMatch(matchData, loweredScrutinee, optTargetType)
    }

    def upcastOne(
        input: LoweredValue,
        targetType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue =
        typegens.SirTypeUplcGenerator.upcastOne(input, targetType, pos)

}
