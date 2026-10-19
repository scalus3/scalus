package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.*

/** Type generator for sum types (enums) annotated with @UplcRepr(UplcConstr).
  *
  * Like SumCaseUplcConstrOnlyEmitter but for Data-compatible types:
  *   - canBeConvertedToData = true
  *   - defaultDataRepresentation returns DataConstr
  *   - defaultTypeVarRepresentation returns DataConstr
  */
object SumCaseUplcConstrEmitter extends SumCaseUplcConstrCommon {

    private def isListType(tp: SIRType): Boolean =
        SIRType.retrieveDataDecl(tp) match
            case Right(decl) =>
                decl.name == SIRType.List.dataDecl.name ||
                decl.name == SIRType.BuiltinList.dataDecl.name
            case _ => false

    override def defaultDataRepresentation(tp: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation =
        if isListType(tp) then SumCaseClassRepresentation.PackedSumDataList
        else SumCaseClassRepresentation.DataConstr

    override def defaultTypeVarReperesentation(
        tp: SIRType
    )(using lctx: LoweringContext): LoweredValueRepresentation =
        if isListType(tp) then SumCaseClassRepresentation.PackedSumDataList
        else SumCaseClassRepresentation.DataConstr

    override def canBeConvertedToData(tp: SIRType)(using lctx: LoweringContext): Boolean = true

    /** Data-shaped fallback: route via the target sum's matching variant by `constrIndex`, then
      * recurse so the base's `ProdUplcConstr` arm wraps it into a single-entry `SumUplcConstr`.
      */
    override protected def upcastOneOther(
        input: LoweredValue,
        targetType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val targetSum = SumUplcConstrOps.buildSumUplcConstr(targetType)
        val constrIndex =
            ProductCaseEmitter.retrieveConstrIndex(input.sirType, pos)
        targetSum.variants.get(constrIndex) match
            case Some(targetProd) =>
                val converted = input.toRepresentation(targetProd, pos)
                upcastOne(converted, targetType, pos)
            case None =>
                throw LoweringException(
                  s"SumCaseUplcConstrEmitter.upcastOne: variant $constrIndex not found in target ${targetType.show}",
                  pos
                )
    }

    override def genConstrLowered(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue =
        ProdUplcConstrOps.genConstr(withCaseClassTp(constr), loweredArgs)

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        ProductCaseUplcConstrOnlyEmitter.genSelect(sel, loweredScrutinee)
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        // SIR type is `@UplcRepr(UplcConstr)`-annotated sum, but the lowered value may
        // still carry a non-UplcConstr repr (e.g. elements produced before the
        // annotation was picked up during `elementReprFor` inference). `SumDispatch.
        // genMatch` does repr-driven dispatch — UplcConstr / SumBuiltinList /
        // PackedSumDataList / DataConstr / TypeVar are all handled there. For any
        // other repr we coerce to this type's default (a `SumUplcConstr`) so the
        // dispatcher's UplcConstr arm fires. This avoids the typegen-direct calls
        // that the previous implementation issued (mirroring `Term.Case` branch
        // order to `genMatchUplcConstr`).
        val effectiveScrutinee = loweredScrutinee.representation match
            case _: SumCaseClassRepresentation.SumUplcConstr |
                _: SumCaseClassRepresentation.SumBuiltinList |
                SumCaseClassRepresentation.PackedSumDataList |
                SumCaseClassRepresentation.DataConstr =>
                loweredScrutinee
            case _ =>
                val targetRepr = defaultRepresentation(loweredScrutinee.sirType)
                loweredScrutinee.toRepresentation(targetRepr, matchData.anns.pos)
        SumDispatch.genMatch(matchData, effectiveScrutinee, optTargetType)
    }

}
